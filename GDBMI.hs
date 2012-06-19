-- hgdbmi: a Haskell interface to GDB/MI.
-- Copyright (C) 2008 Evan Martin <martine@danga.com>

-- |GDB\/MI lets programs drive GDB.  It can be used, for example, by GDB
-- frontends.  This module wraps attaching GDB to a process and parsing the
-- (surprisingly complicated) GDB\/MI output.

module GDBMI (
  GDB,
  attach,
  detach,

  runCommand,
  GDBCommand(..),
  MIOutput(..),
  MIOOB(..),
  MIResult(..),
  MIKeyVal,
  MIValue(..),

  parse
) where

import System.IO
import System.Posix.IO (createPipe, fdToHandle)
import System.Posix.Types (ProcessID)
import System.Process (runProcess, ProcessHandle)
import Text.ParserCombinators.Parsec hiding (parse)
import qualified Text.ParserCombinators.Parsec as Parsec (parse)

-- |A connection to a GDB process.
data GDB = GDB {
  gdbPid      :: ProcessHandle,
  gdbCommand  :: Handle,
  gdbResponse :: Handle
}

-- |A GDB command.  CLICommand is any command you'd normally type at the GDB
-- prompt.  MICommand are more machine-parsing friendly commands; see the
-- GDB\/MI docs for details.
data GDBCommand = CLICommand String
                | MICommand String  -- ^TODO: expand this to support arguments.

-- |The output of running a GDB command.  Output is a collection of Out-Of-Band
-- messages (such as logging information) and an optional final result.
data MIOutput = MIOutput [MIOOB] (Maybe MIResult) deriving Show

-- |The type of OOB mesages.  (TODO: many of these aren't properly parsed yet.)
data MIOOB =
    MIStatus String  -- ^Contains on-going status information about the progress
                     -- of a slow operation.
  | MIExec String    -- ^Contains asynchronous state change on the target
                     -- (stopped, started, disappeared).
  | MINotify String  -- ^Contains supplementary information that the client
                     -- should handle (e.g., a new breakpoint information).
  | MIConsole String -- ^Output that should be displayed as is in the console.
                     -- It is the textual response to a CLI command.
  | MITarget String  -- ^The output produced by the target program.
  | MILog String     -- ^Output text coming from GDB's internals, for instance
                     -- messages that should be displayed as part of an error
                     -- log.
  deriving (Eq, Show)

-- |The type of the GDB result.  (TODO: many result types aren't implemented
-- yet.)
data MIResult =
    MIDone [MIKeyVal] -- ^The synchronous operation was successful,
                      -- along with potential key-value return data.
  | MIError String    -- ^The operation failed. The string contains the
                      -- corresponding error message. 
  deriving (Eq, Show)

-- |A key-value pair output from GDB.
type MIKeyVal = (String, MIValue)

-- |The type of a GDB "value", used in the output of structured data.
data MIValue =
    MIString String
  | MITuple [MIKeyVal]
  deriving (Eq, Show)

-- |Attach to a process, returning an error or the 'GDB' connection and its
-- initial output.
attach :: Maybe FilePath  -- ^Working directory for GDB.  (Important if the
                          -- process has loaded libraries from relative paths.)
       -> ProcessID
       -> IO (Either String (GDB, MIOutput))
attach workdir pid = do
  (commandR,  commandW)  <- createPipe >>= asHandles
  (responseR, responseW) <- createPipe >>= asHandles
  phandle <- runProcess "gdb" ["--interpreter", "mi", "-p", show pid]
                 workdir Nothing{-env-}
                 (Just commandR)   -- stdin
                 (Just responseW)  -- stdout
                 Nothing                        -- stderr
  mapM_ (`hSetBuffering` LineBuffering) [commandW, responseR]
  let gdb = GDB phandle commandW responseR
  resp <- readResponse gdb
  case resp of
    Left err -> return $ Left err
    Right ok -> return $ Right (gdb, ok)
  where
  asHandles (f1, f2) = do
    h1 <- fdToHandle f1; h2 <- fdToHandle f2; return (h1, h2)

-- |Close a 'GDB' connection.
detach :: GDB -> IO ()
-- TODO: we don't examine the result code, because our parser wants each
-- response to be terminated by the "(gdb) " prompt, which this lacks.
detach gdb = hPutStrLn (gdbCommand gdb) "-gdb-exit"

-- |Run a GDB command.
runCommand :: GDBCommand -> GDB -> IO (Either String MIOutput)
runCommand cmd gdb = do
  hPutStrLn (gdbCommand gdb) (cmdStr cmd)
  readResponse gdb
  where
  cmdStr (CLICommand str) = str
  cmdStr (MICommand str) = '-' : str

readResponse :: GDB -> IO (Either String MIOutput)
readResponse gdb = do
  resp <- readResponseLines
  case parse "output" (unlines resp) of
    Left err  -> return $ Left (show err)
    Right out -> return $ Right out
  where
  readResponseLines :: IO [String]
  readResponseLines = do
    line <- hGetLine (gdbResponse gdb)
    if line == "(gdb) "
      then return []
      else do rest <- readResponseLines
              return (line:rest)

-- Our Parsec parsers all start with p_.

-- Parse the main ouptut from GDB.
p_output = do
  oob <- p_oob `sepEndBy` newline
  res <- optionMaybe p_result
  eof
  return $ MIOutput oob res
-- Parse an "OOB" message from GDB.
p_oob = p_console <|> p_log
-- Parse a console OOB message from GDB.
p_console = do char '~'; str <- p_cstring; return $ MIConsole str
-- Parse a log OOB message from GDB.
p_log = do char '&'; str <- p_cstring; return $ MILog str

-- Parse a result message from GDB.
p_result = do
  char '^'
  res <- p_done <|> p_error
  newline; return res
  where
  -- Parse a done result message from GDB.
  p_done = do
    string "done"
    res <- (do char ','; p_keyval `sepBy` char ',') <|> return []
    return $ MIDone res
  -- Parse a error result message from GDB.
  p_error = do
    string "error"
    char ','
    -- XXX: The GDB/MI docs say this should be just a cstring, but my GDB
    -- doesn't agree.
    string "msg="  -- Hack here; perhaps it's really like the "done" output?
    err <- p_cstring
    return $ MIError err

-- Parse a key=val output ("result") from GDB.
p_keyval = do var <- p_var; char '='; val <- p_val; return $ (var, val) where
  p_var = many1 (letter <|> char '-')  -- XXX: this is underspecified.
  p_val = p_const <|> p_tuple
  p_const = do str <- p_cstring; return $ MIString str
  p_tuple = do
    vals <- tuplewrap $ p_keyval `sepBy` char ','
    return $ MITuple vals
  -- It's unclear why they have []-style tuples and {}-style tuples...
  tuplewrap p = between (char '{') (char '}') p
            <|> between (char '[') (char ']') p

-- Parse a C-style string (underspecified by the GDB manual).
p_cstring = between (char '"') (char '"') (many p_cchar) where
  p_cchar = p_cbackslash
        <|> noneOf "\""
  p_cbackslash = do
    char '\\'
    c <- anyChar
    case c of
      '\\' -> return '\\'
      'n' -> return '\n'
      '"' -> return '"'
      _ -> fail $ "unknown backslash escape: " ++ show c

-- |An interface to the output parser.  Just used for testing.
parse = Parsec.parse p_output
