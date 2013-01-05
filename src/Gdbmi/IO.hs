-- | Control execution of a GDB instance, send commands and receive results, notifications and stream information.
--
-- Due to <http://sourceware.org/bugzilla/show_bug.cgi?id=8759> the first command issued should be to set the output terminal for GDB to \/dev\/null. Unfortunatelly, there is no MI command for this, so we have to resort to the CLI command \"tty\". For example:
--
-- >>> ctx  <- setup config callback
-- >>> resp <- send_command ctx (cli_command "tty /dev/null")
-- >>> when (respClass resp /= RCDone) (error ("unexpected response: " ++ show resp))
--
module Gdbmi.IO
-- exports {{{1
(
    Context, Config(..), Callback(..)
  , default_config
  , setup, shutdown, send_command
) where

-- imports {{{1
import Control.Applicative ((<*>), (<$>))
import Control.Concurrent (forkIO, killThread, ThreadId, MVar, newEmptyMVar, tryTakeMVar, putMVar, takeMVar)
import Control.Concurrent.STM (TVar, TChan, TMVar, newEmptyTMVar, newTVarIO, newTChanIO, atomically, takeTMVar, readTVar, writeTVar, writeTChan, readTChan, putTMVar)
import Control.Exception (catchJust)
import Control.Exception.Base (AsyncException(ThreadKilled))
import Control.Monad (replicateM_, when)
import Control.Monad.Fix (mfix)
import Data.List (partition)
import Prelude hiding (catch, interact)
import System.IO (Handle, hSetBuffering, BufferMode(LineBuffering), hPutStr, hWaitForInput, hGetLine, IOMode(WriteMode), stdout, openFile, hFlush, hClose)
import System.Posix.IO (fdToHandle, createPipe)
import System.Process (ProcessHandle, runProcess, waitForProcess)

import qualified Gdbmi.Commands as C
import qualified Gdbmi.Representation as R
import qualified Gdbmi.Responses as S

data Context = Context { -- {{{1
-- gdb process {{{2
    ctxProcess       :: ProcessHandle
  , ctxCommandPipe   :: Handle
  , ctxOutputPipe    :: Handle
  , ctxLog           :: Maybe Handle
-- callback
  , ctxCallback      :: Callback
-- threads
  , ctxCommandThread :: ThreadId
  , ctxOutputThread  :: ThreadId
  , ctxCurrentJob    :: MVar Job
  , ctxFinished      :: MVar ()
-- jobs
  , ctxNextToken     :: TVar R.Token
  , ctxJobs          :: TChan Job
}

data Job = Job {
    jobCommand  :: R.Command
  , jobResponse :: TMVar R.Response
  , jobToken    :: R.Token
  }

-- | Call-back functions for asynchronous GDB output.
-- 
-- The call-backs are called in a separate thread per GDB output, so they may block.
--
-- Stop events are 'Gdbmi.Representation.Notification' events with 'R.NotificationClass' 'R.Exec' and 'R.AsyncClass' 'R.ACStop'.
-- If 'cbStopped' is given stop events are delivered to that call-back instead of 'cbNotify'.
data Callback  -- {{{1
  = Callback {
      cbStream  :: [R.Stream] -> IO ()          -- ^ call-back for 'Gdbmi.Representation.Stream' events
    , cbNotify  :: [R.Notification] -> IO ()    -- ^ call-back for 'Gdbmi.Representation.Notification' events
    , cbStopped :: Maybe ([S.Stopped] -> IO ()) -- ^ optionally a special call-back for 'Gdbmi.Responses.Stopped' events
  }

-- | Configuration
data Config -- {{{1
  = Config {
      confCommandLine :: [String]        -- ^ command line to execute. The library will add \"--interpreter mi\".
    , confLogfile     :: Maybe FilePath  -- ^ optinonally a file path to a log file for GDB\/MI input and output. \'-\' means stdout.
  }

default_config :: Config -- {{{2
-- | Default configuration: "gdb" command line, no log file
default_config = Config ["gdb"] Nothing

setup :: Config -> Callback -> IO Context -- {{{1
-- | Launch a GDB instance in Machine Interface mode.
--
-- The child process is run in a new session to avoid receiving SIGINTs when issuing -exec-interrupt.
setup config callback = do
  (commandR,  commandW)  <- createPipe >>= asHandles
  (outputR,   outputW)   <- createPipe >>= asHandles
  phandle <- runProcess "setsid" (confCommandLine config ++ ["--interpreter", "mi"])
                 Nothing Nothing
                 (Just commandR)
                 (Just outputW)
                 Nothing
  mapM_ (`hSetBuffering` LineBuffering) [commandW, outputR]
  logH <- case (confLogfile config) of
    Nothing  -> return    $ Nothing
    Just "-" -> return    $ Just stdout
    Just f   -> fmap Just $ openFile f WriteMode

  currentJob <- newEmptyMVar
  finished   <- newEmptyMVar
  nextToken  <- newTVarIO 0
  jobs       <- newTChanIO
  ctx        <- mfix (\ctx -> do
      itid <- forkIO (handleCommands ctx)
      otid <- forkIO (handleOutput ctx)
      return $ Context phandle commandW outputR logH callback itid otid currentJob finished nextToken jobs
    )
  return ctx
  where
  asHandles (f1, f2) = do
    h1 <- fdToHandle f1
    h2 <- fdToHandle f2
    return (h1, h2)

shutdown :: Context -> IO () -- {{{1
-- | Shut down the GDB instance and all resources associated with the 'Context'.
shutdown ctx = do
  mapM_ (killThread . ($ctx)) [ctxCommandThread, ctxOutputThread]
  replicateM_ 2 (takeMVar (ctxFinished ctx))
  writeCommand ctx C.gdb_exit 0
  _ <- waitForProcess (ctxProcess ctx)
  putMVar (ctxFinished ctx) ()
  case ctxLog ctx of
    Nothing -> return ()
    Just handle -> 
      if handle /= stdout
        then hClose handle
        else return ()

send_command :: Context -> R.Command -> IO R.Response -- {{{1
-- | Send a GDB command and wait for the response.
--
-- This function is thread safe, i.e., it can be called by multiple threads in an interleaved fashion.
send_command ctx command = checkShutdown >> sendCommand >>= receiveResponse
  where
    checkShutdown = do
      finished <- tryTakeMVar (ctxFinished ctx)
      case finished of
        Nothing -> return ()
        Just () -> error "context has already been shut down"

    sendCommand = atomically $ do
      token <- readTVar (ctxNextToken ctx)
      writeTVar (ctxNextToken ctx) (if token == maxBound then 0 else token + 1)
      response <- newEmptyTMVar
      writeTChan (ctxJobs ctx) $ Job command response token
      return response
    
    receiveResponse = atomically . takeTMVar

-- implementation {{{1
handleCommands :: Context -> IO () -- {{{2
handleCommands ctx = handleKill ctx $ do
  job <- atomically $ readTChan (ctxJobs ctx)
  putMVar (ctxCurrentJob ctx) job
  writeCommand ctx (jobCommand job) (jobToken job)
  handleCommands ctx

handleOutput :: Context -> IO () -- {{{2
handleOutput ctx = handleKill ctx $ do
  output  <- readOutput ctx
  _ <- callBack ctx output
  case R.output_response output of
    Nothing -> return ()
    Just response -> do
      maybJob <- tryTakeMVar (ctxCurrentJob ctx)
      case maybJob of
        Nothing -> error "result record lost!"
        Just job -> 
          if (R.get_token output /= Just (jobToken job))
            then error $ "token missmatch! " ++ show (R.get_token output) ++ " vs. " ++ show (jobToken job)
            else atomically $ putTMVar (jobResponse job) response
  handleOutput ctx  

callBack :: Context -> R.Output -> IO ()
callBack ctx output = forkIO go >> return ()
  where
    go =
      let
        callbacks       = ctxCallback ctx
        streamsCb       = cbStream callbacks
        notifyCb        = cbNotify callbacks
        stoppedCbMb     = cbStopped callbacks

        streams         = R.output_stream output
        notifications   = R.output_notification output
        (stops, others) = partition ((&&) <$> (R.Exec==) . R.notiClass <*> (R.ACStop==) . R.notiAsyncClass) notifications
        Just stops'     = sequence $ map (S.response_stopped . R.notiResults) stops
      in case stoppedCbMb of
        Nothing -> do
          when (not (null streams))       (streamsCb streams)
          when (not (null notifications)) (notifyCb notifications)
        Just stoppedCb -> do
          when (not (null streams))       (streamsCb streams)
          when (not (null others))        (notifyCb others)
          when (not (null stops'))        (stoppedCb stops')

handleKill :: Context -> IO () -> IO ()
handleKill ctx action = catchJust select action handler
  where
    select :: AsyncException -> Maybe ()
    select ThreadKilled = Just ()
    select _ = Nothing

    handler :: () -> IO ()
    handler _ = putMVar (ctxFinished ctx) ()

writeCommand :: Context -> R.Command -> R.Token -> IO () -- {{{2
writeCommand ctx cmd token = 
  let cmdstr = (R.render_command . C.set_token token) cmd in
  do
    debugLog ctx True cmdstr
    hPutStr (ctxCommandPipe ctx) cmdstr

readOutput :: Context -> IO R.Output -- {{{2
readOutput ctx = do
  _ <- hWaitForInput (ctxOutputPipe ctx) (-1)
  str <- outputString (ctxOutputPipe ctx)
  debugLog ctx False str
  return (R.parse_output str)
  where
    outputString handle = outputLines handle >>= return . unlines
    outputLines handle = do
      line <- hGetLine handle
      if line == "(gdb) "
        then return [line]
        else outputLines handle >>= return . (line:)

debugLog :: Context -> Bool -> String -> IO () -- {{{2
debugLog ctx io text = 
  let
    prefix = if io then "/i " else "/o "
    line = ((unlines . map (prefix++) . lines) text)
  in
  case (ctxLog ctx) of
    Nothing -> return ()
    Just h -> hPutStr h line >> hFlush h
