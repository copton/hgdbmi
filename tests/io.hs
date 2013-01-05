{-# LANGUAGE QuasiQuotes #-}
{-
 - Test the IO interface of hgdbmi.
 -
 - Requirements:
 -    - gdb (see config)
 -    - gcc (see setup)
 -
 - Steps:
 -    - create temporary directory
 -    - write test program (example)
 -    - compile test program with debugging
 -    - start debugger
 -    - set breakpoint
 -    - run debugger
 -    - wait for stop event, evaluate variable and continue (10 times)
 -    - quit
 -    - dump log file
 -
 - Output:
 -    - stream and notification events (excluding stop events)
 -    - log file with GDB/MI communication
-}
module Main (main) where

import Control.Concurrent     (newEmptyMVar, putMVar, takeMVar, MVar)
import Control.Exception.Base (bracket)
import Control.Monad          (forM_, when)
import System.Directory       (removeDirectoryRecursive, getCurrentDirectory, setCurrentDirectory)
import System.Exit            (ExitCode(..))
import System.IO.Temp         (createTempDirectory)
import System.Process         (readProcessWithExitCode)
import Text.Printf            (printf)

import Paste (paste)

import qualified Gdbmi.IO             as G
import qualified Gdbmi.Commands       as C
import qualified Gdbmi.Semantics      as S
import qualified Gdbmi.Representation as R

config :: G.Config
config = G.Config ["gdb"] (Just "gdb.log")

example :: String
example = [paste|
#include <stdio.h>

void print(int i) {
    printf("hello world %d\n", i);
}

int main() {
    int i;
    for (i=0; ; i++) {
        print(i);
    }
    return 0;
}
|]

callback :: MVar [S.Stopped] -> G.Callback
callback mv = G.Callback print print (Just (putMVar mv))

setup :: IO ()
setup = do
  writeFile "example.c" example
  (ec, sout, serr) <- readProcessWithExitCode "gcc" (words "-o example -g example.c") "" 
  case ec of
    ExitFailure ec' -> do
      putStrLn sout
      putStrLn serr
      error $ printf "failed to execute gcc: %s" (show ec')
    ExitSuccess -> return ()

command :: G.Context -> R.ResultClass -> R.Command -> IO [R.Result]
command ctx rc cmd = do
  resp <- G.send_command ctx cmd
  let msg = printf "command '%s' failed (%s): %s"
              (R.render_command cmd)
              (show (R.respClass resp))
              ((show . S.response_error . R.respResults) resp)
  when (R.respClass resp /= rc) (error msg)
  return (R.respResults resp)

assert :: (Eq a, Show a) => String -> a -> a -> IO ()
assert what x y = if (x == y)
  then return ()
  else error $ printf "assertion failed: %s: %s vs. %s" what (show x) (show y)

test ::  IO ()
test = do 
  _            <- setup
  mv           <- newEmptyMVar
  ctx          <- G.setup config (callback mv)
  let cmd       = command ctx
  _            <- cmd R.RCDone $ C.cli_command "tty /dev/null"
  _            <- cmd R.RCDone $ C.file_exec_and_symbols (Just "example")
  let loc       = C.file_function_location "example.c" "print"
  bp'          <- cmd R.RCDone $ C.break_insert False False False False False Nothing Nothing Nothing loc
  let (Just bp) = S.response_break_insert bp'
  _            <- cmd R.RCRunning $ C.exec_run (Left True)
  forM_ [(0::Int)..10] (\counter -> do
      [stopped]      <- takeMVar mv 
      assert "breakpoint number" ((S.bkptHitNumber . S.stoppedReason) stopped) (S.bkptNumber bp)
      value'          <- cmd R.RCDone $ C.data_evaluate_expression "i"
      let (Just value) = S.response_data_evaluate_expression value'
      assert "value of i" value (show counter)
      _               <- cmd R.RCRunning $ C.exec_continue False (Left True)
      return ()
    )
  G.shutdown ctx
  readFile "gdb.log" >>= putStr

withTemporaryDirectory :: IO a -> IO a
withTemporaryDirectory f = bracket acquire release inbetween
  where
    acquire = do
      tmpdir <- createTempDirectory "/tmp" "hgdbmi-test"
      curdir <- getCurrentDirectory
      setCurrentDirectory tmpdir
      return (curdir, tmpdir)

    release (curdir, tmpdir) = do
      setCurrentDirectory curdir
      removeDirectoryRecursive tmpdir

    inbetween (_, _) = f

main :: IO ()
main = withTemporaryDirectory test
