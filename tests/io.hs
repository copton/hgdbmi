{-# LANGUAGE QuasiQuotes #-}
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
import qualified Gdbmi.Commands       as G
import qualified Gdbmi.Responses      as G
import qualified Gdbmi.Representation as G

config :: G.Config
config = G.Config (words "schroot -c quantal -p -- gdb") (Just "gdb.log")

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

callback :: MVar [G.Stopped] -> G.Callback
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

command :: G.Context -> G.ResultClass -> G.Command -> IO [G.Result]
command ctx rc cmd = do
  resp <- G.send_command ctx cmd
  let msg = printf "command '%s' failed (%s): %s"
              (G.render_command cmd)
              (show (G.respClass resp))
              ((show . G.response_error . G.respResults) resp)
  when (G.respClass resp /= rc) (error msg)
  return (G.respResults resp)

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
  _            <- cmd G.RCDone $ G.cli_command "tty /dev/null"
  _            <- cmd G.RCDone $ G.file_exec_and_symbols (Just "example")
  let loc       = G.file_function_location "example.c" "print"
  bp'          <- cmd G.RCDone $ G.break_insert False False False False False Nothing Nothing Nothing loc
  let (Just bp) = G.response_break_insert bp'
  _            <- cmd G.RCRunning $ G.exec_run (Left True)
  forM_ [(0::Int)..10] (\counter -> do
      [stopped]      <- takeMVar mv 
      assert "breakpoint number" ((G.bkptHitNumber . G.stoppedReason) stopped) (G.bkptNumber bp)
      value'          <- cmd G.RCDone $ G.data_evaluate_expression "i"
      let (Just value) = G.response_data_evaluate_expression value'
      assert "value of i" value (show counter)
      _               <- cmd G.RCRunning $ G.exec_continue False (Left True)
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
