{-# LANGUAGE QuasiQuotes #-}
module Main (main) where

import Gdbmi.IO
import Gdbmi.Responses
import Control.Exception.Base (bracket)
import System.Directory (removeDirectoryRecursive, getCurrentDirectory, setCurrentDirectory)
import System.IO.Temp (createTempDirectory)

import Paste (paste)

config :: Config
config = Config (words "schroot -c quantal -p -- gdb") (Just "/tmp/gdb.log")

example :: String
example = [paste|
#include <stdio.h>

void print(int i) {
    printf("hello world %d\n", i);
}

int main() {
    int i;
    for (i=0; i<10; i++) {
        print(1);
    }
    return 0;
}
|]

callback :: Callback
callback = Callback print print (Just print)

test :: IO ()
test = do
  writeFile "example.c" example
  ex <- readFile "example.c"
  print ex

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
