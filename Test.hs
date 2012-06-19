-- hgdbmi: a Haskell interface to GDB/MI.
-- Copyright (C) 2008 Evan Martin <martine@danga.com>

import GDBMI
import Test.HUnit

startupText =
  "~\"GNU gdb 6.7.1-debian\\n\"\n" ++
  "~\"Copyright (C) 2007 Free Software Foundation, Inc.\\n\"\n" ++
  "~\"License GPLv3+: GNU GPL version 3 or later <http://gnu.org/licenses/gpl.html>\\n\"\n" ++
  "~\"This is free software: you are free to change and redistribute it.\\n\"\n" ++
  "~\"There is NO WARRANTY, to the extent permitted by law.  Type \\\"show copying\\\"\\n\"\n" ++
  "~\"and \\\"show warranty\\\" for details.\\n\"\n" ++
  "~\"This GDB was configured as \\\"i486-linux-gnu\\\".\\n\"\n" ++
  "~\"Attaching to process 8902\\n\"\n" ++
  "~\"Reading symbols from /home/martine/projects/c-repl/dist/build/child...\"\n" ++
  "~\"done.\\n\"\n" ++
  "~\"Using host libthread_db library \\\"/lib/tls/i686/cmov/libthread_db.so.1\\\".\\n\"\n" ++
  "~\"Reading symbols from /lib/tls/i686/cmov/libdl.so.2...\"\n" ++
  "~\"done.\\n\"\n" ++
  "~\"Loaded symbols for /lib/tls/i686/cmov/libdl.so.2\\n\"\n" ++
  "~\"Reading symbols from /lib/tls/i686/cmov/libc.so.6...\"\n" ++
  "~\"done.\\n\"\n" ++
  "~\"Loaded symbols for /lib/tls/i686/cmov/libc.so.6\\n\"\n" ++
  "~\"Reading symbols from /lib/ld-linux.so.2...\"\n" ++
  "~\"done.\\n\"\n" ++
  "~\"Loaded symbols for /lib/ld-linux.so.2\\n\"\n" ++
  "~\"Reading symbols from /home/martine/projects/c-repl/.c-repl/dl1.so...\"\n" ++
  "~\"done.\\n\"\n" ++
  "~\"Loaded symbols for ./.c-repl/dl1.so\\n\"\n"

testStartup = test $
  case GDBMI.parse "startup" startupText of
    Left err -> assertFailure $ show err
    Right (MIOutput info result) -> do
      assertEqual "oob line count" 23 (length info)
      assertEqual "oob first line"
        (MIConsole "GNU gdb 6.7.1-debian\n") (head info)

printText =
  "&\"p x\\n\"\n" ++
  "~\"$1 = 3\"\n" ++
  "~\"\n\"\n" ++
  "^done,thread-id=\"0\",frame={addr=\"0xb7f0a410\",func=\"__kernel_vsyscall\",args=[]}\n"

testPrint = test $
  case GDBMI.parse "parse 'p x' output" printText of
    Left err -> assertFailure $ show err
    Right (MIOutput info result) -> do
      assertEqual "oob line count" 3 (length info)
      assertEqual "oob first line" (MILog "p x\n") (head info)
      case result of
        Just (MIDone tuples) -> do
          assertEqual "done tuple #1" ("thread-id", MIString "0") (head tuples)
          case lookup "frame" tuples of
            Just (MITuple frame) -> 
              assertEqual "done tuple #4" ("args", MITuple []) (last frame)
            _ -> assertFailure $ "couldn't find 'frame'"
        _ ->
          assertFailure $ "expected done result, got " ++ show result

errorPrintText =
  "&\"p y\\n\"\n" ++
  "&\"No symbol \\\"y\\\" in current context.\\n\"\n" ++
  "^error,msg=\"No symbol \\\"y\\\" in current context.\"\n"

testErrorPrint = test $
  case GDBMI.parse "parse 'p y' error output" errorPrintText of
    Left err -> assertFailure $ show err
    Right (MIOutput info result) -> do
      assertEqual "oob line count" 2 (length info)
      assertEqual "oob first line" (MILog "p y\n") (head info)
      assertEqual "result"
          (Just (MIError "No symbol \"y\" in current context.")) result

varCreateText = "^done,name=\"vx\",numchild=\"0\",value=\"4\",type=\"int\"\n"

testVarCreate = test $
  case GDBMI.parse "parse 'var-create' output" varCreateText of
    Left err -> assertFailure $ show err
    Right (MIOutput info result) -> do
      assertEqual "no oob info" 0 (length info)
      assertEqual "result"
          (Just (MIDone [("name", MIString "vx"), ("numchild", MIString "0"),
                         ("value", MIString "4"), ("type", MIString "int")]))
          result

main =
  runTestTT $ TestList [testStartup, testPrint, testErrorPrint, testVarCreate]
