-- | Constructor functions for 'Gdmi.Representation.Command' values.
-- 
-- Please consult the cited GDB documentation for the semantics of the individual commands.
module Gdbmi.Commands
-- {{{1 exports
(
  -- * Commands
  -- ** Breakpoint Commands
  -- | <http://sourceware.org/gdb/current/onlinedocs/gdb/GDB_002fMI-Breakpoint-Commands.html>
  break_after,
  break_commands,
  break_condition,
  break_delete,
  break_disable,
  break_enable,
  break_info,
  break_insert,
  break_list,
  break_passcount,
  break_watch,

  -- ** Program Context
  -- | <http://sourceware.org/gdb/current/onlinedocs/gdb/GDB_002fMI-Program-Context.html>
  exec_arguments,
  environment_cd,
  environment_directory,
  environment_path,
  environment_pwd,

  -- ** Thread Commands
  -- | <http://sourceware.org/gdb/current/onlinedocs/gdb/GDB_002fMI-Thread-Commands.html>
  thread_info,
  thread_list_ids,
  thread_select,

  -- ** Ada Tasking Commands
  -- | <http://sourceware.org/gdb/current/onlinedocs/gdb/GDB_002fMI-Ada-Tasking-Commands.html>
  ada_task_info,

  -- ** Program Execution
  -- | <http://sourceware.org/gdb/current/onlinedocs/gdb/GDB_002fMI-Program-Execution.html>
  exec_continue,
  exec_finish,
  exec_interrupt,
  exec_jump,
  exec_next,
  exec_next_instruction,
  exec_return,
  exec_run,
  exec_step,
  exec_step_instruction,
  exec_until,

  -- ** Stack Manipulation
  -- | <http://sourceware.org/gdb/current/onlinedocs/gdb/GDB_002fMI-Stack-Manipulation.html>
  stack_info_frame,
  stack_info_depth,
  stack_list_arguments,
  stack_list_arguments',
  stack_list_frames,
  stack_list_locals,
  stack_list_locals',
  stack_list_variables,
  stack_list_variables',
  stack_select_frame,

  -- ** Variable Objects
  -- | <http://sourceware.org/gdb/current/onlinedocs/gdb/GDB_002fMI-Variable-Objects.html>
  enable_pretty_printing,
  var_create,
  var_delete,
  var_set_format,
  var_gdbShow_format,
  var_info_num_children,
  var_list_children,
  var_list_children',
  var_info_type,
  var_info_expression,
  var_info_path_expressoin,
  var_gdbShow_attributes,
  var_evaluate_expression,
  var_assign,
  var_update,
  var_set_frozen,
  var_set_update_range,
  var_set_visualizer,

  -- ** Data Manipulation
  -- | <http://sourceware.org/gdb/current/onlinedocs/gdb/GDB_002fMI-Data-Manipulation.html>
  data_disassemble,
  data_evaluate_expression,
  data_list_changed_registers,
  data_list_register_names,
  data_list_register_values,
  data_read_memory,
  data_read_memory_bytes,
  data_write_memory_bytes,

  -- ** Tracepoint Commands
  -- | <http://sourceware.org/gdb/current/onlinedocs/gdb/GDB_002fMI-Tracepoint-Commands.html>
  trace_find,
  trace_define_variable,
  trace_list_variables,
  trace_save,
  trace_start,
  trace_status,
  trace_stop,

  -- ** Symbol Query
  -- | <http://sourceware.org/gdb/current/onlinedocs/gdb/GDB_002fMI-Symbol-Query.html>
  symbol_list_lines,

  -- ** File Commands
  -- | <http://sourceware.org/gdb/current/onlinedocs/gdb/GDB_002fMI-File-Commands.html>
  file_exec_and_symbols,
  file_exec_file,
  file_list_exec_source_file,
  file_list_exec_source_files,
  file_symbol_file,

  -- ** Target Manipulation
  -- | <http://sourceware.org/gdb/current/onlinedocs/gdb/GDB_002fMI-Target-Manipulation.html>
  target_attach,
  target_detach,
  target_disconnect,
  target_download,
  target_select,

  -- ** File Transfer Commands
  -- | <http://sourceware.org/gdb/current/onlinedocs/gdb/GDB_002fMI-File-Transfer-Commands.html>
  target_file_put,
  target_file_get,
  target_file_delete,

  -- ** Miscellaneous Commmands
  -- | <http://sourceware.org/gdb/current/onlinedocs/gdb/GDB_002fMI-Miscellaneous-Commands.html>
  gdb_exit,
  gdb_set,
  gdb_gdbShow,
  gdb_version,
  list_features,
  list_target_features,
  list_thread_groups,
  info_os,
  add_inferior,
  interpreter_exec,
  inferior_tty_set,
  inferior_tty_gdbShow,
  enable_timings,

  -- * Auxiliary Types
  -- ** Location
  -- | <http://sourceware.org/gdb/current/onlinedocs/gdb/Specify-Location.html>.
  Location,
  positive_offset_location,
  negative_offset_location,
  file_line_location,
  function_location,
  function_label_location,
  file_function_location,
  label_location,
  plain_address_location,
  expr_address_location,
  func_address_location,
  file_func_address_location,

  -- ** Other Types
  -- | See the documentation of the commands that use these types for their semantics.
  PrintValues(..),
  FrameSelect(..),
  FormatSpec(..),
  FrozenFlag(..),
  DisassemblyMode(..),
  DataFormat(..),
  OutputFormat(..),
  TraceMode(..),
  Target(..),
  Medium(..),
  Interpreter(..),
   
  -- * Helper Functions
  cli_command,
  set_token
) where

-- imports {{{1
import Data.List (intersperse)
import Data.Maybe (fromMaybe)
import Prelude hiding (reverse, all, lines)

import Gdbmi.Representation (Command(..), Option(..), Token, Parameter(..), render_command)

-- types {{{1
class GdbShow a where -- {{{2
  gdbShow :: a -> String

instance GdbShow Char where
  gdbShow = (:[])

instance GdbShow a => GdbShow [a] where
  gdbShow = concatMap gdbShow

instance GdbShow Int where
  gdbShow = show

newtype Location = Location String -- {{{2

instance GdbShow Location where -- {{{3
  gdbShow (Location location) = location 

positive_offset_location :: Int -> Location -- {{{3
positive_offset_location offset = Location $ "+" ++ gdbShow offset

negative_offset_location :: Int -> Location -- {{{3
negative_offset_location offset = Location $ "-" ++ gdbShow offset

file_line_location :: String -> Int -> Location -- {{{3
file_line_location filename linenum = Location $ filename ++ ":" ++ gdbShow linenum

function_location :: String -> Location -- {{{3
function_location = Location

function_label_location :: String -> String -> Location -- {{{3
function_label_location function label = Location $ function ++ ":" ++ label

file_function_location :: String -> String -> Location -- {{{3
file_function_location filename function = Location $ filename ++ ":" ++ function

label_location :: String -> Location -- {{{3
label_location = Location

plain_address_location :: String -> Location -- {{{3
plain_address_location addr = Location $ "*" ++ addr

expr_address_location :: String -> Location -- {{{3
expr_address_location = plain_address_location

func_address_location :: String -> Location -- {{{3
func_address_location = plain_address_location

file_func_address_location :: String -> String -> Location -- {{{3
file_func_address_location filename funcaddr = Location $ "'" ++ filename ++ "'::" ++ funcaddr

data PrintValues -- {{{2
  = NoValues     -- ^ \"--no-values\"     or \"0\"
  | AllValues    -- ^ \"--all-values\"    or \"1\"
  | SimpleValues -- ^ \"--simple-values\" or \"2\"

instance GdbShow PrintValues where
  gdbShow NoValues     = "--no-values"
  gdbShow AllValues    = "--all-values"
  gdbShow SimpleValues = "--simple-values"

mapPrintValues :: (PrintValues -> a) -> Int -> a
mapPrintValues f 0 = f NoValues
mapPrintValues f 1 = f AllValues
mapPrintValues f 2 = f SimpleValues
mapPrintValues _ _ = error "valid integers for the print-value parameter range from 0 to 2 only"

data FrameSelect -- {{{2
  = FrameAddr String -- ^ just the given address
  | CurrentFrame     -- ^ \"*\"
  | Floating         -- ^ \"\@\"

instance GdbShow FrameSelect where
  gdbShow (FrameAddr addr) = addr
  gdbShow CurrentFrame = "*"
  gdbShow Floating = "@"

data FormatSpec -- {{{2
  = Binary      -- ^ \"binary\"
  | Decimal     -- ^ \"decimal\"
  | Hexadecimal -- ^ \"hexadecimal\"
  | Octal       -- ^ \"octal\"
  | Natural     -- ^ \"natural\"

instance GdbShow FormatSpec where
  gdbShow Binary = "binary"
  gdbShow Decimal = "decimal"
  gdbShow Hexadecimal = "hexadecimal"
  gdbShow Octal = "octal"
  gdbShow Natural = "natural"

data FrozenFlag -- {{{2
  = Frozen   -- ^ \"1\"
  | Unfrozen -- ^ \"0\"

instance GdbShow FrozenFlag where
  gdbShow Frozen = "1"
  gdbShow Unfrozen = "0"

data DisassemblyMode -- {{{2
  = DisassemblyMode Bool Bool -- ^ with mixed source?, with raw opcodes?

instance GdbShow DisassemblyMode where
  gdbShow (DisassemblyMode False False) = "0"
  gdbShow (DisassemblyMode True False) = "1"
  gdbShow (DisassemblyMode False True) = "2"
  gdbShow (DisassemblyMode True True) = "3"

data DataFormat -- {{{2
  = DHexadecimal -- ^ \"x\"
  | DOctal       -- ^ \"o\"
  | DBinary      -- ^ \"t\"
  | DDecimal     -- ^ \"d\"
  | DRaw         -- ^ \"r\"
  | DNatural     -- ^ \"N\"

instance GdbShow DataFormat where
  gdbShow DHexadecimal = "x"
  gdbShow DOctal = "o"
  gdbShow DBinary = "t"
  gdbShow DDecimal = "d"
  gdbShow DRaw = "r"
  gdbShow DNatural = "N"

data OutputFormat -- {{{2
  = HexadecimalInteger        -- ^ \"x\"
  | SignedDecimalInteger      -- ^ \"d\"
  | UnsignedDecimalInteger    -- ^ \"u\"
  | OctalInteger              -- ^ \"o\"
  | BinaryInteger             -- ^ \"t\"
  | Address                   -- ^ \"a\"
  | CharacterConstantInteger  -- ^ \"c\"
  | FloatingPointNumber       -- ^ \"f\"
  | OString                   -- ^ \"s\"
  | Raw                       -- ^ \"r\"

instance GdbShow OutputFormat where
  gdbShow HexadecimalInteger = "x"
  gdbShow SignedDecimalInteger = "d"
  gdbShow UnsignedDecimalInteger = "u"
  gdbShow OctalInteger = "o"
  gdbShow BinaryInteger = "t"
  gdbShow Address = "a"
  gdbShow CharacterConstantInteger = "c"
  gdbShow FloatingPointNumber = "f"
  gdbShow OString = "s"
  gdbShow Raw = "r"

data TraceMode -- {{{2
  = None                          -- ^ \"none\"
  | FrameNumber Int               -- ^ \"frame-number\"
  | TracepointNumber Int          -- ^ \"tracepoint-number\"
  | PC String                     -- ^ \"pc\"
  | PCInsideRange String String   -- ^ \"pc-inside-range\"
  | PCOutsideRange String String  -- ^ \"pc-outside-range\"
  | Line Location                 -- ^ \"line\"

instance GdbShow TraceMode where
  gdbShow None = "none"
  gdbShow (FrameNumber _) = "frame-number"
  gdbShow (TracepointNumber _) = "tracepoint-number"
  gdbShow (PC _) = "pc"
  gdbShow (PCInsideRange _ _) = "pc-inside-range"
  gdbShow (PCOutsideRange _ _) = "pc-outside-range"
  gdbShow (Line _) = "line"

traceModeOptions :: TraceMode -> [Option]
traceModeOptions None = []
traceModeOptions (FrameNumber x) = [opt x]
traceModeOptions (TracepointNumber x) = [opt x]
traceModeOptions (PC x) = [opt x]
traceModeOptions (PCInsideRange x y) = [opt x, opt y]
traceModeOptions (PCOutsideRange x y) = [opt x, opt y]
traceModeOptions (Line x) = [opt x]

data Target -- {{{2
  = Exec FilePath  -- ^ \"exec\"
  | Core FilePath  -- ^ \"core\"
  | Remote Medium  -- ^ \"remote\"
  | Sim [String]   -- ^ \"sim\"
  | Nrom           -- ^ \"nrom\"

instance GdbShow Target where
  gdbShow (Exec _) = "exec" 
  gdbShow (Core _) = "core"
  gdbShow (Remote _) = "remote" 
  gdbShow (Sim _) = "sim"
  gdbShow Nrom = "nrom"

targetOptions :: Target -> [Option]
targetOptions (Exec x) = [opt x] 
targetOptions (Core x) = [opt x] 
targetOptions (Remote x) = [opt x]
targetOptions (Sim xs) = map opt xs
targetOptions Nrom = []
  
data Medium -- {{{2
  = SerialDevice String  -- ^ just the given device
  | TcpHost String Int   -- ^ \"tcp:%s:%d\" host port
  | UdpHost String Int   -- ^ \"udp:%s:%d\" host port
  | Pipe String          -- ^ \"|%s\" command

instance GdbShow Medium where
  gdbShow (SerialDevice device) = device
  gdbShow (TcpHost host port) = "tcp:" ++ host ++ ":" ++ gdbShow port
  gdbShow (UdpHost host port) = "udp:" ++ host ++ ":" ++ gdbShow port
  gdbShow (Pipe command) = "| " ++ command

data Interpreter -- {{{2
  = Console -- ^ \"console\"
  | MI      -- ^ \"mi\"
  | MI2     -- ^ \"mi2\"
  | MI1     -- ^ \"mi1\"

instance GdbShow Interpreter where
  gdbShow Console = "console"
  gdbShow MI = "mi"
  gdbShow MI2 = "mi2"
  gdbShow MI1 = "mi1"
   
-- helper {{{1
set_token :: Token -> Command -> Command -- {{{2
-- | Set the token of a command.
-- <http://sourceware.org/gdb/current/onlinedocs/gdb/GDB_002fMI-Input-Syntax.html>
--
-- Newly created commands have no token, but setting a token manually is usually not necessary.
-- 'Gdbmi.IO.send_command' will assign a unique token to a given command in any case.
set_token token (MICommand _ x y z) = MICommand (Just token) x y z
set_token token (CLICommand _ x)    = CLICommand (Just token) x

cli_command :: String -> Command
-- | Create a CLI command, i.e., use the provided string literaly.
-- 
-- Some GDB commands are not reflected in the Machine Interface. In those
-- cases one has to resort to Command Line Interface commands, which the MI
-- accepts as well.
cli_command = CLICommand Nothing

-- commands {{{1
-- breakpoint commands {{{2
break_after :: Int -> Int -> Command -- {{{3
break_after number count = cmd "break-after" $ map opt [number, count]

break_commands :: Int -> [String] -> Command -- {{{3
break_commands number commands = cmd "break-commands" $ opt number : map opt commands

break_condition :: Int -> String -> Command -- {{{3
break_condition number expr = cmd "break-condition" $ opt number : opt expr : []

break_delete :: [Int] -> Command -- {{{3
break_delete numbers = cmd "break-delete" $ map optr numbers

break_disable :: [Int] -> Command -- {{{3
break_disable numbers = cmd "break-disable" $ map opt numbers

break_enable :: [Int] -> Command -- {{{3
break_enable numbers = cmd "break-enable" $ map opt numbers

break_info :: Int -> Command -- {{{3
break_info number = cmd "break-info" [opt number]

break_insert :: Bool -> Bool -> Bool -> Bool -> Bool -> Maybe String -> Maybe Int -> Maybe Int -> Location -> Command -- {{{3
break_insert temporary hardware pending disabled tracepoint condition ignoreCount threadId location =
  cmd "break-insert" $ temporary' ?: hardware' ?: pending' ?: disabled' ?: tracepoint' ?: condition' ?: ignoreCount' ?: threadId' ?: opt location : []
  where
    temporary'   = flagOpt "-t" temporary
    hardware'    = flagOpt "-h" hardware
    pending'     = flagOpt "-p" pending
    disabled'    = flagOpt "-d" disabled
    tracepoint'  = flagOpt "-a" tracepoint
    condition'   = valueOpt "-c" condition
    ignoreCount' = valueOpt "-i" ignoreCount
    threadId'    = valueOpt "-p" threadId
    
break_list :: Command -- {{{3
break_list = cmd "break-list" []

break_passcount :: Int -> Int -> Command -- {{{3
break_passcount tracepointNumber passcount = cmd "break-passcount" $ map opt [tracepointNumber, passcount]

break_watch :: Bool -> Command -- {{{3
break_watch access =  cmd "break-watch" [opt (if access then "-a" else "-r")]

-- program context {{{2
exec_arguments :: [String] -> Command -- {{{3
exec_arguments args = cmd "exec-arguments" $ map opt args

environment_cd :: String -> Command -- {{{3
environment_cd pathdir = cmd "environment-cd" [opt pathdir]

environment_directory :: Bool -> [String] -> Command -- {{{3
environment_directory reset pathdirs = cmd "environment-directory" $ flagOpt "-r" reset ?: map opt pathdirs

environment_path :: Bool -> [String] -> Command -- {{{3
environment_path reset pathdirs = cmd "environment-path" $ flagOpt "-r" reset ?: map opt pathdirs

environment_pwd :: Command -- {{{3
environment_pwd = cmd "environment-pwd" []

-- thread commands {{{2
thread_info :: Maybe Int -> Command -- {{{3
thread_info threadId = cmd "thread-info" $ fmap opt threadId ?: []

thread_list_ids :: Command -- {{{3
thread_list_ids = cmd "thread-list-ids" []

thread_select :: Int -> Command -- {{{3
thread_select threadnum = cmd "thread-select" [opt threadnum]

-- ada tasking commands {{{2
ada_task_info :: Maybe Int -> Command -- {{{3
ada_task_info taskId = cmd "ada-task-info" $ fmap opt taskId ?: []

-- program execution {{{2
exec_continue :: Bool -> Either Bool Int -> Command -- {{{3
exec_continue reverse x = cmd "exec-continue" $ reverse' ?: x' ?: []
  where
    reverse'     = flagOpt "--reverse" reverse
    x' = case x of
      Left all -> flagOpt "--all" all
      Right threadGroup -> Just $ opt' "--threadGroup" threadGroup

exec_finish :: Bool -> Command -- {{{3
exec_finish reverse = cmd "exec-finish" $ flagOpt "--reverse" reverse ?: []

exec_interrupt :: Either Bool Int -> Command -- {{{3
exec_interrupt x = cmd "exec-interrupt" $ x' ?: [] 
  where
    x' = case x of
      Left all -> flagOpt "-all" all
      Right threadGroup -> Just $ opt' "--threadGroup" threadGroup

exec_jump :: Location -> Command -- {{{3
exec_jump location = cmd "exec-jump" [opt location]

exec_next :: Command -- {{{3
exec_next = cmd "exec-next" []

exec_next_instruction :: Bool -> Command -- {{{3
exec_next_instruction reverse = cmd "exec-next-instruction" $ flagOpt "--reverse" reverse ?: []

exec_return :: Command -- {{{3
exec_return = cmd "exec-return" []

exec_run :: Either Bool Int -> Command -- {{{3
exec_run x = cmd "exec-run" $ x' ?: []
  where
    x' = case x of
      Left all -> flagOpt "-all" all
      Right threadGroup -> Just $ opt' "--threadGroup" threadGroup

exec_step :: Command -- {{{3
exec_step = cmd "exec-step" []

exec_step_instruction :: Bool -> Command -- {{{3
exec_step_instruction reverse = cmd "exec-step-instruction" $ flagOpt "--reverse" reverse ?: []
  
exec_until :: Location -> Command -- {{{3
exec_until location = cmd "exec-until" [opt location]

-- stack manipulation {{{2
stack_info_frame :: Command -- {{{3
stack_info_frame = cmd "stack-info-frame" []

stack_info_depth :: Maybe Int -> Command  -- {{{3
stack_info_depth maxDepth = cmd "stack-info-depth" $ fmap opt maxDepth ?: []

stack_list_arguments :: PrintValues -> Maybe (Int, Int) -> Command -- {{{3
stack_list_arguments printValues frames = cmd "stack-list-arguments" $ opt printValues : maybTupleOpt frames

stack_list_arguments' :: Int -> Maybe (Int, Int) -> Command
stack_list_arguments' = mapPrintValues stack_list_arguments

stack_list_frames :: Maybe (Int, Int) -> Command -- {{{3
stack_list_frames frames = cmd "stack-list-frames" $ maybTupleOpt frames

stack_list_locals :: PrintValues -> Command -- {{{3
stack_list_locals printValues = cmd "stack-list-locals" [opt printValues]

stack_list_locals' :: Int -> Command -- {{{3
stack_list_locals' = mapPrintValues stack_list_locals

stack_list_variables :: PrintValues -> Command -- {{{3
stack_list_variables printValues = cmd "stack-list-variable" [opt printValues]

stack_list_variables' :: Int -> Command -- {{{3
stack_list_variables' = mapPrintValues stack_list_variables

stack_select_frame :: Int -> Command -- {{{3
stack_select_frame framenum = cmd "stack-select-frame" [opt framenum]

-- variable objects {{{2
enable_pretty_printing :: Command -- {{{3
enable_pretty_printing = cmd "enable-pretty-printing" []

var_create :: Maybe String -> FrameSelect -> String -> Command -- {{{3
var_create name frameSelect expression = cmd "var-create" $ [name', opt frameSelect, opt expression]
  where
    name' = opt (fromMaybe "-" name)

var_delete :: Bool -> String -> Command -- {{{3
var_delete children name = cmd "var-delete" $ flagOpt "-c" children ?: opt name : []

var_set_format :: String -> FormatSpec -> Command -- {{{3
var_set_format name formatSpec = cmd "var-set-format" [opt name, opt formatSpec]

var_gdbShow_format :: String -> Command -- {{{3
var_gdbShow_format name = cmd "var-gdbShow-format" [opt name]

var_info_num_children :: String -> Command -- {{{3
var_info_num_children name = cmd "var-info-num-children" [opt name]

var_list_children :: Maybe PrintValues -> String -> Maybe (Int, Int) -> Command -- {{{3
var_list_children Nothing name range = var_list_children (Just NoValues) name range
var_list_children (Just printValues) name range = cmd "var-list-children" $ opt printValues : opt name : maybTupleOpt range

var_list_children' :: Int -> String -> Maybe (Int, Int) -> Command
var_list_children' = mapPrintValues (var_list_children . Just)

var_info_type :: Command -- {{{3
var_info_type = cmd "var-info-type" []

var_info_expression :: String -> Command -- {{{3
var_info_expression name = cmd "var-info-expression" [opt name]

var_info_path_expressoin :: String -> Command -- {{{3
var_info_path_expressoin name = cmd "var-info-path-expression" [opt name]

var_gdbShow_attributes :: String -> Command -- {{{3
var_gdbShow_attributes name = cmd "var-gdbShow-attributes" [opt name]

var_evaluate_expression :: Maybe FormatSpec -> String -> Command -- {{{3
var_evaluate_expression formatSpec name = cmd "var-evaluate-expression" $ valueOpt "-f" formatSpec ?: opt name : []

var_assign :: String -> String -> Command -- {{{3
var_assign name expression = cmd "var-assign" [opt name, opt expression]

var_update :: Maybe PrintValues -> Maybe String -> Command -- {{{3
var_update Nothing name = var_update (Just NoValues) name
var_update (Just printValues) name = cmd "var-update" $ opt printValues : fmap opt name ?: []

var_set_frozen :: String -> FrozenFlag -> Command -- {{{3
var_set_frozen name flag = cmd "var-set-frozen" [opt name, opt flag]

var_set_update_range :: String -> Int -> Int -> Command -- {{{3
var_set_update_range name from to = cmd "var-set-update-range" [opt name, opt from, opt to]

var_set_visualizer :: String -> String -> Command -- {{{3
var_set_visualizer name visualizer = cmd "ver-set-visualizer" [opt name, opt visualizer]

-- data manipulation {{{2
data_disassemble :: Either (String, String) (String, Int, Maybe Int) -> DisassemblyMode -> Command -- {{{3
data_disassemble x mode = MICommand Nothing "data-disassemble" options [QuotedString . gdbShow $ mode]
  where
    options = case x of
      Left (start, end) -> opt' "-s" start : opt' "-e" end : []
      Right (filename, linenum, lines) -> opt' "-f" filename : opt' "-l" linenum : valueOpt "-n" lines ?: []

data_evaluate_expression :: String -> Command -- {{{3
data_evaluate_expression expr = cmd "data-evaluate-expression" [opt expr]

data_list_changed_registers :: Command -- {{{3
data_list_changed_registers = cmd "data-list-changed-registers" []

data_list_register_names :: [Int] -> Command -- {{{3
data_list_register_names regnos = cmd "data-list-register-names" $ map opt regnos

data_list_register_values :: DataFormat -> [Int] -> Command -- {{{3
data_list_register_values fmt regnos = cmd "data-list-register-values" $ opt fmt : map opt regnos

data_read_memory :: Maybe Int -> String -> OutputFormat -> Int -> Int -> Int -> Maybe Char -> Command -- {{{3
data_read_memory byteOffset address wordFormat wordSize nrRows nrCols asChar =
  cmd "data-read-memory" $ valueOpt "-o" byteOffset ?: opt address : opt wordFormat : opt wordSize : opt nrRows : opt nrCols : fmap opt asChar ?: []

data_read_memory_bytes :: Maybe Int -> String -> Int -> Command -- {{{3
data_read_memory_bytes byteOffset address count = cmd "data-read-memory-bytes" $ valueOpt "-o" byteOffset ?: opt address : opt count : []

data_write_memory_bytes :: String -> String -> Command -- {{{3
data_write_memory_bytes address contents = cmd "data-write-memory-bytes" [opt address, opt contents]

-- tracepoint commands {{{2
trace_find :: TraceMode -> Command -- {{{3
trace_find traceMode = cmd "trace-find" $ opt traceMode : traceModeOptions traceMode

trace_define_variable :: String -> Maybe String -> Command -- {{{3
trace_define_variable name value = cmd "trace-define-variable" $ opt name : fmap opt value ?: []

trace_list_variables :: Command -- {{{3
trace_list_variables = cmd "trace-list-variables" []

trace_save :: Bool -> String -> Command  -- {{{3
trace_save remote filename = cmd "trace-save" $ flagOpt "-r" remote ?: opt filename : []

trace_start :: Command -- {{{3
trace_start = cmd "trace-start" []

trace_status :: Command -- {{{3
trace_status = cmd "trace-status" []

trace_stop :: Command -- {{{3
trace_stop = cmd "trace-stop" []

-- symbol query {{{2

symbol_list_lines :: String -> Command -- {{{3
symbol_list_lines filename = cmd "symbol-list-lines" [opt filename]

-- file commands {{{2
file_exec_and_symbols :: Maybe FilePath -> Command -- {{{3
file_exec_and_symbols file = cmd "file-exec-and-symbols" $ fmap opt file ?: []

file_exec_file :: Maybe FilePath -> Command -- {{{3
file_exec_file file = cmd "file-exec-file" $ fmap opt file ?: []

file_list_exec_source_file :: Command -- {{{3
file_list_exec_source_file = cmd "file-list-exec-source-file" []

file_list_exec_source_files :: Command -- {{{3
file_list_exec_source_files = cmd "file-list-exec-source-files" []

file_symbol_file :: Maybe FilePath -> Command -- {{{3
file_symbol_file file = cmd "file-symbol-file" $ fmap opt file ?: []

-- target manipulation {{{2
target_attach :: Either Int FilePath -> Command -- {{{3
target_attach x = cmd "target-attach" $ x' : []
  where
    x' = case x of
      Left pidOrGid -> opt pidOrGid
      Right file -> opt file

target_detach :: Maybe Int -> Command -- {{{3
target_detach pidOrGid = cmd "target-detach" $ fmap opt pidOrGid ?: []

target_disconnect :: Command -- {{{3
target_disconnect = cmd "target-disconnect" []

target_download :: Command -- {{{3
target_download = cmd "target-download" []

target_select :: Target -> Command -- {{{3
target_select target = cmd "target-select" $ opt target : targetOptions target

-- file transfer commands {{{2
target_file_put :: FilePath -> FilePath -> Command -- {{{3
target_file_put hostfile targetfile = cmd "target-file-put" $ opt hostfile : opt targetfile : []

target_file_get :: FilePath -> FilePath -> Command -- {{{3
target_file_get targetfile hostfile = cmd "target-file-get" $ opt targetfile : opt hostfile : []

target_file_delete :: FilePath -> Command -- {{{3
target_file_delete targetfile = cmd "target-file-delete" $ opt targetfile : []

-- miscellaneous commmands {{{2
gdb_exit :: Command -- {{{3
gdb_exit = cmd "gdb-exit" []

gdb_set :: String -> Command -- {{{3
gdb_set expr = cmd "gdb-set" $ opt expr : []

gdb_gdbShow :: String -> Command -- {{{3
gdb_gdbShow name = cmd "gdb-gdbShow" $ opt name : []

gdb_version :: Command -- {{{3
gdb_version = cmd "gdb-version" []

list_features :: Command -- {{{3
list_features = cmd "list-features" []

list_target_features :: Command -- {{{3
list_target_features = cmd "list-target-features" []

list_thread_groups :: Bool -> Maybe Int -> [Int] -> Command -- {{{3
list_thread_groups available recurse groups = cmd "list-thread-groups" $ flagOpt "--available" available ?: valueOpt "--recurse" recurse ?: map opt groups

info_os :: Maybe String -> Command -- {{{3
info_os type_ = cmd "info-os" $ fmap opt type_ ?: []

add_inferior :: Command -- {{{3
add_inferior = cmd "add-inferior" []

interpreter_exec :: Interpreter -> Command -> Command -- {{{3
interpreter_exec interpreter command = cmd "interpreter-exec" $ opt interpreter : opt ((escapeQuotes . render_command) command) : []

inferior_tty_set :: String -> Command -- {{{3
inferior_tty_set tty = cmd "inferior-tty-set" $ opt tty : []

inferior_tty_gdbShow :: Command -- {{{3
inferior_tty_gdbShow = cmd "inferior-tty-gdbShow" []

enable_timings :: Bool -> Command -- {{{3
enable_timings flag = cmd "enable-timings" $ opt (if flag then "yes" else "no") : []

-- utils {{{1
cmd :: String -> [Option] -> Command -- {{{2
cmd operation options = MICommand Nothing operation options []

opt :: GdbShow a => a -> Option -- {{{2
opt parameter = Option (QuotedString . gdbShow $ parameter) Nothing

optr :: Show a => a -> Option -- {{{2
optr parameter = Option (RawString . show $ parameter) Nothing

opt' :: (GdbShow a, GdbShow b) => a -> b -> Option -- {{{2
opt' name value = Option (QuotedString . gdbShow $ name) (Just (QuotedString . gdbShow $ value))

flagOpt :: String -> Bool -> Maybe Option -- {{{2
flagOpt _ False = Nothing
flagOpt flag True = Just (opt flag)

valueOpt :: GdbShow a => String -> Maybe a -> Maybe Option -- {{{2
valueOpt _ Nothing = Nothing
valueOpt flag param = Just (Option (QuotedString flag) (fmap (QuotedString . gdbShow) param))

maybTupleOpt :: GdbShow a => Maybe (a, a) -> [Option] -- {{{2
maybTupleOpt Nothing = []
maybTupleOpt (Just (lowFrame, highFrame)) = map opt [lowFrame, highFrame]

(?:) :: Maybe a -> [a] -> [a] -- {{{1
(Just x) ?: xs = x : xs
Nothing ?:  xs = xs
infixr 5 ?:

escapeQuotes :: String -> String -- {{{2
escapeQuotes = replace '"' "\\\""

replace :: Char -> String -> String -> String -- {{{2
replace old new = join new . split old
  where
    join delim l = concat (intersperse delim l)
    split _ [] = []
    split c str =
      let (str', rest) = break (==c) str in
      if null rest
        then [str']
        else str' : split c (tail rest)
