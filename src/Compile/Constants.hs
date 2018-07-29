module Compile.Constants where
import Compile.Types.FAST as F
import Compile.Types.EAST as E

declarationPrefix = "1___DECLPREFIX__"
arrayStructPrefix = "arrayzStruct_"
machinePrefix = "" -- Should be _ for Macs, empty string for Linux, don't try running this code on Windows
functionPrefix = "_c0_"
headerPrefix = machinePrefix
scramblePrefix = "___scramble_"
divName = "div"
modName = "mod"
salName = "sal"
sarName = "sar"


allOnes32bit = 4294967295

predeclaredFunctions = 
	[(F.Fdecl F.CInt "main" []), (F.Fdecl F.CVoid ("abort") [])]

predeclaredFunctionsE = 
	[(E.Fdecl E.CInt "main" []), (E.Fdecl E.CVoid ("abort") [])]

labelPrefix = ".L"

max_32bit_unsigned = 2147483647
