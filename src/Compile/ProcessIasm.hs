module Compile.ProcessIasm where
import Compile.Types.IntermediaryAssembly

getMaxTempList :: [IAsm] -> Int
getMaxTempList iasmList = foldl max 0 List.map getMaxTemp iasmList

getMaxTemp :: IAsm -> Int
getMaxTemp (IAsm loc op val) = max(getLocNum loc, getValNum val)
getMaxTemp (IRet val) = getValNum val

getLocNum (ATemp n _) = n
getLocNum (AHeap val _) = getValNum val
getLocNum _ = 0

getValNum (ALoc loc) = getLocNum loc
getValNum _ = 0

processIasm iasmList =
	let
		maxTemp = getMaxTemplist iasmList
		tempToUse = maxTemp + 1
	in
		splitDeref tempToUse iasmList

splitDeref n iasmList =
