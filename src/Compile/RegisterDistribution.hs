module Compile.RegisterDistribution (distributeRegisters) where

import Compile.Types
import qualified Data.Map as Map
import qualified Data.Set as Set
import Compile.RegisterList

numReg :: Int
-- numReg = 14 -- Can only use eax to r14, which is 0 to 13
numReg = 12 -- Temporary hack. We reserve r13 and r14 for RHeap moves and operations
            -- This should be fixed soon.

isTemp :: (ILoc, Int) -> Bool
isTemp (ITemp _ _, _) = True
isTemp _ = False

distributeRegisters :: RegisterColoring -> RegisterMapping
distributeRegisters colorMap =
	let
		colorList = Map.toList colorMap
		filteredOnlyTempsList = filter isTemp colorList
		-- newRegisterMapping = map convertSingle filteredOnlyTempsList
		newRegisterMapping = map convertSingle filteredOnlyTempsList
	in
		Map.fromList newRegisterMapping

convertAllToStack :: (ILoc, Int) -> (Int, RLoc)
convertAllToStack (ITemp n _, i)
        = (n, RMem (8 * n))


convertSingle :: (ILoc, Int) -> (Int, RLoc)
convertSingle (ITemp n _, i)
        | i < numReg = (n, RReg (registers !! i))
        | otherwise = (n, RMem (8 * (i - numReg)))
-- No spilling in IReg
convertSingle (IReg n _, i) = error "RegisterDistribution: Reg cannot be colored?!"
-- The below are not supposed to be used!!
convertSingle (IArg n _, i) = error "RegisterDistribution: IArg cannot be colored?!"
convertSingle (IInArg n _, i) = error "RegisterDistribution: IInArg cannot be colored?!"
convertSingle (IMem n _, i) = error "RegisterDistribution: Memory cannot be colored?!"