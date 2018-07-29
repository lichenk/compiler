module Compile.MakeLLVMLabels where
import Compile.Types.LLVM 
import qualified Data.Map as Map

-- Puts explicit jumps before fall-to labels
explicitJumps :: [LInstr] -> [LInstr]
explicitJumps (x:y:xs) =
  case (x, y) of
    (LBr _, _) -> x:(explicitJumps (y:xs))
    (LBrCond _ _ _, _) -> x:(explicitJumps (y:xs))
    (_, LLabel n) -> x:(LBr n):(explicitJumps (y:xs))
    _ -> x:(explicitJumps (y:xs))
explicitJumps l = l

-- Changes if/while into jumps
makeLabels :: Int -> LFdecl -> (Int, LFdecl)
makeLabels startLabel (LFdecl t fname args body) = 
  let
    (nextLabel, newbody) = makeLabelsHelp startLabel body
  in
    (nextLabel, LFdecl t fname args (explicitJumps newbody))

makeLabelsHelp :: Int -> [LInstr] -> (Int, [LInstr])
makeLabelsHelp n [] = (n, [])
makeLabelsHelp n ((LIf cond ifbody elsebody):xs) =
  let
    (ifCount, newIf) = makeLabelsHelp (n+3) ifbody
    (elseCount, newElse) = makeLabelsHelp ifCount elsebody
    newBlock =
      [LBrCond cond n (n+1), LLabel n] ++ newIf ++ [LBr (n+2), LLabel (n+1)] ++ newElse ++ [LBr (n+2), LLabel (n+2)]
    (retCount, rest) = makeLabelsHelp elseCount xs
  in
    (retCount, newBlock ++ rest)
makeLabelsHelp n ((LWhile cond body):xs) =
  let
    (bodyCount, newBody) = makeLabelsHelp (n+3) body
    newBlock =
      [LLabel n, LBrCond cond (n+1) (n+2), LLabel (n+1)] ++ newBody ++ [LBr n, LLabel (n+2)]
    (retCount, rest) = makeLabelsHelp bodyCount xs
  in
    (retCount, newBlock ++ rest)
makeLabelsHelp n (line:xs) =
  let
    (retCount, rest) = makeLabelsHelp n xs
  in
    (retCount, line:rest)