
module Compile.GenGDecls where
import Compile.Types
import Compile.CodeGen
import Compile.GenerateStruct

genDecls :: [Gdecl] -> [[AAsm]]
genDecls l =
  let
    sdefs = filter isSdef l
    fdefs = filter isFdef l
    structFunctions = buildStructs sdefs
  in
    map (codeGen structFunctions) fdefs

isSdef :: Gdecl -> Bool
isSdef (Sdef _ _) = True
isSdef _ = False

isFdef :: Gdecl -> Bool
isFdef (Fdefn _ _ _ _) = True
isFdef _ = False 
