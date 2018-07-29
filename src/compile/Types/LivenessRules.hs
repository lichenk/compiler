module Compile.Types.LivenessRules where

import Compile.Types.JumpAbstractAssembly
import Compile.Types.IntermediaryAssembly

data Rule = Use ILoc
          | Def ILoc deriving (Eq, Ord)

data JRule = JUse JLoc
           | JDef JLoc deriving (Eq, Ord)

instance Show Rule where
  show (Use iloc) = "Use(" ++ show iloc ++ ")"
  show (Def iloc) = "Def(" ++ show iloc ++ ")"

instance Show JRule where
  show (JUse aloc) = "Use(" ++ show aloc ++ ")"
  show (JDef aloc) = "Def(" ++ show aloc ++ ")"