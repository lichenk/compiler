module Compile.Types.AbstractLivenessRules where

import Compile.Types.AbstractAssembly

data ARule = AUse ALoc
           | ADef ALoc deriving (Eq, Ord)

instance Show Rule where
  show (Use aloc) = "Use(" ++ show aloc ++ ")"
  show (Def aloc) = "Def(" ++ show aloc ++ ")"