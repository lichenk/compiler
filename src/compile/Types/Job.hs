{- L1 Compiler
   Author: Matthew Maurer <mmaurer@andrew.cmu.edu>
   Modified by: Ryan Pearl <rpearl@andrew.cmu.edu>

   Defines a compiler phase or job
-}
module Compile.Types.Job where
import Data.Maybe

data Job = Job
  { jobOut       :: FilePath
  , jobSource    :: FilePath
  , jobOutFormat :: OF
  , jobHeader    :: Maybe FilePath
  , jobOptimize  :: [Char]
  , jobSafe  :: Bool
  }

data OF = C0
        | Asm
        | Obj
        | ELF 
        | LLVM deriving Eq

defaultJob :: Job
defaultJob = Job {jobOut       = "",
                  jobSource    = "",
                  jobOutFormat = Asm,
                  jobHeader       = Nothing,
                  jobOptimize       = "0",
                  jobSafe       = True
                }
