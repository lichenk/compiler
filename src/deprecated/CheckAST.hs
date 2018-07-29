{- L1 Compiler
   Author: Matthew Maurer <mmaurer@andrew.cmu.edu>
   Modified by: Ryan Pearl <rpearl@andrew.cmu.edu>
                Rokhini Prabhu <rokhinip@andrew.cmu.edu>
   Beginnings of a typechecker
-}
module Compile.CheckAST where

import Control.Monad.State
import Control.Monad.Error
import Control.Monad

import qualified Data.Set as Set

import Compile.Types

-- Note to the student:
-- When your checker gets larger, you may wish to formalize the state
-- a little more. This is not designed to scale.

runErrorState :: ErrorT String (State s) a -> s -> Either String a
runErrorState m s = evalState (runErrorT m) s

assertMsg :: (Monad m) => String -> Bool -> ErrorT String m ()
assertMsg s True  = return ()
assertMsg s False = throwError s

assertMsgE :: String -> Bool -> Either String ()
assertMsgE s True  = Right ()
assertMsgE s False = Left s

type TypeCheckState = (Set.Set Ident, Set.Set Ident)

typeCheck :: AST -> Either String ()
typeCheck ast@(Block stmts _) = do
  hasReturn <- fmap or $ runErrorState (mapM checkStmt stmts) $ (Set.empty, Set.empty)
  assertMsgE "main does not return" $ hasReturn
  return ()

checkStmt :: Stmt -> ErrorT String (State TypeCheckState) Bool
checkStmt (Decl decl) = do
  checkDecl decl
  return False
checkStmt (Simp simp) = do
  checkSimp simp
  return False
checkStmt (Ret e) = do
  (declared, defined) <- get
  checkExp e
  put (declared, Set.union declared defined)
  return True

checkDecl :: Decl -> ErrorT String (State TypeCheckState) Bool
checkDecl (JustDecl var _) = do
  (declared, defined) <- get
  assertMsg "Variable already declared" (Set.notMember var declared)
  assertMsg "Variable already defined" (Set.notMember var defined)
  put (Set.insert var declared, defined)
  return False
checkDecl (DeclAsgn var exp _) = do
  (declared, defined) <- get
  assertMsg "Variable already declared" (Set.notMember var declared)
  assertMsg "Variable already defined" (Set.notMember var defined)
  checkExp exp
  put (Set.insert var declared, Set.insert var defined)
  return False  

checkSimp :: Simp -> ErrorT String (State TypeCheckState) Bool
checkSimp (Asgn (Var var) asnop exp _) = do
  (declared, defined) <- get
  assertMsg "Variable not declared earlier" (Set.member var declared)
  case asnop of
    AsnOp _ -> (do
      assertMsg "Variable not defined earlier" (Set.member var defined)
      checkExp exp
      return False)
    Equal -> (do
      checkExp exp
      put (declared, Set.insert var defined)
      return False)

checkExp :: Exp -> ErrorT String (State TypeCheckState) ()
checkExp (Int n _) = do
  assertMsg "Number is too large" (True) --(num n <= 2^31)
checkExp (Ident var _) = do
  (declared, defined) <- get
  assertMsg "Variable not declared" (Set.member var declared)
  assertMsg "Variable not defined" (Set.member var defined)
checkExp (Binop _ exp1 exp2 _) = do
  checkExp exp1
  checkExp exp2
checkExp (Unop _ exp _) = do
  checkExp exp