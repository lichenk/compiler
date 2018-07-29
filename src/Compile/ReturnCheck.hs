module Compile.ReturnCheck (checkAndTruncateReturn) where
import qualified Compile.Types.TAST as E
import Compile.Types
import Debug.Trace

-- If functionType is void, we must add a return to the end
addReturnIfVoid :: E.Block -> E.CType -> E.Block
addReturnIfVoid block functionType =
    case functionType of
        E.CVoid -> block ++ [E.Ctrl E.RetVoid]
        _ -> block


-- checkAndTruncateReturn truncates (deletes) all unreachable statements after a return, and raises an error if there exists a code flow with no return.
checkAndTruncateReturn :: E.Gdecl -> E.Gdecl
checkAndTruncateReturn (E.Fdefn ctype ident paramlist block) =
	let
		blockAddReturnIfVoid::E.Block
		blockAddReturnIfVoid = addReturnIfVoid block ctype
	in
	case (checkAndTruncateBlock blockAddReturnIfVoid) of
		(_, False) -> error ("ReturnCheck.hs: There exists a code flow with no return.")
		(newBlock, True) -> E.Fdefn ctype ident paramlist newBlock

-- 'checkAndTruncateBlock newBlock truncates all unreachable statements after a return, and returns a tuple of the truncated block and whether the block has return statements on all code paths
-- Args:
--  block: Input block
-- Output:
-- A tuple containing
--   newBlock: Contains all statements in input block except statements which are unreachable since they are after a return statement
--   boolReturn: True iff all code paths in the input block have a return statement
checkAndTruncateBlock :: E.Block -> (E.Block, Bool)
checkAndTruncateBlock (stmt:block) =
	let
		(newStmt, boolStmtReturn) = checkAndTruncateStmt stmt
		(newBlock, boolBlockReturn) = checkAndTruncateBlock block
	in
		case boolStmtReturn of
			True -> ([newStmt], boolStmtReturn || boolBlockReturn)
			False -> (newStmt:newBlock, boolStmtReturn || boolBlockReturn)
checkAndTruncateBlock [] = ([], False)

-- 'checkAndTruncateStmt newStmt truncates all unreachable statements after a return, and returns a tuple of the truncated block and whether the stmt has return statements on all code paths
-- Args:
--  stmt: Input statement
-- Output:
-- A tuple containing
--   newStmt: Contains all statements in input stmt except statements which are unreachable since they are after a return statement
--   boolReturn: True iff all code paths in the input stmt have a return statement

checkAndTruncateStmt :: E.Stmt -> (E.Stmt, Bool)
checkAndTruncateStmt (E.Simp simp) = (E.Simp simp, False)
checkAndTruncateStmt (E.Ctrl ctrl) = 
	let (newCtrl, boolCtrlReturn) = checkAndTruncateCtrl ctrl in (E.Ctrl newCtrl, boolCtrlReturn)
checkAndTruncateStmt (E.Blk block) = 
	let (newBlock, blockRet) = checkAndTruncateBlock block in (E.Blk newBlock, blockRet)

-- 'checkAndTruncateCtrl newCtrl truncates all unreachable statements after a return, and returns a tuple of the truncated ctrl and whether the ctrl statement has return statements on all code paths
-- Args:
--  ctrl: Input ctrl statement
-- Output:
-- A tuple containing
--   newCtrl: Contains all statements in input stmt except statements which are unreachable since they are after a return statement
--   boolReturn: True iff all code paths in the input stmt have a return statement

checkAndTruncateCtrl :: E.Ctrl -> (E.Ctrl, Bool)
checkAndTruncateCtrl (E.If expr stmt1 stmt2) = 
	let
		(newStmt, boolStmt1Return) = checkAndTruncateStmt stmt1
		(newStmt2, boolStmt2Return) = checkAndTruncateStmt stmt2
	in
		(E.If expr newStmt newStmt2, boolStmt1Return && boolStmt2Return)
checkAndTruncateCtrl (E.While expr stmt) = 
	let
		(newStmt, stmtReturn) = checkAndTruncateStmt stmt
	in
		(E.While expr newStmt, False)
checkAndTruncateCtrl (E.Ret a) = (E.Ret a, True)
checkAndTruncateCtrl (E.RetVoid) = (E.RetVoid, True)