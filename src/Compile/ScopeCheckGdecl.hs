module Compile.ScopeCheckGdecl (scopeGdeclListHeader, scopeGdeclListSource) where
import qualified Compile.MapWrap as MapWrap
import Compile.ScopeCheck
import qualified Data.Set as Set
import Debug.Trace

import qualified Compile.Types.EAST as E

-- Set of defined functions
type ScopeState = Set.Set E.Ident

-- Adds all defined functions to the scopestate
-- So we can check if we are only calling functions who have been defined
addDefinedFunctionsToScopeState :: ScopeState -> [E.Gdecl] -> ScopeState
addDefinedFunctionsToScopeState scopestate gdeclList =
	foldl addFdefToScopeState scopestate gdeclList

-- Looks at a Gdecl. If it is a function definition, add it to the scope state
addFdefToScopeState :: ScopeState -> E.Gdecl -> ScopeState
addFdefToScopeState scopestate (E.Fdefn ctype ident paramlist block) =
	Set.insert ident scopestate
addFdefToScopeState scopestate _ = scopestate

-- All functions in the header are considered defined
-- Given the Gdecl list for the header file,
-- We add declared functions to the scope state before scopechecking the source file
addDeclaredFunctionsToScopeState :: ScopeState -> [E.Gdecl] -> ScopeState
addDeclaredFunctionsToScopeState scopestate headerGdeclList =
	foldl addFdeclToScopeState scopestate headerGdeclList

-- Looks at a Gdecl. If it is a function definition, add it to the scope state
addFdeclToScopeState :: ScopeState -> E.Gdecl -> ScopeState
addFdeclToScopeState scopestate (E.Fdecl ctype ident paramlist) =
	Set.insert ident scopestate
addFdeclToScopeState scopestate _ = scopestate


-- Scopechecking for source file
-- Add all defined functions to the scopestate
-- Then do our scope checking
-- So we can check that we only call defined functions
-- This is because functions can be defined after they are called, as long as they are
-- declared beforehand.
scopeGdeclListSource :: ScopeState -> [E.Gdecl] -> (ScopeState, Bool)
scopeGdeclListSource state gdeclList =
	let
		stateWithDefinedFunctions = addDefinedFunctionsToScopeState state gdeclList
		(newstate, valid) = scopeGdeclList stateWithDefinedFunctions gdeclList
	in
		(newstate, valid)

-- All functions in the header are considered defined
-- So we have to add those shit to the scopestate
-- So they are viewed as being defined by our source file
scopeGdeclListHeader :: ScopeState -> [E.Gdecl] -> (ScopeState, Bool)
scopeGdeclListHeader state gdeclList =
	let
		(newstate, valid) = scopeGdeclList state gdeclList
		-- All functions in the header are considered defined
		stateWithDeclaredFunctions = addDeclaredFunctionsToScopeState newstate gdeclList
	in
		(stateWithDeclaredFunctions, valid)


-- Scopechecks and adds types to Gdecl in the Gdecl list
scopeGdeclList :: ScopeState -> [E.Gdecl] -> (ScopeState, Bool)
scopeGdeclList state (gdecl:gdeclList) =
	let
		(newstate, valid) = scopeGdecl state gdecl
		(finalstate, finalvalid) = scopeGdeclList newstate gdeclList
	in
		(finalstate, valid && finalvalid)
scopeGdeclList state [] = (state, True)

-- Scopechecks and adds types to Gdecl in the Gdecl list
scopeGdecl :: ScopeState -> E.Gdecl -> (ScopeState, Bool)
scopeGdecl scopestate (E.Fdecl ctype ident paramlist) =
	(Set.insert ident scopestate, True)
scopeGdecl scopestate gdecl@(E.Fdefn ctype ident paramlist block) = checkFunction scopestate gdecl 
scopeGdecl scopestate _ = (scopestate,True)
