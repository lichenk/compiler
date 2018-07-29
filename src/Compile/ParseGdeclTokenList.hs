module Compile.ParseGdeclTokenList where
import Compile.SplitTokensByGdecl
import Compile.Parser
import Compile.ParserFdefn
import Compile.Lexer
import Debug.Trace
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.List.Split as Split
import qualified Compile.Types.FAST as F

-- TypeInfo is the set of all declared types (declared by typedef) which are not structs
-- structs can already be easily parsed, so we don't need the structs
type TypeInfo = Set.Set F.Ident

-- Parses a list of gdecl token lists
-- Glorified foldl wrapper for GdeclToken
parseGdeclTokenList :: TypeInfo -> [GdeclTokenListType] -> (TypeInfo, [F.AST])
parseGdeclTokenList typeinfo (gdeclTokens:gdeclTokenList) = 
	let
		(newtypeinfo, ast) = case gdeclTokens of
			GdeclFdefn tokens -> parseFdefTokens typeinfo tokens
			GdeclOther tokens -> parseGdeclTokens typeinfo tokens
		(finaltypeinfo, finalast) = parseGdeclTokenList newtypeinfo gdeclTokenList
	in
		(finaltypeinfo, ast:finalast)
parseGdeclTokenList typeinfo [] = (typeinfo, [])


-- Parser parses Gdecls which are not Fdefs just fine.
-- Calls Parser.y
parseGdeclTokens :: TypeInfo -> [Token] -> (TypeInfo, F.AST)
parseGdeclTokens typeinfo tokenlist =
	let
		ast = parseTokens tokenlist
		newTypestate = updateTypeState typeinfo ast
	in
		(newTypestate, ast)

-- Get the stupid parser to parse. Shitty useless parser doesn't know about types
-- So we have to baby it by converting all TokIdents which are type names in TokTypeIdents
-- Calls ParserFdefn.y
parseFdefTokens :: TypeInfo -> [Token] -> (TypeInfo, F.AST)
parseFdefTokens typeinfo tokenlist =
	let
		preprocessedTokenList = preprocessTokenList typeinfo tokenlist
		hasInvalidStarPostop = checkForInvalidStarPostop tokenlist
		ast = parseFdefnTokens preprocessedTokenList
		newTypestate = updateTypeState typeinfo ast
	in
		case hasInvalidStarPostop of
			True -> error ("Invalid *x++")
			False -> (newTypestate, ast)


-- Given a typedef, insert the newly defined ident into the set of types
updateTypeState :: TypeInfo -> F.AST -> TypeInfo
updateTypeState typeinfo (F.Typedef ctype ident) = Set.insert ident typeinfo
updateTypeState typeinfo _ = typeinfo

-- Preprocess token list
-- Convert all TokIdents which were defined as types (except structs) into TokTypeIdent
-- Convert all TokIdents which are structs into TokStructIdent (strictly not necessary, the Parser can do this also)
preprocessTokenList :: TypeInfo -> [Token] -> [Token]
preprocessTokenList typeinfo fulltokenlist@(TokTimes:(TokIdent _):(TokPostop _):tokenlist) =
	error("Shit! Ugly hack in ParseGdeclTokenList: Disallow *x++ and *x-- ONLY")
preprocessTokenList typeinfo (TokStruct:(TokIdent ident):tokenlist) =
	TokStruct:(TokStructIdent ident):(preprocessTokenList typeinfo tokenlist)
preprocessTokenList typeinfo (TokDot:(TokIdent ident):tokenlist) =
	TokDot:(TokVarIdent ident):(preprocessTokenList typeinfo tokenlist)
preprocessTokenList typeinfo (TokArrow:(TokIdent ident):tokenlist) =
	TokArrow:(TokVarIdent ident):(preprocessTokenList typeinfo tokenlist)
preprocessTokenList typeinfo ((TokIdent ident):tokenlist) =
	case Set.member ident typeinfo of
		True -> (TokTypeIdent ident):(preprocessTokenList typeinfo tokenlist)
		False -> (TokVarIdent ident):(preprocessTokenList typeinfo tokenlist)
preprocessTokenList typeinfo (othertoken:tokenlist) =
	othertoken:(preprocessTokenList typeinfo tokenlist)
preprocessTokenList typeinfo [] = []

checkForInvalidStarPostop :: [Token] -> Bool
checkForInvalidStarPostop tokenlist = 
	let
		splittedStatements = Split.splitOneOf [TokSemi,TokLBrace,TokRBrace] tokenlist
	in
		any checkForInvalidStarPostopStmt splittedStatements

-- Check if statement is of the form *x++;
checkForInvalidStarPostopStmt :: [Token] -> Bool
checkForInvalidStarPostopStmt tokenlist =
	let
		n = length tokenlist
	in
		case n of
			0 -> False
			1 -> False
			2 -> False
			_ -> (case (head tokenlist, tokenlist !! (n-2), last tokenlist) of
				-- It is of the form *...)++, so it is valid with the parentheses
				(TokTimes, TokRParen, TokPostop _) -> False
				-- It is of the form *...++, so it is invvalid without the parentheses
				(TokTimes, _, TokPostop _) -> True
				-- All others are allowed
				_ -> False
				)

