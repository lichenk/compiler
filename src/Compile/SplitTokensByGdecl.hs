-- Possible optimization:
-- Do structTokens in reverse order
module Compile.SplitTokensByGdecl where
import Compile.Lexer
import qualified Data.List as List
import Debug.Trace
data GdeclTokenListType = GdeclFdefn [Token]
                        | GdeclOther [Token]

-- Given splitByToken token [] tokenlist
-- splitByToken will split tokenlist into 2 token lists where the last element
-- of the first token list is precisely the first occurrence of token in the input
-- tokenlist
splitByToken :: Token -> [Token] -> [Token] -> ([Token],[Token])
splitByToken splitTargetToken tokensBeforeTarget (token:tokensAfterTarget) =
  case (token == splitTargetToken) of
    True -> (tokensBeforeTarget ++ [token], tokensAfterTarget)
    False -> splitByToken splitTargetToken (tokensBeforeTarget ++ [token]) tokensAfterTarget

splitByToken splitTargetToken tokensBeforeTarget [] =
  error("splitByToken cannot find Token: " ++ (show splitTargetToken))

matchBrackets :: Token -> Token -> [Token] -> [Token] -> Int -> Bool -> ([Token], [Token])
matchBrackets openBracket closeBracket functionTokens (tokenlist) bracketDepth encounteredBracketYet =
  let
    matchindex = matchit openBracket closeBracket tokenlist 0 False 0
    (left, right) = List.splitAt matchindex tokenlist
  in
    (functionTokens++left, right)


matchit openBracket closeBracket (token:tokenlist) bracketDepth encounteredBracketYet accum =
  case (token == openBracket, token == closeBracket, bracketDepth, encounteredBracketYet, bracketDepth < 0) of
    (_, _,_,_,True) -> error("matchBrackets has went into negative depth:" ++ (show bracketDepth)) -- ++ " where brackets are:" ++ (show openBracket) ++ (show closeBracket)
    (_, _, 0, True, _) -> accum
    (True, _, _, _, _) -> matchit openBracket closeBracket tokenlist (bracketDepth+1) True accum+1
    (_, True, _, _, _) -> matchit openBracket closeBracket tokenlist (bracketDepth-1) True accum+1
    (_, _,_,_, _) -> matchit openBracket closeBracket tokenlist bracketDepth encounteredBracketYet accum+1

matchit _ _ [] 0 _ accum = accum
matchit _ _ [] _ _ _ = error("No match")




splitProgram :: [GdeclTokenListType] -> [Token] -> [GdeclTokenListType]
splitProgram gdeclList [] = gdeclList
splitProgram gdeclList tokenlist = 
  let
    (gdecl, remainingTokens) = getGdecl tokenlist
  in
    splitProgram (gdeclList ++ [gdecl]) remainingTokens

getGdecl :: [Token] -> (GdeclTokenListType, [Token])
getGdecl (token:tokenlist) =
  case token of
    TokStruct -> getStruct (token : tokenlist)
    TokTypedef -> getTypedef (token : tokenlist)
    TokIdent ident -> getFunction (token : tokenlist)
    TokInt -> getFunction (token : tokenlist)
    TokBool -> getFunction (token : tokenlist)
    TokVoid -> getFunction (token : tokenlist)
    _ -> error("getGdecl could not split gdecl at:" ++ show (token:tokenlist))

-- Gets the typedef
getTypedef :: [Token] -> (GdeclTokenListType, [Token])
getTypedef fullTokenList@(TokTypedef:tokenlist) =
  let
    -- Typedefs are terminated by a semicolon
    (tokensTypedef, tokensAfterSemi) = splitByToken TokSemi [] fullTokenList
  in
    (GdeclOther tokensTypedef, tokensAfterSemi)

-- Struct declaration
getStruct :: [Token] -> (GdeclTokenListType, [Token])
getStruct (TokStruct:(TokIdent ident):TokSemi:remainingTokens) =
  (GdeclOther [TokStruct,TokIdent ident,TokSemi], remainingTokens)
-- Struct definition
getStruct fullTokenList@(TokStruct:(TokIdent ident):TokLBrace:remainingTokens) = 
  let
    -- Find perform matching on LBrace
    (sdefTokens, remainingTokens) = matchBrackets TokLBrace TokRBrace [] fullTokenList 0 False
  in
    case remainingTokens of
      TokSemi:tokensAfterSemi -> (GdeclOther (sdefTokens++[TokSemi]), tokensAfterSemi)
      _ -> error("getStruct: struct definition does not end with semicolon")
-- This may actually be a function with return type struct
-- Eg. struct foo *function
getStruct fullTokenList = getFunction fullTokenList

getFunction :: [Token] -> (GdeclTokenListType, [Token])
getFunction tokenlist =
  let
    -- Find the stupid parameters
    (tokensUpToLParen, tokensAfterLParen) = splitByToken TokLParen [] tokenlist
    -- Find the close parentheses after the parameters
    (tokensUpToRParen, tokensAfterRParen) = splitByToken TokRParen tokensUpToLParen tokensAfterLParen
  in
    -- Now that we are done with the annoying parameters, we can check to see if
    -- this is a function declaration or definition
    case tokensAfterRParen of
      -- If we end with a semicolon, then it is a function declaration
      TokSemi:tokensAfterSemi -> (GdeclOther (tokensUpToRParen ++ [TokSemi]), tokensAfterSemi)
      -- Oh God... it's a function definition. I hate this.
      TokLBrace:tokensAfterLBrace -> 
        let
          (gdeclTokens, otherTokens) = getBlock tokensUpToRParen (TokLBrace:tokensAfterLBrace)
        in
          (GdeclFdefn gdeclTokens, otherTokens)
      _ -> error("getFunction End of params list is:" ++ show tokensAfterRParen)

-- Gets a block by matching parens, appends it to tokenBefore
-- Then dumps all tokens after the block into tokenAfter
getBlock :: [Token] -> [Token] -> ([Token], [Token])
getBlock tokenBefore tokenAfter = matchBrackets TokLBrace TokRBrace tokenBefore tokenAfter 0 False
