module Compile.JumpGen where

import Compile.Types
import qualified Data.Map as Map
import Compile.RegisterList
import Data.Tuple
import Debug.Trace
import Compile.Types.JumpAbstractAssembly

eflagsRegister = (JReg (fromIntegral eflagsRegisterNumber) 0 CAny)

convertComparisonToInvertedJump :: AOp -> JumpType
convertComparisonToInvertedJump ALess    = RJGe
convertComparisonToInvertedJump ALeq     = RJG
convertComparisonToInvertedJump AGeq     = RJL
convertComparisonToInvertedJump AGreater = RJLe
convertComparisonToInvertedJump AEq      = RJne
convertComparisonToInvertedJump ANeq     = RJe


convertToJmp :: [AAsm] -> Int -> ([JAsm], Int)
convertToJmp original labelnum = convertToJmpHelp original labelnum

-- Takes in the AAsm to convert, the current label number
-- it can use, and outputs the converted JAsm and the last label number it used + 1
convertToJmpHelp :: [AAsm] -> Int -> ([JAsm], Int)
convertToJmpHelp [] n = ([], n)
convertToJmpHelp (instruction:rest) currentLabel =
  case instruction of
    AAsm dests op args -> 
      let
        (remaining, finalLabel) = convertToJmpHelp rest currentLabel
      in
        ([JAsm (convertDests dests) op (convertSrcs args)] ++ remaining, finalLabel)
    ARet retval -> 
      let
        (remaining, finalLabel) = convertToJmpHelp rest currentLabel
      in
        ([JRet (convertVal retval)] ++ remaining, finalLabel)
    ACall dest func args -> 
      let
        (remaining, finalLabel) = convertToJmpHelp rest currentLabel
      in
        ([JCall (convertLoc dest) func (convertSrcs args)] ++ remaining, finalLabel)
    AIf (ACond v) ifbrnch elsebrnch ->
      let
        elseNum = currentLabel
        endNum = currentLabel + 1
        makeCond = 
          [JAsm ([JReg 14 0 CBool]) ANop [convertVal v], JAsm [eflagsRegister] ACmp [JLoc $ JReg 14 0 CBool, JImm 0]]
        jumpToElse = [JJmp elseNum [] RJe]
        jumpToEnd = [JJmp endNum [] RJump]
        (ifJAsm, ifLabel) = convertToJmpHelp ifbrnch (currentLabel+2)
        (elseJAsm, elseLabel) = convertToJmpHelp elsebrnch ifLabel
        (remaining, finalLabel) = convertToJmpHelp rest elseLabel
      in
        (makeCond ++ jumpToElse ++ ifJAsm ++ jumpToEnd ++ [JLabel elseNum []] ++ elseJAsm ++ [JLabel endNum []] ++ remaining, finalLabel)
    AIf (ACondOptimized val1 op val2) ifbrnch elsebrnch ->
      let
        valType = getValType val1
        jmpType = convertComparisonToInvertedJump op
        elseNum = currentLabel
        endNum = currentLabel + 1
        makeCond = 
          [JAsm ([JReg 14 0 valType]) ANop [convertVal val1], JAsm [eflagsRegister] ACmp [JLoc $ JReg 14 0 valType, convertVal val2]]
        jumpToElse = [JJmp elseNum [] jmpType]
        jumpToEnd = [JJmp endNum [] RJump]
        (ifJAsm, ifLabel) = convertToJmpHelp ifbrnch (currentLabel+2)
        (elseJAsm, elseLabel) = convertToJmpHelp elsebrnch ifLabel
        (remaining, finalLabel) = convertToJmpHelp rest elseLabel
      in
        (makeCond ++ jumpToElse ++ ifJAsm ++ jumpToEnd ++ [JLabel elseNum []] ++ elseJAsm ++ [JLabel endNum []] ++ remaining, finalLabel)

    AWhile (ACond v) body ->
      let
        makeCond = 
          [JAsm ([JReg 14 0 CBool]) ANop [convertVal v], JAsm [eflagsRegister] ACmp [JLoc $ JReg 14 0 CBool, JImm 0]]
        beginLabel = [JLabel currentLabel []]
        endLabel = [JLabel (currentLabel+1) []]
        jmpBegin = [JJmp currentLabel [] RJump]
        jmpEnd = [JJmp (currentLabel+1) [] RJe]
        (makeBody, bodyLabel) = convertToJmpHelp body (currentLabel+2)
        (remaining, finalLabel) = convertToJmpHelp rest bodyLabel
      in
        (beginLabel ++ makeCond ++ jmpEnd ++ makeBody ++ jmpBegin ++ endLabel ++ remaining, finalLabel)
    AWhile (ACondOptimized val1 op val2) body ->
      let
        valType = getValType val1
        jmpType = convertComparisonToInvertedJump op
        makeCond = 
          [JAsm ([JReg 14 0 valType]) ANop [convertVal val1], JAsm [eflagsRegister] ACmp [JLoc $ JReg 14 0 valType, convertVal val2]]
        beginLabel = [JLabel currentLabel []]
        endLabel = [JLabel (currentLabel+1) []]
        jmpBegin = [JJmp currentLabel [] RJump]
        jmpEnd = [JJmp (currentLabel+1) [] jmpType]
        (makeBody, bodyLabel) = convertToJmpHelp body (currentLabel+2)
        (remaining, finalLabel) = convertToJmpHelp rest bodyLabel
      in
        (beginLabel ++ makeCond ++ jmpEnd ++ makeBody ++ jmpBegin ++ endLabel ++ remaining, finalLabel)

convertDests :: [ALoc] -> [JLoc]
convertDests dests = map convertLoc dests

convertSrcs :: [AVal] -> [JVal]
convertSrcs srcs = map convertVal srcs

convertLoc :: ALoc -> JLoc
convertLoc (AReg n t) = JReg n 0 t
convertLoc (ATemp n t) = JTemp n 0 t
convertLoc (AMem n t) = JMem n 0 t
convertLoc (AArg n t) = JArg n 0 t
convertLoc (AInArg n t) = JInArg n 0 t
convertLoc (AHeap v t) = JHeap (convertVal v) t

convertVal :: AVal -> JVal
convertVal (AImm n) = JImm n
convertVal (ALoc loc) = JLoc (convertLoc loc)

getValType :: AVal -> CType
getValType (ALoc loc) = getLocType loc
getValType (AImm _) = CInt
getLocType :: ALoc -> CType
getLocType (AReg _ t) = t
getLocType (ATemp _ t) = t
getLocType (AMem _ t) = t
getLocType (AInArg _ t) = t
getLocType (AArg _ t) = t
getLocType (AHeap _ t) = t


