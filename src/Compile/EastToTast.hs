module Compile.EastToTast(getTastSdef, getTastFdecl) where
import qualified Compile.Types.EAST as E
import qualified Compile.Types.TAST as T

getTastSdef l = 
	map eastToTast (filter isSdef l)
getTastFdecl l = 
	map eastToTast (filter isFdecl l)

isSdef (E.Sdef _ _) = True
isSdef _ = False

isFdecl (E.Fdecl _ _ _) = True
isFdecl _ = False

eastToTast (E.Fdecl ctype ident paramlist) =
	T.Fdecl (changeType ctype) ident (map changeParam paramlist)

eastToTast (E.Sdef ident fieldlist) = 
	T.Sdef ident (map changeField fieldlist)

changeField (E.Field ctype ident) = T.Field (changeType ctype) ident

changeParam :: E.Param -> T.Param
changeParam (E.Param ctype ident) = T.Param (changeType ctype) ident

changeType :: E.CType -> T.CType
changeType E.CInt =    T.CInt
changeType E.CBool =   T.CBool
changeType E.CNoType = T.CNoType
changeType E.CVoid =   T.CVoid
changeType (E.CPtr ctype   ) = T.CPtr (changeType ctype)
changeType (E.CArray ctype ) = T.CArray (changeType ctype)
changeType (E.CStruct ident) = T.CStruct ident
changeType (E.CAny    ) = T.CAny    

