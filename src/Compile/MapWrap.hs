module Compile.MapWrap where
import Debug.Trace 
import qualified Data.Map as RealMap
infixl 9 !
(!) :: (Ord k, Show k, Show a) => RealMap.Map k a -> k -> a
m ! k = case (RealMap.member k m) of
	True -> (case (RealMap.lookup k m) of
		Just x -> x
		Nothing -> error("Should not reach here in MapWrap")
		)
	False -> traceStack ("MapWrap: Key not in map: Key:" ++ show k ++ " Map:" ++ show m) (error(breakpointPoint m k))

breakpointPoint :: RealMap.Map k a -> k -> String
breakpointPoint m k = ("MapWrap: Key not in map")
