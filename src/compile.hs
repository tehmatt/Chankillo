module Compile (compile) where
	import Data.List
	import System.IO

	compile :: [String] -> IO [String]
	compile code = do
		putChar '\0'
		return (["Compiled:"] ++ (map (\x -> ' ':x) code))
