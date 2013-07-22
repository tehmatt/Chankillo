module Parse (parse, validLineEnding) where
	import Data.List
	import Data.List.Split

	keywords :: IO [String]
	keywords = do
		wordlist <- readFile "../resources/keywords"
		return $ splitOn "\n" wordlist

	operators :: IO [String]
	operators = do
		oplist <- readFile "../resources/operators"
		return $ splitOn "\n" oplist

	parse :: [String] -> [String]
	parse lines = lines

	-- Lines must end with '{','}', or ';'.
	validLineEnding :: String -> String
	validLineEnding x = if any ((last x)==) "{};" then x else (x ++ ";")
