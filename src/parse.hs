module Parse (parse, validLineEnding) where
	import Data.List

	parse :: [String] -> [String]
	parse lines = lines

	-- Lines must end with '{','}', or ';'.
	validLineEnding :: String -> String
	validLineEnding x = if any ((last x)==) "{};" then x else (x ++ ";")
