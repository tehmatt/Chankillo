module Compile (compile) where
	import Data.List
	import Data.List.Split
	import System.IO
	import System.Process

	compile :: [String] -> IO [String]
	compile input =
		let
			path = ".tmpChankillo.c"
		in do
			code <- wrapper input
			writeFile path $ concat $ intersperse "\n" code
			output <- compileCode path
			result <- runCode
			return result

	getStarterCode :: IO [String]
	getStarterCode = do
		starterCode <- readFile "../resources/c_starter"
		return $ splitOn "\n" starterCode

	wrapper :: [String] -> IO [String]
	wrapper code = do
		starterCode <- getStarterCode
		return $ starterCode ++ ("int main() { ":code) ++ ["}"]

	compileCode :: FilePath -> IO [String]
	compileCode path = do
		result <- readProcess "gcc" ["-o",".tmpChankillo", "-lm", path] ""
		return (if result == "" then [] else splitOn "\n" result)

	runCode :: IO [String]
	runCode = do
		result <- readProcess "bin/.tmpChankillo" [] ""
		return $ splitOn "\n" result
