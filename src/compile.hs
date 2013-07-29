module Compile (compile) where
	import Data.List
	import System.IO
	import System.Process
	import System.Directory

	compile :: [String] -> IO [String]
	compile input =
		let
			path = "/tmpChankillo.c"
		in do
			tmpDir <- getTemporaryDirectory;
			code <- wrapper input
			writeFile (tmpDir ++ path) $ concat $ intersperse "\n" code
			output <- compileCode (tmpDir ++ path)
			result <- runCode
			return result

	getStarterCode :: IO [String]
	getStarterCode = do
		workingDir <- getCurrentDirectory
		starterCode <- readFile $ workingDir ++ "/../resources/c_starter"
		return $ lines starterCode

	wrapper :: [String] -> IO [String]
	wrapper code = do
		starterCode <- getStarterCode
		return $ starterCode ++ ("int main() {":code) ++ ["}"]

	compileCode :: FilePath -> IO [String]
	compileCode path = do
		tmpDir <- getTemporaryDirectory
		result <- readProcess "gcc" ["-o", (tmpDir ++ "/tmpChankillo"), "-lm", path] ""
		return (if result == "" then [] else lines result)

	runCode :: IO [String]
	runCode = do
		tmpDir <- getTemporaryDirectory
		result <- readProcess (tmpDir ++ "/tmpChankillo") [] ""
		return $ lines result
