module Compile (compile) where
	import Data.List
	import System.IO
	import System.Process
	import System.Directory
	import System.Exit

	compile :: [String] -> IO Int
	compile input =
		let
			path = "/tmpChankillo.c"
		in do
			tmpDir <- getTemporaryDirectory;
			code <- wrapper input
			writeFile (tmpDir ++ path) $ concat $ intersperse "\n" code
			output <- compileCode (tmpDir ++ path)
			runCode

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

	runCode :: IO Int
	runCode = do
		tmpDir <- getTemporaryDirectory
		running <- runProcess (tmpDir ++ "/tmpChankillo") [] Nothing Nothing Nothing Nothing Nothing
		exitCode <- waitForProcess running
		return (case exitCode of
			ExitSuccess -> 0
			ExitFailure x -> x)
