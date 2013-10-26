module Compile (compile) where
	import Control.Exception
	import System.Directory
	import System.Process
	import System.Exit
	import Data.List
	import System.IO
	import Types

	-- Returns the path of the executable
	compile :: [String] -> IO Compiled
	compile input =
		let
			path = "/tmpChankillo.c"
		in do
			tmpDir <- getTemporaryDirectory;
			code <- wrapper input
			writeFile (tmpDir ++ path) $ concat $ intersperse "\n" code
			compileCode tmpDir path

	getStarterCode :: IO [String]
	getStarterCode = do
		workingDir <- getCurrentDirectory
		starterCode <- readFile $ workingDir ++ "/../resources/c_starter"
		return $ lines starterCode

	wrapper :: [String] -> IO [String]
	wrapper code = do
		starterCode <- getStarterCode
		return $ starterCode ++ ("int main() {":code) ++ ["}"]

	compileCode :: FilePath -> FilePath -> IO Compiled
	compileCode tmpDir path =
		let
			execPath = (tmpDir ++ "/tmpChankillo")
			options = ["-o", execPath, "-std=c99", "-lm", "-O2", "-pipe", "-march=native"]
			gccOptions = options ++ [tmpDir ++ path]
		in do
			compiled <- try (readProcess "gcc" gccOptions []) :: IO (Either IOError String)
			return (case compiled of
				Left e -> CompileError ""
				Right [] -> Compiled execPath
				Right e -> CompileError e)
