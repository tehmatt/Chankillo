module Compile (compile) where
	import Data.List
	import System.IO
	import System.Process
	import System.Directory
	import System.Exit
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
	compileCode tmpDir path = do
		compiled <- readProcess "gcc" ["-o", execPath, "-lm", tmpDir ++ path] []
		return (case compiled of
			[] -> Compiled execPath
			x -> CompileError compiled)
		where execPath = (tmpDir ++ "/tmpChankillo")
