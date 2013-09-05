module Read (repl) where
	import Data.List
	import System.IO
	import Parse
	import Compile
	import Run
	import Types

	prompt :: Bool -> IO String
	prompt compiled = do
		putStr ("\27[" ++ (if compiled then "32m✓" else "31m✗") ++ " >>> \27[0m")
		hFlush stdout
		getLine

	prepend :: Monad m => a -> [a] -> m [a]
	prepend x [] = return [x]
	prepend x xs = return (x:xs)

	repl :: [String] -> Bool -> IO ()
	repl history compiled = do
		line <- prompt compiled
		case () of _
				| elem line ["exit", ":q", "\EOT"] -> return ()
				| elem line ["reset", ":e", ":r"] -> repl [] True
				| otherwise -> do
					history <- prepend (validLineEnding line) history
					compiled <- compile $ reverse $ parse history
					case compiled of
						Compiled path -> do
							run path
							repl ((if (((take 6 line) == "printf") || ((take 6 $ drop 1 line) == "printf")) then tail else \x -> x) history) True
						CompileError err -> do
							putStr err
							repl (tail history) False
