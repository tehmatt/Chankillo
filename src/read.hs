module Read (repl) where
	import Data.List
	import System.IO
	import Parse
	import Compile
	import Run
	import Types

	prompt :: IO String
	prompt = do
		putStr "\27[32m>>> \27[0m"
		hFlush stdout
		getLine

	prepend :: Monad m => a -> [a] -> m [a]
	prepend x [] = return [x]
	prepend x xs = return (x:xs)

	repl :: [String] -> IO ()
	repl history = do
		line <- prompt
		case () of _
				| elem line ["exit", ":q", "\EOT"] -> return ()
				| elem line ["reset", ":e", ":r"] -> repl []
				| otherwise -> do
					history <- (prepend (validLineEnding line) history)
					compiled <- compile $ reverse $ parse history
					case compiled of
						Compiled path -> run path
						CompileError err -> putStrLn err
					repl history
