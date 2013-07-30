module Read (repl) where
	import Data.List
	import System.IO
	import Parse
	import Compile

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
		maybeLine <- prompt;
		case maybeLine of
			"exit" -> return ()
			"\EOT" -> return ()
			line -> do
					history <- (prepend (validLineEnding line) history)
					compile $ reverse $ parse history
					repl history
