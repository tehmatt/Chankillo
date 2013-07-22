module Read (repl) where
	import Data.List
	import System.IO
	import Parse
	import Compile

	prompt :: IO String
	prompt = do
		putStr ">>> "
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
			line ->
				let
					l = validLineEnding line
				in
					do
						history <- (prepend l history)
						compiled <- compile $ parse $ reverse history
						mapM_ putStrLn compiled
						repl history
