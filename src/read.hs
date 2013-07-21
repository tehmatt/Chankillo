module Read (history, repl) where
	import System.IO

	prompt :: IO String
	prompt = do
		putStr ">>> "
		hFlush stdout
		getLine

	history = [] :: [String]

	prepend :: Monad m => a -> [a] -> m [a]
	prepend x [] = return [x]
	prepend x xs = return (x:xs)

	repl :: IO ()
	repl = do
		maybeLine <- prompt;
		case maybeLine of
			"exit" -> return ()
			"\EOT" -> return ()
			line ->
				prepend line history >>= \history -> putStrLn ("Input was " ++ line) >>
				repl
