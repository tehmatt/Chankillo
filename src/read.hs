module Read (history, readEvalPrintLoop) where
	import System.IO

	prompt = ">>> " :: String

	history = [] :: [String]

	prepend :: Monad m => a -> [a] -> m [a]
	prepend x xs = case xs of
		[] -> return [x]
		xs -> return (x:xs)

	readEvalPrintLoop :: IO ()
	readEvalPrintLoop =
		putStr prompt >>
		getLine >>= \line ->
			case line of
				"exit" -> return ()
				"\EOT" -> return ()
				line ->
					prepend line history >>= \history ->
					putStrLn ("Input was " ++ line) >>
					readEvalPrintLoop
