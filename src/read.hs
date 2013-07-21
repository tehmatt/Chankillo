module Read (readEvalPrintLoop) where
	import System.IO

	prompt = ">>> " :: String

	readEvalPrintLoop :: IO ()
	readEvalPrintLoop = do
		putStr prompt
		maybeLine <- getLine
		case maybeLine of
			"exit" -> return ()
			"\EOT" -> return ()
			line ->	do
				putStr $ "Input was " ++ line ++ "\n"
