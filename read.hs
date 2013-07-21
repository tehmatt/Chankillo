:m System.IO

prompt = ">>> "

readEvalPrintLoop :: IO ()
readEvalPrintLoop = do
	maybeLine <- getLine
	case maybeLine of
		Nothing -> return ()
		Just "exit" -> return ()
		Just line -> putStr "Input was " ++ line ++ "\n"
