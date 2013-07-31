module Run (run) where
	import Control.Concurrent
	import System.Process
	import GHC.IO.Handle -- GHC Specific!
	import System.Exit
	import System.IO
	import Forking

	-- Run the compiled code, printing to stdout
	run :: FilePath -> IO ()
	run execPath = do
		(input, out, err, pid) <- runInteractiveProcess execPath [] Nothing Nothing
		-- Redirect input for script's use
		stdin_dup <- hDuplicate stdin
		hDuplicateTo stdin_dup input
		-- Create a lock and pass std(err|out) through wrapper functions
		errLock <- newEmptyMVar
		handler err errLock errWrap
		outLock <- newEmptyMVar
		handler out outLock outWrap
		-- Wait for process and IO threads to finish, then cleanup
		exitCode <- waitForProcess pid
		waitFork errLock
		waitFork outLock
		hClose stdin_dup
		-- Useless but extendable in the future
		return (case exitCode of
			ExitSuccess -> ()
			ExitFailure code -> ())

	-- Given a handle, fork and pass it's contents through
	-- f before printing the result to stdout
	handler :: Handle -> MVar () -> (String -> String) -> IO ()
	handler h lock f =
		ioFork lock
			( do
				output <- catch (hGetLine h) (\_ ->  return "")
				case output of
					"" -> return ()
					output -> putStrLn (f output)
			)

	-- Color the input string red
	errWrap :: String -> String
	errWrap text = "\27[31m" ++ text ++ "\27[0m"

	-- Identity function on the input string
	outWrap :: String -> String
	outWrap text = text
