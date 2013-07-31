module Forking (ioFork, waitFork) where
	import Control.Concurrent

	-- Fork a process
	-- TODO: error handling (mvar needs to be written even in case of crash
	ioFork :: MVar () -> IO () -> IO ()
	ioFork mvar process = do
		_ <- process
		putMVar mvar ()

	waitFork :: MVar () -> IO ()
	waitFork mvar = takeMVar mvar
