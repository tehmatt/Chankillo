module Types (Compiled(..)) where
	import System.IO

	data Compiled = Compiled FilePath
					| CompileError String
