module Types where
{-

data AST a
	= Source { ann :: a }
	| Device
		{ ann :: a
		, device :: String
		, operands :: [String]
		, right
		, bottom :: [AST a]
		}
	| Label --TODO would be nice to separate labels from ast tree
		{ ann :: a
		, target :: String
		, bottom :: [AST a]
		}
	| Jump
		{ ann :: a
		, target :: String
		}
	| Node
		{ ann :: a
		, right
		, bottom :: [AST a]
		}
	| Connector
		{ ann :: a
		, name :: String
		}
	| Continuation
		{ ann :: a
		, name :: String
		, right :: [AST a]
		}

data Network a = Network [(Maybe (String, a), AST a)]

-}
