all:
	cabal clean
	cabal configure --enable-tests
	cabal build
	cabal haddock
	cabal test
