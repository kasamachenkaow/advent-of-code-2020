setup:
	cabal update && cabal install --lib split \
	&& cabal install --lib split \
	&& cabal install --lib regex-tdfa