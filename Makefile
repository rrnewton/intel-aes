
# This is mainly here for the "package" hack.

all:
	cabal configure 
	cabal build

validate:
	rm -rf dist/intel-aes-*
	cabal sdist
	(cd dist/; tar xzvf intel-aes-*)
	rm -f dist/intel-aes-*.tar.gz
	(cd dist/intel-aes-*; cabal install)

doc:
	cabal haddock --hoogle --executables --hyperlink-source --haddock-options="--html"

clean:
	cabal clean