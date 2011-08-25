
# This is mainly here for the "package" hack.

all:
	cabal configure 
	cabal build

# I'm having trouble getting cabal's "data-files" or "extra-source-files" to work.
package:
	rm -rf dist/intel-aes-*
	cabal sdist
	(cd dist/; tar xzvf intel-aes-*)
	rm -f dist/intel-aes-*.tar.gz
        # HACK, manually insert files into distribution package:
	cp -a benchmark-intel-aes-rng.hs dist/intel-aes-*/
	(cd dist/; tar czvf intel-aes-sdist.tar.gz intel-aes-*/)

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