update-and-build: update build

update:
	cabal update
	cabal sandbox init
	cabal install --force-reinstalls --only-dependencies --disable-optimization

test:
	cabal clean
	cabal configure --disable-optimization --enable-coverage --enable-tests
	#cabal build --ghc-options="-Wall -fwarn-incomplete-record-updates -fno-warn-orphans"
	#cabal build
	cabal test --show-details direct

doc:
	cabal haddock --executables

build:
	cabal configure --disable-optimization
	cabal build --ghc-options="-Wall -fwarn-incomplete-record-updates -fno-warn-orphans"

install:
	cp dist/build/hamsql/hamsql /usr/local/bin/ 

# ununsual options

dev-clean:
	cabal clean
	find src/ \( -name '*.hi' -or -name '*.o' \) -exec rm {} ';'

dev-build-without-dep:
	cabal sandbox init
	cabal install frontmatter
	cabal configure --disable-optimization
	cabal build

dev-rebuild:
	cabal clean
	cabal configure --disable-optimization
	cabal build --ghc-options="-fforce-recomp -Wall -fwarn-incomplete-record-updates -fno-warn-orphans"

dev-build-optim:
	cabal configure --enable-optimization
	cabal build --ghc-options="-fforce-recomp"

dev-modules:
	find src/ -name '*.hs' -printf '%P\n' | sed -e 's/\.hs//' -e 's/\//\./g'

dev-package-status:
	dpkg-query -l \
	 ghc \
	 cabal-install \
	 libghc-aeson-dev \
	 libghc-file-embed-dev \
	 libghc-network-uri-dev \
	 libghc-optparse-applicative-dev \
	 libghc-pandoc-dev \
	 libghc-postgresql-simple-dev \
	 libghc-text-dev \
	 libghc-unordered-containers-dev \
	 libghc-yaml-dev

