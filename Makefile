update-and-build: update build doc

update:
	cabal update
	cabal sandbox init
	cabal install --force-reinstalls --only-dependencies --disable-optimization
	cabal configure --disable-optimization --enable-coverage

build:
	cabal build --ghc-options="-Wall -fwarn-incomplete-record-updates -fno-warn-orphans"

install:
	cp dist/build/hamsql/hamsql /usr/local/bin/ 

clean:
	cabal clean
	find src/ \( -name '*.hi' -or -name '*.o' \) -exec rm {} ';'

build-without-dep:
	cabal sandbox init
	cabal install frontmatter
	cabal configure --disable-optimization
	cabal build

build-wall:
	cabal configure --disable-optimization
	cabal build --ghc-options="-fforce-recomp -Wall -fwarn-incomplete-record-updates -fno-warn-orphans"

build-optim:
	cabal configure --enable-optimization
	cabal build --ghc-options="-fforce-recomp"

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

doc:
	cabal haddock --executables

test:
	make -C tests
	-rm -r tests/coverage
	hpc report tests/hamsql.tix --hpcdir dist/hpc/dyn/mix/hamsql
	hpc markup tests/hamsql.tix --hpcdir dist/hpc/dyn/mix/hamsql/ --destdir=tests/coverage --verbosity=0
