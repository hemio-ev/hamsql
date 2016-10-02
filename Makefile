
VERSION = 0.8.0.0
HPCDIRS = --hpcdir dist/hpc/vanilla/mix/hamsql --hpcdir dist/hpc/vanilla/mix/hamsql-${VERSION}

update-and-build: update build

update:
	cabal sandbox init
	cabal update
	cabal install -ffast --force-reinstalls --only-dependencies --disable-optimization

tests:
	cabal configure --disable-optimization --enable-coverage --enable-tests
	cabal test --show-details direct
	cabal build
	-rm test/hamsql.tix
	-rm -r test/coverage
	-rm test/hamsql-stmt-log.sql
	make -C test
	hpc report test/hamsql.tix ${HPCDIRS}
	hpc markup test/hamsql.tix ${HPCDIRS} --destdir=test/coverage --verbosity=0

doc:
	cabal haddock

build:
	cabal configure --disable-optimization
	cabal build --ghc-options="-Wall -fwarn-incomplete-record-updates -fno-warn-orphans"

install:
	cp dist/build/hamsql/hamsql /usr/local/bin/ 

# ununsual options

dev-clean:
	cabal clean
	cabal sandbox delete

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
	find src/ -name '*.hs' -printf '    %P\n' | sort | sed -e 's/\.hs//' -e 's/\//\./g' | sort

dev-lang-ext:
	grep -h -r '# LANGUAGE' src/ | sort | uniq | sed -e 's/{-# LANGUAGE /    /' | sed -e 's/ #-}/,/'

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

