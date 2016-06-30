update-and-build: update build

update:
	cabal update
	cabal sandbox init
	cabal install --force-reinstalls --only-dependencies --disable-optimization
	cabal configure --disable-optimization

build:
	cabal build

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
	cabal build --ghc-options="-fforce-recomp -Wall"

