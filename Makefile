build:
	cabal update
	cabal sandbox init
	cabal install --only-dependencies
	cabal build

install:
	cp dist/build/hamsql/hamsql /usr/local/bin/ 

clean:
	cabal clean
