update-and-build: update build

update:
	cabal update
	cabal sandbox init
	cabal install --only-dependencies

build:
	cabal build

install:
	cp dist/build/hamsql/hamsql /usr/local/bin/ 

clean:
	cabal clean
