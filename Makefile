update-and-build: update build

update:
	cabal update
	cabal sandbox init
	cabal install --only-dependencies --force-reinstalls

build:
	cabal build

install:
	cp dist/build/hamsql/hamsql /usr/local/bin/ 

clean:
	cabal clean
	find src/ \( -name '*.hi' -or -name '*.o' \) -exec rm {} ';'
