
HS = $(shell find app/ src/ test/ -name '*.hs')
VERSION = 0.9.90.0
HPCDIRS = --hpcdir dist/hpc/vanilla/mix/hamsql --hpcdir dist/hpc/vanilla/mix/hamsql-${VERSION}

.PHONY: test $(HS)

update-and-build: update build

update:
	cabal sandbox init
	cabal update
	cabal install -ffast --force-reinstalls --only-dependencies --disable-optimization

test:
	cabal configure --disable-optimization --enable-coverage --enable-tests
	cabal test --show-details=direct --test-option=--color=always

doc:
	cabal haddock

build:
	cabal configure --disable-optimization
	cabal build

install:
	cp dist/build/hamsql/hamsql /usr/local/bin/ 
	hamsql --bash-completion-script hamsql > /etc/bash_completion.d/hamsql

dev-format-code: $(HS)

$(HS):
	-@../hindent/.cabal-sandbox/bin/hindent $@

# ununsual options

dev-rebuild:
	cabal build --ghc-options="-fforce-recomp"

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

