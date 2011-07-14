ghc := ghc
ghcflags := -Wall -Werror

LLVMLIB=/usr/local
llvm_prefix ?= $(LLVMLIB)
prefix ?= $(LLVMLIB)
_lib := $(shell test -d /usr/lib64 && echo lib64 || echo lib)

ifeq ($(prefix),$(HOME))
user_flag := --user
endif

all: configure build

configure: configure.ac
	autoreconf

.PHONY: build
build: dist/setup-config
	./setup build

dist/setup-config: setup configure llvm.cabal llvm.buildinfo.in
	./setup configure --user --prefix=$(prefix) --libdir=$(prefix)/$(_lib) \
	    --configure-option --with-llvm-prefix=$(llvm_prefix) $(user_flag)

setup: Setup.hs
	$(ghc) --make -O -o $@ $<

configure: configure.ac
	autoreconf

.PHONY: examples
examples:
	$(MAKE) -C examples

.PHONY: tests
tests:
	$(MAKE) -C tests

html: INSTALL.html PROBLEMS.html README.html

%.html: %.md
	pandoc -s -o $@ $<

doc haddock: dist/setup-config
	./setup haddock

sdist: dist/setup-config
	./setup sdist

.PHONY: install
install: setup
	./setup install

clean:
	-$(MAKE) -C examples clean
	-$(MAKE) -C tests clean
	-$(MAKE) -C tools clean
	-rm -f Setup.hi Setup.o
	-./setup clean
	-rm -f setup setup.exe setup.exe.manifest
	-rm -f *~
	-rm -rf dist

distclean: clean
	-rm -f setup configure
