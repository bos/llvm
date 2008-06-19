ghc := ghc
ghcflags := -Wall -Werror

LLVMLIB=/usr/local
llvm_prefix ?= $(LLVMLIB)
prefix ?= $(LLVMLIB)
_lib := $(shell test -d /usr/lib64 && echo lib64 || echo lib)

ifeq ($(prefix),$(HOME))
user_flag := --user
endif

all: build

.PHONY: build
build: dist/setup-config
	./setup build

dist/setup-config: setup configure llvm.cabal llvm.buildinfo.in
	./setup configure --prefix=$(prefix) --libdir=$(prefix)/$(_lib) \
	    --configure-option --with-llvm-prefix=$(llvm_prefix) $(user_flag)

setup: Setup.lhs
	$(ghc) --make -O -o $@ $<

configure: configure.ac
	autoreconf

.PHONY: examples
examples:
	$(MAKE) -C examples

.PHONY: tests
tests:
	$(MAKE) -C tests

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
	-rm -f Setup.hi Setup.o
	-./setup clean
	-rm setip

distclean: clean
	-rm -f setup configure
