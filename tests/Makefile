ghc := ghc
ghcflags := -Wall -Werror
tests := TestValue

all: $(tests:%=%.out)

%.out: %.test
	if ! ./$< 2>&1 | tee $@; then mv $@ $(basename $@).err; exit 1; fi

.PRECIOUS: %.test
%.test: %.hs
	$(ghc) $(ghcflags) --make -o $@ -main-is $(basename $<).main $<

clean:
	-rm -f *.o *.hi $(tests) $(tests:%=%.out)
