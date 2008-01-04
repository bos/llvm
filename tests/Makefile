ghc := ghc
ghcflags := -Wall -Werror
tests := TestType TestValue

all: $(tests:%=%.out)

%.out: %.test
	./$< > $@ 2>&1; s=$$?; cat $@; \
	if [ $$s != 0 ]; then mv $@ $(basename $@).err; exit 1; fi

.PRECIOUS: %.test
%.test: %.hs
	$(ghc) $(ghcflags) --make -o $@ -main-is $(basename $<).main $<

clean:
	-rm -f *.o *.hi $(tests:%=%.test) $(tests:%=%.out)
