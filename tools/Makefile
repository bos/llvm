ghc := ghc
ghcflags := -O -Wall -Werror
tools := IntrinsicMangler

all: $(tools)

%: %.hs
	$(ghc) $(ghcflags) --make -o $@ -main-is $(basename $<).main $<

clean:
	-rm -f *.o *.hi $(tools)
