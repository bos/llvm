ghc := ghc
ghcflags := -Wall -Werror
examples := HelloJIT HowToUseJIT

all: $(examples)

%: %.hs
	$(ghc) $(ghcflags) --make -o $@ -main-is $(basename $<).main $<

clean:
	-rm -f *.o *.hi $(examples)
