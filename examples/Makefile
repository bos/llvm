ghc := ghc
ghcflags := -Wall
examples := HelloJIT Fibonacci BrainF
# BrainF

all: $(examples)

%: %.hs
	$(ghc) $(ghcflags) --make -o $@ -main-is $(basename $<).main $<

%.run: %
	./$<

clean:
	-rm -f *.o *.hi $(examples)

run:	HelloJIT.run Fibonacci.run BrainF.run
