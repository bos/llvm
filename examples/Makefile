ghc := ghc
ghcflags := -Wall
examples := HelloJIT Fibonacci BrainF

all: $(examples)

%: %.hs
	$(ghc) $(ghcflags) --make -o $@ -main-is $(basename $<).main $<

%.run: %
	./$<

run:	$(examples:%=%.run)

N=40
fastfib:	Fibonacci
	@rm -f Fib.bc Fib.s
	time ./Fibonacci $(N)
	opt -std-compile-opts Fibonacci.bc -o Fib.bc
	llc Fib.bc
	gcc mainfib.c Fib.s -o Fib
	time ./Fib $(N)

clean:
	rm -f $(examples) *.o *.hi *.s *.bc Fib
