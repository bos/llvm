ghc := ghc
ghcflags := -Wall -optl -w
# -DHAS_GETPOINTERTOGLOBAL=1
examples := HelloJIT Fibonacci BrainF Vector Array DotProd Arith Align Struct

all: $(examples:%=%.exe)

Vector:	Convert.hs

%.exe: %.hs
	$(ghc) $(ghcflags) --make -o $(basename $<).exe -main-is $(basename $<).main $<

Struct.exe:	Struct.hs structCheck.c
	$(ghc) $(ghcflags) --make -o Struct.exe -main-is Struct.main Struct.hs structCheck.c

%.run: %.exe
	./$<

run:	$(examples:%=%.run)

N=40
fastfib:	Fibonacci
	@rm -f Fib.bc Fib.s
	time ./Fibonacci $(N)
	opt -std-compile-opts Fibonacci.bc -o Fib.bc
	llc Fib.bc
	$(CC) mainfib.c Fib.s -o Fib
	time ./Fib $(N)
	@echo Have a look at Fib.s if you like to see clever code.

clean:
	rm -f $(examples) *.o *.hi *.s *.bc Fib *.exe *.exe.manifest *~
