ghc := ghc
ghcflags := -Wall -optl -w
# -DHAS_GETPOINTERTOGLOBAL=1
examples := HelloJIT Fibonacci BrainF Vector Array DotProd Arith Align Struct Varargs List CallConv

all: $(examples:%=%.exe)

Vector:	Convert.hs

%.exe: %.hs
	$(ghc) $(ghcflags) --make -o $@ -main-is $(basename $<).main $<

%_dyn.exe: %.hs
	$(ghc) $(ghcflags) -dynamic --make -o $@ -main-is $(basename $<).main $<

Struct.exe: ghcflags += -fcontext-stack=27
Struct.exe:	Struct.hs structCheck.c
	$(ghc) $(ghcflags) --make -o $@ -main-is Struct.main $^

Struct_dyn.exe:	Struct.hs structCheck.c
	$(ghc) $(ghcflags) -dynamic --make -o $@ -main-is $(basename $<).main $^

%.run: %.exe
	./$<

run:	$(examples:%=%.run)

%.s:	%.bc
	llc -f $<

# This would lead to a cycle with llvm-as.
# %.ll: %.bc
#	llvm-dis -f $<

%-dis.ll:	%.bc
	llvm-dis -o $@ -f $<

%.bc:	%.ll
	llvm-as -f $<

%-opt.bc:	%.bc
	opt -O3 < $< > $@

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
