# makefile for ParScript

all : main

main : 
	@if which ghc >/dev/null; then \
	ghc -O2 --make Main.hs -o par; \
	else echo "No ghc found, exiting..."; exit 1; fi

clean :
	@rm *.o *.hi par

test :
	@echo Running tests... ; \
	perl tests.pl
