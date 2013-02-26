# makefile for ParScript

all : main

main : 
	ghc -O2 --make Main.hs -o par
