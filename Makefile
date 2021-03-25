all: polf.prg demo/demo

polf.prg: polf.asm
	64tass --cbm-prg -o $@ -L polf.lst $<

demo/demo: demo/quickcg.cc demo/quickcg.h demo/raycaster.cc demo/Fixed.h
	g++ --std=c++17 -g -o $@ demo/quickcg.cc demo/raycaster.cc -lSDL
