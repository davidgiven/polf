all: polf.prg demo/demo

polf.prg: polf.asm
	64tass --cbm-prg -o $@ -L polf.lst $<

demo/demo: demo/quickcg.cc demo/quickcg.h demo/raycaster.cc
	g++ -Os -g -o $@ demo/quickcg.cc demo/raycaster.cc -lSDL
