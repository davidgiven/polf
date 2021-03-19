all: polf.prg

polf.prg: polf.asm
	64tass --cbm-prg -o $@ $<


