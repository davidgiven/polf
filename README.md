POLF 0.1
========

© 2021 David Given

Portable Object reLocation Force: the game of objects, holes, and dubious physics

What?
-----

POLF is a game of the Commodore PET. You are placed in a maze; your goal is to
search the maze for the Portable Object, and use your Impulse Pusher (space
bar) to move the Object into the Hole. There is no plot; please feel free to
make up your own.

It should run on any 40-column PET with 16kB of RAM or more. (If anyone knows
how to switch an 8032 from 80-column to 40-column mode, please get in touch.)
It supports both Business and Graphics keyboards.

How?
----

POLF is written with the [64tass](http://tass64.sourceforge.net/) assembler.
Simply assemble the `polf.asm` file with the `--cbm-prg` option and you should
have a working `.prg` file. Transfer this to your pet by whichever means makes
you happy. There is a Makefile which will build it for you.

Load the program, then `RUN` it. POLF should start.

Instructions: use `WASD` to move; use `,` and `.` to turn left or right (or `,`
and `;` on a graphical keyboard). Press `SPACE` to push the Portable Object if
you're close enough. The radar instruments at the bottom of the screen will
tell you how close you are to both Object and Hole.

There's a secret feature! You can use `A` and `D` on the level overview screen
to select any level (there are 100).

Why?
----

I wanted to write a proper 3D game for the Commodore PET. Given that the PET
cannot actually display graphics, this seemed like a pleasantly futile task.

I originally wanted to use block graphics, which would have allowed a 80x50
graphics resolution, but it actually turns out that given the usual
small-computer angle representation of 256 b-radians in a circle, then a
comfortable field of view of 56° corresponds to 40 b-radians. This controls the
highest possible resolution, because the program can't represent smaller angles
than that. This is probably a good thing, as the rather feeble 1MHz 6502
processor produces a small enough frame rate as it is.

To draw the maze, raycasting is used, following [Lode's Computer Graphics
Tutorial](https://lodev.org/cgtutor/raycasting.html). The `demo` directory
contains a considerably hacked version of Lode's demo program, ported to use
8-bit fixed point graphics. POLF's number representation is 3.5 fixed-point,
which allows numbers to be represented from 0 to 7 31/32 at 1/32 intervals.
This is why the map is only 8x8, which is uncomfortably small.

In fact, this isn't really enough precision, even switching to 16-bit precision
for a few operations. I've managed to special-case most of the graphical
glitches but there are still some. Entertain yourself by finding them!

It should be easily portable to any other 6502 platform with a 40x25 text-mode
screen and 16kB of RAM.

There's about 2.5kB of actual code, and 6kB of lookup tables.

License?
--------

POLF is © 2021 David Given, and is distributable under the terms of the MIT
public license. See the `LICENSE` file for the full text.

Who?
----

POLF was written by me, David Given. You may contact me at dg@cowlark.com, or
visit my website at http://www.cowlark.com. There may or may not be anything
interesting there.

