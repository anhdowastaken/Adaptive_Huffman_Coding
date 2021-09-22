all: main

main:
	gcc -O2 errhand.c bitio.c ahuff.c main-c.c -o main-c
	gcc -O2 errhand.c bitio.c ahuff.c main-e.c -o main-e

debug:
	gcc -g errhand.c bitio.c ahuff.c main-c.c -o main-c
	gcc -g errhand.c bitio.c ahuff.c main-e.c -o main-e
