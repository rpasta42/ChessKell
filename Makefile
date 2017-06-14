
build: clean
	ghc main.hs +RTS -N3 -s -RTS -threaded -O2

run: build
	time ./main +RTS -N3

clean:
	rm -Rf main *.o *.hi

