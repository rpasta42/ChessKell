
runflags=+RTS -N3
guiData=-mode MachineBlack
#guiData=-mode MachineWhite

build: #clean
	ghc main.hs -O3 +RTS -N3 -s -RTS -threaded

run: build
	time ./main $(runflags)




gui: build
	xboard -fcp "./main $(runflags)" -debug -debugMode true -engineDebugOutput 2 -debugFile xboard.log $(guiData) #; cat xboard.log


gui2: build
	./main $(runflags) | ./misc/gui/pxboard.sh

gui3: #build
	runhaskell main.hs | ./misc/gui/pxboard.sh

clean:
	rm -Rf main *.o *.hi

