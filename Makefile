

botDepth=4
buildFlags=+RTS -N3 -s -RTS -rtsopts
runflags=-d $(botDepth) +RTS -N3
guiData=-mode MachineBlack
#guiData=-mode MachineWhite

#debugFileArg=-debugFile xboard.log
#debugFileArg=-nameOfDebufFile xboard.log

build: #clean
	ghc main.hs -O3 $(buildFlags) -threaded

#player vs player command line
run: build
	time ./main hvh $(runflags)

#player vs bot xboard
gui: build
	xboard -fcp "./main hve $(runflags)" -debug -debugMode true -engineDebugOutput 2 $(debugFileArg) $(guiData) #; cat xboard.log


#bot vs bot xboard
gui2: build
	./main eve $(runflags) | ./misc/gui/pxboard.sh

gui3: #build
	runhaskell main.hs | ./misc/gui/pxboard.sh

clean:
	rm -Rf main *.o *.hi

