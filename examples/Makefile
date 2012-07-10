all: clean
		ghc -Wall --make -O3 -rtsopts -outputdir `pwd` Example01.hs

prof: clean
		ghc -Wall --make -O3 -fforce-recomp -prof -auto -auto-all -caf-all -rtsopts -outputdir `pwd` Example01.hs

clean:
		rm -rf *.o *.hi *.prof Data Example01
