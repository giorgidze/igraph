clean:
		rm -rf *.o *.hi *.prof Data Example01

all: clean
		ghc -Wall --make -O3 -rtsopts -outputdir `pwd` Example01.hs

prof: clean
		ghc -Wall --make -O3 -fforce-recomp -prof -auto -auto-all -caf-all -rtsopts -outputdir `pwd` Example01.hs

debug: clean
		ghc -Wall --make -O3 -fforce-recomp -rtsopts -dcore-lint -debug Example01.hs
