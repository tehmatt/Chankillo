FILES=src/*.hs
FLAGS=-O3 -outputdir bin -o bin/Chankillo

all: $(FILES)
	ghc $(FLAGS) $(FILES)
