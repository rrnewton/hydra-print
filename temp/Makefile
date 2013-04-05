

all: tile_test.exe

tile_test.exe: tile_test.hs
	ghc --make -DNOTESTING -i.. tile_test.hs -o tile_test.exe

clean:
	rm -f *.hi *.o *.exe

