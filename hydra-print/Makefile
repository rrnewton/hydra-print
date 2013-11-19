


all:
	ghc -threaded -rtsopts --make -DNOTESTING SystemTest.hs -o SystemTest.exe
#	ghc --make -DNOTESTING -i. tests/tile_test.hs -o tests/tile_test.exe
#	ghc --make TestFramework.hs -o TestFramework.exe 

run:
	./Test.exe	

clean:
	rm -f *.hi *.o *.exe
	rm -f tests/*.hi tests/*.o tests/*.exe
