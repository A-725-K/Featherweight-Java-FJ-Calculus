CC        = ghc
MAKE      = --make
FLAGS     = -O2
EXE_NAME  = FJ_Interpreter.exe
TEST_FILE = unit_test.sh
MAIN_FILE = MainFJ.hs
FJ_DIR    = Featherweight-Java-FJ-Calculus

all: clean
	$(CC) $(MAKE) $(MAIN_FILE) -o $(EXE_NAME) $(FLAGS)
	rm -rf *.hi
	rm -rf *.o

test:
	bats $(TEST_FILE)

zip:
	zip -r ../$(FJ_DIR).zip .

clean:
	rm -rf $(EXE_NAME)
	rm -rf ../$(FJ_DIR).zip
