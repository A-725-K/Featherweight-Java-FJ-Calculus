CC        = ghc
FLAGS     = --make
EXE_NAME  = FJ_Interpreter.exe
TEST_FILE = unit_test.sh
MAIN_FILE = MainFJ.hs

all: clean
	$(CC) $(FLAGS) $(MAIN_FILE) -o $(EXE_NAME)
	rm -rf *.hi
	rm -rf *.o

test:
	bats $(TEST_FILE)

clean:
	rm -rf $(EXE_NAME)
