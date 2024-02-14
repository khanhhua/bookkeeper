OBJ_FILES = booklinkedlist.o main.o

%.o: %.c
		gcc -c $< -o build/$@

link: $(OBJ_FILES)
		gcc build/*.o -o build/bookkeeper

clean:
		mkdir -p build
		rm -Rf build/*

build: clean link

run: build
		sh -c build/bookkeeper
