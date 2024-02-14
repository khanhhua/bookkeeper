#include "bookkeeper.h"

#include <stdio.h>
#include <stdlib.h>

int DB_init(Database *instance, char *filename) {
  signed short version;
  int bookCount;

  FILE *file = fopen(filename, "r");
  if (file == NULL) {
    file = fopen(filename, "w+");
    version = 1;
    bookCount = 0;
    fwrite(&version, sizeof(unsigned short), 1, file);
    fwrite(&bookCount, sizeof(int), 1, file);
    fclose(file);

    return 0;
  }

  if (1 != fread(&version, sizeof(unsigned short), 1, file)) {
    return 1;
  }
  instance->version = version;

  if (1 != fread(&bookCount, sizeof(int), 1, file)) {
    return 1;
  }

  BookLinkedList *books = (BookLinkedList *)calloc(1, sizeof(BookLinkedList));
  BLL_init(books);

  Book bookArr[bookCount];
  int temp = 0;
  if (bookCount != (temp = fread(bookArr, sizeof(Book), bookCount, file))) {
    printf("Corruption error\n");
    printf("Book count in file: %d vs %d\n", bookCount, temp);
    return 1;
  }
  fclose(file);

  for (int i = bookCount - 1; i >= 0; i--) {
    BLL_add(books, bookArr[i]);
  }

  instance->books = books;

  return 0;
}

void DB_save(Database *database, char *filename) {
  FILE *file = fopen(filename, "w");

  fwrite(&database->version, sizeof(unsigned short), 1, file);
  fwrite(&database->books->count, sizeof(int), 1, file);

  BookNode *iter = database->books->head;
  while (iter != NULL) {
    fwrite(&iter->value, sizeof(Book), 1, file);
    iter = iter->next;
  }

  fclose(file);
}
