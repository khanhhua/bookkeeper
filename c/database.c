#include "bookkeeper.h"

#include <stdio.h>
#include <stdlib.h>

int DB_init(Database *instance, char *filename) {
  signed short version;
  unsigned long bookCount, checkoutCount;

  FILE *file = fopen(filename, "r");
  if (file == NULL) {
    file = fopen(filename, "w+");
    version = 1;
    bookCount = 0;
    checkoutCount = 0;
    fwrite(&version, sizeof(unsigned short), 1, file);
    fwrite(&bookCount, sizeof(unsigned long), 1, file);
    fwrite(&checkoutCount, sizeof(unsigned long), 1, file);
    fclose(file);

    BookLinkedList *books = (BookLinkedList *)calloc(1, sizeof(BookLinkedList));
    BLL_init(books);
    instance->books = books;

    CheckoutLinkedList *checkouts =
      (CheckoutLinkedList *)calloc(1, sizeof(CheckoutLinkedList));
    CHKLL_init(checkouts);
    instance->checkouts = checkouts;

    return 0;
  }

  if (1 != fread(&version, sizeof(unsigned short), 1, file)) {
    return 1;
  }
  instance->version = version;

  if (1 != fread(&bookCount, sizeof(unsigned long), 1, file)) {
    fclose(file);
    return 1;
  }

  if (1 != fread(&checkoutCount, sizeof(unsigned long), 1, file)) {
    fclose(file);
    return 1;
  }

  BookLinkedList *books = (BookLinkedList *)calloc(1, sizeof(BookLinkedList));
  BLL_init(books);
  if (bookCount > 0) {
    Book bookArr[bookCount];
    int temp = 0;
    if (bookCount != (temp = fread(bookArr, sizeof(Book), bookCount, file))) {
      printf("Corruption error\n");
      printf("Book count in file: %ld vs %d\n", bookCount, temp);

      fclose(file);

      return 1;
    }

    for (int i = bookCount - 1; i >= 0; i--) {
      BLL_add(books, bookArr[i]);
    }
  }
  instance->books = books;

  CheckoutLinkedList *checkouts =
      (CheckoutLinkedList *)calloc(1, sizeof(CheckoutLinkedList));
  CHKLL_init(checkouts);
  if (checkoutCount > 0) {
    Checkout checkoutArr[checkoutCount];
    int temp = 0;
    if (checkoutCount !=
        (temp = fread(checkoutArr, sizeof(Checkout), checkoutCount, file))) {
      printf("Corruption error\n");
      printf("Checkout count in file: %ld vs %d\n", checkoutCount, temp);

      fclose(file);

      return 1;
    }

    for (int i = checkoutCount - 1; i >= 0; i--) {
      CHKLL_add(checkouts, checkoutArr[i]);
    }
  }
  instance->checkouts = checkouts;

  fclose(file);
  return 0;
}

void DB_save(Database *database, char *filename) {
  FILE *file = fopen(filename, "w");

  fwrite(&database->version, sizeof(unsigned short), 1, file);
  fwrite(&database->books->count, sizeof(unsigned long), 1, file);
  fwrite(&database->checkouts->count, sizeof(unsigned long), 1, file);

  BookNode *iterBooks = database->books->head;
  while (iterBooks != NULL) {
    fwrite(&iterBooks->value, sizeof(Book), 1, file);
    iterBooks = iterBooks->next;
  }

  CheckoutNode *iterCheckouts = database->checkouts->head;
  while (iterCheckouts != NULL) {
    fwrite(&iterCheckouts->value, sizeof(Checkout), 1, file);
    iterCheckouts = iterCheckouts->next;
  }

  fclose(file);
}
