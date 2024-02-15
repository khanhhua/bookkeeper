#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "bookkeeper.h"

#define DATAFILE "books.dat"
/* APPLICATION STORAGE */
Database database;
BookLinkedList *books;
CheckoutLinkedList *checkouts;

void nreads(char *s, size_t limit) {
  do {
    fgets(s, limit, stdin);
  } while (s[0] == '\n');
  // Drop the newline character at the n-1 position
  s[strlen(s) - 1] = '\0';
}

int menu() {
  printf("|MENU                    |\n"
         "+------------------------+\n"
         "1. Search for book\n"
         "2. Checkout a book\n"
         "3. Return a lease\n"
         "90. Add new book\n"
         "99. Save\n"
         "0. Quit\n");

  int choice;
  scanf("%d", &choice);
  return choice;
}

void collectBook() {
  Book book;

  printf("Title: ");
  nreads(book.title, 50);

  printf("ISBN: ");
  nreads(book.isbn, 17);

  printf("Authors: ");
  nreads(book.authors, 50);

  BLL_add(books, book);
}

void searchBook() {
  Book *book = NULL;

  char isbn[18];
  printf("Enter ISBN: ");
  nreads(isbn, 17);

  book = BLL_find_by_isbn(books, isbn);
  if (book != NULL) {
    printf("Found: Title \"%s\" by %s\n", book->title, book->authors);
  }
}

void checkoutBook() {
  Book *book = NULL;

  char isbn[18];
  printf("Enter ISBN: ");
  nreads(isbn, 17);

  book = BLL_find_by_isbn(books, isbn);
  if (book == NULL) {
    return;
  }

  printf("You are checking out book title \"%s\"", book->title);
  Checkout checkout;
  memcpy(checkout.isbn, book->isbn, strlen(book->isbn) + 1);
  memcpy(checkout.created_on, "20240215", 8);
  memcpy(checkout.expired_on, "20240301", 8);

  CHKLL_add(checkouts, checkout);
  printf("Book has been successfulled checked out");
}

void saveDatabase() {
  if (database.books == NULL) {
    database.books = books;
  }
  if (database.checkouts == NULL) {
    database.checkouts = checkouts;
  }

  if (database.books != books) {
    printf("Referential error: Books\n");
    return;
  }

  DB_save(&database, DATAFILE);
}

void app() {
  int choice = menu();

  switch (choice) {
  case 1:
    searchBook();
    break;
  case 2:
    checkoutBook();
    break;
  case 90:
    collectBook();
    break;
  case 99:
    saveDatabase();
    break;
  }

  if (choice) {
    app();
  }
}

int main() {
  DB_init(&database, DATAFILE);
  books = database.books;
  checkouts = database.checkouts;

  app();

  BookNode *iter = books->head;
  while (iter != NULL) {
    printf("Book %s by %s\n", iter->value.title, iter->value.authors);
    iter = iter->next;
  }

  CheckoutNode *iterC = checkouts->head;
  while (iterC != NULL) {
    printf("ISBN %s expires on %s\n", iterC->value.isbn,
           iterC->value.expired_on);
    iterC = iterC->next;
  }
}
