#pragma once

#include <stdio.h>

typedef char Date[8]; // 20240131 means January first 2024

typedef struct {
  char isbn[18];
  char title[51];
  char authors[51];
} Book;

typedef struct {
  char isbn[18];
  Date created_on;
  Date expired_on;
} Checkout;

typedef struct BookNode {
  Book value;
  struct BookNode *next;
} BookNode;

typedef struct {
  int count;
  BookNode *head;
} BookLinkedList;

typedef struct {
  unsigned short version;
  BookLinkedList *books;
} Database;

void BLL_init(BookLinkedList *);
void BLL_add(BookLinkedList *, Book);
Book *BLL_find_by_isbn(BookLinkedList *, char isbn[18]);

int DB_init(Database *, char *filename);
void DB_save(Database *, char *filename);
