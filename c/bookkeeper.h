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
  unsigned long count;
  BookNode *head;
} BookLinkedList;

typedef struct CheckoutNode {
  Checkout value;
  struct CheckoutNode *next;
} CheckoutNode;

typedef struct {
  unsigned long count;
  CheckoutNode *head;
} CheckoutLinkedList;

typedef struct {
  unsigned short version;
  BookLinkedList *books;
  CheckoutLinkedList *checkouts;
} Database;

void BLL_init(BookLinkedList *);
void BLL_add(BookLinkedList *, Book);
Book *BLL_find_by_isbn(BookLinkedList *, char isbn[18]);

void CHKLL_init(CheckoutLinkedList *);
void CHKLL_add(CheckoutLinkedList *, Checkout);
Checkout *CHKLL_find_by_isbn(CheckoutLinkedList *, char isbn[18]);
void CHKLL_remove(CheckoutLinkedList *, Checkout *);

int DB_init(Database *, char *filename);
void DB_save(Database *, char *filename);
