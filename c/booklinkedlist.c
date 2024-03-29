#include "bookkeeper.h"
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>

static void promoteHead(BookLinkedList *instance, BookNode *newHead) {
  newHead->next = instance->head;
  instance->head = newHead;
}

void BLL_init(BookLinkedList *instance) {
  instance->head = NULL;
  instance->count = 0L;
}

void BLL_add(BookLinkedList *instance, Book book) {
  BookNode *newHead = (BookNode *)calloc(1, sizeof(BookNode));
  newHead->value = book;
  instance->count++;

  if (instance->head == NULL) {
    instance->head = newHead;
    return;
  }

  promoteHead(instance, newHead);
}

Book *BLL_find_by_isbn(BookLinkedList *instance, char isbn[18]) {
  if (instance == NULL) {
    return NULL;
  }

  if (instance->head == NULL) {
    return NULL;
  }

  BookNode *iter = instance->head;
  bool found;
  do {
    found = strcmp(iter->value.isbn, isbn);
    if (found == 0) {
      return &iter->value;
    }

    iter = iter->next;
  } while (iter != NULL);

  return NULL;
}

/* PRIVATE SECTION */
