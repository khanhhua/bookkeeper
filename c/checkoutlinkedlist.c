#include "bookkeeper.h"
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>

static void promoteHead(CheckoutLinkedList *instance, CheckoutNode *newHead) {
  newHead->next = instance->head;
  instance->head = newHead;
}

void CHKLL_init(CheckoutLinkedList *instance) {
  instance->head = NULL;
  instance->count = 0;
}

void CHKLL_add(CheckoutLinkedList *instance, Checkout book) {
  CheckoutNode *newHead = (CheckoutNode *)calloc(1, sizeof(CheckoutNode));
  newHead->value = book;

  instance->count++;
  if (instance->head == NULL) {
    instance->head = newHead;
    return;
  }

  promoteHead(instance, newHead);
}

Checkout *CHKLL_find_by_isbn(CheckoutLinkedList *instance, char isbn[18]) {
  if (instance == NULL) {
    return NULL;
  }

  if (instance->head == NULL) {
    return NULL;
  }

  CheckoutNode *iter = instance->head;
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
