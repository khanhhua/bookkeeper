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
  instance->count = 0L;
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

void CHKLL_remove(CheckoutLinkedList *instance, Checkout *checkout) {
  if (instance == NULL) {
    return;
  }

  if (instance->head == NULL) {
    return;
  }

  char *isbn = checkout->isbn;
  CheckoutNode *iter = instance->head;
  CheckoutNode *prev = NULL;
  bool found;

  do {
    found = strcmp(iter->value.isbn, isbn);
    if (found == 0) {
      instance->count--;

      if (prev == NULL) {
        instance->head = iter->next;
      } else {
        prev->next = iter->next;
      }

      return;
    }

    prev = iter;
    iter = iter->next;
  } while (iter != NULL);
}
/* PRIVATE SECTION */
