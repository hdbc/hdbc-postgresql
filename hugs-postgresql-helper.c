#include <libpq-fe.h>
#include <stdio.h>
#include <stdlib.h>
#include "hugs-postgresql-helper.h"

/* Things can't finalize more than once.  
We'd like to let people call them from the app.
Yet we'd also like to be able to have a ForeignPtr finalize them.

So, here's a little wrapper for things. */
  
void PQfinish_hugs_app(int *hasfinalized, PGconn *conn) {
  if (*hasfinalized)
    return;
  PQfinish(conn);
  *hasfinalized = 1;
  return;
}

void PQfinish_hugs_fptr(int *hasfinalized, PGconn *conn) {
  PQfinish_hugs_app(hasfinalized, conn);
  free(hasfinalized);
}

void PQclear_hugs_app(int *hasfinalized, PGresult *res) {
  if (*hasfinalized)
    return;
  PQclear(res);
  *hasfinalized = 1;
  return;
}

void PQclear_hugs_fptr(int *hasfinalized, PGresult *res) {
  PQclear_hugs_app(hasfinalized, res);
  free(hasfinalized);
}

int *PQhugs_alloc_env(void) {
  int *iptr;
  iptr = malloc(sizeof(int));
  if (iptr == NULL) {
    fprintf(stderr, "PQhugs_alloc_env failed to allocate memory\n");
    return NULL;
  }
  *iptr = 0;
  return iptr;
}
