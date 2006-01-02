#include <libpq-fe.h>
#include <stdio.h>
#include <stdlib.h>
#include "hdbc-postgresql-helper.h"

/* Things can't finalize more than once.  
We'd like to let people call them from the app.
Yet we'd also like to be able to have a ForeignPtr finalize them.

So, here's a little wrapper for things. */

finalizeonce *wrapobj(void *obj) {
  finalizeonce *newobj;
  newobj = malloc(sizeof(finalizeonce));
  if (newobj == NULL) {
    fprintf(stderr, "HDBC: could not allocate wrapper!\n");
    return NULL;
  }
  newobj->isfinalized = 0;
  newobj->encapobj = obj;
  return newobj;
}
  
void PQfinish_app(finalizeonce *conn) {
  if (conn->isfinalized)
    return;
  PQfinish((PGconn *) (conn->encapobj));
  conn->isfinalized = 1;
}

void PQfinish_finalizer(finalizeonce *conn) {
  PQfinish_app(conn);
  free(conn);
}

void PQclear_app(finalizeonce *res) {
  if (res->isfinalized)
    return;
  PQclear((PGres *) (res->encapobj));
  res->isfinalized = 1;
}

void PQclear_finalizer(finalizeonce *res) {
  PQclear_app(res);
  free(res);
}

