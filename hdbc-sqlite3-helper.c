#include <sqlite3.h>
#include <stdio.h>
#include <stdlib.h>
#include "hdbc-sqlite3-helper.h"

int sqlite3_bind_text2(sqlite3_stmt* a, int b, const char *c, int d) {
    sqlite3_bind_text(a, b, c, d, SQLITE_TRANSIENT);
}

/* Sqlite things can't finalize more than once.  
We'd like to let people call them from the app to get the error, if any.
Yet we'd also like to be able to have a ForeignPtr finalize them.

So, here's a little wrapper for things. */
  

int sqlite3_open2(const char *filename, finalizeonce **ppo) {
  sqlite3 *ppDb;
  finalizeonce *newobj;
  int res;

  res = sqlite3_open(filename, &ppDb);
  newobj = malloc(sizeof(finalizeonce));
  if (newobj == NULL) {
    fprintf(stderr, "\nhdbc sqlite internal error: couldn't malloc memory for newobj\n");
    return -999;
  }
  newobj->encapobj = (void *) ppDb;
  newobj->isfinalized = 0;
  *ppo = newobj;
  fprintf(stderr, "\nAllocated db at %p %p\n", newobj, newobj->encapobj);
  return res;
}

int sqlite3_close_app(finalizeonce *ppdb) {
  int res;
  if (ppdb->isfinalized) {
    fprintf(stderr, "\nclose_app on already finalized %p\n", ppdb);
    return SQLITE_OK;
  }
  fprintf(stderr, "\nclose_app on non-finalized %p\n", ppdb);
  res = sqlite3_close((sqlite3 *) (ppdb->encapobj));
  ppdb->isfinalized = 1;
  return res;
}

void sqlite3_close_finalizer(finalizeonce *ppdb) {
  fprintf(stderr, "\nclose_finalizer on %p: %d\n", ppdb, ppdb->isfinalized);
  sqlite3_close_app(ppdb);
  free(ppdb);
}

int sqlite3_prepare2(sqlite3 *db, const char *zSql,
                     int nBytes, finalizeonce **ppo,
                     const char **pzTail) {

  sqlite3_stmt *ppst;
  finalizeonce *newobj;
  int res;

  fprintf(stderr, "\nCalling prepare on %p", db);
  res = sqlite3_prepare(db, zSql, nBytes, &ppst,
                        pzTail);
  /* We don't try to deallocate this in Haskell if there
     was an error. */
  if (res != SQLITE_OK) {
    /*
    if (ppst != NULL) {
      sqlite3_finalize(ppst);
    }
    */
    return res;
   
  }

  newobj = malloc(sizeof(finalizeonce));
  if (newobj == NULL) {
    fprintf(stderr, "\nhdbc sqlite3 internal error: couldn't malloc memory for newobj\n");
    return -999;
  }
  newobj->encapobj = (void *) ppst;
  newobj->isfinalized = 0;
  *ppo = newobj;
  fprintf(stderr, "\nAllocated stmt at %p %p\n", newobj, newobj->encapobj);
  return res;
}

int sqlite3_finalize_app(finalizeonce *ppst) {
  int res;
  if (ppst->isfinalized) {
    fprintf(stderr, "\nfinalize_app on already finalized %p\n", ppst);
    return SQLITE_OK;
  }
  fprintf(stderr, "\nfinalize_app on non-finalized %p\n", ppst);
  res = sqlite3_finalize((sqlite3_stmt *) (ppst->encapobj));
  ppst->isfinalized = 1;
  return res;
}

void sqlite3_finalize_finalizer(finalizeonce *ppst) {
  fprintf(stderr, "\nfinalize_finalizer on %p: %d\n", ppst, ppst->isfinalized);
  sqlite3_finalize_app(ppst);
  free(ppst);
}
