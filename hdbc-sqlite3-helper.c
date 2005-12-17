#include <sqlite3.h>
#include <stdio.h>

int sqlite3_bind_text2(sqlite3_stmt* a, int b, const char *c, int d) {
    sqlite3_bind_text(a, b, c, d, SQLITE_TRANSIENT);
}

int sqlite3_prepare_dbg(sqlite3 *db, const char *zSql, int nBytes,
                        sqlite3_stmt **ppStmt,
                        const char **pzTail) {
  int retval;
  char *newstr = strndup(zSql, nBytes);
  printf("\nGot nBytes: %d, zSql at %p\n", nBytes, zSql);
  printStr(zSql, nBytes);
  retval = sqlite3_prepare(db, newstr, nBytes, ppStmt, pzTail);
  printf("\nCall returned %d\n", retval);
  //printf("\npzTail points at %p", *pzTail);
  return retval;
}

void printStr(const char *istr, int nBytes) {
  int counter;
  for (counter = 0; counter < nBytes; counter++) {
    printf("Byte %d is %d: '%c'\n", counter,
           (int) istr[counter],
           (int) istr[counter]);
  }
}
