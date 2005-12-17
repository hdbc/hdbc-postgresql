#include <sqlite3.h>

extern int sqlite3_bind_text2(sqlite3_stmt* a, int b, const char *c, int d);

extern int sqlite3_prepare_dbg(sqlite3 *db, const char *zSql, int nBytes,
                               sqlite3_stmt **ppStmt,
                               const char **pzTail);

