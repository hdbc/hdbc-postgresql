#include <sqlite3.h>

int sqlite3_bind_text2(sqlite3_stmt* a, int b, const char *c, int d) {
    sqlite3_bind_test(a, b, c, d, SQLITE_TRANSIENT);
}

