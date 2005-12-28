#include <libpq-fe.h>

extern int *PQhugs_alloc_env();
extern void PQfinish_hugs_app(int *hasfinalized, PGconn *conn);
extern void PQfinish_hugs_fptr(int *hasfinalized, PGconn *conn);
extern void PQclear_hugs_app(int *hasfinalized, PGresult *res);
extern void PQclear_hugs_fptr(int *hasfinalized, PGresult *res);
