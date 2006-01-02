#include <libpq-fe.h>

typedef struct TAG_finalizeonce {
  void *encapobj;
  int isfinalized;
} finalizeonce;

extern int *PQhugs_alloc_env();

extern void PQfinish_app(finalizeonce *conn);
extern void PQfinish_finalizer(finalizeonce *conn);

extern void PQclear_hdbc_app(finalizeonce *res);
extern void PQclear_hdbc_finalizer(finalizeonce *res);
