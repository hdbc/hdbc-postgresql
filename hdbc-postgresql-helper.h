#include <libpq-fe.h>

typedef struct TAG_finalizeonce {
  void *encapobj;
  int isfinalized;
} finalizeonce;

extern finalizeonce *wrapobj(void *obj);

extern void PQfinish_app(finalizeonce *conn);
extern void PQfinish_finalizer(finalizeonce *conn);

extern void PQclear_hdbc_app(finalizeonce *res);
extern void PQclear_hdbc_finalizer(finalizeonce *res);
