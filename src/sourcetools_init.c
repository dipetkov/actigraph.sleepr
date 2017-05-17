
#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME:
Check these declarations against the C/Fortran source code.
*/

/* .Call calls */
extern SEXP actigraph_sleepr_overlap(SEXP, SEXP);
extern SEXP actigraph_sleepr_wle(SEXP, SEXP, SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
  {"actigraph_sleepr_overlap", (DL_FUNC) &actigraph_sleepr_overlap, 2},
  {"actigraph_sleepr_wle",     (DL_FUNC) &actigraph_sleepr_wle,     4},
  {NULL, NULL, 0}
};

void R_init_actigraph_sleepr(DllInfo *dll)
{
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}
