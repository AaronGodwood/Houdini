#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>


extern SEXP C_find_matching_brace(SEXP text, SEXP start);


static const R_CallMethodDef CallEntries[] = {
  {"C_find_matching_brace", (DL_FUNC) &C_find_matching_brace, 2},
  {NULL, NULL, 0}
};

void R_init_Houdini(DllInfo *dll) {
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}
