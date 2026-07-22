#include <R.h>
#include <Rinternals.h>
#include <string.h>
#include <stdlib.h>
#include <ctype.h>



/*
 * C_find_matching_brace(text, start)
 *
 * Scan from byte position `start` (1-based) for the matching closing brace.
 * Returns the 1-based position of the matching '}', or NA_INTEGER if not found.
 */
SEXP C_find_matching_brace(SEXP text, SEXP start) {
  const char *s = CHAR(STRING_ELT(text, 0));
  int pos = INTEGER(start)[0] - 1;  /* convert to 0-based */
  int n = (int)strlen(s);
  int depth = 0;

  while (pos < n) {
    char c = s[pos];
    if (c == '{') {
      depth++;
    } else if (c == '}') {
      depth--;
      if (depth == 0) {
        return ScalarInteger(pos + 1);  /* back to 1-based */
      }
    }
    pos++;
  }

  return ScalarInteger(NA_INTEGER);
}
