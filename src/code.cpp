#include <Rcpp.h>


// Define the method signature

#ifdef __cplusplus
extern "C" {
#endif

#include "net.h"

#ifdef __cplusplus
}
#endif

//' @export
// [[Rcpp::export]]
SEXP rnbernexp_CPP(SEXP g, SEXP ep, SEXP oh, SEXP th) {

  // Compute the result in _C_ from _C++_.
  SEXP out = rnbernexp_C(g, ep, oh, th);

  return out;
}
