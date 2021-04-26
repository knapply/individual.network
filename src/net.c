#include "net.h"

#include <Rmath.h>

#include <network.h>
#include <netregistration.h>

SEXP rnbernexp_C(SEXP g, SEXP ep, SEXP oh, SEXP th)
  /*
   C-Language code for a simple random dynamic network generator.  Arguments are
   as follows:
   g - a pre-initialized network object
   ep - the edge probability parameter
   oh - the edge onset hazard parameter
   th - the edge termination hazard parameter
   */
{
  int n, i, w;
  double u, fet, let, *vfet, *vlet, ot, tt;
  SEXP tail, head, atl, atlnam, sot, stt, ec;

  /*Verify that we were called properly, and set things up*/
  netRegisterFunctions();
  if(!netIsNetwork(g))
    error("rnbernexp_R must be called with a network object.\n");
  if(netIsDir(g))
    error("Network passed to rnbernexp_R should be undirected.\n");
  n = netNetSize(g);
  PROTECT(ep = coerceVector(ep, REALSXP));
  PROTECT(oh = coerceVector(oh, REALSXP));
  PROTECT(th = coerceVector(th, REALSXP));
  PROTECT(ec = allocVector(LGLSXP, 1));
  LOGICAL(ec)[0] = 0;
  GetRNGstate();

  /*Allocate memory for first/last edge time trackers*/
  vfet = (double *)R_alloc(n, sizeof(double));
  vlet = (double *)R_alloc(n, sizeof(double));
  for(i = 0; i < n; i++)
    vfet[i] = vlet[i] = NA_REAL;
  fet = let = NA_REAL;

  /*Draw the network information*/
  w = -1;
  i = 1;
  while(i < n){
    u = runif(0.0, 1.0);
    w += 1+ (int)floor(log(1.0 - u) / log(1.0 - REAL(ep)[0]));
    while((w >= i) && (i < n)){
      w -= i;
      i++;
    }
    if(i < n){                                       /*Generate an edge*/
  /*Draw and track timing information*/
  ot = rexp(REAL(oh)[0]);
      tt = ot + rexp(REAL(th)[0]);
      fet = ((ISNA(fet)) || (ot < fet)) ? ot : fet;
      let = ((ISNA(let)) || (tt > let)) ? tt : let;
      vfet[i] = ((ISNA(vfet[i])) || (ot < vfet[i])) ? ot : vfet[i];
      vlet[i] = ((ISNA(vlet[i])) || (tt > vlet[i])) ? tt : vlet[i];
      /*Allocate memory for the new edge*/
      PROTECT(tail = allocVector(INTSXP, 1));        /*Allocate head/tail*/
      PROTECT(head = allocVector(INTSXP, 1));
      INTEGER(tail)[0] = i + 1;
      INTEGER(head)[0] = w + 1;
      PROTECT(atl = allocVector(VECSXP, 2));         /*Allocate attributes*/
      PROTECT(sot = allocVector(REALSXP, 1));
      PROTECT(stt = allocVector(REALSXP, 1));
      PROTECT(atlnam = allocVector(STRSXP, 2));
      SET_STRING_ELT(atlnam, 0, mkChar("OnsetTime"));
      SET_STRING_ELT(atlnam, 1, mkChar("TerminationTime"));
      REAL(sot)[0] = ot;
      REAL(stt)[0] = tt;
      SET_VECTOR_ELT(atl, 0, sot);
      SET_VECTOR_ELT(atl, 1, stt);
      g = netAddEdge(g, tail, head, atlnam, atl, ec);    /*Add the edge*/
      UNPROTECT(6);
    }
  }

  /*Add network and vertex attributes*/
  for(i = 0; i < n; i++){
    PROTECT(sot = allocVector(REALSXP, 1));
    PROTECT(stt = allocVector(REALSXP, 1));
    REAL(sot)[0] = vfet[i];
    REAL(stt)[0] = vlet[i];
    g = netSetVertexAttrib(g, "FirstOnsetTime", sot, i + 1);
    g = netSetVertexAttrib(g, "LastTerminationTime", stt, i + 1);
    UNPROTECT(2);
  }
  PROTECT(sot = allocVector(REALSXP, 1));
  PROTECT(stt = allocVector(REALSXP, 1));
  REAL(sot)[0] = fet;
  REAL(stt)[0] = let;
  g = netSetNetAttrib(g, "FirstOnsetTime", sot);
  g = netSetNetAttrib(g, "LastTerminationTime", stt);

  /*Clear protection stack and return*/
  PutRNGstate();
  UNPROTECT(6);
  return g;
}
