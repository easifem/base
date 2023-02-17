#ifndef  _LAGUERRE_H
#define  _LAGUERRE_H

#ifndef  _INLINE_H
#define  _INLINE_H
#include"inline.h"
#endif



void RecurCoefLague (double n, double alpha,  double *A, double *B, double *C);
void RecurCoefLague_DD (double n, double alpha,  dd_real *A, dd_real *B, dd_real *C);


double LagueVal(double *P, unsigned int n, double x, double alpha);
double CompLagueVal(double *P, unsigned int n, double x, double alpha);
dd_real AccLagueVal(double *P, unsigned int n, double x, double alpha);

double LagueDer(double *P, unsigned int n, double x, double alpha);
double CompLagueDer(double *P, unsigned int n, double x, double alpha);
dd_real AccLagueDer(double *P, unsigned int n, double x, double alpha);

double LagueDerK(double *P, unsigned int n, double x, double alpha, unsigned int k);
double CompLagueDerK(double *P, unsigned int n, double x, double alpha, unsigned int k);
dd_real AccLagueDerK(double *P, unsigned int n, double x, double alpha, unsigned int k);

double LagueValwErr(double *P, unsigned int n, double x, double alpha,  double * runerrbound);
double LagueDerwErr(double *P, unsigned int n, double x, double alpha,  double * runerrbound);
double LagueDerKwErr(double *P, unsigned int n, double x, double alpha,  double * runerrbound, unsigned int k);

double CompLagueValwErr(double *P, unsigned int n, double x, double alpha,  double * runerrbound);
double CompLagueDerwErr(double *P, unsigned int n, double x, double alpha,  double * runerrbound);
double CompLagueDerKwErr(double *P, unsigned int n, double x, double alpha,  double * runerrbound, unsigned int k);

dd_real AccLagueValwErr(double *P, unsigned int n, double x, double alpha,  double * runerrbound);
dd_real AccLagueDerwErr(double *P, unsigned int n, double x, double alpha,  double * runerrbound);
dd_real AccLagueDerKwErr(double *P, unsigned int n, double x, double alpha,  double * runerrbound, unsigned int k);

#endif
