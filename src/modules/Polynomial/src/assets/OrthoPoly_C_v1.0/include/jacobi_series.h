#ifndef  _CHEBYSHEV_H
#define  _CHEBYSHEV_H

#ifndef  _INLINE_H
#define  _INLINE_H
#include"inline.h"
#endif



void RecurCoefJacob (double n, double alpha, double beta,  double *A, double *B, double *C);
void RecurCoefJacob_DD (double n, double alpha, double beta,  dd_real *A, dd_real *B, dd_real *C);



double JacobVal(double *P, unsigned int n, double x, double alpha, double beta);
double CompJacobVal(double *P, unsigned int n, double x, double alpha, double beta);
dd_real AccJacobVal(double *P, unsigned int n, double x, double alpha, double beta);


double JacobDer(double *P, unsigned int n, double x, double alpha, double beta);
double CompJacobDer(double *P, unsigned int n, double x, double alpha, double beta);
dd_real AccJacobDer(double *P, unsigned int n, double x, double alpha, double beta);

double JacobDerK(double *P, unsigned int n, double x, double alpha, double beta, unsigned int k);
double CompJacobDerK(double *P, unsigned int n, double x, double alpha, double beta, unsigned int k);
dd_real AccJacobDerK(double *P, unsigned int n, double x, double alpha, double beta, unsigned int k);

double JacobValwErr(double *P, unsigned int n, double x, double alpha, double beta, double * runerrbound);
double JacobDerwErr(double *P, unsigned int n, double x, double alpha, double beta, double * runerrbound);
double JacobDerKwErr(double *P, unsigned int n, double x, double alpha, double beta, double * runerrbound, unsigned int k);

double CompJacobValwErr(double *P, unsigned int n, double x, double alpha, double beta, double * runerrbound);
double CompJacobDerwErr(double *P, unsigned int n, double x, double alpha, double beta, double * runerrbound);
double CompJacobDerKwErr(double *P, unsigned int n, double x, double alpha, double beta, double * runerrbound, unsigned int k);

dd_real AccJacobValwErr(double *P, unsigned int n, double x, double alpha, double beta, double * runerrbound);
dd_real AccJacobDerwErr(double *P, unsigned int n, double x, double alpha, double beta, double * runerrbound);
dd_real AccJacobDerKwErr(double *P, unsigned int n, double x, double alpha, double beta, double * runerrbound, unsigned int k);

#endif
