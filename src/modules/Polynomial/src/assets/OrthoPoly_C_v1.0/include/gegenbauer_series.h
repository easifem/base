#ifndef  _GEGENBAUER_H
#define  _GEGENBAUER_H

#ifndef  _INLINE_H
#define  _INLINE_H
#include"inline.h"
#endif



void RecurCoefGegen (double n, double lambda,  double *A, double *C);
void RecurCoefGegen_DD (double n, double lambda,  dd_real *A, dd_real *C);


double GegenVal(double *P, unsigned int n, double x, double lambda);
double CompGegenVal(double *P, unsigned int n, double x, double lambda);
dd_real AccGegenVal(double *P, unsigned int n, double x, double lambda);

double GegenDer(double *P, unsigned int n, double x, double lambda);
double CompGegenDer(double *P, unsigned int n, double x, double lambda);
dd_real AccGegenDer(double *P, unsigned int n, double x, double lambda);

double GegenDerK(double *P, unsigned int n, double x, double lambda, unsigned int k);
double CompGegenDerK(double *P, unsigned int n, double x, double lambda, unsigned int k);
dd_real AccGegenDerK(double *P, unsigned int n, double x, double lambda, unsigned int k);

double GegenValwErr(double *P, unsigned int n, double x, double lambda,  double * runerrbound);
double GegenDerwErr(double *P, unsigned int n, double x, double lambda,  double * runerrbound);
double GegenDerKwErr(double *P, unsigned int n, double x, double lambda,  double * runerrbound, unsigned int k);

double CompGegenValwErr(double *P, unsigned int n, double x, double lambda,  double * runerrbound);
double CompGegenDerwErr(double *P, unsigned int n, double x, double lambda,  double * runerrbound);
double CompGegenDerKwErr(double *P, unsigned int n, double x, double lambda,  double * runerrbound, unsigned int k);

dd_real AccGegenValwErr(double *P, unsigned int n, double x, double lambda,  double * runerrbound);
dd_real AccGegenDerwErr(double *P, unsigned int n, double x, double lambda,  double * runerrbound);
dd_real AccGegenDerKwErr(double *P, unsigned int n, double x, double lambda,  double * runerrbound, unsigned int k);

#endif
