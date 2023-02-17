#ifndef _CHEBYSHEV_H
#define _CHEBYSHEV_H

#ifndef _INLINE_H
#define _INLINE_H
#include "inline.h"
#endif

double Cheb1Val(double *P, unsigned int n, double x);
double Cheb2Val(double *P, unsigned int n, double x);
double CompCheb1Val(double *P, unsigned int n, double x);
double CompCheb2Val(double *P, unsigned int n, double x);
dd_real AccCheb1Val(double *P, unsigned int n, double x);
dd_real AccCheb2Val(double *P, unsigned int n, double x);

double Cheb1Der(double *P, unsigned int n, double x);
double Cheb2Der(double *P, unsigned int n, double x);
double CompCheb1Der(double *P, unsigned int n, double x);
double CompCheb2Der(double *P, unsigned int n, double x);
dd_real AccCheb1Der(double *P, unsigned int n, double x);
dd_real AccCheb2Der(double *P, unsigned int n, double x);

double Cheb1DerK(double *P, unsigned int n, double x, unsigned int k);
double Cheb2DerK(double *P, unsigned int n, double x, unsigned int k);
double CompCheb1DerK(double *P, unsigned int n, double x, unsigned int k);
double CompCheb2DerK(double *P, unsigned int n, double x, unsigned int k);
dd_real AccCheb1DerK(double *P, unsigned int n, double x, unsigned int k);
dd_real AccCheb2DerK(double *P, unsigned int n, double x, unsigned int k);

double Cheb1ValwErr(double *P, unsigned int n, double x, double *runerrbound);
double Cheb2ValwErr(double *P, unsigned int n, double x, double *runerrbound);
double Cheb1DerwErr(double *P, unsigned int n, double x, double *runerrbound);
double Cheb2DerwErr(double *P, unsigned int n, double x, double *runerrbound);
double Cheb1DerKwErr(double *P, unsigned int n, double x, double *runerrbound, unsigned int k);
double Cheb2DerKwErr(double *P, unsigned int n, double x, double *runerrbound, unsigned int k);

double CompCheb1ValwErr(double *P, unsigned int n, double x, double *runerrbound);
double CompCheb2ValwErr(double *P, unsigned int n, double x, double *runerrbound);
double CompCheb1DerwErr(double *P, unsigned int n, double x, double *runerrbound);
double CompCheb2DerwErr(double *P, unsigned int n, double x, double *runerrbound);
double CompCheb1DerKwErr(double *P, unsigned int n, double x, double *runerrbound, unsigned int k);
double CompCheb2DerKwErr(double *P, unsigned int n, double x, double *runerrbound, unsigned int k);

dd_real AccCheb1ValwErr(double *P, unsigned int n, double x, double *runerrbound);
dd_real AccCheb2ValwErr(double *P, unsigned int n, double x, double *runerrbound);
dd_real AccCheb1DerwErr(double *P, unsigned int n, double x, double *runerrbound);
dd_real AccCheb2DerwErr(double *P, unsigned int n, double x, double *runerrbound);
dd_real AccCheb1DerKwErr(double *P, unsigned int n, double x, double *runerrbound, unsigned int k);
dd_real AccCheb2DerKwErr(double *P, unsigned int n, double x, double *runerrbound, unsigned int k);
#endif
