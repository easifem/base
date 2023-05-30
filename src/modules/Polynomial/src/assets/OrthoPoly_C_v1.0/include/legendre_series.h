#ifndef  _LEGENDRE_H
#define  _LEGENDRE_H

#ifndef  _INLINE_H
#define  _INLINE_H
#include"inline.h"
#endif


double LegenVal(double *P, unsigned int n, double x);
double CompLegenVal(double *P, unsigned int n, double x);
dd_real AccLegenVal(double *P, unsigned int n, double x);

double LegenDer(double *P, unsigned int n, double x);
double CompLegenDer(double *P, unsigned int n, double x);
dd_real AccLegenDer(double *P, unsigned int n, double x);

double LegenDerK(double *P, unsigned int n, double x ,unsigned int k);
double CompLegenDerK(double *P, unsigned int n, double x ,unsigned int k);
dd_real AccLegenDerK(double *P, unsigned int n, double x, unsigned int k);

double LegenValwErr(double *P, unsigned int n, double x, double * runerrbound);
double LegenDerwErr(double *P, unsigned int n, double x, double * runerrbound);
double LegenDerKwErr(double *P, unsigned int n, double x, double * runerrbound, unsigned int k);

double CompLegenValwErr(double *P, unsigned int n, double x, double * runerrbound);
double CompLegenDerwErr(double *P, unsigned int n, double x, double * runerrbound);
double CompLegenDerKwErr(double *P, unsigned int n, double x, double * runerrbound, unsigned int k);

dd_real AccLegenValwErr(double *P, unsigned int n, double x, double * runerrbound);
dd_real AccLegenDerwErr(double *P, unsigned int n, double x, double * runerrbound);
dd_real AccLegenDerKwErr(double *P, unsigned int n, double x, double * runerrbound, unsigned int k);

#endif
