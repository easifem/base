#ifndef  _HERMITE_H
#define  _HERMITE_H

#ifndef  _INLINE_H
#define  _INLINE_H
#include"inline.h"
#endif

double Herm1Val(double *P, unsigned int n, double x);
double Herm2Val(double *P, unsigned int n, double x);
double CompHerm1Val(double *P, unsigned int n, double x);
double CompHerm2Val(double *P, unsigned int n, double x);
dd_real AccHerm1Val(double *P, unsigned int n, double x);
dd_real AccHerm2Val(double *P, unsigned int n, double x);

double Herm1Der(double *P, unsigned int n, double x);
double Herm2Der(double *P, unsigned int n, double x);
double CompHerm1Der(double *P, unsigned int n, double x);
double CompHerm2Der(double *P, unsigned int n, double x);
dd_real AccHerm1Der(double *P, unsigned int n, double x);
dd_real AccHerm2Der(double *P, unsigned int n, double x);

double Herm1DerK(double *P, unsigned int n, double x, unsigned int k);
double Herm2DerK(double *P, unsigned int n, double x, unsigned int k);
double CompHerm1DerK(double *P, unsigned int n, double x, unsigned int k);
double CompHerm2DerK(double *P, unsigned int n, double x, unsigned int k);
dd_real AccHerm1DerK(double *P, unsigned int n, double x, unsigned int k);
dd_real AccHerm2DerK(double *P, unsigned int n, double x, unsigned int k);

double Herm1ValwErr(double *P, unsigned int n, double x, double * runerrbound);
double Herm2ValwErr(double *P, unsigned int n, double x, double * runerrbound);
double Herm1DerwErr(double *P, unsigned int n, double x, double * runerrbound);
double Herm2DerwErr(double *P, unsigned int n, double x, double * runerrbound);
double Herm1DerKwErr(double *P, unsigned int n, double x, double * runerrbound, unsigned int k);
double Herm2DerKwErr(double *P, unsigned int n, double x, double * runerrbound, unsigned int k);

double CompHerm1ValwErr(double *P, unsigned int n, double x, double * runerrbound);
double CompHerm2ValwErr(double *P, unsigned int n, double x, double * runerrbound);
double CompHerm1DerwErr(double *P, unsigned int n, double x, double * runerrbound);
double CompHerm2DerwErr(double *P, unsigned int n, double x, double * runerrbound);
double CompHerm1DerKwErr(double *P, unsigned int n, double x, double * runerrbound, unsigned int k);
double CompHerm2DerKwErr(double *P, unsigned int n, double x, double * runerrbound, unsigned int k);

dd_real AccHerm1ValwErr(double *P, unsigned int n, double x, double * runerrbound);
dd_real AccHerm2ValwErr(double *P, unsigned int n, double x, double * runerrbound);
dd_real AccHerm1DerwErr(double *P, unsigned int n, double x, double * runerrbound);
dd_real AccHerm2DerwErr(double *P, unsigned int n, double x, double * runerrbound);
dd_real AccHerm1DerKwErr(double *P, unsigned int n, double x, double * runerrbound, unsigned int k);
dd_real AccHerm2DerKwErr(double *P, unsigned int n, double x, double * runerrbound, unsigned int k);
#endif
