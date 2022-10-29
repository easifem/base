/******************************************************************************
%  Set of subroutines and functions to evaluate series of LEGENDRE polynomials at the point x, which is in [-1, 1].
%     It allows to evaluate the series and derivatives of it at any point using standard double precision with and without
%     error bounds to provide information of the quality of the evaluation. It also allows ACCURATE evaluations using
%     a new compensated algorithm.
%
%  Licensing:
%
%    This code is distributed under the GNU General Public License 3 (GPLv3).
%
%  Modified:
%
%    5 November 2016
%
%  Authors:
%
%    Roberto Barrio, Peibing Du, Hao Jiang and Sergio Serrano
%
%  Algorithm:
%               BCS-algorithm with running error bound
%  References:
%               R. Barrio, J.M. Pe~na,
%               Numerical evaluation of the pth derivative of Jacobi series,
%               Applied Numerical Mathmetics 43 335-357, (2002).
%
%               H. Jiang, R. Barrio, H. Li, X. Liao, L. Cheng, F. Su,
%               Accurate evaluation of a polynomial in Chebyshev form
%               Applied Mathematics and Computation 217 (23), 9702-9716, (2011).
%*****************************************************************************/

#include <stdio.h>
#include <assert.h>
#include <stdlib.h>
#include <math.h>

#include "inline.h"
#include "legendre_series.h"

/* LegenVal evaluates a series of legendre polynomial at the point x, which is in [-1, 1].*/
double LegenVal(double *P, unsigned int n, double x)
{
    assert(0 < n);
    assert(fabs(x) <= 1.0);
    double t, b1 = 0, b2 = 0;
    int j;
    double i;

    for (j = n; j >= 1; j--)
    {
        i = 1.0 * j;
        t = (2 * i + 1) / (i + 1) * x * b1 - (i + 1) / (i + 2) * b2 + P[j];
        b2 = b1;
        b1 = t;
    }
    return x * b1 - b2 / 2 + P[0];
}

/*CompLegenVal evaluates a series of legendre polynomial at the point x, which is in [-1, 1], with compensated method.*/
double CompLegenVal(double *P, unsigned int n, double x)
{
    assert(0 < n);
    assert(fabs(x) <= 1.0);
    double b1 = 0, b2 = 0;
    double err_temp, s, t, errb1 = 0, errb2 = 0, res;
    dd_real A, C, temp1, temp2, temp3, temp4, temp5;
    int j;
    double i;

    for (j = n; j >= 1; j--)
    {
        i = 1.0 * j;
        A = div_d_d(2 * i + 1, i + 1);
        C = div_d_d(i + 1, i + 2);
        temp1 = two_prod(A.H, x);
        temp2 = two_prod(temp1.H, b1);
        s = temp1.L * b1 + temp2.L;

        temp3 = two_prod(C.H, b2);

        temp4 = two_sum(temp2.H, -temp3.H);
        temp5 = two_sum(temp4.H, P[j]);

        t = A.L * x * b1 - C.L * b2;

        b2 = b1;
        b1 = temp5.H;
        //--------------the compensated part---------------//
        err_temp = temp1.H * errb1 - C.H * errb2 + (s - temp3.L + temp4.L + temp5.L + t);
        errb2 = errb1;
        errb1 = err_temp;
    }
    temp1 = two_prod(x, b1);
    s = -b2 / 2;
    temp2 = two_sum(temp1.H, s);
    temp3 = two_sum(temp2.H, P[0]);
    //--------------the compensated part---------------//
    err_temp = x * errb1 - errb2 / 2 + (temp1.L + temp2.L + temp3.L);
    res = temp3.H + err_temp;

    return res;
}

/*AccLegenVal evaluates a series of legendre polynomial at the point x, which is in [-1, 1], with compensated method.*/
dd_real AccLegenVal(double *P, unsigned int n, double x)
{
    assert(0 < n);
    assert(fabs(x) <= 1.0);
    double b1 = 0, b2 = 0;
    double err_temp, s, t, errb1 = 0, errb2 = 0;
    dd_real res, A, C, temp1, temp2, temp3, temp4, temp5;
    int j;
    double i;

    for (j = n; j >= 1; j--)
    {
        i = 1.0 * j;
        A = div_d_d(2 * i + 1, i + 1);
        C = div_d_d(i + 1, i + 2);
        temp1 = two_prod(A.H, x);
        temp2 = two_prod(temp1.H, b1);
        s = temp1.L * b1 + temp2.L;

        temp3 = two_prod(C.H, b2);

        temp4 = two_sum(temp2.H, -temp3.H);
        temp5 = two_sum(temp4.H, P[j]);

        t = A.L * x * b1 - C.L * b2;

        b2 = b1;
        b1 = temp5.H;
        //--------------the compensated part---------------//
        err_temp = temp1.H * errb1 - C.H * errb2 + (s - temp3.L + temp4.L + temp5.L + t);
        errb2 = errb1;
        errb1 = err_temp;
    }
    temp1 = two_prod(x, b1);
    s = -b2 / 2;
    temp2 = two_sum(temp1.H, s);
    temp3 = two_sum(temp2.H, P[0]);
    //--------------the compensated part---------------//
    err_temp = x * errb1 - errb2 / 2 + (temp1.L + temp2.L + temp3.L);
    res = quick_two_sum(temp3.H, err_temp);

    return res;
}

/*LegenDer evaluates  the first derivative of a series of legendre polynomial at the point x, which is in [-1, 1]. */
double LegenDer(double *P, unsigned int n, double x)
{
    assert(0 < n);
    assert(fabs(x) <= 1.0);
    double t, b1 = 0, b2 = 0;
    int j;
    double i;

    for (j = n - 1; j >= 0; j--)
    {
        i = 1.0 * j;
        t = (2 * i + 3) / (i + 1) * x * b1 - (i + 3) / (i + 2) * b2 + P[j + 1];
        b2 = b1;
        b1 = t;
    }
    return b1;
}

/*CompLegenDer evaluates the first derivative of a series of legendre polynomial at the point x, which is in [-1, 1], with compensated method.*/
double CompLegenDer(double *P, unsigned int n, double x)
{
    assert(0 < n);
    assert(fabs(x) <= 1.0);
    double b1 = 0, b2 = 0;
    double err_temp, s, t, errb1 = 0, errb2 = 0, res;
    dd_real A1, A2, temp1, temp2, temp3, temp4, temp5;
    int j;
    double i;

    for (j = n - 1; j >= 0; j--)
    {
        i = 1.0 * j;
        A1 = div_d_d(2 * i + 3, i + 1);
        A2 = div_d_d(i + 3, i + 2);
        temp1 = two_prod(A1.H, x);
        temp2 = two_prod(temp1.H, b1);
        s = temp1.L * b1 + temp2.L;

        temp3 = two_prod(A2.H, b2);

        temp4 = two_sum(temp2.H, -temp3.H);
        temp5 = two_sum(temp4.H, P[j + 1]);

        t = A1.L * x * b1 - A2.L * b2;

        b2 = b1;
        b1 = temp5.H;
        //--------------the compensated part---------------//
        err_temp = A1.H * x * errb1 - A2.H * errb2 + (s - temp3.L + temp4.L + temp5.L + t);
        errb2 = errb1;
        errb1 = err_temp;
    }
    res = b1 + errb1;
    return res;
}

/*AccLegenDer evaluates the first derivative of a series of legendre polynomial at the point x, which is in [-1, 1], with compensated method.*/
dd_real AccLegenDer(double *P, unsigned int n, double x)
{
    assert(0 < n);
    assert(fabs(x) <= 1.0);
    double b1 = 0, b2 = 0;
    double err_temp, s, t, errb1 = 0, errb2 = 0;
    dd_real res, A1, A2, temp1, temp2, temp3, temp4, temp5;
    int j;
    double i;

    for (j = n - 1; j >= 0; j--)
    {
        i = 1.0 * j;
        A1 = div_d_d(2 * i + 3, i + 1);
        A2 = div_d_d(i + 3, i + 2);
        temp1 = two_prod(A1.H, x);
        temp2 = two_prod(temp1.H, b1);
        s = temp1.L * b1 + temp2.L;

        temp3 = two_prod(A2.H, b2);

        temp4 = two_sum(temp2.H, -temp3.H);
        temp5 = two_sum(temp4.H, P[j + 1]);

        t = A1.L * x * b1 - A2.L * b2;

        b2 = b1;
        b1 = temp5.H;
        //--------------the compensated part---------------//
        err_temp = A1.H * x * errb1 - A2.H * errb2 + (s - temp3.L + temp4.L + temp5.L + t);
        errb2 = errb1;
        errb1 = err_temp;
    }
    res = quick_two_sum(b1, errb1);
    return res;
}

/*LegenDerK evaluates  the k-th derivative of a series of legendre polynomial at the point x, which is in [-1, 1]. */
double LegenDerK(double *P, unsigned int n, double x, unsigned int k)
{
    assert(0 < n);
    assert(fabs(x) <= 1.0);
    double s = 1.0, t, b1 = 0, b2 = 0;
    int j;
    double i;

    for (j = 2 * k - 1; j > 0; j = j - 2)
    {
        s = j * s;
    }
    double A1, A2;

    for (j = n - k; j >= 0; j--)
    {
        i = 1.0 * j;
        A1 = (2 * i + 2 * k + 1) / (i + 1) * x;
        A2 = -(i + 2 * k + 1) / (i + 2);
        t = A1 * b1 + A2 * b2 + P[j + k];

        b2 = b1;
        b1 = t;
    }
    return s * b1;
}

/*CompLegenDerK evaluates the k-th derivative of a series of legendre polynomial at the point x, which is in [-1, 1], with compensated method.*/
double CompLegenDerK(double *P, unsigned int n, double x, unsigned int k)
{
    assert(0 < n);
    assert(fabs(x) <= 1.0);
    double s = 1.0, t, b1 = 0, b2 = 0;
    double err_temp, errb1 = 0, errb2 = 0, res;
    dd_real A1, A2, temp1, temp2, temp3, temp4, temp5;
    int i;

    for (i = 2 * k - 1; i > 0; i = i - 2)
    {
        s = i * s;
    }

    for (i = n - k; i >= 0; i--)
    {
        A1 = div_d_d(2 * i + 2 * k + 1, i + 1);
        A2 = div_d_d(i + 2 * k + 1, i + 2);
        temp1 = two_prod(A1.H, x);
        temp2 = two_prod(temp1.H, b1);
        temp2.L = temp1.L * b1 + temp2.L;

        temp3 = two_prod(A2.H, b2);

        temp4 = two_sum(temp2.H, -temp3.H);
        temp5 = two_sum(temp4.H, P[i + k]);

        t = A1.L * x * b1 - A2.L * b2;

        b2 = b1;
        b1 = temp5.H;
        //--------------the compensated part---------------//
        err_temp = temp1.H * errb1 - A2.H * errb2 + (temp2.L - temp3.L + temp4.L + temp5.L + t);
        errb2 = errb1;
        errb1 = err_temp;
    }
    temp1 = two_prod(s, b1);
    temp1.L = s * errb1 + temp1.L;
    res = temp1.H + temp1.L;

    return res;
}

/*AccLegenDerK evaluates the k-th derivative of a series of legendre polynomial at the point x, which is in [-1, 1], with compensated method.*/
dd_real AccLegenDerK(double *P, unsigned int n, double x, unsigned int k)
{
    assert(0 < n);
    assert(fabs(x) <= 1.0);
    double s = 1.0, t, b1 = 0, b2 = 0;
    double err_temp, errb1 = 0, errb2 = 0;
    dd_real res, A1, A2, temp1, temp2, temp3, temp4, temp5;
    int i;

    for (i = 2 * k - 1; i > 0; i = i - 2)
    {
        s = i * s;
    }

    for (i = n - k; i >= 0; i--)
    {
        A1 = div_d_d(2 * i + 2 * k + 1, i + 1);
        A2 = div_d_d(i + 2 * k + 1, i + 2);
        temp1 = two_prod(A1.H, x);
        temp2 = two_prod(temp1.H, b1);
        temp2.L = temp1.L * b1 + temp2.L;

        temp3 = two_prod(A2.H, b2);

        temp4 = two_sum(temp2.H, -temp3.H);
        temp5 = two_sum(temp4.H, P[i + k]);

        t = A1.L * x * b1 - A2.L * b2;

        b2 = b1;
        b1 = temp5.H;
        //--------------the compensated part---------------//
        err_temp = temp1.H * errb1 - A2.H * errb2 + (temp2.L - temp3.L + temp4.L + temp5.L + t);
        errb2 = errb1;
        errb1 = err_temp;
    }
    temp1 = two_prod(s, b1);
    temp1.L = s * errb1 + temp1.L;
    res = quick_two_sum(temp1.H, temp1.L);

    return res;
}

/*----------------------- LegenValwErr evaluates a series of Legendre polynomial at the point x, which is in [-1, 1], and performs a running-error bound */
double LegenValwErr(double *P, unsigned int n, double x, double *runerrbound)
{
    assert(0 < n);
    assert(fabs(x) <= 1.0);
    double t, b1 = 0, b2 = 0;
    double abst, absb1 = 0;
    int i;
    double j;
    double A1, A2, absx;
    absx = fabs(x);
    int na, nb;
    na = 2;
    nb = 1;
    double errz, errz1, errz2, errztemp;
    errz1 = 0;
    errz2 = 0;
    errztemp = 0;

    for (i = n; i >= 1; i--)
    {
        j = 1.0 * i;
        A1 = (2 * j + 1) / (j + 1);
        A2 = -(j + 1) / (j + 2);
        t = A1 * x * b1 + A2 * b2 + P[i];
        abst = fabs(t);

        errz = errz1 * A1 * absx + errz2 * fabs(A2) + fabs(P[i]);
        errz2 = errztemp + (nb + 2) * absb1;
        errz1 = errz + (na + 3) * abst;

        b2 = b1;
        b1 = t;

        errztemp = errz;
        absb1 = abst;
    }
    t = x * b1 - b2 / 2 + P[0];
    abst = fabs(t);

    errz = errz1 * absx + errz2 / 2 + fabs(P[0]);
    errz2 = errztemp + (nb + 2) * absb1;
    errz1 = errz + (na + 3) * abst;

    b2 = b1;
    b1 = t;

    *(runerrbound) = (errz1 - (2 + na) * abst) * unit;

    return b1;
}

/*----------------------- LegenDerwErr evaluates  the first derivative of a series of Legendre polynomial at the point x, which is in [-1, 1], and performs a running-error bound  */
double LegenDerwErr(double *P, unsigned int n, double x, double *runerrbound)
{
    assert(0 < n);
    assert(fabs(x) <= 1.0);
    double t, b1 = 0, b2 = 0;
    double A1, A2;
    double abst = 0, absb1 = 0;
    int i;
    double j;
    int na, nb, nc;
    na = 2;
    nb = 1;
    nc = 0;
    double errz, errz1, errz2, errztemp;
    errz1 = 0;
    errz2 = 0;
    errztemp = 0;

    for (i = n - 1; i >= 0; i--)
    {
        j = 1.0 * i;
        A1 = (2 * j + 3) / (j + 1) * x;
        A2 = -(j + 3) / (j + 2);
        t = A1 * b1 + A2 * b2 + P[i + 1];

        abst = fabs(t);

        errz = errz1 * fabs(A1) + errz2 * fabs(A2) + (nc + 2) * fabs(P[i + 1]);
        errz2 = errztemp + (nb + 2) * absb1;
        errz1 = errz + (na + 3) * abst;

        b2 = b1;
        b1 = t;

        errztemp = errz;
        absb1 = abst;
    }
    *(runerrbound) = (errz1 - (1 + na) * abst) * unit;

    return b1;
}

/*----------------------- LegenDerKwErr evaluates  the k-th derivative of a series of Legendre polynomial at the point x, which is in [-1, 1], and performs a running-error bound */
double LegenDerKwErr(double *P, unsigned int n, double x, double *runerrbound, unsigned int k)
{
    assert(0 < n);
    assert(fabs(x) <= 1.0);
    double s = 1.0, t, b1 = 0, b2 = 0;
    int j;
    double i;

    for (j = 2 * k - 1; j > 0; j = j - 2)
    {
        s = j * s;
    }
    double A1, A2;
    double abst = 0, absb1 = 0;

    int na, nb, nc;
    na = 2;
    nb = 1;
    nc = 0;
    double errz, errz1, errz2, errztemp;
    errz1 = 0;
    errz2 = 0;
    errztemp = 0;

    for (j = n - k; j >= 0; j--)
    {
        i = 1.0 * j;
        A1 = (2 * i + 2 * k + 1) / (i + 1) * x;
        A2 = -(i + 2 * k + 1) / (i + 2);
        t = A1 * b1 + A2 * b2 + P[j + k];

        abst = fabs(t);

        errz = errz1 * fabs(A1) + errz2 * fabs(A2) + (nc + 2) * fabs(P[j + k]);
        errz2 = errztemp + (nb + 2) * absb1;
        errz1 = errz + (na + 3) * abst;

        b2 = b1;
        b1 = t;

        errztemp = errz;
        absb1 = abst;
    }
    *(runerrbound) = (errz1 - (1 + na) * abst) * unit * s;

    return s * b1;
}

/*CompLegenValwErr evaluates a series of Legendre polynomial at the point x, which is in [-1, 1], with compensated method, together with the running error bound.*/
double CompLegenValwErr(double *P, unsigned int n, double x, double *runerrbound)
{
    assert(0 < n);
    assert(fabs(x) <= 1.0);
    double b1 = 0, b2 = 0;
    double res, err_temp, errb1 = 0, errb2 = 0;
    dd_real A1, A2, temp1, temp2, temp3, temp4, temp5;
    int i;
    double j;

    double abst = 0, absb1 = 0;
    int na, nb, ne;
    na = 2;
    nb = 1;
    ne = 8;

    double errz, errz1, errz2, errztemp, err_P;
    errz1 = 0;
    errz2 = 0;
    errztemp = 0;

    for (i = n; i >= 0; i--)
    {
        j = 1.0 * i;

        A1 = div_d_d(2 * j + 1, j + 1);

        A2 = div_d_d(-j - 1, j + 2);

        temp1 = two_prod(A1.H, x);
        temp2 = two_prod(temp1.H, b1);

        temp3 = two_prod(A2.H, b2);

        temp4 = two_sum(temp2.H, temp3.H);
        temp5 = two_sum(temp4.H, P[i]);
        //--------------the compensated part---------------//
        err_P = temp1.L * b1 + temp2.L + temp3.L + temp4.L + temp5.L + A1.L * x * b1 + A2.L * b2;
        err_temp = temp1.H * errb1 + A2.H * errb2 + err_P;
        //--------------the running error bound------------//
        abst = fabs(err_temp);
        errz = errz1 * fabs(temp1.H) + errz2 * (-A2.H) + (ne + 1) * fabs(err_P);

        errz2 = errztemp + (nb + 2) * absb1;
        errz1 = errz + (na + 3) * abst;
        //--------------the next iteration-----------------//
        b2 = b1;
        b1 = temp5.H;

        errb2 = errb1;
        errb1 = err_temp;

        errztemp = errz;
        absb1 = abst;
    }
    res = b1 + errb1;
    *(runerrbound) = ((errz1 - (2 + na) * abst) * unit + fabs(errb1 - (res - b1))) / (1 - unit * 2);

    return res;
}

/*----------------------- CompLegenDerwErr evaluates  the first derivative of a series of Legendre polynomial  at the point x, which is in [-1, 1], with compensated method, and performs a running-error bound */
double CompLegenDerwErr(double *P, unsigned int n, double x, double *runerrbound)
{
    assert(0 < n);
    assert(fabs(x) <= 1.0);
    double b1 = 0, b2 = 0;
    double res, err_temp, errb1 = 0, errb2 = 0;
    dd_real temp1, temp2, temp3, temp4;
    dd_real Temp, A1, A2;
    int i;
    double j;

    double abst = 0, absb1 = 0;
    int na, nb, ne;
    na = 2;
    nb = 1;
    ne = 6;

    double errz, errz1, errz2, errztemp, err_P;
    errz1 = 0;
    errz2 = 0;
    errztemp = 0;

    for (i = n - 1; i >= 0; i--)
    {
        j = 1.0 * i;

        Temp = two_prod(2 * j + 3, x);
        A1 = div_dd_d(Temp, j + 1);

        A2 = div_d_d(-j - 3, j + 2);

        temp1 = two_prod(A1.H, b1);
        temp2 = two_prod(A2.H, b2);
        temp3 = two_sum(temp1.H, temp2.H);
        temp4 = two_sum(temp3.H, P[i + 1]);
        //--------------the compensated part---------------//
        err_P = A1.L * b1 + A2.L * b2 + temp1.L + temp2.L + temp3.L + temp4.L;
        err_temp = A1.H * errb1 + A2.H * errb2 + err_P;
        //--------------the running error bound------------//
        abst = fabs(err_temp);
        errz = errz1 * fabs(A1.H) + errz2 * fabs(A2.H) + (ne + 1) * fabs(err_P);

        errz2 = errztemp + (nb + 2) * absb1;
        errz1 = errz + (na + 3) * abst;
        //------------- the next step---------------------//
        b2 = b1;
        b1 = temp4.H;

        errb2 = errb1;
        errb1 = err_temp;

        errztemp = errz;
        absb1 = abst;
    }
    res = b1 + errb1;
    *(runerrbound) = ((errz1 - (1 + na) * abst) * unit + fabs(errb1 - (res - b1))) / (1 - unit * 2);

    return res;
}

/*----------------------- CompLegenDerKwErr evaluates  the k-th derivative of a series of Legendre polynomial  at the point x, which is in [-1, 1], with compensated method, and performs a running-error bound */
double CompLegenDerKwErr(double *P, unsigned int n, double x, double *runerrbound, unsigned int k)
{
    assert(0 < n);
    assert(fabs(x) <= 1.0);

    double s = 1.0, t, b1 = 0, b2 = 0;
    double res, err_temp, errb1 = 0, errb2 = 0;
    dd_real A1, A2, temp1, temp2, temp3, temp4, temp5;
    int i;

    for (i = 2 * k - 1; i > 0; i = i - 2)
    {
        s = i * s;
    }
    double abst = 0, absb1 = 0;
    int na, nb, ne;
    na = 2;
    nb = 1;
    ne = 6;

    double errz, errz1, errz2, errztemp, err_P;
    errz1 = 0;
    errz2 = 0;
    errztemp = 0;

    for (i = n - k; i >= 0; i--)
    {
        A1 = div_d_d(2 * i + 2 * k + 1, i + 1);
        A2 = div_d_d(i + 2 * k + 1, i + 2);

        temp1 = two_prod(A1.H, x);
        temp2 = two_prod(temp1.H, b1);
        temp2.L = temp1.L * b1 + temp2.L;

        temp3 = two_prod(A2.H, b2);

        temp4 = two_sum(temp2.H, -temp3.H);
        temp5 = two_sum(temp4.H, P[i + k]);

        t = A1.L * x * b1 - A2.L * b2;
        //--------------the compensated part---------------//
        err_P = temp2.L - temp3.L + temp4.L + temp5.L + t;
        err_temp = temp1.H * errb1 - A2.H * errb2 + err_P;
        //--------------the running error bound------------//
        abst = fabs(err_temp);
        errz = errz1 * fabs(temp1.H) + errz2 * A2.H + (ne + 1) * fabs(err_P);

        errz2 = errztemp + (nb + 2) * absb1;
        errz1 = errz + (na + 3) * abst;
        //------------- the next step---------------------//
        b2 = b1;
        b1 = temp5.H;

        errb2 = errb1;
        errb1 = err_temp;

        errztemp = errz;
        absb1 = abst;
    }
    temp1 = two_prod(s, b1);
    temp1.L = s * errb1 + temp1.L;

    res = temp1.H + temp1.L;
    *(runerrbound) = ((errz1 - (2 + na) * abst) * unit * s + fabs(temp1.L - (res - temp1.H))) / (1 - unit * 3);

    return res;
}

/*----------------------- AccLegenValwErr evaluates a series of Legendre polynomial  at the point x, which is in [-1, 1], with compensated method, and performs a running-error bound */
dd_real AccLegenValwErr(double *P, unsigned int n, double x, double *runerrbound)
{
    assert(0 < n);
    assert(fabs(x) <= 1.0);
    double b1 = 0, b2 = 0;
    double err_temp, errb1 = 0, errb2 = 0;
    dd_real A1, A2, res, temp1, temp2, temp3, temp4, temp5;
    int i;
    double j;

    double abst = 0, absb1 = 0;
    int na, nb, ne;
    na = 2;
    nb = 1;
    ne = 8;

    double errz, errz1, errz2, errztemp, err_P;
    errz1 = 0;
    errz2 = 0;
    errztemp = 0;

    for (i = n; i >= 0; i--)
    {
        j = 1.0 * i;

        A1 = div_d_d(2 * j + 1, j + 1);
        A2 = div_d_d(-j - 1, j + 2);

        temp1 = two_prod(A1.H, x);
        temp2 = two_prod(temp1.H, b1);

        temp3 = two_prod(A2.H, b2);

        temp4 = two_sum(temp2.H, temp3.H);
        temp5 = two_sum(temp4.H, P[i]);
        //--------------the compensated part---------------//
        err_P = temp1.L * b1 + temp2.L + temp3.L + temp4.L + temp5.L + A1.L * x * b1 + A2.L * b2;
        err_temp = temp1.H * errb1 + A2.H * errb2 + err_P;
        //--------------the running error bound------------//
        abst = fabs(err_temp);
        errz = errz1 * fabs(temp1.H) + errz2 * (-A2.H) + (ne + 1) * fabs(err_P);

        errz2 = errztemp + (nb + 2) * absb1;
        errz1 = errz + (na + 3) * abst;
        //--------------the next iteration-----------------//
        b2 = b1;
        b1 = temp5.H;

        errb2 = errb1;
        errb1 = err_temp;

        errztemp = errz;
        absb1 = abst;
    }
    res = quick_two_sum(b1, errb1);
    *(runerrbound) = (errz1 - (2 + na) * abst) * unit;

    return res;
}

/*----------------------- AccLegenDerwErr evaluates  the first derivative of a series of Legendre polynomial  at the point x, which is in [-1, 1], with compensated method, and performs a running-error bound */
dd_real AccLegenDerwErr(double *P, unsigned int n, double x, double *runerrbound)
{
    assert(0 < n);
    assert(fabs(x) <= 1.0);
    double b1 = 0, b2 = 0;
    double err_temp, errb1 = 0, errb2 = 0;
    dd_real res, temp1, temp2, temp3, temp4;
    dd_real Temp, A1, A2;
    int i;
    double j;

    double abst = 0, absb1 = 0;
    int na, nb, ne;
    na = 2;
    nb = 1;
    ne = 6;

    double errz, errz1, errz2, errztemp, err_P;
    errz1 = 0;
    errz2 = 0;
    errztemp = 0;

    for (i = n - 1; i >= 0; i--)
    {
        j = 1.0 * i;

        Temp = two_prod(2 * j + 3, x);
        A1 = div_dd_d(Temp, j + 1);
        A2 = div_d_d(-j - 3, j + 2);

        temp1 = two_prod(A1.H, b1);
        temp2 = two_prod(A2.H, b2);
        temp3 = two_sum(temp1.H, temp2.H);
        temp4 = two_sum(temp3.H, P[i + 1]);
        //--------------the compensated part---------------//
        err_P = A1.L * b1 + A2.L * b2 + temp1.L + temp2.L + temp3.L + temp4.L;
        err_temp = A1.H * errb1 + A2.H * errb2 + err_P;
        //--------------the running error bound------------//
        abst = fabs(err_temp);
        errz = errz1 * fabs(A1.H) + errz2 * fabs(A2.H) + (ne + 1) * fabs(err_P);

        errz2 = errztemp + (nb + 2) * absb1;
        errz1 = errz + (na + 3) * abst;
        //------------- the next step---------------------//
        b2 = b1;
        b1 = temp4.H;

        errb2 = errb1;
        errb1 = err_temp;

        errztemp = errz;
        absb1 = abst;
    }
    res = quick_two_sum(b1, errb1);
    *(runerrbound) = (errz1 - (1 + na) * abst) * unit;

    return res;
}

/*----------------------- AccLegenDerKwErr evaluates  the k-th derivative of a series of Legendre polynomial  at the point x, which is in [-1, 1], with compensated method, and performs a running-error bound */
dd_real AccLegenDerKwErr(double *P, unsigned int n, double x, double *runerrbound, unsigned int k)
{
    assert(0 < n);
    assert(fabs(x) <= 1.0);

    double s = 1.0, t, b1 = 0, b2 = 0;
    double err_temp, errb1 = 0, errb2 = 0;
    dd_real res, A1, A2, temp1, temp2, temp3, temp4, temp5;
    int i;

    for (i = 2 * k - 1; i > 0; i = i - 2)
    {
        s = i * s;
    }
    double abst = 0, absb1 = 0;
    int na, nb, ne;
    na = 2;
    nb = 1;
    ne = 6;

    double errz, errz1, errz2, errztemp, err_P;
    errz1 = 0;
    errz2 = 0;
    errztemp = 0;

    for (i = n - k; i >= 0; i--)
    {
        A1 = div_d_d(2 * i + 2 * k + 1, i + 1);
        A2 = div_d_d(i + 2 * k + 1, i + 2);

        temp1 = two_prod(A1.H, x);
        temp2 = two_prod(temp1.H, b1);
        temp2.L = temp1.L * b1 + temp2.L;

        temp3 = two_prod(A2.H, b2);

        temp4 = two_sum(temp2.H, -temp3.H);
        temp5 = two_sum(temp4.H, P[i + k]);

        t = A1.L * x * b1 - A2.L * b2;
        //--------------the compensated part---------------//
        err_P = temp2.L - temp3.L + temp4.L + temp5.L + t;
        err_temp = temp1.H * errb1 - A2.H * errb2 + err_P;
        //--------------the running error bound------------//
        abst = fabs(err_temp);
        errz = errz1 * fabs(temp1.H) + errz2 * A2.H + (ne + 1) * fabs(err_P);

        errz2 = errztemp + (nb + 2) * absb1;
        errz1 = errz + (na + 3) * abst;
        //------------- the next step---------------------//
        b2 = b1;
        b1 = temp5.H;

        errb2 = errb1;
        errb1 = err_temp;

        errztemp = errz;
        absb1 = abst;
    }
    temp1 = two_prod(s, b1);
    temp1.L = s * errb1 + temp1.L;
    res = quick_two_sum(temp1.H, temp1.L);
    *(runerrbound) = (errz1 - (2 + na) * abst) * unit * s;

    return res;
}
