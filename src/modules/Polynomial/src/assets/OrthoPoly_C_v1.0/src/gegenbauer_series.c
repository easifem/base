/******************************************************************************
%  Set of subroutines and functions to evaluate series of GEGENBAUER polynomials at the point x, which is in [-1, 1].
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
%    6 November 2016
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
#include "gegenbauer_series.h"

/*The three-term recurrence coefficients of gegenbauer polynomial */
void RecurCoefGegen(double n, double lambda, double *A, double *C)
{
    int i;
    double j;
    for (i = 1; i <= n + 2; i++)
    {
        j = 1.0 * i;
        *(A + i - 1) = 2 * (j + lambda - 1) / j;
        *(C + i - 1) = (j + 2 * lambda - 2) / j;
    }
}

void RecurCoefGegen_DD(double n, double lambda, dd_real *A, dd_real *C)
{
    int i;
    double j;
    dd_real s0, s1, s2, s3;

    for (i = 1; i <= n + 2; i++)
    {
        j = 1.0 * i;
        s0 = two_sum(j - 1, lambda);
        s1 = prod_dd_d(s0, 2);
        *(A + i - 1) = div_dd_d(s1, j);
        s2 = two_prod(2, lambda);
        s3 = add_dd_d(s2, j - 2);
        *(C + i - 1) = div_dd_d(s3, j);
    }
}

/*GegenVal evaluates a series of Gegenbauer polynomial at the point x, which is in [-1, 1] */
double GegenVal(double *P, unsigned int n, double x, double lambda)
{
    assert(0 < n);
    assert(fabs(x) <= 1.0);
    assert(lambda > -0.5);
    assert(lambda != 0);
    double t, b1 = 0, b2 = 0;
    int j;

    double *A = (double *)malloc(sizeof(double) * (n + 2));
    double *C = (double *)malloc(sizeof(double) * (n + 2));

    RecurCoefGegen(n, lambda, A, C);

    for (j = n; j >= 0; j--)
    {
        t = *(A + j) * x * b1 - *(C + j + 1) * b2 + P[j];
        b2 = b1;
        b1 = t;
    }
    free(A);
    free(C);
    return b1;
}

/*CompGegenVal evaluates a series of Gegenbauer polynomial  at the point x, which is in [-1, 1], with compensated method */
double CompGegenVal(double *P, unsigned int n, double x, double lambda)
{
    assert(0 < n);
    assert(fabs(x) <= 1.0);
    assert(lambda > -0.5);
    assert(lambda != 0);
    double b1 = 0, b2 = 0, res;
    double err_temp, err_temp1, err_temp2, err_temp3, errb1 = 0, errb2 = 0;
    dd_real temp1, temp2, temp3, temp4, temp5;
    int j;

    dd_real *A = (dd_real *)malloc(sizeof(dd_real) * (n + 2));
    dd_real *C = (dd_real *)malloc(sizeof(dd_real) * (n + 2));

    RecurCoefGegen_DD(n, lambda, A, C);

    for (j = n; j >= 0; j--)
    {
        temp1 = two_prod((A + j)->H, x);
        temp2 = two_prod(temp1.H, b1);
        temp3 = two_prod((C + j + 1)->H, b2);
        temp4 = two_sum(temp2.H, -temp3.H);
        temp5 = two_sum(temp4.H, P[j]);
        //--------------the compensated part---------------//
        err_temp1 = temp1.L * b1 + temp2.L;
        err_temp2 = ((A + j)->L) * x * b1 - ((C + j + 1)->L) * b2;
        err_temp3 = err_temp1 - temp3.L + temp4.L + temp5.L + err_temp2;
        err_temp = temp1.H * errb1 - ((C + j + 1)->H) * errb2 + err_temp3;
        //--------------the next iteration-----------------//
        b2 = b1;
        b1 = temp5.H;
        errb2 = errb1;
        errb1 = err_temp;
    }
    res = b1 + errb1;
    free(A);
    free(C);

    return res;
}

/*AccGegenVal evaluates a series of Gegenbauer polynomial  at the point x, which is in [-1, 1], with compensated method */
dd_real AccGegenVal(double *P, unsigned int n, double x, double lambda)
{
    assert(0 < n);
    assert(fabs(x) <= 1.0);
    assert(lambda > -0.5);
    assert(lambda != 0);
    double b1 = 0, b2 = 0;
    double err_temp, err_temp1, err_temp2, err_temp3, errb1 = 0, errb2 = 0;
    dd_real res, temp1, temp2, temp3, temp4, temp5;
    int j;

    dd_real *A = (dd_real *)malloc(sizeof(dd_real) * (n + 2));
    dd_real *C = (dd_real *)malloc(sizeof(dd_real) * (n + 2));

    RecurCoefGegen_DD(n, lambda, A, C);

    for (j = n; j >= 0; j--)
    {
        temp1 = two_prod((A + j)->H, x);
        temp2 = two_prod(temp1.H, b1);
        temp3 = two_prod((C + j + 1)->H, b2);
        temp4 = two_sum(temp2.H, -temp3.H);
        temp5 = two_sum(temp4.H, P[j]);
        //--------------the compensated part---------------//
        err_temp1 = temp1.L * b1 + temp2.L;
        err_temp2 = ((A + j)->L) * x * b1 - ((C + j + 1)->L) * b2;
        err_temp3 = err_temp1 - temp3.L + temp4.L + temp5.L + err_temp2;
        err_temp = temp1.H * errb1 - ((C + j + 1)->H) * errb2 + err_temp3;
        //--------------the next iteration-----------------//
        b2 = b1;
        b1 = temp5.H;
        errb2 = errb1;
        errb1 = err_temp;
    }
    res = quick_two_sum(b1, errb1);
    free(A);
    free(C);

    return res;
}

/*GegenDer evaluates  the first derivative of a series of Gegenbauer polynomial at the point x, which is in [-1, 1] */
double GegenDer(double *P, unsigned int n, double x, double lambda)
{
    assert(0 < n);
    assert(fabs(x) <= 1.0);
    assert(lambda > -0.5);
    assert(lambda != 0);
    double t, b1 = 0, b2 = 0;
    double A1, A2;
    int i;
    double j;
    double C = 2 * lambda;

    for (i = n - 1; i >= 0; i--)
    {
        j = 1.0 * i;
        //-------------recurrence coefficients-----------------------//
        A1 = 2 * (j + 1 + lambda) * x / (j + 1);
        A2 = -(j + 2 * lambda + 2) / (j + 2);
        //--------------iteration------------------------------------//
        t = A1 * b1 + A2 * b2 + P[i + 1];
        b2 = b1;
        b1 = t;
    }
    return C * b1;
}

/*CompGegenDer evaluates  the first derivative of a series of Gegenbauer polynomial  at the point x, which is in [-1, 1], with compensated method */
double CompGegenDer(double *P, unsigned int n, double x, double lambda)
{
    assert(0 < n);
    assert(fabs(x) <= 1.0);
    assert(lambda > -0.5);
    assert(lambda != 0);
    double b1 = 0, b2 = 0, temp, err_temp, errb1 = 0, errb2 = 0, res;
    dd_real temp1, temp2, temp3, temp4;
    dd_real A1, A2;
    dd_real s0, s1, s2;
    int i;
    double j;
    double C;
    C = 2 * lambda;

    for (i = n - 1; i >= 0; i--)
    {
        j = 1.0 * i;
        //-------------recurrence coefficients-----------------------//
        s0 = two_sum(2 * j + 2, 2 * lambda);
        s1 = prod_dd_d(s0, x);
        A1 = div_dd_d(s1, j + 1);
        s2 = two_sum(C, j + 2);
        A2 = div_dd_d(s2, -j - 2);
        //--------------iteration------------------------------------//
        temp1 = two_prod(A1.H, b1);
        temp2 = two_prod(A2.H, b2);
        temp3 = two_sum(temp1.H, temp2.H);
        temp4 = two_sum(temp3.H, P[i + 1]);
        temp = temp1.L + temp2.L + temp3.L + temp4.L;
        err_temp = A1.L * b1 + A2.L * b2 + A1.H * errb1 + A2.H * errb2 + temp;

        b2 = b1;
        b1 = temp4.H;
        errb2 = errb1;
        errb1 = err_temp;
    }
    temp1 = two_prod(C, b1);
    err_temp = C * errb1 + temp1.L;
    res = temp1.H + err_temp;

    return res;
}

/*AccGegenDer evaluates  the first derivative of a series of Gegenbauer polynomial  at the point x, which is in [-1, 1], with compensated method */
dd_real AccGegenDer(double *P, unsigned int n, double x, double lambda)
{
    assert(0 < n);
    assert(fabs(x) <= 1.0);
    assert(lambda > -0.5);
    assert(lambda != 0);
    double b1 = 0, b2 = 0, temp, err_temp, errb1 = 0, errb2 = 0;
    dd_real res, temp1, temp2, temp3, temp4;
    dd_real A1, A2;
    dd_real s0, s1, s2;
    int i;
    double j;
    double C;
    C = 2 * lambda;

    for (i = n - 1; i >= 0; i--)
    {
        j = 1.0 * i;
        //-------------recurrence coefficients-----------------------//
        s0 = two_sum(2 * j + 2, 2 * lambda);
        s1 = prod_dd_d(s0, x);
        A1 = div_dd_d(s1, j + 1);
        s2 = two_sum(C, j + 2);
        A2 = div_dd_d(s2, -j - 2);
        //--------------iteration-----------------------------------//
        temp1 = two_prod(A1.H, b1);
        temp2 = two_prod(A2.H, b2);
        temp3 = two_sum(temp1.H, temp2.H);
        temp4 = two_sum(temp3.H, P[i + 1]);
        temp = temp1.L + temp2.L + temp3.L + temp4.L;
        err_temp = A1.L * b1 + A2.L * b2 + A1.H * errb1 + A2.H * errb2 + temp;

        b2 = b1;
        b1 = temp4.H;
        errb2 = errb1;
        errb1 = err_temp;
    }
    temp1 = two_prod(C, b1);
    err_temp = C * errb1 + temp1.L;
    res = quick_two_sum(temp1.H, err_temp);

    return res;
}

/*GegenDerK evaluates  the k-th derivative of a series of Gegenbauer polynomial at the point x, which is in [-1, 1] */
double GegenDerK(double *P, unsigned int n, double x, double lambda, unsigned int k)
{
    assert(0 < n);
    assert(fabs(x) <= 1.0);
    assert(lambda > -0.5);
    assert(lambda != 0);
    double s = 1.0, t, b1 = 0, b2 = 0;
    double A1, A2;
    int i;
    double j;
    for (i = 1; i <= k; i++)
    {
        s = 2 * s * (lambda + i - 1);
    }

    for (i = n - k; i >= 0; i--)
    {
        j = 1.0 * i;
        //-------------recurrence coefficients-----------------------//
        A1 = 2 * (j + k + lambda) * x / (j + 1);
        A2 = -(j + 2 * lambda + 2 * k) / (j + 2);
        //--------------iteration-----------------------------------//
        t = A1 * b1 + A2 * b2 + P[i + k];

        b2 = b1;
        b1 = t;
    }
    return s * b1;
}

/*CompGegenDerK evaluates  the k-th derivative of a series of Gegenbauer polynomial  at the point x, which is in [-1, 1], with compensated method */
double CompGegenDerK(double *P, unsigned int n, double x, double lambda, unsigned int k)
{
    assert(0 < n);
    assert(fabs(x) <= 1.0);
    assert(lambda > -0.5);
    assert(lambda != 0);
    double b1 = 0, b2 = 0, err_temp, errb1 = 0, errb2 = 0, res;
    dd_real temp1, temp2, temp3, temp4;
    dd_real C, Ctemp, A1, A2;
    C.H = 1.0;
    C.L = 0;
    dd_real s0, s1, s2;
    int i;
    double j;
    for (i = 1; i <= k; i++)
    {
        Ctemp = two_sum(lambda, i - 1);
        C = prod_dd_dd(C, Ctemp);
        C.H = 2 * C.H;
        C.L = 2 * C.L;
    }

    for (i = n - k; i >= 0; i--)
    {
        j = 1.0 * i;
        //-------------recurrence coefficients-----------------------//
        s0 = two_sum(2 * j + 2 * k, 2 * lambda);
        s1 = prod_dd_d(s0, x);
        A1 = div_dd_d(s1, j + 1);
        s2 = two_sum(2 * lambda, j + 2 * k);
        A2 = div_dd_d(s2, -j - 2);
        //--------------iteration-----------------------------------//
        temp1 = two_prod(A1.H, b1);
        temp2 = two_prod(A2.H, b2);
        temp3 = two_sum(temp1.H, temp2.H);
        temp4 = two_sum(temp3.H, P[i + k]);
        err_temp = A1.L * b1 + A2.L * b2 + A1.H * errb1 + A2.H * errb2 + temp1.L + temp2.L + temp3.L + temp4.L;

        b2 = b1;
        b1 = temp4.H;
        errb2 = errb1;
        errb1 = err_temp;
    }
    temp1 = two_prod(C.H, b1);
    temp1.L = C.H * errb1 + temp1.L + C.L * b1;
    res = temp1.H + temp1.L;

    return res;
}

/*AccGegenDerK evaluates  the k-th derivative of a series of Gegenbauer polynomial  at the point x, which is in [-1, 1], with compensated method */
dd_real AccGegenDerK(double *P, unsigned int n, double x, double lambda, unsigned int k)
{
    assert(0 < n);
    assert(fabs(x) <= 1.0);
    assert(lambda > -0.5);
    assert(lambda != 0);
    double b1 = 0, b2 = 0, err_temp, errb1 = 0, errb2 = 0;
    dd_real res, temp1, temp2, temp3, temp4;
    dd_real C, Ctemp, A1, A2;
    C.H = 1.0;
    C.L = 0;
    dd_real s0, s1, s2;
    int i;
    double j;
    for (i = 1; i <= k; i++)
    {
        Ctemp = two_sum(lambda, i - 1);
        C = prod_dd_dd(C, Ctemp);
        C.H = 2 * C.H;
        C.L = 2 * C.L;
    }

    for (i = n - k; i >= 0; i--)
    {
        j = 1.0 * i;
        //-------------recurrence coefficients-----------------------//
        s0 = two_sum(2 * j + 2 * k, 2 * lambda);
        s1 = prod_dd_d(s0, x);
        A1 = div_dd_d(s1, j + 1);
        s2 = two_sum(2 * lambda, j + 2 * k);
        A2 = div_dd_d(s2, -j - 2);
        //--------------iteration-----------------------------------//
        temp1 = two_prod(A1.H, b1);
        temp2 = two_prod(A2.H, b2);
        temp3 = two_sum(temp1.H, temp2.H);
        temp4 = two_sum(temp3.H, P[i + k]);
        err_temp = A1.L * b1 + A2.L * b2 + A1.H * errb1 + A2.H * errb2 + temp1.L + temp2.L + temp3.L + temp4.L;

        b2 = b1;
        b1 = temp4.H;
        errb2 = errb1;
        errb1 = err_temp;
    }
    temp1 = two_prod(C.H, b1);
    temp1.L = C.H * errb1 + temp1.L + C.L * b1;
    res = quick_two_sum(temp1.H, temp1.L);

    return res;
}

/*GegenValwErr evaluates a series of Gegenbauer polynomial at the point x, which is in [-1, 1], and performs a running-error bound */
double GegenValwErr(double *P, unsigned int n, double x, double lambda, double *runerrbound)
{
    assert(0 < n);
    assert(fabs(x) <= 1.0);
    assert(lambda > -0.5);
    assert(lambda != 0);
    double t, b1 = 0, b2 = 0;
    int j;
    double abst = 0, absb1 = 0;
    int na, nb;
    na = 3;
    nb = 2;
    double errz, errz1, errz2, errztemp;
    errz1 = 0;
    errz2 = 0;
    errztemp = 0;

    double *A = (double *)malloc(sizeof(double) * (n + 2));
    double *C = (double *)malloc(sizeof(double) * (n + 2));

    RecurCoefGegen(n, lambda, A, C);

    for (j = n; j >= 0; j--)
    {
        t = *(A + j) * x * b1 - *(C + j + 1) * b2 + P[j];
        abst = fabs(t);

        errz = errz1 * fabs(*(A + j) * x) + errz2 * fabs(*(C + j + 1)) + fabs(P[j]);
        errz2 = errztemp + (nb + 2) * absb1;
        errz1 = errz + (na + 3) * abst;

        b2 = b1;
        b1 = t;

        errztemp = errz;
        absb1 = abst;
    }
    free(A);
    free(C);
    *(runerrbound) = (errz1 - (2 + na) * abst) * unit;

    return b1;
}

/*GegenDerwErr evaluates  the first derivative of a series of Gegenbauer polynomial at the point x, which is in [-1, 1], and performs a running-error bound  */
double GegenDerwErr(double *P, unsigned int n, double x, double lambda, double *runerrbound)
{
    assert(0 < n);
    assert(fabs(x) <= 1.0);
    assert(lambda > -0.5);
    assert(lambda != 0);
    double t, b1 = 0, b2 = 0;
    double A1, A2;
    int i;
    double j;
    double abst = 0, absb1 = 0;
    int na, nb, nc;
    na = 3;
    nb = 2;
    nc = 0;
    double errz, errz1, errz2, errztemp;
    errz1 = 0;
    errz2 = 0;
    errztemp = 0;
    double C;
    C = 2 * lambda;

    for (i = n - 1; i >= 0; i--)
    {
        j = 1.0 * i;
        //-------------recurrence coefficients-----------------------//
        A1 = 2 * (j + 1 + lambda) * x / (j + 1);
        A2 = -(j + 2 * lambda + 2) / (j + 2);
        //--------------iteration-----------------------------------//
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
    *(runerrbound) = (errz1 - (1 + na) * abst) * unit * C;
    return C * b1;
}

/*GegenDerKwErr evaluates  the k-th derivative of a series of Gegenbauer polynomial at the point x, which is in [-1, 1], and performs a running-error bound */
double GegenDerKwErr(double *P, unsigned int n, double x, double lambda, double *runerrbound, unsigned int k)
{
    assert(0 < n);
    assert(fabs(x) <= 1.0);
    assert(lambda > -0.5);
    assert(lambda != 0);
    double s = 1.0, t, b1 = 0, b2 = 0;
    double A1, A2;
    int i;
    double j;
    for (i = 1; i <= k; i++)
    {
        s = 2 * s * (lambda + i - 1);
    }
    double abst = 0, absb1 = 0;
    int na, nb, nc;
    na = 3;
    nb = 2;
    nc = 0;
    double errz, errz1, errz2, errztemp;
    errz1 = 0;
    errz2 = 0;
    errztemp = 0;

    for (i = n - k; i >= 0; i--)
    {
        j = 1.0 * i;
        //-------------recurrence coefficients-----------------------//
        A1 = 2 * (j + k + lambda) * x / (j + 1);
        A2 = -(j + 2 * lambda + 2 * k) / (j + 2);
        //--------------iteration-----------------------------------//
        t = A1 * b1 + A2 * b2 + P[i + k];

        abst = fabs(t);
        errz = errz1 * fabs(A1) + errz2 * fabs(A2) + (nc + 2) * fabs(P[i + k]);
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

/*CompGegenValwErr evaluates a series of Gegenbauer polynomial  at the point x, which is in [-1, 1], with compensated method, and performs a running-error bound */
double CompGegenValwErr(double *P, unsigned int n, double x, double lambda, double *runerrbound)
{
    assert(0 < n);
    assert(fabs(x) <= 1.0);
    assert(lambda > -0.5);
    assert(lambda != 0);
    double b1 = 0, b2 = 0;
    double res, err_temp, err_temp1, err_temp2, err_temp3, errb1 = 0, errb2 = 0;
    dd_real temp1, temp2, temp3, temp4, temp5;
    int j;
    double abst = 0, absb1 = 0;
    int na, nb, ne;
    na = 3;
    nb = 2;
    ne = 8;
    double errz, errz1, errz2, errztemp;
    errz1 = 0;
    errz2 = 0;
    errztemp = 0;

    dd_real *A = (dd_real *)malloc(sizeof(dd_real) * (n + 2));
    dd_real *C = (dd_real *)malloc(sizeof(dd_real) * (n + 2));

    RecurCoefGegen_DD(n, lambda, A, C);

    for (j = n; j >= 0; j--)
    {
        temp1 = two_prod((A + j)->H, x);
        temp2 = two_prod(temp1.H, b1);
        temp3 = two_prod((C + j + 1)->H, b2);
        temp4 = two_sum(temp2.H, -temp3.H);
        temp5 = two_sum(temp4.H, P[j]);
        //--------------the compensated part---------------//
        err_temp1 = temp1.L * b1 + temp2.L;
        err_temp2 = ((A + j)->L) * x * b1 - ((C + j + 1)->L) * b2;
        err_temp3 = err_temp1 - temp3.L + temp4.L + temp5.L + err_temp2;
        err_temp = temp1.H * errb1 - ((C + j + 1)->H) * errb2 + err_temp3;
        //--------------the running error bound-----------//
        abst = fabs(err_temp);
        errz = errz1 * fabs(temp1.H) + errz2 * fabs((C + j + 1)->H) + (ne + 1) * fabs(err_temp3);
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
    free(A);
    free(C);

    return res;
}

/*CompGegenDerwErr evaluates  the first derivative of a series of Gegenbauer polynomial  at the point x, which is in [-1, 1], with compensated method, and performs a running-error bound */
double CompGegenDerwErr(double *P, unsigned int n, double x, double lambda, double *runerrbound)
{
    assert(0 < n);
    assert(fabs(x) <= 1.0);
    assert(lambda > -0.5);
    assert(lambda != 0);
    double b1 = 0, b2 = 0;
    double res, err_temp, errb1 = 0, errb2 = 0;
    dd_real temp1, temp2, temp3, temp4;
    dd_real A1, A2;
    dd_real s0, s1, s2;
    int i;
    double j;
    double abst = 0, absb1 = 0;
    int na, nb, ne;
    na = 3;
    nb = 2;
    ne = 6;
    double errz, errz1, errz2, errztemp, err_P;
    errz1 = 0;
    errz2 = 0;
    errztemp = 0;
    double C;
    C = 2 * lambda;

    for (i = n - 1; i >= 0; i--)
    {
        j = 1.0 * i;
        //-------------recurrence coefficients-----------------------//
        s0 = two_sum(2 * j + 2, 2 * lambda);
        s1 = prod_dd_d(s0, x);
        A1 = div_dd_d(s1, j + 1);
        s2 = two_sum(C, j + 2);
        A2 = div_dd_d(s2, -j - 2);
        //--------------iteration-----------------------------------//
        temp1 = two_prod(A1.H, b1);
        temp2 = two_prod(A2.H, b2);
        temp3 = two_sum(temp1.H, temp2.H);
        temp4 = two_sum(temp3.H, P[i + 1]);
        //--------------the compensated part------------------------//
        err_P = A1.L * b1 + A2.L * b2 + temp1.L + temp2.L + temp3.L + temp4.L;
        err_temp = A1.H * errb1 + A2.H * errb2 + err_P;
        //--------------the runninge error bound-------------------//
        abst = fabs(err_temp);
        errz = errz1 * fabs(A1.H) + errz2 * fabs(A2.H) + (ne + 1) * fabs(err_P);
        errz2 = errztemp + (nb + 2) * absb1;
        errz1 = errz + (na + 3) * abst;

        b2 = b1;
        b1 = temp4.H;
        errb2 = errb1;
        errb1 = err_temp;
        errztemp = errz;
        absb1 = abst;
    }
    temp1 = two_prod(C, b1);
    res = temp1.H + C * errb1 + temp1.L;
    *(runerrbound) = ((errz1 - (1 + na) * abst) * unit * C + fabs(C * errb1 + temp1.L - (res - temp1.H))) / (1 - unit * 2);

    return res;
}

/*CompGegenDerKwErr evaluates  the k-th derivative of a series of Gegenbauer polynomial  at the point x, which is in [-1, 1], with compensated method, and performs a running-error bound */
double CompGegenDerKwErr(double *P, unsigned int n, double x, double lambda, double *runerrbound, unsigned int k)
{
    assert(0 < n);
    assert(fabs(x) <= 1.0);
    assert(lambda > -0.5);
    assert(lambda != 0);
    double res, b1 = 0, b2 = 0, err_temp, errb1 = 0, errb2 = 0;
    dd_real temp1, temp2, temp3, temp4;
    dd_real C, Ctemp, A1, A2;
    dd_real s0, s1, s2;
    int i;
    double j;
    C.H = 1.0;
    C.L = 0;
    for (i = 1; i <= k; i++)
    {
        Ctemp = two_sum(lambda, i - 1);
        C = prod_dd_dd(C, Ctemp);
        C.H = 2 * C.H;
        C.L = 2 * C.L;
    }

    double abst = 0, absb1 = 0;
    int na, nb, ne;
    na = 3;
    nb = 2;
    ne = 6;
    double errz, errz1, errz2, errztemp, err_P;
    errz1 = 0;
    errz2 = 0;
    errztemp = 0;

    for (i = n - k; i >= 0; i--)
    {
        j = 1.0 * i;
        //-------------recurrence coefficients-----------------------//
        s0 = two_sum(2 * j + 2 * k, 2 * lambda);
        s1 = prod_dd_d(s0, x);
        A1 = div_dd_d(s1, j + 1);
        s2 = two_sum(2 * lambda, j + 2 * k);
        A2 = div_dd_d(s2, -j - 2);
        //--------------iteration-----------------------------------//
        temp1 = two_prod(A1.H, b1);
        temp2 = two_prod(A2.H, b2);
        temp3 = two_sum(temp1.H, temp2.H);
        temp4 = two_sum(temp3.H, P[i + k]);
        //--------------the compensated part------------------------//
        err_P = A1.L * b1 + A2.L * b2 + temp1.L + temp2.L + temp3.L + temp4.L;
        err_temp = A1.H * errb1 + A2.H * errb2 + err_P;
        //--------------the runninge error bound-------------------//
        abst = fabs(err_temp);
        errz = errz1 * fabs(A1.H) + errz2 * fabs(A2.H) + (ne + 1) * fabs(err_P);
        errz2 = errztemp + (nb + 2) * absb1;
        errz1 = errz + (na + 3) * abst;

        b2 = b1;
        b1 = temp4.H;

        errb2 = errb1;
        errb1 = err_temp;
        errztemp = errz;
        absb1 = abst;
    }
    temp1 = two_prod(C.H, b1);
    temp1.L = C.H * errb1 + temp1.L + C.L * b1;
    res = temp1.H + temp1.L;
    *(runerrbound) = ((errz1 - (2 + na) * abst) * unit * C.H + fabs(temp1.L - (res - temp1.H))) / (1 - unit * 3);

    return res;
}

/*AccGegenValwErr evaluates a series of Gegenbauer polynomial  at the point x, which is in [-1, 1], with compensated method, and performs a running-error bound */
dd_real AccGegenValwErr(double *P, unsigned int n, double x, double lambda, double *runerrbound)
{
    assert(0 < n);
    assert(fabs(x) <= 1.0);
    assert(lambda > -0.5);
    assert(lambda != 0);
    double b1 = 0, b2 = 0;
    double err_temp, err_temp1, err_temp2, err_temp3, errb1 = 0, errb2 = 0;
    dd_real res, temp1, temp2, temp3, temp4, temp5;
    int j;
    double abst = 0, absb1 = 0;
    int na, nb, ne;
    na = 3;
    nb = 2;
    ne = 8;
    double errz, errz1, errz2, errztemp;
    errz1 = 0;
    errz2 = 0;
    errztemp = 0;

    dd_real *A = (dd_real *)malloc(sizeof(dd_real) * (n + 2));
    dd_real *C = (dd_real *)malloc(sizeof(dd_real) * (n + 2));

    RecurCoefGegen_DD(n, lambda, A, C);

    for (j = n; j >= 0; j--)
    {
        temp1 = two_prod((A + j)->H, x);
        temp2 = two_prod(temp1.H, b1);
        temp3 = two_prod((C + j + 1)->H, b2);
        temp4 = two_sum(temp2.H, -temp3.H);
        temp5 = two_sum(temp4.H, P[j]);
        //--------------the compensated part---------------//
        err_temp1 = temp1.L * b1 + temp2.L;
        err_temp2 = ((A + j)->L) * x * b1 - ((C + j + 1)->L) * b2;
        err_temp3 = err_temp1 - temp3.L + temp4.L + temp5.L + err_temp2;
        err_temp = temp1.H * errb1 - ((C + j + 1)->H) * errb2 + err_temp3;
        //--------------the running error bound-----------//
        abst = fabs(err_temp);
        errz = errz1 * fabs(temp1.H) + errz2 * fabs((C + j + 1)->H) + (ne + 1) * fabs(err_temp3);
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
    free(A);
    free(C);

    return res;
}

/*AccGegenDerwErr evaluates  the first derivative of a series of Gegenbauer polynomial  at the point x, which is in [-1, 1], with compensated method, and performs a running-error bound */
dd_real AccGegenDerwErr(double *P, unsigned int n, double x, double lambda, double *runerrbound)
{
    assert(0 < n);
    assert(fabs(x) <= 1.0);
    assert(lambda > -0.5);
    assert(lambda != 0);
    double b1 = 0, b2 = 0;
    double err_temp, errb1 = 0, errb2 = 0;
    dd_real res, temp1, temp2, temp3, temp4;
    dd_real A1, A2;
    dd_real s0, s1, s2;
    int i;
    double j;
    double abst = 0, absb1 = 0;
    int na, nb, ne;
    na = 3;
    nb = 2;
    ne = 6;
    double errz, errz1, errz2, errztemp, err_P;
    errz1 = 0;
    errz2 = 0;
    errztemp = 0;
    double C;
    C = 2 * lambda;

    for (i = n - 1; i >= 0; i--)
    {
        j = 1.0 * i;
        //-------------recurrence coefficients-----------------------//
        s0 = two_sum(2 * j + 2, 2 * lambda);
        s1 = prod_dd_d(s0, x);
        A1 = div_dd_d(s1, j + 1);
        s2 = two_sum(C, j + 2);
        A2 = div_dd_d(s2, -j - 2);
        //--------------iteration-----------------------------------//
        temp1 = two_prod(A1.H, b1);
        temp2 = two_prod(A2.H, b2);
        temp3 = two_sum(temp1.H, temp2.H);
        temp4 = two_sum(temp3.H, P[i + 1]);
        //--------------the compensated part------------------------//
        err_P = A1.L * b1 + A2.L * b2 + temp1.L + temp2.L + temp3.L + temp4.L;
        err_temp = A1.H * errb1 + A2.H * errb2 + err_P;
        //--------------the runninge error bound-------------------//
        abst = fabs(err_temp);
        errz = errz1 * fabs(A1.H) + errz2 * fabs(A2.H) + (ne + 1) * fabs(err_P);
        errz2 = errztemp + (nb + 2) * absb1;
        errz1 = errz + (na + 3) * abst;

        b2 = b1;
        b1 = temp4.H;

        errb2 = errb1;
        errb1 = err_temp;
        errztemp = errz;
        absb1 = abst;
    }
    temp1 = two_prod(C, b1);
    res = quick_two_sum(temp1.H, C * errb1 + temp1.L);
    *(runerrbound) = (errz1 - (1 + na) * abst) * unit * C;

    return res;
}

/*AccGegenDerKwErr evaluates  the k-th derivative of a series of Gegenbauer polynomial  at the point x, which is in [-1, 1], with compensated method, and performs a running-error bound */
dd_real AccGegenDerKwErr(double *P, unsigned int n, double x, double lambda, double *runerrbound, unsigned int k)
{
    assert(0 < n);
    assert(fabs(x) <= 1.0);
    assert(lambda > -0.5);
    assert(lambda != 0);
    double b1 = 0, b2 = 0, err_temp, errb1 = 0, errb2 = 0;
    dd_real res, temp1, temp2, temp3, temp4;
    dd_real C, Ctemp, A1, A2;
    dd_real s0, s1, s2;
    int i;
    double j;
    C.H = 1.0;
    C.L = 0;
    for (i = 1; i <= k; i++)
    {
        Ctemp = two_sum(lambda, i - 1);
        C = prod_dd_dd(C, Ctemp);
        C.H = 2 * C.H;
        C.L = 2 * C.L;
    }
    double abst = 0, absb1 = 0;
    int na, nb, ne;
    na = 3;
    nb = 2;
    ne = 6;
    double errz, errz1, errz2, errztemp, err_P;
    errz1 = 0;
    errz2 = 0;
    errztemp = 0;

    for (i = n - k; i >= 0; i--)
    {
        j = 1.0 * i;
        //-------------recurrence coefficients-----------------------//
        s0 = two_sum(2 * j + 2 * k, 2 * lambda);
        s1 = prod_dd_d(s0, x);
        A1 = div_dd_d(s1, j + 1);
        s2 = two_sum(2 * lambda, j + 2 * k);
        A2 = div_dd_d(s2, -j - 2);
        //--------------iteration-----------------------------------//
        temp1 = two_prod(A1.H, b1);
        temp2 = two_prod(A2.H, b2);
        temp3 = two_sum(temp1.H, temp2.H);
        temp4 = two_sum(temp3.H, P[i + k]);
        //--------------the compensated part------------------------//
        err_P = A1.L * b1 + A2.L * b2 + temp1.L + temp2.L + temp3.L + temp4.L;
        err_temp = A1.H * errb1 + A2.H * errb2 + err_P;
        //--------------the runninge error bound-------------------//
        abst = fabs(err_temp);
        errz = errz1 * fabs(A1.H) + errz2 * fabs(A2.H) + (ne + 1) * fabs(err_P);
        errz2 = errztemp + (nb + 2) * absb1;
        errz1 = errz + (na + 3) * abst;

        b2 = b1;
        b1 = temp4.H;

        errb2 = errb1;
        errb1 = err_temp;
        errztemp = errz;
        absb1 = abst;
    }
    temp1 = two_prod(C.H, b1);
    temp1.L = C.H * errb1 + temp1.L + C.L * b1;
    res = quick_two_sum(temp1.H, temp1.L);
    *(runerrbound) = (errz1 - (2 + na) * abst) * unit * C.H;

    return res;
}
