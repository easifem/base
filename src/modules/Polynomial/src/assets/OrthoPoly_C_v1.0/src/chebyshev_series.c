/******************************************************************************
%  Set of subroutines and functions to evaluate series of CHEBYSHEV polynomials at the point x, which is in [-1, 1].
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
%    4 November 2016
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
#include "chebyshev_series.h"

/* Cheb1Val evaluates a series of Chebyshev polynomial of the first kind at the point x, which is in [-1, 1].*/
double Cheb1Val(double *P, unsigned int n, double x)
{
    assert(0 < n);
    assert(fabs(x) <= 1.0);
    double xx, t, b1 = 0, b2 = 0;
    int i;
    xx = 2 * x;

    for (i = n; i >= 1; i--)
    {
        t = xx * b1 - b2 + P[i];
        b2 = b1;
        b1 = t;
    }
    return x * b1 - b2 + P[0];
}

/*CompCheb1Val evaluates a series of Chebyshev polynomial of the first kind at the point x, which is in [-1, 1], with compensated method.*/
double CompCheb1Val(double *P, unsigned int n, double x)
{
    assert(0 < n);
    assert(fabs(x) <= 1.0);
    double xx, b1 = 0, b2 = 0;
    double err_temp, errb1 = 0, errb2 = 0, res;
    dd_real temp1, temp2, temp3;
    int i;
    xx = 2 * x;

    for (i = n; i >= 1; i--)
    {
        temp1 = two_prod(xx, b1);
        temp2 = two_sum(temp1.H, -b2);
        temp3 = two_sum(temp2.H, P[i]);

        b2 = b1;
        b1 = temp3.H;
        //--------------the compensated part---------------//
        err_temp = xx * errb1 - errb2 + (temp1.L + temp2.L + temp3.L);
        errb2 = errb1;
        errb1 = err_temp;
    }
    temp1 = two_prod(x, b1);
    temp2 = two_sum(temp1.H, -b2);
    temp3 = two_sum(temp2.H, P[0]);
    //--------------the compensated part---------------//
    err_temp = x * errb1 - errb2 + (temp1.L + temp2.L + temp3.L);
    res = temp3.H + err_temp;

    return res;
}

/*AccCheb1Val evaluates a series of Chebyshev polynomial of the first kind at the point x, which is in [-1, 1], with compensated method.*/
dd_real AccCheb1Val(double *P, unsigned int n, double x)
{
    assert(0 < n);
    assert(fabs(x) <= 1.0);
    double xx, b1 = 0, b2 = 0;
    double err_temp, errb1 = 0, errb2 = 0;
    dd_real res, temp1, temp2, temp3;
    int i;
    xx = 2 * x;

    for (i = n; i >= 1; i--)
    {
        temp1 = two_prod(xx, b1);
        temp2 = two_sum(temp1.H, -b2);
        temp3 = two_sum(temp2.H, P[i]);

        b2 = b1;
        b1 = temp3.H;
        //--------------the compensated part---------------//
        err_temp = xx * errb1 - errb2 + (temp1.L + temp2.L + temp3.L);
        errb2 = errb1;
        errb1 = err_temp;
    }
    temp1 = two_prod(x, b1);
    temp2 = two_sum(temp1.H, -b2);
    temp3 = two_sum(temp2.H, P[0]);
    //--------------the compensated part---------------//
    err_temp = x * errb1 - errb2 + (temp1.L + temp2.L + temp3.L);
    res = quick_two_sum(temp3.H, err_temp);

    return res;
}

/*Cheb2Val evaluates a series of Chebyshev polynomial of the second kind at the point x, which is in [-1, 1]. */
double Cheb2Val(double *P, unsigned int n, double x)
{
    assert(0 < n);
    assert(fabs(x) <= 1.0);
    double xx, t, b1 = 0, b2 = 0;
    int i;
    xx = 2 * x;

    for (i = n; i >= 1; i--)
    {
        t = xx * b1 - b2 + P[i];
        b2 = b1;
        b1 = t;
    }
    return 2 * x * b1 - b2 + P[0];
}

/*CompCheb2Val evaluates a series of Chebyshev polynomial of the second kind at the point x, which is in [-1, 1], with compensated method.*/
double CompCheb2Val(double *P, unsigned int n, double x)
{
    assert(0 < n);
    assert(fabs(x) <= 1.0);
    double xx, b1 = 0, b2 = 0;
    double err_temp, errb1 = 0, errb2 = 0, res;
    dd_real temp1, temp2, temp3;
    int i;
    xx = 2 * x;

    for (i = n; i >= 1; i--)
    {
        temp1 = two_prod(xx, b1);
        temp2 = two_sum(temp1.H, -b2);
        temp3 = two_sum(temp2.H, P[i]);

        b2 = b1;
        b1 = temp3.H;
        //--------------the compensated part---------------//
        err_temp = xx * errb1 - errb2 + (temp1.L + temp2.L + temp3.L);
        errb2 = errb1;
        errb1 = err_temp;
    }
    temp1 = two_prod(xx, b1);
    temp2 = two_sum(temp1.H, -b2);
    temp3 = two_sum(temp2.H, P[0]);
    //--------------the compensated part---------------//
    err_temp = xx * errb1 - errb2 + (temp1.L + temp2.L + temp3.L);
    res = temp3.H + err_temp;

    return res;
}

/*AccCheb2Val evaluates a series of Chebyshev polynomial of the second kind at the point x, which is in [-1, 1], with compensated method.*/
dd_real AccCheb2Val(double *P, unsigned int n, double x)
{
    assert(0 < n);
    assert(fabs(x) <= 1.0);
    double xx, b1 = 0, b2 = 0;
    double err_temp, errb1 = 0, errb2 = 0;
    dd_real res, temp1, temp2, temp3;
    int i;
    xx = 2 * x;

    for (i = n; i >= 1; i--)
    {
        temp1 = two_prod(xx, b1);
        temp2 = two_sum(temp1.H, -b2);
        temp3 = two_sum(temp2.H, P[i]);

        b2 = b1;
        b1 = temp3.H;
        //--------------the compensated part---------------//
        err_temp = xx * errb1 - errb2 + (temp1.L + temp2.L + temp3.L);
        errb2 = errb1;
        errb1 = err_temp;
    }
    temp1 = two_prod(xx, b1);
    temp2 = two_sum(temp1.H, -b2);
    temp3 = two_sum(temp2.H, P[0]);
    //--------------the compensated part---------------//
    err_temp = xx * errb1 - errb2 + (temp1.L + temp2.L + temp3.L);
    res = quick_two_sum(temp3.H, err_temp);

    return res;
}

/*Cheb1Der evaluates  the first derivative of a series of Chebyshev polynomial of the first kind at the point x, which is in [-1, 1]. */
double Cheb1Der(double *P, unsigned int n, double x)
{
    assert(0 < n);
    assert(fabs(x) <= 1.0);
    double xx, t, b1 = 0, b2 = 0;
    int i;
    xx = 2 * x;

    for (i = n - 1; i >= 0; i--)
    {
        t = xx * b1 - b2 + (i + 1) * P[i + 1];
        b2 = b1;
        b1 = t;
    }
    return b1;
}

/*CompCheb1Der evaluates the first derivative of a series of Chebyshev polynomial of the first kind at the point x, which is in [-1, 1], with compensated method.*/
double CompCheb1Der(double *P, unsigned int n, double x)
{
    assert(0 < n);
    assert(fabs(x) <= 1.0);
    double xx, b1 = 0, b2 = 0;
    double err_temp, errb1 = 0, errb2 = 0, res;
    dd_real temp1, temp2, temp3, temp4;
    int i;
    xx = 2 * x;

    for (i = n - 1; i >= 0; i--)
    {
        temp1 = two_prod(xx, b1);
        temp2 = two_sum(temp1.H, -b2);
        temp3 = two_prod(i + 1, P[i + 1]);
        temp4 = two_sum(temp2.H, temp3.H);

        b2 = b1;
        b1 = temp4.H;
        //--------------the compensated part---------------//
        err_temp = xx * errb1 - errb2 + (temp1.L + temp2.L + temp3.L + temp4.L);
        errb2 = errb1;
        errb1 = err_temp;
    }
    res = b1 + errb1;

    return res;
}

/*AccCheb1Der evaluates the first derivative of a series of Chebyshev polynomial of the first kind at the point x, which is in [-1, 1], with compensated method.*/
dd_real AccCheb1Der(double *P, unsigned int n, double x)
{
    assert(0 < n);
    assert(fabs(x) <= 1.0);
    double xx, b1 = 0, b2 = 0;
    double err_temp, errb1 = 0, errb2 = 0;
    dd_real res, temp1, temp2, temp3, temp4;
    int i;
    xx = 2 * x;

    for (i = n - 1; i >= 0; i--)
    {
        temp1 = two_prod(xx, b1);
        temp2 = two_sum(temp1.H, -b2);
        temp3 = two_prod(i + 1, P[i + 1]);
        temp4 = two_sum(temp2.H, temp3.H);

        b2 = b1;
        b1 = temp4.H;
        //--------------the compensated part---------------//
        err_temp = xx * errb1 - errb2 + (temp1.L + temp2.L + temp3.L + temp4.L);
        errb2 = errb1;
        errb1 = err_temp;
    }
    res = quick_two_sum(b1, errb1);

    return res;
}

/*Cheb2Der evaluates  the first  derivative of a series of Chebyshev polynomial of the second kind at the point x, which is in [-1, 1]. */
double Cheb2Der(double *P, unsigned int n, double x)
{
    assert(0 < n);
    assert(fabs(x) <= 1.0);
    double t, b1 = 0, b2 = 0;
    double A1, A2;
    int i;
    double C;
    C = 2;

    for (i = n - 1; i >= 0; i--)
    {
        A1 = (2 * i + 4) * x / (i + 1);
        A2 = -1.0 * (i + 4) / (i + 2);
        t = A1 * b1 + A2 * b2 + P[i + 1];
        b2 = b1;
        b1 = t;
    }
    return C * b1;
}

/*CompCheb2Der evaluates the first derivative of a series of Chebyshev polynomial of the second kind at the point x, which is in [-1, 1], with compensated method.*/
double CompCheb2Der(double *P, unsigned int n, double x)
{
    assert(0 < n);
    assert(fabs(x) <= 1.0);
    double b1 = 0, b2 = 0;
    double err_temp, errb1 = 0, errb2 = 0, res;
    dd_real temp1, temp2, temp3, temp4;
    dd_real Temp, AA1, AA2;
    int i;
    double C;
    C = 2;

    for (i = n - 1; i >= 0; i--)
    {
        Temp = div_d_d(2 * i + 4, i + 1);
        AA1 = two_prod(Temp.H, x);
        AA1.L = x * Temp.L + AA1.L;

        AA2 = div_d_d(-(i + 4), i + 2);

        temp1 = two_prod(AA1.H, b1);
        temp2 = two_prod(AA2.H, b2);
        temp3 = two_sum(temp1.H, temp2.H);
        temp4 = two_sum(temp3.H, P[i + 1]);
        //--------------the compensated part---------------//
        err_temp = AA1.H * errb1 + AA2.H * errb2 + AA1.L * b1 + AA2.L * b2 + (temp1.L + temp2.L + temp3.L + temp4.L);
        //------------- the next step---------------------//
        b2 = b1;
        b1 = temp4.H;
        errb2 = errb1;
        errb1 = err_temp;
    }
    res = C * (b1 + errb1);

    return res;
}

/*AccCheb2Der evaluates the first derivative of a series of Chebyshev polynomial of the second kind at the point x, which is in [-1, 1], with compensated method.*/
dd_real AccCheb2Der(double *P, unsigned int n, double x)
{
    assert(0 < n);
    assert(fabs(x) <= 1.0);
    double b1 = 0, b2 = 0;
    double err_temp, errb1 = 0, errb2 = 0;
    dd_real res, temp1, temp2, temp3, temp4;
    dd_real Temp, AA1, AA2;
    int i;
    double C;
    C = 2;

    for (i = n - 1; i >= 0; i--)
    {
        Temp = div_d_d(2 * i + 4, i + 1);
        AA1 = two_prod(Temp.H, x);
        AA1.L = x * Temp.L + AA1.L;

        AA2 = div_d_d(-(i + 4), i + 2);

        temp1 = two_prod(AA1.H, b1);
        temp2 = two_prod(AA2.H, b2);
        temp3 = two_sum(temp1.H, temp2.H);
        temp4 = two_sum(temp3.H, P[i + 1]);
        //--------------the compensated part---------------//
        err_temp = AA1.H * errb1 + AA2.H * errb2 + AA1.L * b1 + AA2.L * b2 + (temp1.L + temp2.L + temp3.L + temp4.L);
        //------------- the next step---------------------//
        b2 = b1;
        b1 = temp4.H;
        errb2 = errb1;
        errb1 = err_temp;
    }
    res = quick_two_sum(C * b1, C * errb1);

    return res;
}

/*Cheb1DerK evaluates  the k-th derivative of a series of Chebyshev polynomial of the first kind at the point x, which is in [-1, 1]. */
double Cheb1DerK(double *P, unsigned int n, double x, unsigned int k)
{
    assert(0 < n);
    assert(fabs(x) <= 1.0);
    double s = 1.0, t, b1 = 0, b2 = 0;
    int i;
    double j;
    if (k == 0)
    {
        j = Cheb1Val(P, n, x);
        return j;
    }

    for (i = k - 1; i > 0; i--)
    {
        s = 2 * s * i;
    }

    for (i = n - k; i >= 0; i--)
    {
        j = 1.0 * i;
        t = 2 * (j + k) / (j + 1) * x * b1 - (j + 2 * k) / (j + 2) * b2 + (j + k) * P[i + k];
        b2 = b1;
        b1 = t;
    }
    return s * b1;
}

/*CompCheb1DerK evaluates the k-th derivative of a series of Chebyshev polynomial of the first kind at the point x, which is in [-1, 1], with compensated method.*/
double CompCheb1DerK(double *P, unsigned int n, double x, unsigned int k)
{
    assert(0 < n);
    assert(fabs(x) <= 1.0);
    double s = 1.0, b1 = 0, b2 = 0;
    double err_temp, errb1 = 0, errb2 = 0, res;
    dd_real temp1, temp2, temp3, temp4, temp5;
    dd_real Temp, AA1, AA2;
    int i;
    if (k == 0)
    {
        res = CompCheb1Val(P, n, x);
        return res;
    }

    for (i = k - 1; i > 0; i--)
    {
        s = 2 * s * i;
    }

    for (i = n - k; i >= 0; i--)
    {
        Temp = div_d_d(2 * i + 2 * k, i + 1);
        AA1 = two_prod(Temp.H, x);
        AA1.L = x * Temp.L + AA1.L;

        AA2 = div_d_d(i + 2 * k, -i - 2);

        temp1 = two_prod(AA1.H, b1);
        temp2 = two_prod(AA2.H, b2);
        temp3 = two_sum(temp1.H, temp2.H);
        temp4 = two_prod(i + k, P[i + k]);
        temp5 = two_sum(temp3.H, temp4.H);

        Temp.H = AA1.L * b1 + AA2.L * b2;

        b2 = b1;
        b1 = temp5.H;
        //--------------the compensated part---------------//
        err_temp = AA1.H * errb1 + AA2.H * errb2 + (temp1.L + temp2.L + temp3.L + temp4.L + temp5.L + Temp.H);
        errb2 = errb1;
        errb1 = err_temp;
    }
    Temp = two_prod(b1, s);
    Temp.L = s * errb1 + Temp.L;
    res = Temp.H + Temp.L;

    return res;
}

/*AccCheb1DerK evaluates the k-th derivative of a series of Chebyshev polynomial of the first kind at the point x, which is in [-1, 1], with compensated method.*/
dd_real AccCheb1DerK(double *P, unsigned int n, double x, unsigned int k)
{
    assert(0 < n);
    assert(fabs(x) <= 1.0);
    double s = 1.0, b1 = 0, b2 = 0;
    double err_temp, errb1 = 0, errb2 = 0;
    dd_real res, temp1, temp2, temp3, temp4, temp5;
    dd_real Temp, AA1, AA2;
    int i;
    if (k == 0)
    {
        res = AccCheb1Val(P, n, x);
        return res;
    }

    for (i = k - 1; i > 0; i--)
    {
        s = 2 * s * i;
    }

    for (i = n - k; i >= 0; i--)
    {
        Temp = div_d_d(2 * i + 2 * k, i + 1);
        AA1 = two_prod(Temp.H, x);
        AA1.L = x * Temp.L + AA1.L;

        AA2 = div_d_d(i + 2 * k, -i - 2);

        temp1 = two_prod(AA1.H, b1);
        temp2 = two_prod(AA2.H, b2);
        temp3 = two_sum(temp1.H, temp2.H);
        temp4 = two_prod(i + k, P[i + k]);
        temp5 = two_sum(temp3.H, temp4.H);

        Temp.H = AA1.L * b1 + AA2.L * b2;

        b2 = b1;
        b1 = temp5.H;
        //--------------the compensated part---------------//
        err_temp = AA1.H * errb1 + AA2.H * errb2 + (temp1.L + temp2.L + temp3.L + temp4.L + temp5.L + Temp.H);
        errb2 = errb1;
        errb1 = err_temp;
    }
    Temp = two_prod(b1, s);
    Temp.L = s * errb1 + Temp.L;
    res = quick_two_sum(Temp.H, Temp.L);

    return res;
}

/*Cheb2DerK evaluates  the k-th  derivative of a series of Chebyshev polynomial of the second kind at the point x, which is in [-1, 1]. */
double Cheb2DerK(double *P, unsigned int n, double x, unsigned int k)
{
    assert(0 < n);
    assert(fabs(x) <= 1.0);
    double t, b1 = 0, b2 = 0;
    int i;
    double s = 1.0, j;
    for (i = k; i > 0; i--)
    {
        s = 2 * s * i;
    }

    for (i = n - k; i >= 0; i--)
    {
        j = 1.0 * i;
        t = 2 * (j + k + 1) / (j + 1) * x * b1 - (j + 2 * k + 2) / (j + 2) * b2 + P[i + k];
        b2 = b1;
        b1 = t;
    }
    return s * b1;
}

/*CompCheb2DerK evaluates the k-th derivative of a series of Chebyshev polynomial of the second kind at the point x, which is in [-1, 1], with compensated method.*/
double CompCheb2DerK(double *P, unsigned int n, double x, unsigned int k)
{
    assert(0 < n);
    assert(fabs(x) <= 1.0);
    double b1 = 0, b2 = 0;
    double err_temp, errb1 = 0, errb2 = 0, res;
    dd_real temp1, temp2, temp3, temp4;
    dd_real Temp, AA1, AA2;
    int i;
    double s = 1.0;
    for (i = k; i > 0; i--)
    {
        s = 2 * s * i;
    }

    for (i = n - k; i >= 0; i--)
    {
        Temp = div_d_d(2 * i + 2 * k + 2, i + 1);
        AA1 = two_prod(Temp.H, x);
        AA1.L = x * Temp.L + AA1.L;

        AA2 = div_d_d(i + 2 * k + 2, -i - 2);

        temp1 = two_prod(AA1.H, b1);
        temp2 = two_prod(AA2.H, b2);
        temp3 = two_sum(temp1.H, temp2.H);
        temp4 = two_sum(temp3.H, P[i + k]);

        Temp.H = AA1.L * b1 + AA2.L * b2;

        b2 = b1;
        b1 = temp4.H;
        //--------------the compensated part---------------//
        err_temp = AA1.H * errb1 + AA2.H * errb2 + (temp1.L + temp2.L + temp3.L + temp4.L + Temp.H);
        //------------- the next step---------------------//
        errb2 = errb1;
        errb1 = err_temp;
    }
    Temp = two_prod(b1, s);
    Temp.L = s * errb1 + Temp.L;
    res = Temp.H + Temp.L;

    return res;
}

/*AccCheb2DerK evaluates the k-th derivative of a series of Chebyshev polynomial of the second kind at the point x, which is in [-1, 1], with compensated method.*/
dd_real AccCheb2DerK(double *P, unsigned int n, double x, unsigned int k)
{
    assert(0 < n);
    assert(fabs(x) <= 1.0);
    double b1 = 0, b2 = 0;
    double err_temp, errb1 = 0, errb2 = 0;
    dd_real res, temp1, temp2, temp3, temp4;
    dd_real Temp, AA1, AA2;
    int i;
    double s = 1.0;
    for (i = k; i > 0; i--)
    {
        s = 2 * s * i;
    }

    for (i = n - k; i >= 0; i--)
    {
        Temp = div_d_d(2 * i + 2 * k + 2, i + 1);
        AA1 = two_prod(Temp.H, x);
        AA1.L = x * Temp.L + AA1.L;

        AA2 = div_d_d(i + 2 * k + 2, -i - 2);

        temp1 = two_prod(AA1.H, b1);
        temp2 = two_prod(AA2.H, b2);
        temp3 = two_sum(temp1.H, temp2.H);
        temp4 = two_sum(temp3.H, P[i + k]);

        Temp.H = AA1.L * b1 + AA2.L * b2;

        b2 = b1;
        b1 = temp4.H;
        //--------------the compensated part---------------//
        err_temp = AA1.H * errb1 + AA2.H * errb2 + (temp1.L + temp2.L + temp3.L + temp4.L + Temp.H);
        //------------- the next step---------------------//
        errb2 = errb1;
        errb1 = err_temp;
    }
    Temp = two_prod(b1, s);
    Temp.L = s * errb1 + Temp.L;
    res = quick_two_sum(Temp.H, Temp.L);

    return res;
}

/*Cheb1ValwErr evaluates a series of Chebyshev polynomial of the first kind at the point x, which is in [-1, 1], together with running error bound. */
double Cheb1ValwErr(double *P, unsigned int n, double x, double *runerrbound)
{
    assert(0 < n);
    assert(fabs(x) <= 1.0);
    double xx, t, b1 = 0, b2 = 0;
    double abst, absb1 = 0;
    int i;
    xx = 2 * x;
    double absxx;
    absxx = fabs(xx);
    int na, nb;
    na = 0;
    nb = 0;

    double errz, errz1, errz2, errztemp;
    errz1 = 0;
    errz2 = 0;
    errztemp = 0;

    for (i = n; i >= 1; i--)
    {
        t = xx * b1 - b2 + P[i];
        abst = fabs(t);

        errz = errz1 * absxx + errz2 + fabs(P[i]);

        errz2 = errztemp + (nb + 2) * absb1;
        errz1 = errz + (na + 3) * abst;

        b2 = b1;
        b1 = t;

        errztemp = errz;
        absb1 = abst;
    }
    t = x * b1 - b2 + P[0];
    abst = fabs(t);

    errz = errz1 * fabs(x) + errz2 + fabs(P[0]);

    errz2 = errztemp + (nb + 2) * absb1;
    errz1 = errz + (na + 3) * abst;

    b2 = b1;
    b1 = t;

    *(runerrbound) = (errz1 - (2 + na) * abst) * unit;

    return b1;
}

/*Cheb2ValwErr evaluates   a series of Chebyshev polynomial of the second kind at the point x, which is in [-1, 1], together with running error bound. */
double Cheb2ValwErr(double *P, unsigned int n, double x, double *runerrbound)
{
    assert(0 < n);
    assert(fabs(x) <= 1.0);
    double xx, t, b1 = 0, b2 = 0;
    double abst = 0, absb1 = 0;
    int i;
    xx = 2 * x;
    double absxx;
    absxx = fabs(xx);
    int na, nb;
    na = 0;
    nb = 0;

    double errz, errz1, errz2, errztemp;
    errz1 = 0;
    errz2 = 0;
    errztemp = 0;

    for (i = n; i >= 0; i--)
    {
        t = xx * b1 - b2 + P[i];
        abst = fabs(t);

        errz = errz1 * absxx + errz2 + fabs(P[i]);

        errz2 = errztemp + (nb + 2) * absb1;
        errz1 = errz + (na + 3) * abst;

        b2 = b1;
        b1 = t;

        errztemp = errz;
        absb1 = abst;
    }
    *(runerrbound) = (errz1 - (2 + na) * abst) * unit;

    return b1;
}

/*Cheb1DerwErr evaluates the first derivative of a series of Chebyshev polynomial of the first kind at the point x, which is in [-1, 1], together with running error bound. */
double Cheb1DerwErr(double *P, unsigned int n, double x, double *runerrbound)
{
    assert(0 < n);
    assert(fabs(x) <= 1.0);

    double xx, t, b1 = 0, b2 = 0;
    double abst = 0, absb1 = 0;
    int i;
    xx = 2 * x;
    double absxx;
    absxx = fabs(xx);
    int na, nb, nc;
    na = 0;
    nb = 0;
    nc = 0;

    double errz, errz1, errz2, errztemp;
    errz1 = 0;
    errz2 = 0;
    errztemp = 0;

    for (i = n - 1; i >= 0; i--)
    {
        t = xx * b1 - b2 + (i + 1) * P[i + 1];
        abst = fabs(t);

        errz = errz1 * absxx + errz2 + (nc + 2) * (i + 1) * fabs(P[i + 1]);

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

/*Cheb2DerwErr evaluates the first derivative of  a series of Chebyshev polynomial of the second kind at the point x, which is in [-1, 1], together with running error bound. */
double Cheb2DerwErr(double *P, unsigned int n, double x, double *runerrbound)
{
    assert(0 < n);
    assert(fabs(x) <= 1.0);
    double t, b1 = 0, b2 = 0;
    double A1, A2, Atemp;
    double C;
    C = 2;

    double abst = 0, absb1 = 0;
    int i;
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
        A1 = (2 * i + 4) * x / (i + 1);
        Atemp = 1.0 * (i + 4) / (i + 2);
        A2 = -Atemp;
        t = A1 * b1 + A2 * b2 + P[i + 1];

        abst = fabs(t);

        errz = errz1 * fabs(A1) + errz2 * Atemp + (nc + 2) * fabs(P[i + 1]);

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

/*Cheb1DerKwErr evaluates the k-th derivative of a series of Chebyshev polynomial of the first kind at the point x, which is in [-1, 1], together with running error bound. */
double Cheb1DerKwErr(double *P, unsigned int n, double x, double *runerrbound, unsigned int k)
{
    assert(0 < n);
    assert(fabs(x) <= 1.0);
    double s = 1.0, t, b1 = 0, b2 = 0, A1, A2;
    int i;
    double j, Atemp;
    if (k == 0)
    {
        j = Cheb1ValwErr(P, n, x, runerrbound);
        return j;
    }

    for (i = k - 1; i > 0; i--)
    {
        s = 2 * s * i;
    }
    double abst = 0, absb1 = 0;
    int na, nb, nc;
    na = 2;
    nb = 1;
    nc = 0;
    if (k == 1)
    {
        na = 0;
        nb = 0;
    }

    double errz, errz1, errz2, errztemp;
    errz1 = 0;
    errz2 = 0;
    errztemp = 0;

    for (i = n - k; i >= 0; i--)
    {
        j = 1.0 * i;
        A1 = 2 * (j + k) / (j + 1) * x;
        Atemp = fabs(A1);
        A2 = (j + 2 * k) / (j + 2);

        t = A1 * b1 - A2 * b2 + (j + k) * P[i + k];
        abst = fabs(t);

        errz = errz1 * Atemp + errz2 * A2 + (nc + 2) * (j + k) * fabs(P[i + k]);

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

/*Cheb2DerKwErr evaluates the k-th derivative of a series of Chebyshev polynomial of the second kind at the point x, which is in [-1, 1], together with running error bound. */
double Cheb2DerKwErr(double *P, unsigned int n, double x, double *runerrbound, unsigned int k)
{
    assert(0 < n);
    assert(fabs(x) <= 1.0);
    double t, b1 = 0, b2 = 0, A1, A2, Atemp;
    int i;
    double s = 1.0, j;
    for (i = k; i > 0; i--)
    {
        s = 2 * s * i;
    }
    double abst = 0, absb1 = 0;
    int na, nb, nc;
    na = 2;
    nb = 1;
    nc = 0;
    if (k == 0)
    {
        na = 0;
        nb = 0;
    }
    double errz, errz1, errz2, errztemp;
    errz1 = 0;
    errz2 = 0;
    errztemp = 0;

    for (i = n - k; i >= 0; i--)
    {
        j = 1.0 * i;

        A1 = 2 * (j + k + 1) / (j + 1) * x;
        Atemp = (j + 2 * k + 2) / (j + 2);
        A2 = -Atemp;
        t = A1 * b1 + A2 * b2 + P[i + k];

        abst = fabs(t);

        errz = errz1 * fabs(A1) + errz2 * Atemp + (nc + 2) * fabs(P[i + k]);

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

/*CompCheb1ValwErr evaluates a series of Chebyshev polynomial of the first kind at the point x, which is in [-1, 1], with compensated method, together with the running error bound.*/
double CompCheb1ValwErr(double *P, unsigned int n, double x, double *runerrbound)
{
    assert(0 < n);
    assert(fabs(x) <= 1.0);
    double xx, b1 = 0, b2 = 0;
    double res, err_temp, errb1 = 0, errb2 = 0;
    dd_real temp1, temp2, temp3;
    int i;
    xx = 2 * x;

    double abst = 0, absb1 = 0;
    double absxx;
    absxx = fabs(xx);
    int na, nb, ne;
    na = 0;
    nb = 0;
    ne = 2;

    double errz, errz1, errz2, errztemp, err_P;
    errz1 = 0;
    errz2 = 0;
    errztemp = 0;

    for (i = n; i >= 1; i--)
    {
        temp1 = two_prod(xx, b1);
        temp2 = two_sum(temp1.H, -b2);
        temp3 = two_sum(temp2.H, P[i]);

        b2 = b1;
        b1 = temp3.H;
        //--------------the compensated part---------------//
        err_P = temp1.L + temp2.L + temp3.L;
        err_temp = xx * errb1 - errb2 + err_P;
        //--------------the running error bound------------//
        abst = fabs(err_temp);
        errz = errz1 * absxx + errz2 + (ne + 1) * fabs(err_P);

        errz2 = errztemp + (nb + 2) * absb1;
        errz1 = errz + (na + 3) * abst;

        errb2 = errb1;
        errb1 = err_temp;

        errztemp = errz;
        absb1 = abst;
    }
    temp1 = two_prod(x, b1);
    temp2 = two_sum(temp1.H, -b2);
    temp3 = two_sum(temp2.H, P[0]);
    //--------------the compensated part---------------//
    err_P = temp1.L + temp2.L + temp3.L;
    err_temp = x * errb1 - errb2 + err_P;
    //--------------the running error bound------------//
    abst = fabs(err_temp);
    errz = errz1 * fabs(x) + errz2 + (ne + 1) * fabs(err_P);

    errz1 = errz + (na + 3) * abst;

    res = temp3.H + err_temp;
    *(runerrbound) = ((errz1 - (2 + na) * abst) * unit + fabs(err_temp - (res - temp3.H))) / (1 - unit * 2);

    return res;
}

/*CompCheb2ValwErr evaluates a series of Chebyshev polynomial of the second kind at the point x, which is in [-1, 1], with compensated method, together with the running error bound.*/
double CompCheb2ValwErr(double *P, unsigned int n, double x, double *runerrbound)
{
    assert(0 < n);
    assert(fabs(x) <= 1.0);
    double xx, b1 = 0, b2 = 0;
    double res, err_temp, errb1 = 0, errb2 = 0;
    dd_real temp1, temp2, temp3;
    int i;
    xx = 2 * x;

    double abst = 0, absb1 = 0;
    double absxx;
    absxx = fabs(xx);
    int na, nb, ne;
    na = 0;
    nb = 0;
    ne = 2;

    double errz, errz1, errz2, errztemp, err_P;
    errz1 = 0;
    errz2 = 0;
    errztemp = 0;

    for (i = n; i >= 1; i--)
    {
        temp1 = two_prod(xx, b1);
        temp2 = two_sum(temp1.H, -b2);
        temp3 = two_sum(temp2.H, P[i]);

        b2 = b1;
        b1 = temp3.H;
        //--------------the compensated part---------------//
        err_P = temp1.L + temp2.L + temp3.L;
        err_temp = xx * errb1 - errb2 + err_P;
        //--------------the running error bound------------//
        abst = fabs(err_temp);
        errz = errz1 * absxx + errz2 + (ne + 3) * fabs(err_P);

        errz2 = errztemp + (nb + 2) * absb1;
        errz1 = errz + (na + 3) * abst;

        errb2 = errb1;
        errb1 = err_temp;

        errztemp = errz;
        absb1 = abst;
    }
    temp1 = two_prod(xx, b1);
    temp2 = two_sum(temp1.H, -b2);
    temp3 = two_sum(temp2.H, P[0]);
    //--------------the compensated part---------------//
    err_P = temp1.L + temp2.L + temp3.L;
    err_temp = xx * errb1 - errb2 + err_P;
    //--------------the running error bound------------//
    abst = fabs(err_temp);
    errz = errz1 * absxx + errz2 + (ne + 3) * fabs(err_P);

    errz1 = errz + (na + 3) * abst;

    res = temp3.H + err_temp;
    *(runerrbound) = ((errz1 - (2 + na) * abst) * unit + fabs(err_temp - (res - temp3.H))) / (1 - unit * 2);

    return res;
}

/*CompCheb1DerwErr evaluates the first derivative of a series of Chebyshev polynomial of the first kind at the point x, which is in [-1, 1], with compensated method, together with running error bound.*/

double CompCheb1DerwErr(double *P, unsigned int n, double x, double *runerrbound)
{
    assert(0 < n);
    assert(fabs(x) <= 1.0);
    double xx, b1 = 0, b2 = 0;
    double res, err_temp, errb1 = 0, errb2 = 0;
    dd_real temp1, temp2, temp3, temp4;
    int i;
    xx = 2 * x;

    double abst = 0, absb1 = 0;
    double absxx;
    absxx = fabs(xx);
    int na, nb, ne;
    na = 0;
    nb = 0;
    ne = 3;

    double errz, errz1, errz2, errztemp, err_P;
    errz1 = 0;
    errz2 = 0;
    errztemp = 0;

    for (i = n - 1; i >= 0; i--)
    {
        temp1 = two_prod(xx, b1);
        temp2 = two_prod(i + 1, P[i + 1]);
        temp3 = two_sum(temp1.H, temp2.H);
        temp4 = two_sum(temp3.H, -b2);

        b2 = b1;
        b1 = temp4.H;
        //--------------the compensated part---------------//
        err_P = temp1.L + temp2.L + temp3.L + temp4.L;
        err_temp = xx * errb1 - errb2 + err_P;
        //--------------the running error bound------------//
        abst = fabs(err_temp);
        errz = errz1 * absxx + errz2 + (ne + 1) * fabs(err_P);

        errz2 = errztemp + (nb + 2) * absb1;
        errz1 = errz + (na + 3) * abst;

        errb2 = errb1;
        errb1 = err_temp;

        errztemp = errz;
        absb1 = abst;
    }
    res = b1 + errb1;
    *(runerrbound) = ((errz1 - (1 + na) * abst) * unit + fabs(errb1 - (res - b1))) / (1 - unit * 2);

    return res;
}

/*CompCheb2DerwErr evaluates the first derivative of a series of Chebyshev polynomial of the second kind at the point x, which is in [-1, 1], with compensated method.,together with the running error bound*/
double CompCheb2DerwErr(double *P, unsigned int n, double x, double *runerrbound)
{
    assert(0 < n);
    assert(fabs(x) <= 1.0);
    double b1 = 0, b2 = 0;
    double res, err_temp, errb1 = 0, errb2 = 0;
    dd_real temp1, temp2, temp3, temp4;
    dd_real Temp, AA1, AA2;
    int i;
    double C;
    C = 2;

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
        Temp = div_d_d(2 * i + 4, i + 1);
        AA1 = two_prod(Temp.H, x);
        AA1.L = x * Temp.L + AA1.L;

        AA2 = div_d_d(-(i + 4), i + 2);

        temp1 = two_prod(AA1.H, b1);
        temp2 = two_prod(AA2.H, b2);
        temp3 = two_sum(temp1.H, temp2.H);
        temp4 = two_sum(temp3.H, P[i + 1]);
        //--------------the compensated part---------------//
        err_P = AA1.L * b1 + AA2.L * b2 + temp1.L + temp2.L + temp3.L + temp4.L;
        err_temp = AA1.H * errb1 + AA2.H * errb2 + err_P;
        //--------------the running error bound------------//
        abst = fabs(err_temp);
        errz = errz1 * fabs(AA1.H) + errz2 * fabs(AA2.H) + (ne + 1) * fabs(err_P);

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
    res = C * (b1 + errb1);
    *(runerrbound) = ((errz1 - (1 + na) * abst) * unit * C + fabs(C * errb1 - (res - C * b1))) / (1 - unit * 2);

    return res;
}

/*CompCheb1DerKwErr evaluates the k-th derivative of a series of Chebyshev polynomial of the first kind at the point x, which is in [-1, 1], with compensated method, together with running error bound.*/
double CompCheb1DerKwErr(double *P, unsigned int n, double x, double *runerrbound, unsigned int k)
{
    assert(0 < n);
    assert(fabs(x) <= 1.0);
    double s = 1.0, b1 = 0, b2 = 0;
    double res, err_temp, errb1 = 0, errb2 = 0;
    dd_real temp1, temp2, temp3, temp4, temp5;
    dd_real Temp, AA1, AA2;
    int i;
    if (k == 0)
    {
        res = CompCheb1ValwErr(P, n, x, runerrbound);
        return res;
    }
    for (i = k - 1; i > 0; i--)
    {
        s = 2 * s * i;
    }

    double abst = 0, absb1 = 0;
    int na, nb, ne;
    na = 2;
    nb = 1;
    ne = 7;
    if (k == 1)
    {
        na = 0;
        nb = 0;
        ne = 3;
    }

    double errz, errz1, errz2, errztemp, err_P;
    errz1 = 0;
    errz2 = 0;
    errztemp = 0;

    for (i = n - k; i >= 0; i--)
    {
        Temp = div_d_d(2 * i + 2 * k, i + 1);
        AA1 = two_prod(Temp.H, x);
        AA1.L = x * Temp.L + AA1.L;

        AA2 = div_d_d(i + 2 * k, -i - 2);

        temp1 = two_prod(AA1.H, b1);
        temp2 = two_prod(AA2.H, b2);
        temp3 = two_sum(temp1.H, temp2.H);
        temp4 = two_prod(i + k, P[i + k]);
        temp5 = two_sum(temp3.H, temp4.H);

        Temp.H = AA1.L * b1 + AA2.L * b2;
        //--------------the compensated part---------------//
        err_P = temp1.L + temp2.L + temp3.L + temp4.L + temp5.L + Temp.H;
        err_temp = AA1.H * errb1 + AA2.H * errb2 + err_P;
        //--------------the running error bound------------//
        abst = fabs(err_temp);
        errz = errz1 * fabs(AA1.H) + fabs(AA2.H) * errz2 + (ne + 1) * fabs(err_P);

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
    Temp = two_prod(b1, s);
    Temp.L = s * errb1 + Temp.L;
    res = Temp.H + Temp.L;
    *(runerrbound) = ((errz1 - (2 + na) * abst) * unit * s + fabs(Temp.L - (res - Temp.H))) / (1 - 3 * unit);

    return res;
}

/*CompCheb2DerKwErr evaluates the k-th derivative of a series of Chebyshev polynomial of the second kind at the point x, which is in [-1, 1], with compensated method.,together with the running error bound*/
double CompCheb2DerKwErr(double *P, unsigned int n, double x, double *runerrbound, unsigned int k)
{
    assert(0 < n);
    assert(fabs(x) <= 1.0);
    double b1 = 0, b2 = 0;
    double res, err_temp, errb1 = 0, errb2 = 0;
    dd_real temp1, temp2, temp3, temp4;
    dd_real Temp, AA1, AA2;
    int i;
    double s = 1.0;
    for (i = k; i > 0; i--)
    {
        s = 2 * s * i;
    }
    double abst = 0, absb1 = 0;
    int na, nb, ne;
    na = 2;
    nb = 1;
    ne = 6;
    if (k == 0)
    {
        na = 0;
        nb = 0;
        ne = 2;
    }

    double errz, errz1, errz2, errztemp, err_P;
    errz1 = 0;
    errz2 = 0;
    errztemp = 0;

    for (i = n - k; i >= 0; i--)
    {
        Temp = div_d_d(2 * i + 2 * k + 2, i + 1);
        AA1 = two_prod(Temp.H, x);
        AA1.L = x * Temp.L + AA1.L;

        AA2 = div_d_d(i + 2 * k + 2, -i - 2);

        temp1 = two_prod(AA1.H, b1);
        temp2 = two_prod(AA2.H, b2);
        temp3 = two_sum(temp1.H, temp2.H);
        temp4 = two_sum(temp3.H, P[i + k]);

        Temp.H = AA1.L * b1 + AA2.L * b2;
        //--------------the compensated part---------------//
        err_P = temp1.L + temp2.L + temp3.L + temp4.L + Temp.H;
        err_temp = AA1.H * errb1 + AA2.H * errb2 + err_P;
        //--------------the running error bound------------//
        abst = fabs(err_temp);
        errz = errz1 * fabs(AA1.H) + errz2 * fabs(AA2.H) + (ne + 1) * fabs(err_P);

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
    Temp = two_prod(b1, s);
    Temp.L = s * errb1 + Temp.L;
    res = Temp.H + Temp.L;
    *(runerrbound) = ((errz1 - (2 + na) * abst) * unit * s + fabs(Temp.L - (res - Temp.H))) / (1 - unit * 2);

    return res;
}

/*AccCheb1ValwErr evaluates a series of Chebyshev polynomial of the first kind at the point x, which is in [-1, 1], with compensated method, together with the running error bound.*/
dd_real AccCheb1ValwErr(double *P, unsigned int n, double x, double *runerrbound)
{
    assert(0 < n);
    assert(fabs(x) <= 1.0);
    double xx, b1 = 0, b2 = 0;
    double err_temp, errb1 = 0, errb2 = 0;
    dd_real res, temp1, temp2, temp3;
    int i;
    xx = 2 * x;
    double abst = 0, absb1 = 0;
    double absxx;
    absxx = fabs(xx);
    int na, nb, ne;
    na = 0;
    nb = 0;
    ne = 2;

    double errz, errz1, errz2, errztemp, err_P;
    errz1 = 0;
    errz2 = 0;
    errztemp = 0;

    for (i = n; i >= 1; i--)
    {
        temp1 = two_prod(xx, b1);
        temp2 = two_sum(temp1.H, -b2);
        temp3 = two_sum(temp2.H, P[i]);

        b2 = b1;
        b1 = temp3.H;
        //--------------the compensated part---------------//
        err_P = temp1.L + temp2.L + temp3.L;
        err_temp = xx * errb1 - errb2 + err_P;
        //--------------the running error bound------------//
        abst = fabs(err_temp);
        errz = errz1 * absxx + errz2 + (ne + 1) * fabs(err_P);

        errz2 = errztemp + (nb + 2) * absb1;
        errz1 = errz + (na + 3) * abst;

        errb2 = errb1;
        errb1 = err_temp;

        errztemp = errz;
        absb1 = abst;
    }
    temp1 = two_prod(x, b1);
    temp2 = two_sum(temp1.H, -b2);
    temp3 = two_sum(temp2.H, P[0]);
    //--------------the compensated part---------------//
    err_P = temp1.L + temp2.L + temp3.L;
    err_temp = x * errb1 - errb2 + err_P;
    //--------------the running error bound------------//
    abst = fabs(err_temp);
    errz = errz1 * fabs(x) + errz2 + (ne + 1) * fabs(err_P);
    errz1 = errz + (na + 3) * abst;

    res = quick_two_sum(temp3.H, err_temp);
    *(runerrbound) = (errz1 - (2 + na) * abst) * unit;

    return res;
}

/*AccCheb2ValwErr evaluates a series of Chebyshev polynomial of the second kind at the point x, which is in [-1, 1], with compensated method, together with the running error bound.*/
dd_real AccCheb2ValwErr(double *P, unsigned int n, double x, double *runerrbound)
{
    assert(0 < n);
    assert(fabs(x) <= 1.0);
    double xx, b1 = 0, b2 = 0;
    double err_temp, errb1 = 0, errb2 = 0;
    dd_real res, temp1, temp2, temp3;
    int i;
    xx = 2 * x;
    double abst = 0, absb1 = 0;
    double absxx;
    absxx = fabs(xx);
    int na, nb, ne;
    na = 0;
    nb = 0;
    ne = 2;

    double errz, errz1, errz2, errztemp, err_P;
    errz1 = 0;
    errz2 = 0;
    errztemp = 0;

    for (i = n; i >= 1; i--)
    {
        temp1 = two_prod(xx, b1);
        temp2 = two_sum(temp1.H, -b2);
        temp3 = two_sum(temp2.H, P[i]);

        b2 = b1;
        b1 = temp3.H;
        //--------------the compensated part---------------//
        err_P = temp1.L + temp2.L + temp3.L;
        err_temp = xx * errb1 - errb2 + err_P;
        //--------------the running error bound------------//
        abst = fabs(err_temp);
        errz = errz1 * absxx + errz2 + (ne + 3) * fabs(err_P);

        errz2 = errztemp + (nb + 2) * absb1;
        errz1 = errz + (na + 3) * abst;

        errb2 = errb1;
        errb1 = err_temp;

        errztemp = errz;
        absb1 = abst;
    }
    temp1 = two_prod(xx, b1);
    temp2 = two_sum(temp1.H, -b2);
    temp3 = two_sum(temp2.H, P[0]);
    //--------------the compensated part---------------//
    err_P = temp1.L + temp2.L + temp3.L;
    err_temp = xx * errb1 - errb2 + err_P;
    //--------------the running error bound------------//
    abst = fabs(err_temp);
    errz = errz1 * absxx + errz2 + (ne + 3) * fabs(err_P);
    errz1 = errz + (na + 3) * abst;

    res = quick_two_sum(temp3.H, err_temp);
    *(runerrbound) = (errz1 - (2 + na) * abst) * unit;

    return res;
}

/*AccCheb1DerwErr evaluates the first derivative of a series of Chebyshev polynomial of the first kind at the point x, which is in [-1, 1], with compensated method, together with running error bound.*/
dd_real AccCheb1DerwErr(double *P, unsigned int n, double x, double *runerrbound)
{
    assert(0 < n);
    assert(fabs(x) <= 1.0);
    double xx, b1 = 0, b2 = 0;
    double err_temp, errb1 = 0, errb2 = 0;
    dd_real res, temp1, temp2, temp3, temp4;
    int i;
    xx = 2 * x;
    double abst = 0, absb1 = 0;
    double absxx;
    absxx = fabs(xx);
    int na, nb, ne;
    na = 0;
    nb = 0;
    ne = 3;

    double errz, errz1, errz2, errztemp, err_P;
    errz1 = 0;
    errz2 = 0;
    errztemp = 0;

    for (i = n - 1; i >= 0; i--)
    {
        temp1 = two_prod(xx, b1);
        temp2 = two_prod(i + 1, P[i + 1]);
        temp3 = two_sum(temp1.H, temp2.H);
        temp4 = two_sum(temp3.H, -b2);

        b2 = b1;
        b1 = temp4.H;
        //--------------the compensated part---------------//
        err_P = temp1.L + temp2.L + temp3.L + temp4.L;
        err_temp = xx * errb1 - errb2 + err_P;
        //--------------the running error bound------------//
        abst = fabs(err_temp);
        errz = errz1 * absxx + errz2 + (ne + 1) * fabs(err_P);

        errz2 = errztemp + (nb + 2) * absb1;
        errz1 = errz + (na + 3) * abst;

        errb2 = errb1;
        errb1 = err_temp;

        errztemp = errz;
        absb1 = abst;
    }
    res = quick_two_sum(b1, errb1);
    *(runerrbound) = (errz1 - (1 + na) * abst) * unit;

    return res;
}

/*AccCheb2DerwErr evaluates the first derivative of a series of Chebyshev polynomial of the second kind at the point x, which is in [-1, 1], with compensated method.,together with the running error bound*/
dd_real AccCheb2DerwErr(double *P, unsigned int n, double x, double *runerrbound)
{
    assert(0 < n);
    assert(fabs(x) <= 1.0);
    double b1 = 0, b2 = 0;
    double err_temp, errb1 = 0, errb2 = 0;
    dd_real res, temp1, temp2, temp3, temp4;
    dd_real Temp, AA1, AA2;
    int i;
    double C;
    C = 2;

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
        Temp = div_d_d(2 * i + 4, i + 1);
        AA1 = two_prod(Temp.H, x);
        AA1.L = x * Temp.L + AA1.L;

        AA2 = div_d_d(-(i + 4), i + 2);

        temp1 = two_prod(AA1.H, b1);
        temp2 = two_prod(AA2.H, b2);
        temp3 = two_sum(temp1.H, temp2.H);
        temp4 = two_sum(temp3.H, P[i + 1]);
        //--------------the compensated part---------------//
        err_P = AA1.L * b1 + AA2.L * b2 + temp1.L + temp2.L + temp3.L + temp4.L;
        err_temp = AA1.H * errb1 + AA2.H * errb2 + err_P;
        //--------------the running error bound------------//
        abst = fabs(err_temp);
        errz = errz1 * fabs(AA1.H) + errz2 * fabs(AA2.H) + (ne + 1) * fabs(err_P);

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
    res = quick_two_sum(C * b1, C * errb1);
    *(runerrbound) = (errz1 - (1 + na) * abst) * unit * C;

    return res;
}

/*AccCheb1DerKwErr evaluates the k-th derivative of a series of Chebyshev polynomial of the first kind at the point x, which is in [-1, 1], with compensated method, together with running error bound.*/
dd_real AccCheb1DerKwErr(double *P, unsigned int n, double x, double *runerrbound, unsigned int k)
{
    assert(0 < n);
    assert(fabs(x) <= 1.0);
    double s = 1.0, b1 = 0, b2 = 0;
    double err_temp, errb1 = 0, errb2 = 0;
    dd_real res, temp1, temp2, temp3, temp4, temp5;
    dd_real Temp, AA1, AA2;
    int i;
    if (k == 0)
    {
        res = AccCheb1ValwErr(P, n, x, runerrbound);
        return res;
    }
    for (i = k - 1; i > 0; i--)
    {
        s = 2 * s * i;
    }

    double abst = 0, absb1 = 0;
    int na, nb, ne;
    na = 2;
    nb = 1;
    ne = 7;
    if (k == 1)
    {
        na = 0;
        nb = 0;
        ne = 3;
    }

    double errz, errz1, errz2, errztemp, err_P;
    errz1 = 0;
    errz2 = 0;
    errztemp = 0;

    for (i = n - k; i >= 0; i--)
    {
        Temp = div_d_d(2 * i + 2 * k, i + 1);
        AA1 = two_prod(Temp.H, x);
        AA1.L = x * Temp.L + AA1.L;

        AA2 = div_d_d(i + 2 * k, -i - 2);

        temp1 = two_prod(AA1.H, b1);
        temp2 = two_prod(AA2.H, b2);
        temp3 = two_sum(temp1.H, temp2.H);
        temp4 = two_prod(i + k, P[i + k]);
        temp5 = two_sum(temp3.H, temp4.H);

        Temp.H = AA1.L * b1 + AA2.L * b2;
        //--------------the compensated part---------------//
        err_P = temp1.L + temp2.L + temp3.L + temp4.L + temp5.L + Temp.H;
        err_temp = AA1.H * errb1 + AA2.H * errb2 + err_P;
        //--------------the running error bound------------//
        abst = fabs(err_temp);
        errz = errz1 * fabs(AA1.H) + fabs(AA2.H) * errz2 + (ne + 1) * fabs(err_P);

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
    Temp = two_prod(b1, s);
    Temp.L = s * errb1 + Temp.L;
    res = quick_two_sum(Temp.H, Temp.L);
    *(runerrbound) = (errz1 - (2 + na) * abst) * unit * s;

    return res;
}

/*AccCheb2DerKwErr evaluates the k-th derivative of a series of Chebyshev polynomial of the second kind at the point x, which is in [-1, 1], with compensated method.,together with the running error bound*/
dd_real AccCheb2DerKwErr(double *P, unsigned int n, double x, double *runerrbound, unsigned int k)
{
    assert(0 < n);
    assert(fabs(x) <= 1.0);
    double b1 = 0, b2 = 0;
    double err_temp, errb1 = 0, errb2 = 0;
    dd_real res, temp1, temp2, temp3, temp4;
    dd_real Temp, AA1, AA2;
    int i;
    double s = 1.0;
    for (i = k; i > 0; i--)
    {
        s = 2 * s * i;
    }

    double abst = 0, absb1 = 0;
    int na, nb, ne;
    na = 2;
    nb = 1;
    ne = 6;
    if (k == 0)
    {
        na = 0;
        nb = 0;
        ne = 2;
    }

    double errz, errz1, errz2, errztemp, err_P;
    errz1 = 0;
    errz2 = 0;
    errztemp = 0;

    for (i = n - k; i >= 0; i--)
    {
        Temp = div_d_d(2 * i + 2 * k + 2, i + 1);
        AA1 = two_prod(Temp.H, x);
        AA1.L = x * Temp.L + AA1.L;

        AA2 = div_d_d(i + 2 * k + 2, -i - 2);

        temp1 = two_prod(AA1.H, b1);
        temp2 = two_prod(AA2.H, b2);
        temp3 = two_sum(temp1.H, temp2.H);
        temp4 = two_sum(temp3.H, P[i + k]);

        Temp.H = AA1.L * b1 + AA2.L * b2;
        //--------------the compensated part---------------//
        err_P = temp1.L + temp2.L + temp3.L + temp4.L + Temp.H;
        err_temp = AA1.H * errb1 + AA2.H * errb2 + err_P;
        //--------------the running error bound------------//
        abst = fabs(err_temp);
        errz = errz1 * fabs(AA1.H) + errz2 * fabs(AA2.H) + (ne + 1) * fabs(err_P);

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
    Temp = two_prod(b1, s);
    Temp.L = s * errb1 + Temp.L;
    res = quick_two_sum(Temp.H, Temp.L);
    *(runerrbound) = (errz1 - (2 + na) * abst) * unit * s;

    return res;
}
