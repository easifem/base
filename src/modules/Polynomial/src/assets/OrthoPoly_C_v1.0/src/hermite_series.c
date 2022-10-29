/******************************************************************************
%  Set of subroutines and functions to evaluate series of Hermite polynomials at the point x. 
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
%    7 November 2016
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

#include<stdio.h>
#include<assert.h>
#include<stdlib.h>
#include<math.h>

#include"inline.h"
#include"hermite_series.h"

/* Herm1Val evaluates a series of Hermite polynomial He at the point x.*/
double Herm1Val(double *P, unsigned int n, double x)
{
    assert ( 0 < n);
    double  t,b1=0,b2=0;
    int i;

    for(i=n; i>=0; i--) 
    {
        t=x*b1-(i+1)*b2+P[i];
        b2=b1;
        b1=t;
    }
    return b1;
}

/*CompHerm1Val evaluates a series of Hermite polynomial He at the point x, with compensated method.*/
double CompHerm1Val(double *P, unsigned int n, double x)
{
    assert ( 0 < n);
    double  b1=0,b2=0;
    double  err_temp, errb1=0, errb2=0, res;
    dd_real temp1,temp2,temp3,temp4;
    int i;

    for(i=n; i>=0; i--) 
    {
        temp1=two_prod(x,b1);
        temp2=two_prod(i+1,b2);
        temp3=two_sum(temp1.H,-temp2.H);
        temp4=two_sum(temp3.H,P[i]);

        b2=b1;
        b1=temp4.H;
//--------------the compensated part---------------//
        err_temp=x*errb1-(i+1)*errb2+(temp1.L-temp2.L+temp3.L+temp4.L);
        errb2=errb1;
        errb1=err_temp;
    }
    res=b1+errb1;

    return res;
}

/*AccHerm1Val evaluates a series of Hermite polynomial He at the point x, with compensated method.*/
dd_real AccHerm1Val(double *P, unsigned int n, double x)
{
    assert ( 0 < n);
    double  b1=0,b2=0;
    double  err_temp, errb1=0, errb2=0;
    dd_real res,temp1,temp2,temp3,temp4;
    int i;

    for(i=n; i>=0; i--) 
    {
        temp1=two_prod(x,b1);
        temp2=two_prod(i+1,b2);
        temp3=two_sum(temp1.H,-temp2.H);
        temp4=two_sum(temp3.H,P[i]);

        b2=b1;
        b1=temp4.H;
//--------------the compensated part---------------//
        err_temp=x*errb1-(i+1)*errb2+(temp1.L-temp2.L+temp3.L+temp4.L);
        errb2=errb1;
        errb1=err_temp;
    }
    res=quick_two_sum(b1,errb1);

    return res;
}

/*Herm2Val evaluates a series of Hermite polynomial H at the point x. */
double Herm2Val(double *P, unsigned int n, double x)
{
    assert ( 0 < n);
    double  xx,t,b1=0,b2=0;
    int i;
    xx=2*x;

    for(i=n; i>=0; i--) 
    {
        t=xx*b1-2*(i+1)*b2+P[i];
        b2=b1;
        b1=t;
    }
    return b1;
}

/*CompHerm2Val evaluates a series of Hermite polynomial H at the point x, with compensated method.*/
double CompHerm2Val(double *P, unsigned int n, double x)
{
    assert ( 0 < n);
    double  xx,b1=0,b2=0;
    double  err_temp, errb1=0, errb2=0, res;
    dd_real temp1,temp2,temp3,temp4;
    int i;
    xx=2*x;

    for(i=n; i>=0; i--) 
    {
        temp1=two_prod(xx,b1);
        temp2=two_prod(2*i+2,b2);
        temp3=two_sum(temp1.H,-temp2.H);
        temp4=two_sum(temp3.H,P[i]);

        b2=b1;
        b1=temp4.H;
//--------------the compensated part---------------//
        err_temp=xx*errb1-(2*i+2)*errb2+(temp1.L-temp2.L+temp3.L+temp4.L);
        errb2=errb1;
        errb1=err_temp;
    }
    res=b1+errb1;

    return res;
}

/*AccHerm2Val evaluates a series of Hermite polynomial H at the point x, with compensated method.*/
dd_real AccHerm2Val(double *P, unsigned int n, double x)
{
    assert ( 0 < n);
    double  xx,b1=0,b2=0;
    double  err_temp, errb1=0, errb2=0;
    dd_real res,temp1,temp2,temp3,temp4;
    int i;
    xx=2*x;

    for(i=n; i>=0; i--) 
    {
        temp1=two_prod(xx,b1);
        temp2=two_prod(2*i+2,b2);
        temp3=two_sum(temp1.H,-temp2.H);
        temp4=two_sum(temp3.H,P[i]);

        b2=b1;
        b1=temp4.H;
//--------------the compensated part---------------//
        err_temp=xx*errb1-(2*i+2)*errb2+(temp1.L-temp2.L+temp3.L+temp4.L);
        errb2=errb1;
        errb1=err_temp;
    }
    res=quick_two_sum(b1,errb1);

    return res;
}

/*Herm1Der evaluates the first derivative of a series of Hermite polynomial He at the point x. */
double Herm1Der(double *P, unsigned int n, double x)
{
    assert ( 0 < n);
    double  t,b1=0,b2=0;
    int i;

    for(i=n-1; i>=0; i--) 
    {
        t=x*b1-(i+1)*b2+(i+1)*P[i+1];
        b2=b1;
        b1=t;
    }
    return b1;
}

/*CompHerm1Der evaluates the first derivative of a series of Hermite polynomial He at the point x, with compensated method.*/
double CompHerm1Der(double *P, unsigned int n, double x)
{
    assert ( 0 < n);
    double  b1=0,b2=0;
    double  err_temp, errb1=0, errb2=0, res;
    dd_real temp1,temp2,temp3,temp4,temp5;
    int i;

    for(i=n-1; i>=0; i--) 
    {
        temp1=two_prod(x,b1);
        temp2=two_prod(i+1,b2);
        temp3=two_sum(temp1.H,-temp2.H);
        temp4=two_prod(i+1,P[i+1]);
        temp5=two_sum(temp3.H,temp4.H);

        b2=b1;
        b1=temp5.H;
//--------------the compensated part---------------//
        err_temp=x*errb1-(i+1)*errb2+(temp1.L-temp2.L+temp3.L+temp4.L+temp5.L);
        errb2=errb1;
        errb1=err_temp;
    }
    res=b1+errb1;

    return res;
}

/*AccHerm1Der evaluates the first derivative of a series of Hermite polynomial He at the point x, with compensated method.*/
dd_real AccHerm1Der(double *P, unsigned int n, double x)
{
    assert ( 0 < n);
    double  b1=0,b2=0;
    double  err_temp, errb1=0, errb2=0;
    dd_real res,temp1,temp2,temp3,temp4,temp5;
    int i;

    for(i=n-1; i>=0; i--) 
    {
        temp1=two_prod(x,b1);
        temp2=two_prod(i+1,b2);
        temp3=two_sum(temp1.H,-temp2.H);
        temp4=two_prod(i+1,P[i+1]);
        temp5=two_sum(temp3.H,temp4.H);

        b2=b1;
        b1=temp5.H;
//--------------the compensated part---------------//
        err_temp=x*errb1-(i+1)*errb2+(temp1.L-temp2.L+temp3.L+temp4.L+temp5.L);
        errb2=errb1;
        errb1=err_temp;
    }
    res=quick_two_sum(b1,errb1);

    return res;
}

/*Herm2Der evaluates  the first  derivative of a series of Hermite polynomial H at the point x. */
double Herm2Der(double *P, unsigned int n, double x)
{
    assert ( 0 < n);
    double  xx,t,b1=0,b2=0;
    int i;
    xx=2*x;

    for(i=n-1; i>=0; i--) 
    {
        t=xx*b1-2*(i+1)*b2+2*(i+1)*P[i+1];
        b2=b1;
        b1=t;
    }
    return  b1;
}

/*CompHerm2Der evaluates the first derivative of a series of Hermite polynomial H at the point x, with compensated method.*/
double CompHerm2Der(double *P, unsigned int n, double x)
{
    assert ( 0 < n);
    double  xx,b1=0,b2=0;
    double  err_temp, errb1=0, errb2=0, res;
    dd_real temp1,temp2,temp3,temp4,temp5;
    int i;
    xx=2*x;

    for(i=n-1; i>=0; i--) 
    {
        temp1=two_prod(xx,b1);
        temp2=two_prod(2*i+2,b2);
        temp3=two_sum(temp1.H,-temp2.H);
        temp4=two_prod(2*i+2,P[i+1]);
        temp5=two_sum(temp3.H,temp4.H);

        b2=b1;
        b1=temp5.H;
//--------------the compensated part---------------//
        err_temp=xx*errb1-2*(i+1)*errb2+(temp1.L-temp2.L+temp3.L+temp4.L+temp5.L);
        errb2=errb1;
        errb1=err_temp;
    }
    res=b1+errb1;

    return res;
}

/*AccHerm2Der evaluates the first derivative of a series of Hermite polynomial H at the point x, with compensated method.*/
dd_real AccHerm2Der(double *P, unsigned int n, double x)
{
    assert ( 0 < n);
    double  xx,b1=0,b2=0;
    double  err_temp, errb1=0, errb2=0;
    dd_real res,temp1,temp2,temp3,temp4,temp5;
    int i;
    xx=2*x;

    for(i=n-1; i>=0; i--) 
    {
        temp1=two_prod(xx,b1);
        temp2=two_prod(2*i+2,b2);
        temp3=two_sum(temp1.H,-temp2.H);
        temp4=two_prod(2*i+2,P[i+1]);
        temp5=two_sum(temp3.H,temp4.H);

        b2=b1;
        b1=temp5.H;
//--------------the compensated part---------------//
        err_temp=xx*errb1-2*(i+1)*errb2+(temp1.L-temp2.L+temp3.L+temp4.L+temp5.L);
        errb2=errb1;
        errb1=err_temp;
    }
    res=quick_two_sum(b1,errb1);

    return res;
}

/*Herm1DerK evaluates  the k-th derivative of a series of Hermite polynomial He at the point x. */
double Herm1DerK(double *P, unsigned int n, double x, unsigned int k)
{
    assert ( 0 < n);
    double  t,s,b1=0,b2=0;
    int i,j;

    for(i=n-k; i>=0; i--) 
    {
        s=1.0;
        for(j=i+k;j>=i+1;j--)
        {
            s=s*j;
        }
        t=x*b1-(i+1)*b2+s*P[i+k];
        b2=b1;
        b1=t;
    }
    return b1;
}

/*CompHerm1DerK evaluates the k-th derivative of a series of Hermite polynomial He at the point x, with compensated method.*/
double CompHerm1DerK(double *P, unsigned int n, double x, unsigned int k)
{
    assert ( 0 < n);
    double  s,b1=0,b2=0;
    double  err_temp, errb1=0, errb2=0, res;
    dd_real temp1,temp2,temp3,temp4,temp5;
    int i,j;

    for(i=n-k; i>=0; i--) 
    {
        s=1.0;
        for(j=i+k;j>=i+1;j--)
        {
            s=s*j;
        }
        temp1=two_prod(x,b1);
        temp2=two_prod(i+1,b2);
        temp3=two_sum(temp1.H,-temp2.H);
        temp4=two_prod(s,P[i+k]);
        temp5=two_sum(temp3.H,temp4.H);

        b2=b1;
        b1=temp5.H;
//--------------the compensated part---------------//
        err_temp=x*errb1-(i+1)*errb2+(temp1.L-temp2.L+temp3.L+temp4.L+temp5.L);
        errb2=errb1;
        errb1=err_temp;
    }
    res=b1+errb1;

    return res;
}

/*AccHerm1DerK evaluates the k-th derivative of a series of Hermite polynomial He at the point x, with compensated method.*/
dd_real AccHerm1DerK(double *P, unsigned int n, double x, unsigned int k)
{
    assert ( 0 < n);
    double  s,b1=0,b2=0;
    double  err_temp, errb1=0, errb2=0;
    dd_real res,temp1,temp2,temp3,temp4,temp5;
    int i,j;

    for(i=n-k; i>=0; i--) 
    {
        s=1.0;
        for(j=i+k;j>=i+1;j--)
        {
            s=s*j;
        }
        temp1=two_prod(x,b1);
        temp2=two_prod(i+1,b2);
        temp3=two_sum(temp1.H,-temp2.H);
        temp4=two_prod(s,P[i+k]);
        temp5=two_sum(temp3.H,temp4.H);

        b2=b1;
        b1=temp5.H;
//--------------the compensated part---------------//
        err_temp=x*errb1-(i+1)*errb2+(temp1.L-temp2.L+temp3.L+temp4.L+temp5.L);
        errb2=errb1;
        errb1=err_temp;
    }
    res=quick_two_sum(b1,errb1);

    return res;
}

/*Herm2DerK evaluates  the k-th  derivative of a series of Hermite polynomial H at the point x. */
double Herm2DerK(double *P, unsigned int n, double x, unsigned int k)
{
    assert ( 0 < n);
    double  xx,C=1.0,s,t,b1=0,b2=0;
    int i,j;
    for(i=k;i>0;i--)
        {
            C=2*C;
        }
        xx=2*x;

    for(i=n-k; i>=0; i--) 
    {
        s=1.0;
        for(j=i+k;j>=i+1;j--)
        {
            s=s*j;
        }
        t=xx*b1-2*(i+1)*b2+s*P[i+k];

        b2=b1;
        b1=t;
    }
return  C*b1;
}

/*CompHerm2DerK evaluates the k-th derivative of a series of Hermite polynomial H at the point x, with compensated method.*/
double CompHerm2DerK(double *P, unsigned int n, double x, unsigned int k)
{
    assert ( 0 < n);
    double  xx,C=1.0,s,b1=0,b2=0;
    double  err_temp, errb1=0, errb2=0, res;
    dd_real temp1,temp2,temp3,temp4,temp5;
    int i,j;
    for(i=k;i>0;i--)
    {
        C=2*C;
    }
    xx=2*x;

    for(i=n-k; i>=0; i--) 
    {
        s=1.0;
        for(j=i+k;j>=i+1;j--)
        {
            s=s*j;
        }
        temp1=two_prod(xx,b1);
        temp2=two_prod(2*i+2,b2);
        temp3=two_sum(temp1.H,-temp2.H);
        temp4=two_prod(s,P[i+k]);
        temp5=two_sum(temp3.H,temp4.H);

        b2=b1;
        b1=temp5.H;
//--------------the compensated part---------------//
        err_temp=xx*errb1-2*(i+1)*errb2+(temp1.L-temp2.L+temp3.L+temp4.L+temp5.L);
        errb2=errb1;
        errb1=err_temp;
    }
    res=C*(b1+errb1);

    return res;
}

/*AccHerm2DerK evaluates the k-th derivative of a series of Hermite polynomial H at the point x, with compensated method.*/
dd_real AccHerm2DerK(double *P, unsigned int n, double x, unsigned int k)
{
    assert ( 0 < n);
    double  xx,C=1.0,s,b1=0,b2=0;
    double  err_temp, errb1=0, errb2=0;
    dd_real res,temp1,temp2,temp3,temp4,temp5;
    int i,j;
    for(i=k;i>0;i--)
    {
        C=2*C;
    }
    xx=2*x;

    for(i=n-k; i>=0; i--) 
    {
        s=1.0;
        for(j=i+k;j>=i+1;j--)
        {
            s=s*j;
        }
        temp1=two_prod(xx,b1);
        temp2=two_prod(2*i+2,b2);
        temp3=two_sum(temp1.H,-temp2.H);
        temp4=two_prod(s,P[i+k]);
        temp5=two_sum(temp3.H,temp4.H);

        b2=b1;
        b1=temp5.H;
//--------------the compensated part---------------//
        err_temp=xx*errb1-2*(i+1)*errb2+(temp1.L-temp2.L+temp3.L+temp4.L+temp5.L);
        errb2=errb1;
        errb1=err_temp;
    }
    res=quick_two_sum(C*b1,C*errb1);

    return res;
}

/* Herm1ValwErr evaluates a series of Hermite polynomial He at the point x, and performs a running-error bound.*/
double Herm1ValwErr(double *P, unsigned int n, double x, double * runerrbound)
{
    assert ( 0 < n);
    double  t,b1=0,b2=0;
    double  abst=0,absb1=0;
    int i;
    double j;
    double absx;
    absx=fabs(x);
    int na,nb;
    na=0;
    nb=0;

    double errz,errz1,errz2, errztemp;
    errz1=0;
    errz2=0;
    errztemp=0;

    for(i=n; i>=0; i--) 
    {
        j=1.0*i;
        t=x*b1-(j+1)*b2+P[i];                   
        abst=fabs(t);

        errz=errz1*absx+(j+1)*errz2+fabs(P[i]);
        errz2=errztemp+(nb+2)*absb1;
        errz1=errz+(na+3)*abst;
 
        b2=b1;
        b1=t;

        errztemp=errz;
        absb1=abst;
    }
    *(runerrbound)=(errz1-(2+na)*abst)*unit;

    return b1;
}

/*Herm2ValwErr evaluates a series of Hermite polynomial H at the point x, and performs a running-error bound.*/
double Herm2ValwErr(double *P, unsigned int n, double x, double * runerrbound)
{
    assert ( 0 < n);
    double  xx,t,b1=0,b2=0;
    double  abst=0,absb1=0;
    int i;
    double j;
    xx=2*x;
    double absxx;
    absxx=fabs(xx);
    int na,nb;
    na=0;
    nb=0;

    double errz,errz1,errz2, errztemp;
    errz1=0;
    errz2=0;
    errztemp=0;

    for(i=n; i>=0; i--) 
    {
        j=1.0*i;
        t=xx*b1-2*(j+1)*b2+P[i];                   
        abst=fabs(t);

        errz=errz1*absxx+2*(j+1)*errz2+fabs(P[i]);
        errz2=errztemp+(nb+2)*absb1;
        errz1=errz+(na+3)*abst;
 
        b2=b1;
        b1=t;

        errztemp=errz;
        absb1=abst;
    }
    *(runerrbound)=(errz1-(2+na)*abst)*unit;

    return b1;
}

/* Herm1DerwErr evaluates the first derivative of series of Hermite polynomial He at the point x, and performs a running-error bound.*/
double Herm1DerwErr(double *P, unsigned int n, double x, double * runerrbound)
{
    assert ( 0 < n);

    double  t,b1=0,b2=0;
    double  abst=0,absb1=0;
    int i;
    double j;
    double absx;
    absx=fabs(x);
    int na,nb,nc;
    na=0;
    nb=0;
    nc=0;

    double errz,errz1,errz2, errztemp;
    errz1=0;
    errz2=0;
    errztemp=0;

    for(i=n-1; i>=0; i--)
    {
        j=1.0*i;
        t=x*b1-(j+1)*b2+(j+1)*P[i+1];

        abst=fabs(t);

        errz=errz1*absx+(j+1)*errz2+(nc+2)*(j+1)*fabs(P[i+1]);
        errz2=errztemp+(nb+2)*absb1;
        errz1=errz+(na+3)*abst;

        b2=b1;
        b1=t;

        errztemp=errz;
        absb1=abst;
    }
    *(runerrbound)=(errz1-(1+na)*abst)*unit;
        
    return  b1;
}


/*Herm2DerwErr evaluates the first derivative of series of Hermite polynomial H at the point x, and performs a running-error bound.*/

double Herm2DerwErr(double *P, unsigned int n, double x, double * runerrbound)
{
    assert ( 0 < n);
    double  xx,t,b1=0,b2=0;

    double  abst=0,absb1=0,absxx;
    xx=2*x;
    absxx=fabs(xx);
    int i;
    double j;
    int na,nb,nc;
    na=0;
    nb=0;
    nc=0;

    double errz,errz1,errz2, errztemp;
    errz1=0;
    errz2=0;
    errztemp=0;

    for(i=n-1; i>=0; i--) 
    {
        j=1.0*i;
        t=xx*b1-2*(j+1)*b2+2*(j+1)*P[i+1];

        abst=fabs(t);

        errz=errz1*absxx+errz2*2*(j+1)+2*(j+1)*(nc+2)*fabs(P[i+1]);
        errz2=errztemp+(nb+2)*absb1;
        errz1=errz+(na+3)*abst;

        b2=b1;
        b1=t;

        errztemp=errz;
        absb1=abst;
    }
    *(runerrbound)=(errz1-(1+na)*abst)*unit;

    return  b1;
}

/* Herm1DerKwErr evaluates the k-th derivative of series of Hermite polynomial He at the point x, and performs a running-error bound.*/
double Herm1DerKwErr(double *P, unsigned int n, double x, double * runerrbound, unsigned int k)
{
    assert ( 0 < n);

    double  t,s,b1=0,b2=0;
    int i,j;

    double  abst=0,absb1=0;
    double absx;
    absx=fabs(x);
    int na,nb,nc;
    na=0;
    nb=0;
    nc=0;

    double errz,errz1,errz2, errztemp;
    errz1=0;
    errz2=0;
    errztemp=0;

    for(i=n-k; i>=0; i--)
    {
        s=1.0;
        for(j=i+k;j>=i+1;j--)
        {
            s=s*j;
        }
        t=x*b1-(i+1)*b2+s*P[i+k];;

        abst=fabs(t);

        errz=errz1*absx+(i+1)*errz2+(nc+2)*(s)*fabs(P[i+k]);
        errz2=errztemp+(nb+2)*absb1;
        errz1=errz+(na+3)*abst;

        b2=b1;
        b1=t;

        errztemp=errz;
        absb1=abst;
    }
    *(runerrbound)=(errz1-(1+na)*abst)*unit;
    
    return  b1;
}

/*Herm2DerKwErr evaluates the k-th derivative of series of Hermite polynomial H at the point x, and performs a running-error bound */
double Herm2DerKwErr(double *P, unsigned int n, double x, double * runerrbound, unsigned int k)
{
    assert ( 0 < n);

    int na,nb,nc;
    na=0;
    nb=0;
    nc=0;

    double errz,errz1,errz2, errztemp;
    errz1=0;
    errz2=0;
    errztemp=0;
    double  xx,C=1.0,s,t,b1=0,b2=0;
    int i,j;
    for(i=k;i>0;i--)
    {
        C=2*C;
    }
    xx=2*x;
    double  abst=0,absb1=0,absxx;
    absxx=fabs(xx);

    for(i=n-k; i>=0; i--) 
    {
        s=1.0;
        for(j=i+k;j>=i+1;j--)
        {
            s=s*j;
        }
        t=xx*b1-2*(i+1)*b2+s*P[i+k];

        abst=fabs(t);

        errz=errz1*absxx+errz2*2*(i+1)+(s)*(nc+2)*fabs(P[i+k]);
        errz2=errztemp+(nb+2)*absb1;
        errz1=errz+(na+3)*abst;

        b2=b1;
        b1=t;

        errztemp=errz;
        absb1=abst;
    }
    *(runerrbound)=(errz1-(1+na)*abst)*unit*C;

    return  C*b1;
}

/*CompHerm1ValwErr evaluates a series of Hermite polynomial He at the point x, with compensated method, together with the running error bound.*/
double CompHerm1ValwErr(double *P, unsigned int n, double x, double * runerrbound)
{
    assert ( 0 < n);
    double  b1=0,b2=0;
    double  res,err_temp, errb1=0, errb2=0;
    dd_real temp1,temp2,temp3,temp4;
    int i;

    double  abst=0,absb1=0;
    double absx;
    absx=fabs(x);
    int na,nb,ne;
    na=0;
    nb=0;
    ne=3;

    double errz,errz1,errz2, errztemp, err_P;
    errz1=0;
    errz2=0;
    errztemp=0;

    for(i=n; i>=0; i--) 
    {
        temp1=two_prod(x,b1);
        temp2=two_prod(i+1,b2);
        temp3=two_sum(temp1.H,-temp2.H);
        temp4=two_sum(temp3.H,P[i]);
//--------------the compensated part---------------//
        err_P=temp1.L-temp2.L+temp3.L+temp4.L;
        err_temp=x*errb1-(i+1)*errb2+err_P;
//--------------the running error bound------------//
        abst=fabs(err_temp);
        errz=errz1*absx+(i+1)*errz2+(ne+1)*fabs(err_P);

        errz2=errztemp+(nb+2)*absb1;
        errz1=errz+(na+3)*abst;
//--------------the next iteration-----------------//
        b2=b1;
        b1=temp4.H;

        errb2=errb1;
        errb1=err_temp;

        errztemp=errz;
        absb1=abst;
    }
    res=b1+errb1;
    *(runerrbound)=((errz1-(2+na)*abst)*unit+fabs(errb1-(res-b1)))/(1-unit*2);

    return res;
}

/*CompHerm2ValwErr evaluates a series of Hermite polynomial H at the point x, with compensated method, together with the running error bound.*/
double CompHerm2ValwErr(double *P, unsigned int n, double x, double * runerrbound)
{
    assert ( 0 < n);
    double  xx,b1=0,b2=0;
    double  res,err_temp=0, errb1=0, errb2=0;
    dd_real temp1,temp2,temp3,temp4;
    int i;
    xx=2*x;

    double  abst=0,absb1=0;
    double absxx;
    absxx=fabs(xx);
    int na,nb,ne;
    na=0;
    nb=0;
    ne=3;

    double errz,errz1,errz2, errztemp, err_P;
    errz1=0;
    errz2=0;
    errztemp=0;

    for(i=n; i>=0; i--) 
    {
        temp1=two_prod(xx,b1);
        temp2=two_prod(2*(i+1),b2);
        temp3=two_sum(temp1.H,-temp2.H);
        temp4=two_sum(temp3.H,P[i]);

        b2=b1;
        b1=temp4.H;
//--------------the compensated part---------------//
        err_P=temp1.L-temp2.L+temp3.L+temp4.L;
        err_temp=xx*errb1-2*(i+1)*errb2+err_P;
//--------------the running error bound------------//
        abst=fabs(err_temp);
        errz=errz1*absxx+2*(i+1)*errz2+(ne+1)*fabs(err_P);

        errz2=errztemp+(nb+2)*absb1;
        errz1=errz+(na+3)*abst;

        errb2=errb1;
        errb1=err_temp;

        errztemp=errz;
        absb1=abst;
    }
    res=b1+err_temp;
    *(runerrbound)=((errz1-(2+na)*abst)*unit+fabs(err_temp-(res-b1)))/(1-unit*2);

    return res;
}

/*CompHerm1DerwErr evaluates the first derivative of a series of Hermite polynomial He at the point x, with compensated method, together with running error bound.*/
double CompHerm1DerwErr(double *P, unsigned int n, double x, double * runerrbound)
{
    assert ( 0 < n);
    double  b1=0,b2=0;
    double  res,err_temp, errb1=0, errb2=0;
    dd_real temp1,temp2,temp3,temp4,temp5;
    int i;

    double abst=0,absb1=0;
    double absx;
    absx=fabs(x);
    int na,nb,ne;
    na=0;
    nb=0;
    ne=4;
    
    double errz,errz1,errz2, errztemp, err_P;
    errz1=0;
    errz2=0;
    errztemp=0;

    for(i=n-1; i>=0; i--) 
    {
        temp1=two_prod(x,b1);
        temp2=two_prod(i+1,P[i+1]);
        temp3=two_sum(temp1.H, temp2.H);
        temp4=two_prod(i+1,b2);
        temp5=two_sum(temp3.H,-temp4.H);

        b2=b1;
        b1=temp5.H;
//--------------the compensated part---------------//
        err_P=temp1.L+temp2.L+temp3.L-temp4.L+temp5.L;
        err_temp=x*errb1-(i+1)*errb2+err_P;
//--------------the running error bound------------//
        abst=fabs(err_temp);
        errz=errz1*absx+(i+1)*errz2+(ne+1)*fabs(err_P);

        errz2=errztemp+(nb+2)*absb1;
        errz1=errz+(na+3)*abst;

        errb2=errb1;
        errb1=err_temp;

        errztemp=errz;
        absb1=abst;
    }
    res=b1+errb1;
    *(runerrbound)=((errz1-(1+na)*abst)*unit+fabs(errb1-(res-b1)))/(1-unit*2);

    return res;
}

/*CompHerm2DerwErr evaluates the first derivative of a series of Hermite polynomial H at the point x, with compensated method, together with the running error bound.*/
double CompHerm2DerwErr(double *P, unsigned int n, double x, double * runerrbound)
{
    assert ( 0 < n);
    double  xx,b1=0,b2=0;
    xx=2*x;
    double  res,err_temp, errb1=0, errb2=0;
    dd_real temp1,temp2,temp3,temp4,temp5;

    int i;
    double absxx;
    absxx=fabs(xx);

    double abst=0,absb1=0;
    int na,nb,ne;
    na=0;
    nb=0;
    ne=4;

    double errz,errz1,errz2, errztemp, err_P;
    errz1=0;
    errz2=0;
    errztemp=0;

    for(i=n-1; i>=0; i--) 
    {
        temp1=two_prod(xx,b1);
        temp2=two_prod(2*(i+1),b2);
        temp3=two_sum(temp1.H,-temp2.H);
        temp4=two_prod(2*(i+1),P[i+1]);
        temp5=two_sum(temp3.H,temp4.H);
//--------------the compensated part---------------//
        err_P=temp1.L-temp2.L+temp3.L+temp4.L+temp5.L;
        err_temp=xx*errb1-2*(i+1)*errb2+err_P;
//--------------the running error bound------------//
        abst=fabs(err_temp);
        errz=errz1*absxx+2*(i+1)*errz2+(ne+1)*fabs(err_P);

        errz2=errztemp+(nb+2)*absb1;
        errz1=errz+(na+3)*abst;
//------------- the next step---------------------//
        b2=b1;
        b1=temp5.H;

        errb2=errb1;
        errb1=err_temp;

        errztemp=errz;
        absb1=abst;
    }
    res=b1+errb1;
    *(runerrbound)=((errz1-(1+na)*abst)*unit+fabs(errb1-(res-b1)))/(1-unit*2);

    return res;
}

/*CompHerm1DerKwErr evaluates the k-th derivative of a series of Hermite polynomial He at the point x, with compensated method, together with running error bound.*/
double CompHerm1DerKwErr(double *P, unsigned int n, double x, double * runerrbound, unsigned int k)
{
    assert ( 0 < n);
    double  s,b1=0,b2=0;
    double  res,err_temp, errb1=0, errb2=0;
    dd_real temp1,temp2,temp3,temp4,temp5;
    int i,j;

    double abst=0,absb1=0;
    double absx;
    absx=fabs(x);
    int na,nb,ne;
    na=0;
    nb=0;
    ne=4;

    double errz,errz1,errz2, errztemp, err_P;
    errz1=0;
    errz2=0;
    errztemp=0;

    for(i=n-k; i>=0; i--) 
    {
        s=1.0;
        for(j=i+k;j>=i+1;j--)
        {
            s=s*j;
        }
        temp1=two_prod(x,b1);
        temp2=two_prod(i+1,b2);
        temp3=two_sum(temp1.H,-temp2.H);
        temp4=two_prod(s,P[i+k]);
        temp5=two_sum(temp3.H,temp4.H);
//--------------the compensated part---------------//
        err_P=temp1.L-temp2.L+temp3.L+temp4.L+temp5.L;
        err_temp=x*errb1-(i+1)*errb2+err_P;
//--------------the running error bound------------//
        abst=fabs(err_temp);
        errz=errz1*absx+(i+1)*errz2+(ne+1)*fabs(err_P);

        errz2=errztemp+(nb+2)*absb1;
        errz1=errz+(na+3)*abst;

        b2=b1;
        b1=temp5.H;

        errb2=errb1;
        errb1=err_temp;

        errztemp=errz;
        absb1=abst;
    }
    res=b1+errb1;
    *(runerrbound)=((errz1-(2+na)*abst)*unit+fabs(errb1-(res-b1)))/(1-unit*2);

    return res;
}

/*CompHerm2DerKwErr evaluates the k-th derivative of a series of Hermite polynomial H at the point x, with compensated method, together with the running error bound.*/
double CompHerm2DerKwErr(double *P, unsigned int n, double x, double * runerrbound, unsigned int k)
{
    assert ( 0 < n);
    double  xx,C=1.0,s,b1=0,b2=0;
    double  res,err_temp, errb1=0, errb2=0;
    dd_real temp1,temp2,temp3,temp4,temp5;
    int i,j;
    for(i=k;i>0;i--)
    {
        C=2*C;
    }
    xx=2*x;
    double absxx;
    absxx=fabs(xx);

    double abst=0,absb1=0;
    int na,nb,ne;
    na=0;
    nb=0;
    ne=4;

    double errz,errz1,errz2, errztemp, err_P;
    errz1=0;
    errz2=0;
    errztemp=0;

    for(i=n-k; i>=0; i--) 
    {
        s=1.0;
        for(j=i+k;j>=i+1;j--)
        {
            s=s*j;
        }
        temp1=two_prod(xx,b1);
        temp2=two_prod(2*i+2,b2);
        temp3=two_sum(temp1.H,-temp2.H);
        temp4=two_prod(s,P[i+k]);
        temp5=two_sum(temp3.H,temp4.H);
//--------------the compensated part---------------//
        err_P=temp1.L-temp2.L+temp3.L+temp4.L+temp5.L;
        err_temp=xx*errb1-2*(i+1)*errb2+err_P;
//--------------the running error bound------------//
        abst=fabs(err_temp);
        errz=errz1*absxx+2*(i+1)*errz2+(ne+1)*fabs(err_P);

        errz2=errztemp+(nb+2)*absb1;
        errz1=errz+(na+3)*abst;
//------------- the next step---------------------//
        b2=b1;
        b1=temp5.H;

        errb2=errb1;
        errb1=err_temp;

        errztemp=errz;
        absb1=abst;
    }
    res=C*(b1+errb1);
    *(runerrbound)=((errz1-(2+na)*abst)*unit*C+fabs(C*errb1-(res-C*b1)))/(1-unit*2);

    return res;
}

/*AccHerm1ValwErr evaluates a series of Hermite polynomial He at the point x, with compensated method, together with the running error bound.*/
dd_real AccHerm1ValwErr(double *P, unsigned int n, double x, double * runerrbound)
{
    assert ( 0 < n);
    double  b1=0,b2=0;
    double  err_temp, errb1=0, errb2=0;
    dd_real res,temp1,temp2,temp3,temp4;
    int i;

    double  abst=0,absb1=0;
    double absx;
    absx=fabs(x);
    int na,nb,ne;
    na=0;
    nb=0;
    ne=3;

    double errz,errz1,errz2, errztemp, err_P;
    errz1=0;
    errz2=0;
    errztemp=0;

    for(i=n; i>=0; i--) 
    {
        temp1=two_prod(x,b1);
        temp2=two_prod(i+1,b2);
        temp3=two_sum(temp1.H,-temp2.H);
        temp4=two_sum(temp3.H,P[i]);
//--------------the compensated part---------------//
        err_P=temp1.L-temp2.L+temp3.L+temp4.L;
        err_temp=x*errb1-(i+1)*errb2+err_P;
//--------------the running error bound------------//
        abst=fabs(err_temp);
        errz=errz1*absx+(i+1)*errz2+(ne+1)*fabs(err_P);

        errz2=errztemp+(nb+2)*absb1;
        errz1=errz+(na+3)*abst;
//--------------the next iteration-----------------//
        b2=b1;
        b1=temp4.H;

        errb2=errb1;
        errb1=err_temp;

        errztemp=errz;
        absb1=abst;
    }
    res=quick_two_sum(b1,errb1);
    *(runerrbound)=(errz1-(2+na)*abst)*unit;

    return res;
}

/*AccHerm2ValwErr evaluates a series of Hermite polynomial H at the point x, with compensated method, together with the running error bound.*/
dd_real AccHerm2ValwErr(double *P, unsigned int n, double x, double * runerrbound)
{
    assert ( 0 < n);
    double  xx,b1=0,b2=0;
    double  err_temp=0, errb1=0, errb2=0;
    dd_real res,temp1,temp2,temp3,temp4;
    int i;
    xx=2*x;

    double  abst=0,absb1=0;
    double absxx;
    absxx=fabs(xx);
    int na,nb,ne;
    na=0;
    nb=0;
    ne=3;

    double errz,errz1,errz2, errztemp, err_P;
    errz1=0;
    errz2=0;
    errztemp=0;

    for(i=n; i>=0; i--) 
    {
        temp1=two_prod(xx,b1);
        temp2=two_prod(2*(i+1),b2);
        temp3=two_sum(temp1.H,-temp2.H);
        temp4=two_sum(temp3.H,P[i]);

        b2=b1;
        b1=temp4.H;
//--------------the compensated part---------------//
        err_P=temp1.L-temp2.L+temp3.L+temp4.L;
        err_temp=xx*errb1-2*(i+1)*errb2+err_P;
//--------------the running error bound------------//
        abst=fabs(err_temp);
        errz=errz1*absxx+2*(i+1)*errz2+(ne+1)*fabs(err_P);

        errz2=errztemp+(nb+2)*absb1;
        errz1=errz+(na+3)*abst;

        errb2=errb1;
        errb1=err_temp;

        errztemp=errz;
        absb1=abst;
    }
    res=quick_two_sum(b1,err_temp);
    *(runerrbound)=(errz1-(2+na)*abst)*unit;

    return res;
}

/*AccHerm1DerwErr evaluates the first derivative of a series of Hermite polynomial He at the point x, with compensated method, together with running error bound.*/
dd_real AccHerm1DerwErr(double *P, unsigned int n, double x, double * runerrbound)
{
    assert ( 0 < n);
    double  b1=0,b2=0;
    double  err_temp, errb1=0, errb2=0;
    dd_real res,temp1,temp2,temp3,temp4,temp5;
    int i;

    double abst=0,absb1=0;
    double absx;
    absx=fabs(x);
    int na,nb,ne;
    na=0;
    nb=0;
    ne=4;

    double errz,errz1,errz2, errztemp, err_P;
    errz1=0;
    errz2=0;
    errztemp=0;

    for(i=n-1; i>=0; i--) 
    {
        temp1=two_prod(x,b1);
        temp2=two_prod(i+1,P[i+1]);
        temp3=two_sum(temp1.H, temp2.H);
        temp4=two_prod(i+1,b2);
        temp5=two_sum(temp3.H,-temp4.H);

        b2=b1;
        b1=temp5.H;
//--------------the compensated part---------------//
        err_P=temp1.L+temp2.L+temp3.L-temp4.L+temp5.L;
        err_temp=x*errb1-(i+1)*errb2+err_P;
//--------------the running error bound------------//
        abst=fabs(err_temp);
        errz=errz1*absx+(i+1)*errz2+(ne+1)*fabs(err_P);

        errz2=errztemp+(nb+2)*absb1;
        errz1=errz+(na+3)*abst;

        errb2=errb1;
        errb1=err_temp;

        errztemp=errz;
        absb1=abst;
    }
    res=quick_two_sum(b1,errb1);
    *(runerrbound)=(errz1-(1+na)*abst)*unit;

    return res;
}

/*AccHerm2DerwErr evaluates the first derivative of a series of Hermite polynomial H at the point x, with compensated method, together with the running error bound.*/
dd_real AccHerm2DerwErr(double *P, unsigned int n, double x, double * runerrbound)
{
    assert ( 0 < n);
    double  xx,b1=0,b2=0;
    xx=2*x;
    double  err_temp, errb1=0, errb2=0;
    dd_real res,temp1,temp2,temp3,temp4,temp5;

    int i;
    double absxx;
    absxx=fabs(xx);

    double abst=0,absb1=0;
    int na,nb,ne;
    na=0;
    nb=0;
    ne=4;

    double errz,errz1,errz2, errztemp, err_P;
    errz1=0;
    errz2=0;
    errztemp=0;

    for(i=n-1; i>=0; i--) 
    {
        temp1=two_prod(xx,b1);
        temp2=two_prod(2*(i+1),b2);
        temp3=two_sum(temp1.H,-temp2.H);
        temp4=two_prod(2*(i+1),P[i+1]);
        temp5=two_sum(temp3.H,temp4.H);
//--------------the compensated part---------------//
        err_P=temp1.L-temp2.L+temp3.L+temp4.L+temp5.L;
        err_temp=xx*errb1-2*(i+1)*errb2+err_P;
//--------------the running error bound------------//
        abst=fabs(err_temp);
        errz=errz1*absxx+2*(i+1)*errz2+(ne+1)*fabs(err_P);

        errz2=errztemp+(nb+2)*absb1;
        errz1=errz+(na+3)*abst;
//------------- the next step---------------------//
        b2=b1;
        b1=temp5.H;

        errb2=errb1;
        errb1=err_temp;

        errztemp=errz;
        absb1=abst;
    }
    res=quick_two_sum(b1,errb1);
    *(runerrbound)=(errz1-(1+na)*abst)*unit;

    return res;
}

/*AccHerm1DerKwErr evaluates the k-th derivative of a series of Hermite polynomial He at the point x, with compensated method, together with running error bound.*/
dd_real AccHerm1DerKwErr(double *P, unsigned int n, double x, double * runerrbound, unsigned int k)
{
    assert ( 0 < n);
    double  s,b1=0,b2=0;
    double  err_temp, errb1=0, errb2=0;
    dd_real res,temp1,temp2,temp3,temp4,temp5;
    int i,j;

    double abst=0,absb1=0;
    double absx;
    absx=fabs(x);
    int na,nb,ne;
    na=0;
    nb=0;
    ne=4;

    double errz,errz1,errz2, errztemp, err_P;
    errz1=0;
    errz2=0;
    errztemp=0;

    for(i=n-k; i>=0; i--) 
    {
        s=1.0;
        for(j=i+k;j>=i+1;j--)
        {
            s=s*j;
        }
        temp1=two_prod(x,b1);
        temp2=two_prod(i+1,b2);
        temp3=two_sum(temp1.H,-temp2.H);
        temp4=two_prod(s,P[i+k]);
        temp5=two_sum(temp3.H,temp4.H);
//--------------the compensated part---------------//
        err_P=temp1.L-temp2.L+temp3.L+temp4.L+temp5.L;
        err_temp=x*errb1-(i+1)*errb2+err_P;
//--------------the running error bound------------//
        abst=fabs(err_temp);
        errz=errz1*absx+(i+1)*errz2+(ne+1)*fabs(err_P);

        errz2=errztemp+(nb+2)*absb1;
        errz1=errz+(na+3)*abst;

        b2=b1;
        b1=temp5.H;

        errb2=errb1;
        errb1=err_temp;

        errztemp=errz;
        absb1=abst;
    }
    res=quick_two_sum(b1,errb1);
    *(runerrbound)=(errz1-(2+na)*abst)*unit;

    return res;
}

/*AccHerm2DerKwErr evaluates the k-th derivative of a series of Hermite polynomial H at the point x, with compensated method, together with the running error bound.*/
dd_real AccHerm2DerKwErr(double *P, unsigned int n, double x, double * runerrbound, unsigned int k)
{
    assert ( 0 < n);
    double  xx,C=1.0,s,b1=0,b2=0;
    double  err_temp, errb1=0, errb2=0;
    dd_real res,temp1,temp2,temp3,temp4,temp5;
    int i,j;
    for(i=k;i>0;i--)
    {
        C=2*C;
    }
    xx=2*x;
    double absxx;
    absxx=fabs(xx);

    double abst=0,absb1=0;
    int na,nb,ne;
    na=0;
    nb=0;
    ne=4;

    double errz,errz1,errz2, errztemp, err_P;
    errz1=0;
    errz2=0;
    errztemp=0;

    for(i=n-k; i>=0; i--) 
    {
        s=1.0;
        for(j=i+k;j>=i+1;j--)
        {
            s=s*j;
        }
        temp1=two_prod(xx,b1);
        temp2=two_prod(2*i+2,b2);
        temp3=two_sum(temp1.H,-temp2.H);
        temp4=two_prod(s,P[i+k]);
        temp5=two_sum(temp3.H,temp4.H);
//--------------the compensated part---------------//
        err_P=temp1.L-temp2.L+temp3.L+temp4.L+temp5.L;
        err_temp=xx*errb1-2*(i+1)*errb2+err_P;
//--------------the running error bound------------//
        abst=fabs(err_temp);
        errz=errz1*absxx+2*(i+1)*errz2+(ne+1)*fabs(err_P);

        errz2=errztemp+(nb+2)*absb1;
        errz1=errz+(na+3)*abst;
//------------- the next step---------------------//
        b2=b1;
        b1=temp5.H;

        errb2=errb1;
        errb1=err_temp;

        errztemp=errz;
        absb1=abst;
    }
    res=quick_two_sum(C*b1,C*errb1);
    *(runerrbound)=(errz1-(2+na)*abst)*unit*C;

    return res;
}









