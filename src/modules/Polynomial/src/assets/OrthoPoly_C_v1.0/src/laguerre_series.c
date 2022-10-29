/******************************************************************************
%  Set of subroutines and functions to evaluate series of LAGUERRE polynomials at the point x, which is in (0, +infty). 
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
%    8 November 2016
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
#include"laguerre_series.h"

//----------------------- The three-term recurrence coefficients of laguerre polynomial ----------------------------------
void RecurCoefLague (double n, double alpha,  double *A, double *B, double *C)
{
    int i;
    double j;
    for (i=1;i<=n+2;i++)
    {
        j=1.0*i;
        *(A+i-1)=-1/j;
        *(B+i-1)=(2*j+alpha-1)/j;
        *(C+i-1)=(j-1+alpha)/j;
    }
}

void RecurCoefLague_DD (double n, double alpha,  dd_real *A, dd_real *B, dd_real *C)
{
    int i;
    double j;
    dd_real s0,s1;

    for (i=1;i<=n+2;i++)
    {
        j=1.0*i;
        *(A+i-1)=div_d_d(-1,j);
        s0=two_sum(2*j-1,alpha);
        *(B+i-1)=div_dd_d(s0,j);
        s1=two_sum(j-1,alpha);
        *(C+i-1)=div_dd_d(s1,j);
    }
}

/* LagueVal evaluates a series of Laguerre polynomial at the point x, which is in (0, +infty).*/
double LagueVal(double *P, unsigned int n, double x, double alpha)
{
    assert ( 0 < n);
    assert (x>0);
    assert (alpha>-1.0);
    double  t,b1=0,b2=0;
    int j;

    double *A=(double *) malloc(sizeof(double)*(n+2));
    double *B=(double *) malloc(sizeof(double)*(n+2));
    double *C=(double *) malloc(sizeof(double)*(n+2));

    RecurCoefLague(n,alpha,A,B,C);

    for(j=n; j>=0; j--) 
    {
        t=(*(A+j)*x+*(B+j))*b1-*(C+j+1)*b2+P[j];

        b2=b1;
        b1=t;
    }
    free(A);
    free(B);
    free(C);

    return b1;
}

/*CompLagueVal evaluates a series of Laguerre polynomial  at the point x, which is in (0, +infty), with compensated method.*/
double CompLagueVal(double *P, unsigned int n, double x, double alpha)
{
    assert ( 0 < n);
    assert (x>0);
    assert (alpha>-1.0);
    double  b1=0,b2=0, res;
    double  err_temp, err_temp1, err_temp2, err_temp3, errb1=0, errb2=0;
    dd_real temp1,temp2,temp3,temp4,temp5,temp6;
    int j;

    dd_real *A=(dd_real *) malloc(sizeof(dd_real)*(n+2));
    dd_real *B=(dd_real *) malloc(sizeof(dd_real)*(n+2));
    dd_real *C=(dd_real *) malloc(sizeof(dd_real)*(n+2));

    RecurCoefLague_DD(n,alpha,A,B,C);

    for(j=n; j>=0; j--) 
    {
        temp1=two_prod((A+j)->H,x);
        temp2=two_sum(temp1.H,(B+j)->H);
        temp3=two_prod(temp2.H,b1);

        temp4=two_prod((C+j+1)->H,b2);
        temp5=two_sum(temp3.H,-temp4.H);
        temp6=two_sum(temp5.H,P[j]);
//--------------the compensated part---------------//
        err_temp1=(temp1.L+temp2.L)*b1+temp3.L;
        err_temp2=(((A+j)->L)*x+((B+j)->L))*b1-((C+j+1)->L)*b2;
        err_temp3=err_temp1-temp4.L+temp5.L+temp6.L+err_temp2;
        err_temp=temp2.H*errb1-((C+j+1)->H)*errb2+err_temp3;
//--------------the next iteration-----------------//
        b2=b1;
        b1=temp6.H;
        errb2=errb1;
        errb1=err_temp;
    }
    res=b1+errb1;
    free(A);
    free(B);
    free(C);

    return res;
}

/*AccLagueVal evaluates a series of Laguerre polynomial  at the point x, which is in (0, +infty), with compensated method.*/
dd_real AccLagueVal(double *P, unsigned int n, double x, double alpha)
{
    assert ( 0 < n);
    assert (x>0);
    assert (alpha>-1.0);
    double  b1=0,b2=0;
    double  err_temp, err_temp1, err_temp2, err_temp3, errb1=0, errb2=0;
    dd_real res,temp1,temp2,temp3,temp4,temp5,temp6;
    int j;

    dd_real *A=(dd_real *) malloc(sizeof(dd_real)*(n+2));
    dd_real *B=(dd_real *) malloc(sizeof(dd_real)*(n+2));
    dd_real *C=(dd_real *) malloc(sizeof(dd_real)*(n+2));

    RecurCoefLague_DD(n,alpha,A,B,C);

    for(j=n; j>=0; j--) 
    {
        temp1=two_prod((A+j)->H,x);
        temp2=two_sum(temp1.H,(B+j)->H);
        temp3=two_prod(temp2.H,b1);

        temp4=two_prod((C+j+1)->H,b2);
        temp5=two_sum(temp3.H,-temp4.H);
        temp6=two_sum(temp5.H,P[j]);
//--------------the compensated part---------------//
        err_temp1=(temp1.L+temp2.L)*b1+temp3.L;
        err_temp2=(((A+j)->L)*x+((B+j)->L))*b1-((C+j+1)->L)*b2;
        err_temp3=err_temp1-temp4.L+temp5.L+temp6.L+err_temp2;
        err_temp=temp2.H*errb1-((C+j+1)->H)*errb2+err_temp3;
//--------------the next iteration-----------------//
        b2=b1;
        b1=temp6.H;
        errb2=errb1;
        errb1=err_temp;
    }
    res=quick_two_sum(b1,errb1);
    free(A);
    free(B);
    free(C);

    return res;
}

/*LagueDer evaluates  the first derivative of a series of Laguerre polynomial at the point x, which is in (0, +infty). */
double LagueDer(double *P, unsigned int n, double x, double alpha)
{
    assert ( 0 < n);
    assert (x>0);
    assert (alpha>-1.0);
    double  t,b1=0,b2=0;
    double A1,A2;
    int i;
    double j;
    double C=1;

    for(i=n-1; i>=0; i--) 
    {
        j=1.0*i;
//-------------recurrence coefficients-----------------------//
        A1=-x/(j+1)+(2*j+alpha+2)/(j+1);
        A2=-(j+alpha+2)/(j+2);
//--------------iteration-----------------------------------//
        t=A1*b1+A2*b2-P[i+1];

        b2=b1;
        b1=t;
    }
    return  C*b1;
}

/*CompLagueDer evaluates  the first derivative of a series of Laguerre polynomial  at the point x, which is in (0, +infty), with compensated method */
double CompLagueDer(double *P, unsigned int n, double x, double alpha)
{
    assert ( 0 < n);
    assert (x>0);
    assert (alpha>-1.0);
    double  b1=0,b2=0;
    double  err_temp, errb1=0, errb2=0, res;
    dd_real temp1,temp2,temp3,temp4;
    dd_real A1,A2;
    dd_real s0,s1,s2;
    int i;
    double j,temp;

    for(i=n-1; i>=0; i--) 
    {
        j=1.0*i;
//-------------recurrence coefficients-----------------------//
        s0=two_sum(2*j+2,alpha);
        s1=add_dd_d(s0,-x);
        A1=div_dd_d(s1,j+1);

        s2=two_sum(j+2,alpha);
        s2.H=-s2.H;
        s2.L=-s2.L;
        A2=div_dd_d(s2,j+2);
//--------------iteration-----------------------------------//
        temp1=two_prod(A1.H,b1);
        temp2=two_prod(A2.H,b2);
        temp3=two_sum(-P[i+1],temp1.H);
        temp4=two_sum(temp2.H,temp3.H);
        temp=temp1.L+temp2.L+temp3.L+temp4.L;
        err_temp=A1.L*b1+A2.L*b2+A1.H*errb1+A2.H*errb2+temp;

        b2=b1;
        b1=temp4.H;
        errb2=errb1;
        errb1=err_temp;
    }
    res=b1+errb1;

    return  res;
}

/*AccLagueDer evaluates  the first derivative of a series of Laguerre polynomial  at the point x, which is in (0, +infty), with compensated method */
dd_real AccLagueDer(double *P, unsigned int n, double x, double alpha)
{
    assert ( 0 < n);
    assert (x>0);
    assert (alpha>-1.0);
    double  b1=0,b2=0;
    double  err_temp, errb1=0, errb2=0;
    dd_real res,temp1,temp2,temp3,temp4;
    dd_real A1,A2;
    dd_real s0,s1,s2;
    int i;
    double j,temp;

    for(i=n-1; i>=0; i--) 
    {
        j=1.0*i;
//-------------recurrence coefficients-----------------------//
        s0=two_sum(2*j+2,alpha);
        s1=add_dd_d(s0,-x);
        A1=div_dd_d(s1,j+1);

        s2=two_sum(j+2,alpha);
        s2.H=-s2.H;
        s2.L=-s2.L;
        A2=div_dd_d(s2,j+2);
//--------------iteration-----------------------------------//
        temp1=two_prod(A1.H,b1);
        temp2=two_prod(A2.H,b2);
        temp3=two_sum(-P[i+1],temp1.H);
        temp4=two_sum(temp2.H,temp3.H);
        temp=temp1.L+temp2.L+temp3.L+temp4.L;
        err_temp=A1.L*b1+A2.L*b2+A1.H*errb1+A2.H*errb2+temp;

        b2=b1;
        b1=temp4.H;
        errb2=errb1;
        errb1=err_temp;
    }
    res=quick_two_sum(b1,errb1);

    return  res;
}

/*LagueDerK evaluates  the k-th derivative of a series of Laguerre polynomial at the point x, which is in (0, +infty). */
double LagueDerK(double *P, unsigned int n, double x, double alpha, unsigned int k)
{
    assert ( 0 < n);
    assert (x>0);
    assert (alpha>-1.0);
    double  t,b1=0,b2=0;
    double A1,A2;
    int i;
    double j;
    int C=1;
    for(i=k;i>0;i--)
    {
        C=-C;
    }

    for(i=n-k; i>=0; i--) 
    {
        j=1.0*i;
//-------------recurrence coefficients-----------------------//
        A1=-x/(j+1)+(2*j+alpha+k+1)/(j+1);
        A2=-(j+alpha+k+1)/(j+2);
//--------------iteration-----------------------------------//
        t=A1*b1+A2*b2+P[i+k];

        b2=b1;
        b1=t;
    }
    return  C*b1;
}

/*CompLagueDerK evaluates  the k-th derivative of a series of Laguerre polynomial  at the point x, which is in (0, +infty), with compensated method */
double CompLagueDerK(double *P, unsigned int n, double x, double alpha, unsigned int k)
{
    assert ( 0 < n);
    assert (x>0);
    assert (alpha>-1.0);
    double  b1=0,b2=0;
    double  err_temp, errb1=0, errb2=0, res;
    dd_real temp1,temp2,temp3,temp4;
    dd_real A1,A2;
    dd_real s0,s1,s2;
    int i;
    double j,temp;
    int C=1;
    for(i=k;i>0;i--)
    {
        C=-C;
    }

    for(i=n-k; i>=0; i--) 
    {
        j=1.0*i;
//-------------recurrence coefficients-----------------------//
        s0=two_sum(2*j+k+1,alpha);
        s1=add_dd_d(s0,-x);
        A1=div_dd_d(s1,j+1);

        s2=two_sum(j+k+1,alpha);
        A2=div_dd_d(s2,-j-2);
//--------------iteration-----------------------------------//
        temp1=two_prod(A1.H,b1);
        temp2=two_prod(A2.H,b2);
        temp3=two_sum(P[i+k],temp1.H);
        temp4=two_sum(temp2.H,temp3.H);
        temp=temp1.L+temp2.L+temp3.L+temp4.L;
        err_temp=A1.L*b1+A2.L*b2+A1.H*errb1+A2.H*errb2+temp;

        b2=b1;
        b1=temp4.H;
        errb2=errb1;
        errb1=err_temp;
    }
    res=C*(b1+errb1);

    return  res;
}

/*AccLagueDerK evaluates  the k-th derivative of a series of Laguerre polynomial  at the point x, which is in (0, +infty), with compensated method */
dd_real AccLagueDerK(double *P, unsigned int n, double x, double alpha, unsigned int k)
{
    assert ( 0 < n);
    assert (x>0);
    assert (alpha>-1.0);
    double  b1=0,b2=0;
    double  err_temp, errb1=0, errb2=0;
    dd_real res,temp1,temp2,temp3,temp4;
    dd_real A1,A2;
    dd_real s0,s1,s2;
    int i;
    double j,temp;
    int C=1;
    for(i=k;i>0;i--)
    {
        C=-C;
    }

    for(i=n-k; i>=0; i--) 
    {
        j=1.0*i;
//-------------recurrence coefficients-----------------------//
        s0=two_sum(2*j+k+1,alpha);
        s1=add_dd_d(s0,-x);
        A1=div_dd_d(s1,j+1);

        s2=two_sum(j+k+1,alpha);
        A2=div_dd_d(s2,-j-2);
//--------------iteration-----------------------------------//
        temp1=two_prod(A1.H,b1);
        temp2=two_prod(A2.H,b2);
        temp3=two_sum(P[i+k],temp1.H);
        temp4=two_sum(temp2.H,temp3.H);
        temp=temp1.L+temp2.L+temp3.L+temp4.L;
        err_temp=A1.L*b1+A2.L*b2+A1.H*errb1+A2.H*errb2+temp;

        b2=b1;
        b1=temp4.H;
        errb2=errb1;
        errb1=err_temp;
    }
    res=quick_two_sum(C*b1,C*errb1);

    return  res;
}

/*----------------------- LagueValwErr evaluates a series of Laguerre polynomial at the point x, which is in (0, +infty), and performs a running-error bound */
double LagueValwErr(double *P, unsigned int n, double x, double alpha,  double * runerrbound)
{
    assert ( 0 < n);
    assert (x>0);
    assert (alpha>-1.0);
    double  t,b1=0,b2=0;
    int j;
    double  abst=0,absb1=0;
    int na,nb;
    na=3;
    nb=2;
    double errz,errz1,errz2, errztemp;
    errz1=0;
    errz2=0;
    errztemp=0;
    double coef1;

    double *A=(double *) malloc(sizeof(double)*(n+2));
    double *B=(double *) malloc(sizeof(double)*(n+2));
    double *C=(double *) malloc(sizeof(double)*(n+2));

    RecurCoefLague(n,alpha,A,B,C);

    for(j=n; j>=0; j--) 
    {
        coef1=(*(A+j)*x+*(B+j));
        t=coef1*b1-*(C+j+1)*b2+P[j];
        abst=fabs(t);

        errz=errz1*fabs(coef1)+errz2*fabs(*(C+j+1))+fabs(P[j]);
        errz2=errztemp+(nb+2)*absb1;
        errz1=errz+(na+3)*abst;

        b2=b1;
        b1=t;

        errztemp=errz;
        absb1=abst;
    }
    free(A);
    free(B);
    free(C);
    *(runerrbound)=(errz1-(2+na)*abst)*unit;

    return b1;
}

/*----------------------- LagueDerwErr evaluates  the first derivative of a series of Laguerre polynomial at the point x, which is in (0, +infty), and performs a running-error bound  */
double LagueDerwErr(double *P, unsigned int n, double x, double alpha,  double * runerrbound)
{
    assert ( 0 < n);
    assert (x>0);
    assert (alpha>-1.0);
    double  t,b1=0,b2=0;
    double A1, A2;
    int i;
    double j;

    double  abst=0,absb1=0;
    int na,nb,nc;
    na=3;
    nb=2;
    nc=0;
    double errz,errz1,errz2, errztemp;
    errz1=0;
    errz2=0;
    errztemp=0;

    for(i=n-1; i>=0; i--) 
    {
        j=1.0*i;
//-------------recurrence coefficients-----------------------//
        A1=(2*(j+1)+alpha-x)/(j+1);
        A2=-(j+alpha+2)/(j+2);
//--------------iteration-----------------------------------//
        t=A1*b1+A2*b2-P[i+1];
        abst=fabs(t);

        errz=errz1*fabs(A1)+errz2*fabs(A2)+(nc+2)*fabs(P[i+1]);
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

/*----------------------- LagueDerKwErr evaluates  the k-th derivative of a series of Laguerre polynomial at the point x, which is in (0, +infty), and performs a running-error bound */
double LagueDerKwErr(double *P, unsigned int n, double x, double alpha,  double * runerrbound, unsigned int k)
{
    assert ( 0 < n);
    assert (x>0);
    assert (alpha>-1.0);
    double  t,b1=0,b2=0;
    double A1,A2;
    int i;
    double j;
    int C=1;
    for(i=k;i>0;i--)
    {
        C=-C;
    }
    double  abst=0,absb1=0;
    int na,nb,nc;
    na=3;
    nb=2;
    nc=0;
    double errz,errz1,errz2, errztemp;
    errz1=0;
    errz2=0;
    errztemp=0;

    for(i=n-k; i>=0; i--) 
    {
        j=1.0*i;
//-------------recurrence coefficients-----------------------//
        A1=-x/(j+1)+(2*j+alpha+k+1)/(j+1);
        A2=-(j+alpha+k+1)/(j+2);
//--------------iteration-----------------------------------//
        t=A1*b1+A2*b2+P[i+k];
        abst=fabs(t);

        errz=errz1*fabs(A1)+errz2*fabs(A2)+(nc+2)*fabs(P[i+k]);
        errz2=errztemp+(nb+2)*absb1;
        errz1=errz+(na+3)*abst;

        b2=b1;
        b1=t;

        errztemp=errz;
        absb1=abst;
    }
    *(runerrbound)=(errz1-(1+na)*abst)*unit;
    return  C*b1;
}

/*----------------------- CompLagueValwErr evaluates a series of Laguerre polynomial  at the point x, which is in (0, +infty), with compensated method, and performs a running-error bound */
double CompLagueValwErr(double *P, unsigned int n, double x, double alpha,  double * runerrbound)
{
    assert ( 0 < n);
    assert (x>0);
    assert (alpha>-1.0);
    double  b1=0,b2=0;
    double  res,err_temp, err_temp1, err_temp2, err_temp3, errb1=0, errb2=0;
    dd_real temp1,temp2,temp3,temp4,temp5,temp6;
    int j;

    double  abst=0,absb1=0;
    int na,nb,ne;
    na=3;
    nb=2;
    ne=9;
    double errz,errz1,errz2, errztemp;
    errz1=0;
    errz2=0;
    errztemp=0;

    dd_real *A=(dd_real *) malloc(sizeof(dd_real)*(n+2));
    dd_real *B=(dd_real *) malloc(sizeof(dd_real)*(n+2));
    dd_real *C=(dd_real *) malloc(sizeof(dd_real)*(n+2));

    RecurCoefLague_DD(n,alpha,A,B,C);

    for(j=n; j>=0; j--) 
    {
        temp1=two_prod((A+j)->H,x);
        temp2=two_sum(temp1.H,(B+j)->H);
        temp3=two_prod(temp2.H,b1);

        temp4=two_prod((C+j+1)->H,b2);
        temp5=two_sum(temp3.H,-temp4.H);
        temp6=two_sum(temp5.H,P[j]);
//--------------the compensated part---------------//
        err_temp1=(temp1.L+temp2.L)*b1+temp3.L;
        err_temp2=(((A+j)->L)*x+((B+j)->L))*b1-((C+j+1)->L)*b2;
        err_temp3=err_temp1-temp4.L+temp5.L+temp6.L+err_temp2;
        err_temp=temp2.H*errb1-((C+j+1)->H)*errb2+err_temp3;
//--------------the running error bound-----------//
        abst=fabs(err_temp);
        errz=errz1*fabs(temp2.H)+errz2*fabs((C+j+1)->H)+(ne+1)*fabs(err_temp3);
        errz2=errztemp+(nb+2)*absb1;
        errz1=errz+(na+3)*abst;
//--------------the next iteration-----------------//
        b2=b1;
        b1=temp6.H;

        errb2=errb1;
        errb1=err_temp;

        errztemp=errz;
        absb1=abst;
    }
    res=b1+errb1;
    *(runerrbound)=((errz1-(2+na)*abst)*unit+fabs(errb1-(res-b1)))/(1-unit*2);
    free(A);
    free(B);
    free(C);

    return res;
}

/*----------------------- CompLagueDerwErr evaluates  the first derivative of a series of Laguerre polynomial  at the point x, which is in (0, +infty), with compensated method, and performs a running-error bound */
double CompLagueDerwErr(double *P, unsigned int n, double x, double alpha,  double * runerrbound)
{
    assert ( 0 < n);
    assert (x>0);
    assert (alpha>-1.0);
    double  b1=0,b2=0;
    double  res,err_temp, errb1=0, errb2=0;
    dd_real temp1,temp2,temp3,temp4;
    dd_real A1, A2;
    dd_real s0,s1,s2;
    int i;
    double j;

    double  abst=0,absb1=0;
    int na,nb,ne;
    na=3;
    nb=2;
    ne=6;
    double errz,errz1,errz2, errztemp, err_P;
    errz1=0;
    errz2=0;
    errztemp=0;

    for(i=n-1; i>=0; i--) 
    {
        j=1.0*i;
//-------------recurrence coefficients-----------------------//
        s0=two_sum(2*j+2,alpha);
        s1=add_dd_d(s0,-x);
        A1=div_dd_d(s1,j+1);

        s2=two_sum(j+2,alpha);
        s2.H=-s2.H;
        s2.L=-s2.L;
        A2=div_dd_d(s2,j+2);
//--------------iteration-----------------------------------//
        temp1=two_prod(A1.H,b1);
        temp2=two_prod(A2.H,b2);
        temp3=two_sum(temp1.H,temp2.H);
        temp4=two_sum(temp3.H,-P[i+1]);
//--------------the compensated part------------------------//
        err_P=A1.L*b1+A2.L*b2+temp1.L+temp2.L+temp3.L+temp4.L;
        err_temp=A1.H*errb1+A2.H*errb2+err_P;
//--------------the runninge error bound-------------------//
        abst=fabs(err_temp);
        errz=errz1*fabs(A1.H)+errz2*fabs(A2.H)+(ne+1)*fabs(err_P);

        errz2=errztemp+(nb+2)*absb1;
        errz1=errz+(na+3)*abst;

        b2=b1;
        b1=temp4.H;

        errb2=errb1;
        errb1=err_temp;

        errztemp=errz;
        absb1=abst;
    }
    res=b1+errb1;
    *(runerrbound)=((errz1-(1+na)*abst)*unit+fabs(errb1-(res-b1)))/(1-unit*2);

    return  res;
}

/*----------------------- CompLagueDerKwErr evaluates  the k-th derivative of a series of Laguerre polynomial  at the point x, which is in (0, +infty), with compensated method, and performs a running-error bound */
double CompLagueDerKwErr(double *P, unsigned int n, double x, double alpha,  double * runerrbound, unsigned int k)
{
    assert ( 0 < n);
    assert (x>0);
    assert (alpha>-1.0);
    double  b1=0,b2=0;
    double  res,err_temp, errb1=0, errb2=0;
    dd_real temp1,temp2,temp3,temp4;
    dd_real A1,A2;
    dd_real s0,s1,s2;
    int i;
    double j;
    int C=1;
    for(i=k;i>0;i--)
    {
        C=-C;
    }
    double  abst=0,absb1=0;
    int na,nb,ne;
    na=3;
    nb=2;
    ne=6;
    double errz,errz1,errz2, errztemp, err_P;
    errz1=0;
    errz2=0;
    errztemp=0;

    for(i=n-k; i>=0; i--) 
    {
        j=1.0*i;
//-------------recurrence coefficients-----------------------//
        s0=two_sum(2*j+k+1,alpha);
        s1=add_dd_d(s0,-x);
        A1=div_dd_d(s1,j+1);

        s2=two_sum(j+k+1,alpha);
        A2=div_dd_d(s2,-j-2);
//--------------iteration-----------------------------------//
        temp1=two_prod(A1.H,b1);
        temp2=two_prod(A2.H,b2);
        temp3=two_sum(P[i+k],temp1.H);
        temp4=two_sum(temp2.H,temp3.H);
//--------------the compensated part------------------------//
        err_P=A1.L*b1+A2.L*b2+temp1.L+temp2.L+temp3.L+temp4.L;
        err_temp=A1.H*errb1+A2.H*errb2+err_P;
//--------------the runninge error bound-------------------//
        abst=fabs(err_temp);
        errz=errz1*fabs(A1.H)+errz2*fabs(A2.H)+(ne+1)*fabs(err_P);

        errz2=errztemp+(nb+2)*absb1;
        errz1=errz+(na+3)*abst;

        b2=b1;
        b1=temp4.H;

        errb2=errb1;
        errb1=err_temp;

        errztemp=errz;
        absb1=abst;
    }
    res=C*(b1+errb1);
    *(runerrbound)=((errz1-(2+na)*abst)*unit+fabs(C*errb1-(res-C*b1)))/(1-unit*3);

    return  res;
}

/*----------------------- AccLagueValwErr evaluates a series of Laguerre polynomial  at the point x, which is in (0, +infty), with compensated method, and performs a running-error bound */
dd_real AccLagueValwErr(double *P, unsigned int n, double x, double alpha,  double * runerrbound)
{
    assert ( 0 < n);
    assert (x>0);
    assert (alpha>-1.0);
    double  b1=0,b2=0;
    double  err_temp, err_temp1, err_temp2, err_temp3, errb1=0, errb2=0;
    dd_real res,temp1,temp2,temp3,temp4,temp5,temp6;
    int j;

    double  abst=0,absb1=0;
    int na,nb,ne;
    na=3;
    nb=2;
    ne=9;
    double errz,errz1,errz2, errztemp;
    errz1=0;
    errz2=0;
    errztemp=0;

    dd_real *A=(dd_real *) malloc(sizeof(dd_real)*(n+2));
    dd_real *B=(dd_real *) malloc(sizeof(dd_real)*(n+2));
    dd_real *C=(dd_real *) malloc(sizeof(dd_real)*(n+2));

    RecurCoefLague_DD(n,alpha,A,B,C);

    for(j=n; j>=0; j--) 
    {
        temp1=two_prod((A+j)->H,x);
        temp2=two_sum(temp1.H,(B+j)->H);
        temp3=two_prod(temp2.H,b1);

        temp4=two_prod((C+j+1)->H,b2);
        temp5=two_sum(temp3.H,-temp4.H);
        temp6=two_sum(temp5.H,P[j]);
//--------------the compensated part---------------//
        err_temp1=(temp1.L+temp2.L)*b1+temp3.L;
        err_temp2=(((A+j)->L)*x+((B+j)->L))*b1-((C+j+1)->L)*b2;
        err_temp3=err_temp1-temp4.L+temp5.L+temp6.L+err_temp2;
        err_temp=temp2.H*errb1-((C+j+1)->H)*errb2+err_temp3;
//--------------the running error bound-----------//
        abst=fabs(err_temp);
        errz=errz1*fabs(temp2.H)+errz2*fabs((C+j+1)->H)+(ne+1)*fabs(err_temp3);

        errz2=errztemp+(nb+2)*absb1;
        errz1=errz+(na+3)*abst;
//--------------the next iteration-----------------//
        b2=b1;
        b1=temp6.H;

        errb2=errb1;
        errb1=err_temp;

        errztemp=errz;
        absb1=abst;
    }
    res=quick_two_sum(b1,errb1);
    *(runerrbound)=(errz1-(2+na)*abst)*unit;
    free(A);
    free(B);
    free(C);

    return res;
}

/*----------------------- AccLagueDerwErr evaluates  the first derivative of a series of Laguerre polynomial  at the point x, which is in (0, +infty), with compensated method, and performs a running-error bound */
dd_real AccLagueDerwErr(double *P, unsigned int n, double x, double alpha,  double * runerrbound)
{
    assert ( 0 < n);
    assert (x>0);
    assert (alpha>-1.0);
    double  b1=0,b2=0;
    double  err_temp, errb1=0, errb2=0;
    dd_real res,temp1,temp2,temp3,temp4;
    dd_real A1, A2;
    dd_real s0,s1,s2;
    int i;
    double j;

    double  abst=0,absb1=0;
    int na,nb,ne;
    na=3;
    nb=2;
    ne=6;
    double errz,errz1,errz2, errztemp, err_P;
    errz1=0;
    errz2=0;
    errztemp=0;

    for(i=n-1; i>=0; i--) 
    {
        j=1.0*i;
//-------------recurrence coefficients-----------------------//
        s0=two_sum(2*j+2,alpha);
        s1=add_dd_d(s0,-x);
        A1=div_dd_d(s1,j+1);

        s2=two_sum(j+2,alpha);
        s2.H=-s2.H;
        s2.L=-s2.L;
        A2=div_dd_d(s2,j+2);
//--------------iteration-----------------------------------//
        temp1=two_prod(A1.H,b1);
        temp2=two_prod(A2.H,b2);
        temp3=two_sum(temp1.H,temp2.H);
        temp4=two_sum(temp3.H,-P[i+1]);
//--------------the compensated part------------------------//
        err_P=A1.L*b1+A2.L*b2+temp1.L+temp2.L+temp3.L+temp4.L;
        err_temp=A1.H*errb1+A2.H*errb2+err_P;
//--------------the runninge error bound-------------------//
        abst=fabs(err_temp);
        errz=errz1*fabs(A1.H)+errz2*fabs(A2.H)+(ne+1)*fabs(err_P);

        errz2=errztemp+(nb+2)*absb1;
        errz1=errz+(na+3)*abst;

        b2=b1;
        b1=temp4.H;

        errb2=errb1;
        errb1=err_temp;

        errztemp=errz;
        absb1=abst;
    }
    res=quick_two_sum(b1,errb1);
    *(runerrbound)=(errz1-(1+na)*abst)*unit;

    return  res;
}

/*----------------------- AccLagueDerKwErr evaluates  the k-th derivative of a series of Laguerre polynomial  at the point x, which is in (0, +infty), with compensated method, and performs a running-error bound */
dd_real AccLagueDerKwErr(double *P, unsigned int n, double x, double alpha,  double * runerrbound, unsigned int k)
{
    assert ( 0 < n);
    assert (x>0);
    assert (alpha>-1.0);
    double  b1=0,b2=0;
    double  err_temp, errb1=0, errb2=0;
    dd_real res,temp1,temp2,temp3,temp4;
    dd_real A1,A2;
    dd_real s0,s1,s2;
    int i;
    double j;
    int C=1;
    for(i=k;i>0;i--)
    {
        C=-C;
    }
    double  abst=0,absb1=0;
    int na,nb,ne;
    na=3;
    nb=2;
    ne=6;
    double errz,errz1,errz2, errztemp, err_P;
    errz1=0;
    errz2=0;
    errztemp=0;

    for(i=n-k; i>=0; i--) 
    {
        j=1.0*i;
//-------------recurrence coefficients-----------------------//
        s0=two_sum(2*j+k+1,alpha);
        s1=add_dd_d(s0,-x);
        A1=div_dd_d(s1,j+1);
   
        s2=two_sum(j+k+1,alpha);
        A2=div_dd_d(s2,-j-2);
//--------------iteration-----------------------------------//
        temp1=two_prod(A1.H,b1);
        temp2=two_prod(A2.H,b2);
        temp3=two_sum(P[i+k],temp1.H);
        temp4=two_sum(temp2.H,temp3.H);
//--------------the compensated part------------------------//
        err_P=A1.L*b1+A2.L*b2+temp1.L+temp2.L+temp3.L+temp4.L;
        err_temp=A1.H*errb1+A2.H*errb2+err_P;
//--------------the runninge error bound-------------------//
        abst=fabs(err_temp);
        errz=errz1*fabs(A1.H)+errz2*fabs(A2.H)+(ne+1)*fabs(err_P);

        errz2=errztemp+(nb+2)*absb1;
        errz1=errz+(na+3)*abst;

        b2=b1;
        b1=temp4.H;

        errb2=errb1;
        errb1=err_temp;

        errztemp=errz;
        absb1=abst;
    }
    res=quick_two_sum(C*b1,C*errb1);
    *(runerrbound)=(errz1-(2+na)*abst)*unit;

    return  res;
}





