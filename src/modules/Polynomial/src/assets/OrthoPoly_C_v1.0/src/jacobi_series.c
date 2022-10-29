/******************************************************************************
%  Set of subroutines and functions to evaluate series of JACOBI polynomials at the point x, which is in [-1, 1].
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
%               Numerical evaluation of the pth derivative of Jacobobi series,
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
#include"jacobi_series.h"
//----------------------- The three-term recurrence coefficients of jacobi polynomial ----------------------------------
void RecurCoefJacob (double n, double alpha, double beta,  double *A, double *B, double *C)
{
    int i;
    double j;
    for (i=1;i<=n+2;i++)
    {
        j=1.0*i;
        *(A+i-1)=(2*j+alpha+beta-1)*(2*j+alpha+beta)/(2*j*(j+alpha+beta));
        *(B+i-1)=(alpha*alpha-beta*beta)*(2*j+alpha+beta-1)/(2*j*(j+alpha+beta)*(2*j+alpha+beta-2));
        *(C+i-1)=(j-1+alpha)*(j-1+beta)*(2*j+alpha+beta)/(j*(j+alpha+beta)*(2*j+alpha+beta-2));
    }
}

void RecurCoefJacob_DD (double n, double alpha, double beta,  dd_real *A, dd_real *B, dd_real *C)
{
    int i;
    double j;
    dd_real tempA,tempB,temp1,temp2,temp3,temp4,temp5, temp6;
    dd_real s0,s1,s2,s3,w1,w2;
    tempA=two_sum(alpha,beta);
    tempB=two_sum(alpha,-beta);

    for (i=1;i<=n+2;i++)
    {
            j=1.0*i;
            temp1=add_dd_d(tempA,j);
            temp2=add_dd_d(tempA,2*j-2);
            temp3=add_dd_d(tempA,2*j-1);
            temp4=add_dd_d(tempA,2*j);
            temp5=prod_dd_d(temp1,j);
            temp6.H=2*temp5.H;
            temp6.L=2*temp5.L;
            s0=prod_dd_dd(temp3,temp4);
            *(A+i-1)=div_dd_dd(s0,temp6);
            s1=prod_dd_dd(tempA,tempB);
            s2=prod_dd_dd(s1,temp3);
            s3=prod_dd_dd(temp6,temp2);
            *(B+i-1)=div_dd_dd(s2,s3);
            w1=two_sum(j-1,alpha);
            w2=two_sum(j-1,beta);
            s1=prod_dd_dd(w1,w2);
            s2=prod_dd_dd(s1,temp4);
            s3=prod_dd_dd(temp5,temp2);
            *(C+i-1)=div_dd_dd(s2,s3);
    }
}

/* JacobVal evaluates a series of Jacobobi polynomial at the point x, which is in [-1, 1].*/
double JacobVal(double *P, unsigned int n, double x, double alpha, double beta)
{
    assert ( 0 < n);
    assert (fabs(x)<=1.0);
    assert (alpha>-1.0);
    assert (beta>-1.0);

    double  t,b1=0,b2=0;
    int j;

    double *A=(double *) malloc(sizeof(double)*(n+2));
    double *B=(double *) malloc(sizeof(double)*(n+2));
    double *C=(double *) malloc(sizeof(double)*(n+2));

    RecurCoefJacob(n,alpha,beta,A,B,C);

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

/*CompJacobVal evaluates a series of Jacobobi polynomial  at the point x, which is in [-1, 1], with compensated method.*/
double CompJacobVal(double *P, unsigned int n, double x, double alpha, double beta)
{
    assert ( 0 < n);
    assert (fabs(x)<=1.0);
    assert (alpha>-1.0);
    assert (beta>-1.0);
    double  b1=0,b2=0;
    double  res,err_temp, err_temp1, err_temp2, err_temp3, errb1=0, errb2=0;
    dd_real temp1,temp2,temp3,temp4,temp5,temp6;
    int j;

    dd_real *A=(dd_real *) malloc(sizeof(dd_real)*(n+2));
    dd_real *B=(dd_real *) malloc(sizeof(dd_real)*(n+2));
    dd_real *C=(dd_real *) malloc(sizeof(dd_real)*(n+2));

    RecurCoefJacob_DD(n,alpha,beta,A,B,C);

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

/*AccJacobVal evaluates a series of Jacobobi polynomial  at the point x, which is in [-1, 1], with compensated method.*/
dd_real AccJacobVal(double *P, unsigned int n, double x, double alpha, double beta)
{
    assert ( 0 < n);
    assert (fabs(x)<=1.0);
    assert (alpha>-1.0);
    assert (beta>-1.0);
    double  b1=0,b2=0;
    double  err_temp, err_temp1, err_temp2, err_temp3, errb1=0, errb2=0;
    dd_real res,temp1,temp2,temp3,temp4,temp5,temp6;
    int j;

    dd_real *A=(dd_real *) malloc(sizeof(dd_real)*(n+2));
    dd_real *B=(dd_real *) malloc(sizeof(dd_real)*(n+2));
    dd_real *C=(dd_real *) malloc(sizeof(dd_real)*(n+2));

    RecurCoefJacob_DD(n,alpha,beta,A,B,C);

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

/*JacobDer evaluates  the first derivative of a series of Jacobobi polynomial at the point x, which is in [-1, 1]. */
double JacobDer(double *P, unsigned int n, double x, double alpha, double beta)
{
    assert ( 0 < n);
    assert (fabs(x)<=1.0);
    assert (alpha>-1.0);
    assert (beta>-1.0);
    double  t,b1=0,b2=0;
    double Ac, A1, A2;
    double a10,a11,a12,a20,a21;
    int i;
    double j;
    double C=0.5;

    for(i=n-1; i>=0; i--)
    {
        j=1.0*i;
//-------------recurrence coefficients-----------------------//
        Ac=j+2+alpha+beta;
        a10=(2*j+3+alpha+beta)/((2*j+2)*(j+3+alpha+beta));
        a11=(2*j+4+alpha+beta)*x;
        a12=((alpha-beta)*(alpha+beta+2))/(alpha+beta+2*j+2);
        A1=a10*(a11+a12);
        a20=-(j+2+alpha)*(j+2+beta)/((j+2)*(alpha+beta+j+4));
        a21=(alpha+beta+2*j+6)/(alpha+beta+2*j+4);
        A2=a20*a21;
//--------------iteration-----------------------------------//
        t=A1*b1+A2*b2+Ac*P[i+1];

        b2=b1;
        b1=t;
    }
    return  C*b1;
}

/*CompJacobDer evaluates  the first derivative of a series of Jacobobi polynomial  at the point x, which is in [-1, 1], with compensated method */
double CompJacobDer(double *P, unsigned int n, double x, double alpha, double beta)
{
    assert ( 0 < n);
    assert (fabs(x)<=1.0);
    assert (alpha>-1.0);
    assert (beta>-1.0);
    double  b1=0,b2=0;
    double  res,err_temp, errb1=0, errb2=0;
    dd_real temp1,temp2,temp3,temp4,temp5;
    dd_real Ac, A1, A2;
    dd_real a10,a11,a12,a20,a21;
    dd_real u,u1,u2,u3,u4,u5,u6,u7;
    dd_real w,s1,s2,s3,s4,s5,s6,s7,s8;
    int i;
    double j,temp;
    double C=0.5;

    u=two_sum(alpha,beta);
    w=two_sum(alpha,-beta);

    for(i=n-1; i>=0; i--)
    {
        j=1.0*i;

        u1=add_dd_d(u,j+2);
        u2=add_dd_d(u,j+3);
        u3=add_dd_d(u,j+4);
        u4=add_dd_d(u,2*j+2);
        u5=add_dd_d(u,2*j+3);
        u6=add_dd_d(u,2*j+4);
        u7=add_dd_d(u,2*j+6);
//-------------recurrence coefficients-----------------------//
        Ac=u1;

        s1=prod_dd_d(u2,(2*j+2));
        a10=div_dd_dd(u5,s1);
        a11=prod_dd_d(u6,x);
        s2=add_dd_d(u,2);
        s3=prod_dd_dd(w,s2);
        a12=div_dd_dd(s3,u4);
        s4=add_dd_dd(a11,a12);
        A1=prod_dd_dd(a10,s4);

        s5=two_sum(j+2,alpha);
        s6=two_sum(j+2,beta);
        s5.H=-s5.H;
        s5.L=-s5.L;
        s7=prod_dd_dd(s5,s6);
        s8=prod_dd_d(u3,j+2);
        a20=div_dd_dd(s7,s8);
        a21=div_dd_dd(u7,u6);
        A2=prod_dd_dd(a20,a21);
//--------------iteration-----------------------------------//
        temp1=two_prod(Ac.H,P[i+1]);
        temp2=two_prod(A1.H,b1);
        temp3=two_prod(A2.H,b2);
        temp4=two_sum(temp1.H,temp2.H);
        temp5=two_sum(temp3.H,temp4.H);
        temp=temp1.L+temp2.L+temp3.L+temp4.L+temp5.L;
        err_temp=Ac.L*P[i+1]+A1.L*b1+A2.L*b2+A1.H*errb1+A2.H*errb2+temp;

        b2=b1;
        b1=temp5.H;
        errb2=errb1;
        errb1=err_temp;
    }
    res=C*(b1+errb1);
    return  res;
}

/*AccJacobDer evaluates  the first derivative of a series of Jacobobi polynomial  at the point x, which is in [-1, 1], with compensated method */
dd_real AccJacobDer(double *P, unsigned int n, double x, double alpha, double beta)
{
    assert ( 0 < n);
    assert (fabs(x)<=1.0);
    assert (alpha>-1.0);
    assert (beta>-1.0);
    double  b1=0,b2=0;
    double  err_temp, errb1=0, errb2=0;
    dd_real res,temp1,temp2,temp3,temp4,temp5;
    dd_real Ac, A1, A2;
    dd_real a10,a11,a12,a20,a21;
    dd_real u,u1,u2,u3,u4,u5,u6,u7;
    dd_real w,s1,s2,s3,s4,s5,s6,s7,s8;
    int i;
    double j,temp;
    double C=0.5;

    u=two_sum(alpha,beta);
    w=two_sum(alpha,-beta);

    for(i=n-1; i>=0; i--)
    {
        j=1.0*i;

        u1=add_dd_d(u,j+2);
        u2=add_dd_d(u,j+3);
        u3=add_dd_d(u,j+4);
        u4=add_dd_d(u,2*j+2);
        u5=add_dd_d(u,2*j+3);
        u6=add_dd_d(u,2*j+4);
        u7=add_dd_d(u,2*j+6);
//-------------recurrence coefficients-----------------------//
        Ac=u1;

        s1=prod_dd_d(u2,(2*j+2));
        a10=div_dd_dd(u5,s1);
        a11=prod_dd_d(u6,x);
        s2=add_dd_d(u,2);
        s3=prod_dd_dd(w,s2);
        a12=div_dd_dd(s3,u4);
        s4=add_dd_dd(a11,a12);
        A1=prod_dd_dd(a10,s4);

        s5=two_sum(j+2,alpha);
        s6=two_sum(j+2,beta);
        s5.H=-s5.H;
        s5.L=-s5.L;
        s7=prod_dd_dd(s5,s6);
        s8=prod_dd_d(u3,j+2);
        a20=div_dd_dd(s7,s8);
        a21=div_dd_dd(u7,u6);
        A2=prod_dd_dd(a20,a21);
//--------------iteration-----------------------------------//
        temp1=two_prod(Ac.H,P[i+1]);
        temp2=two_prod(A1.H,b1);
        temp3=two_prod(A2.H,b2);
        temp4=two_sum(temp1.H,temp2.H);
        temp5=two_sum(temp3.H,temp4.H);
        temp=temp1.L+temp2.L+temp3.L+temp4.L+temp5.L;
        err_temp=Ac.L*P[i+1]+A1.L*b1+A2.L*b2+A1.H*errb1+A2.H*errb2+temp;

        b2=b1;
        b1=temp5.H;
        errb2=errb1;
        errb1=err_temp;
    }
    res=quick_two_sum(b1,errb1);
    res.H=C*res.H;
    res.L=C*res.L;

    return  res;
}

/*JacobDerK evaluates the k-th derivative of a series of Jacobobi polynomial at the point x, which is in [-1, 1]. */
double JacobDerK(double *P, unsigned int n, double x, double alpha, double beta, unsigned int k)
{
    assert ( 0 < n);
    assert (fabs(x)<=1.0);
    assert (alpha>-1.0);
    assert (beta>-1.0);
    double  t,b1=0,b2=0;
    double A1, A2,s;
    double a10,a11,a12,a20,a21;
    int i,j;
    double C=1.0;
    for(i=k;i>0;i--)
    {
        C=C/2.0;
    }

    for(i=n-k; i>=0; i--)
    {
//-------------recurrence coefficients-----------------------//
        s=1.0;
        for(j=1;j<=k;j++)
        {
            s=s*(alpha+beta+i+k+j);
        }
        a10=(2*i+1+2*k+alpha+beta)/((2*i+2)*(i+1+2*k+alpha+beta));
        a11=(2*i+2+2*k+alpha+beta)*x;
        a12=((alpha-beta)*(alpha+beta+2*k))/(alpha+beta+2*i+2*k);
        A1=a10*(a11+a12);

        a20=-(i+1+k+alpha)*(i+1+k+beta)/((i+2)*(alpha+beta+i+2+2*k));
        a21=(alpha+beta+2*i+4+2*k)/(alpha+beta+2*i+2+2*k);
        A2=a20*a21;
//--------------iteration-----------------------------------//
    t=A1*b1+A2*b2+s*P[i+k];

    b2=b1;
    b1=t;
    }
    return  C*b1;
}

/*CompJacobDerK evaluates  the k-th derivative of a series of Jacobobi polynomial  at the point x, which is in [-1, 1], with compensated method */
double CompJacobDerK(double *P, unsigned int n, double x, double alpha, double beta, unsigned int k)
{
    assert ( 0 < n);
    assert (fabs(x)<=1.0);
    assert (alpha>-1.0);
    assert (beta>-1.0);
    double  b1=0,b2=0;
    double  res,err_temp, errb1=0, errb2=0;
    dd_real temp1,temp2,temp3,temp4,temp5;
    dd_real A1, A2;
    dd_real a10,a11,a12,a20,a21;
    dd_real u,u1,u2,u3,u4,u5,u6,u7,u8,u9;
    dd_real w,s1,s2,s3,s4,s5,s6,s7,s8;
    int i,j;
    double temp;
    double C=1.0;
    for(i=k;i>0;i--)
    {
        C=C/2.0;
    }
    u=two_sum(alpha,beta);
    w=two_sum(alpha,-beta);

    for(j=n-k; j>=0; j--)
    {
        u1=add_dd_d(u,j+k+1);
        u2=add_dd_d(u,j+2*k+1);
        u3=add_dd_d(u,j+2*k+2);
        u4=add_dd_d(u,2*j+2*k);
        u5=add_dd_d(u,2*j+2*k+1);
        u6=add_dd_d(u,2*j+2*k+2);
        u7=add_dd_d(u,2*j+2*k+4);
//-------------recurrence coefficients-----------------------//
        u9.H=1.0;
        u9.L=0.0;
        for(i=1;i<=k;i++)
        {
            u8=add_dd_d(u1,i-1);
            u9=prod_dd_dd(u9,u8);
        }
        s1=prod_dd_d(u2,(2*j+2));
        a10=div_dd_dd(u5,s1);
        a11=prod_dd_d(u6,x);
        s2=add_dd_d(u,2*k);
        s3=prod_dd_dd(w,s2);
        a12=div_dd_dd(s3,u4);
        s4=add_dd_dd(a11,a12);
        A1=prod_dd_dd(a10,s4);

        s5=two_sum(j+1+k,alpha);
        s6=two_sum(j+1+k,beta);
        s5.H=-s5.H;
        s5.L=-s5.L;
        s7=prod_dd_dd(s5,s6);
        s8=prod_dd_d(u3,j+2);
        a20=div_dd_dd(s7,s8);
        a21=div_dd_dd(u7,u6);
        A2=prod_dd_dd(a20,a21);
//--------------iteration-----------------------------------//
        temp1=two_prod(u9.H,P[j+k]);
        temp2=two_prod(A1.H,b1);
        temp3=two_prod(A2.H,b2);
        temp4=two_sum(temp1.H,temp2.H);
        temp5=two_sum(temp3.H,temp4.H);
        temp=temp1.L+temp2.L+temp3.L+temp4.L+temp5.L;
        err_temp=u9.L*P[j+k]+A1.L*b1+A2.L*b2+A1.H*errb1+A2.H*errb2+temp;

        b2=b1;
        b1=temp5.H;
        errb2=errb1;
        errb1=err_temp;
    }
    res=C*(b1+errb1);
    return  res;
}

/*AccJacobDerK evaluates  the k-th derivative of a series of Jacobobi polynomial  at the point x, which is in [-1, 1], with compensated method */
dd_real AccJacobDerK(double *P, unsigned int n, double x, double alpha, double beta, unsigned int k)
{
    assert ( 0 < n);
    assert (fabs(x)<=1.0);
    assert (alpha>-1.0);
    assert (beta>-1.0);
    double  b1=0,b2=0;
    double  err_temp, errb1=0, errb2=0;
    dd_real res,temp1,temp2,temp3,temp4,temp5;
    dd_real A1, A2;
    dd_real a10,a11,a12,a20,a21;
    dd_real u,u1,u2,u3,u4,u5,u6,u7,u8,u9;
    dd_real w,s1,s2,s3,s4,s5,s6,s7,s8;
    int i,j;
    double temp;
    double C=1.0;
    for(i=k;i>0;i--)
    {
        C=C/2.0;
    }
    u=two_sum(alpha,beta);
    w=two_sum(alpha,-beta);

    for(j=n-k; j>=0; j--)
    {
        u1=add_dd_d(u,j+k+1);
        u2=add_dd_d(u,j+2*k+1);
        u3=add_dd_d(u,j+2*k+2);
        u4=add_dd_d(u,2*j+2*k);
        u5=add_dd_d(u,2*j+2*k+1);
        u6=add_dd_d(u,2*j+2*k+2);
        u7=add_dd_d(u,2*j+2*k+4);
//-------------recurrence coefficients-----------------------//
        u9.H=1.0;
        u9.L=0.0;
        for(i=1;i<=k;i++)
        {
            u8=add_dd_d(u1,i-1);
            u9=prod_dd_dd(u9,u8);
        }
        s1=prod_dd_d(u2,(2*j+2));
        a10=div_dd_dd(u5,s1);
        a11=prod_dd_d(u6,x);
        s2=add_dd_d(u,2*k);
        s3=prod_dd_dd(w,s2);
        a12=div_dd_dd(s3,u4);
        s4=add_dd_dd(a11,a12);
        A1=prod_dd_dd(a10,s4);

        s5=two_sum(j+1+k,alpha);
        s6=two_sum(j+1+k,beta);
        s5.H=-s5.H;
        s5.L=-s5.L;
        s7=prod_dd_dd(s5,s6);
        s8=prod_dd_d(u3,j+2);
        a20=div_dd_dd(s7,s8);
        a21=div_dd_dd(u7,u6);
        A2=prod_dd_dd(a20,a21);
//--------------iteration-----------------------------------//
        temp1=two_prod(u9.H,P[j+k]);
        temp2=two_prod(A1.H,b1);
        temp3=two_prod(A2.H,b2);
        temp4=two_sum(temp1.H,temp2.H);
        temp5=two_sum(temp3.H,temp4.H);
        temp=temp1.L+temp2.L+temp3.L+temp4.L+temp5.L;
        err_temp=u9.L*P[j+k]+A1.L*b1+A2.L*b2+A1.H*errb1+A2.H*errb2+temp;

        b2=b1;
        b1=temp5.H;
        errb2=errb1;
        errb1=err_temp;
    }
    res=quick_two_sum(C*b1,C*errb1);
    return  res;
}

/*----------------------- JacobValwErr evaluates a series of Jacobobi polynomial at the point x, which is in [-1, 1], and performs a running-error bound */
double JacobValwErr(double *P, unsigned int n, double x, double alpha, double beta, double * runerrbound)
{
    assert ( 0 < n);
    assert (fabs(x)<=1.0);
    assert (alpha>-1.0);
    assert (beta>-1.0);
    double  t,b1=0,b2=0;
    int j;
    double  abst=0,absb1=0;
    int na,nb;
    na=6;
    nb=5;
    double errz,errz1,errz2, errztemp;
    errz1=0;
    errz2=0;
    errztemp=0;
    double coef1;

    double *A=(double *) malloc(sizeof(double)*(n+2));
    double *B=(double *) malloc(sizeof(double)*(n+2));
    double *C=(double *) malloc(sizeof(double)*(n+2));

    RecurCoefJacob(n,alpha,beta,A,B,C);

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

/*----------------------- JacobDerwErr evaluates the first derivative of a series of Jacobi polynomial at the point x, which is in [-1, 1], and performs a running-error bound  */
double JacobDerwErr(double *P, unsigned int n, double x, double alpha, double beta, double * runerrbound)
{
    assert ( 0 < n);
    assert (fabs(x)<=1.0);
    assert (alpha>-1.0);
    assert (beta>-1.0);
    double  t,b1=0,b2=0;
    double Ac, A1, A2;
    double a10,a11,a12,a20,a21;
    int i;
    double j;
    double C=0.5;

    double  abst=0,absb1=0;
    int na,nb,nc;
    na=7;
    nb=5;
    nc=2;
    double errz,errz1,errz2, errztemp;
    errz1=0;
    errz2=0;
    errztemp=0;

    for(i=n-1; i>=0; i--)
    {
        j=1.0*i;
//-------------recurrence coefficients-----------------------//
        Ac=j+2+alpha+beta;
        a10=(2*j+3+alpha+beta)/((2*j+2)*(j+3+alpha+beta));
        a11=(2*j+4+alpha+beta)*x;
        a12=((alpha-beta)*(alpha+beta+2))/(alpha+beta+2*j+2);
        A1=a10*(a11+a12);
        a20=-(j+2+alpha)*(j+2+beta)/((j+2)*(alpha+beta+j+4));
        a21=(alpha+beta+2*j+6)/(alpha+beta+2*j+4);
        A2=a20*a21;
//--------------iteration-----------------------------------//
        t=A1*b1+A2*b2+Ac*P[i+1];
        abst=fabs(t);

        errz=errz1*fabs(A1)+errz2*fabs(A2)+(nc+2)*fabs(Ac)*fabs(P[i+1]);
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

/*----------------------- JacobDerKwErr evaluates  the k-th derivative of a series of Jacobi polynomial at the point x, which is in [-1, 1], and performs a running-error bound */
double JacobDerKwErr(double *P, unsigned int n, double x, double alpha, double beta, double * runerrbound, unsigned int k)
{
    assert ( 0 < n);
    assert (fabs(x)<=1.0);
    assert (alpha>-1.0);
    assert (beta>-1.0);
    double  t,b1=0,b2=0;
    double A1, A2,s;
    double a10,a11,a12,a20,a21;
    int i,j;
    double C=1.0;
    for(i=k;i>0;i--)
    {
        C=C/2.0;
    }

    double  abst=0,absb1=0;
    int na,nb,nc;
    na=7;
    nb=5;
    nc=k+1;
    if (k==0){
      na=6;
      nc=1;
    }
    double errz,errz1,errz2, errztemp;
    errz1=0;
    errz2=0;
    errztemp=0;

    for(i=n-k; i>=0; i--)
    {
//-------------recurrence coefficients-----------------------//
        s=1.0;
        for(j=1;j<=k;j++)
        {
            s=s*(alpha+beta+i+k+j);
        }

        a10=(2*i+1+2*k+alpha+beta)/((2*i+2)*(i+1+2*k+alpha+beta));
        a11=(2*i+2+2*k+alpha+beta)*x;
        a12=((alpha-beta)*(alpha+beta+2*k))/(alpha+beta+2*i+2*k);
        A1=a10*(a11+a12);

        a20=-(i+1+k+alpha)*(i+1+k+beta)/((i+2)*(alpha+beta+i+2+2*k));
        a21=(alpha+beta+2*i+4+2*k)/(alpha+beta+2*i+2+2*k);
        A2=a20*a21;
//--------------iteration-----------------------------------//
        t=A1*b1+A2*b2+s*P[i+k];
        abst=fabs(t);

        errz=errz1*fabs(A1)+errz2*fabs(A2)+(nc+2)*fabs(s)*fabs(P[i+k]);
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

/*----------------------- CompJacobValwErr evaluates a series of Jacobi polynomial  at the point x, which is in [-1, 1], with compensated method, and performs a running-error bound */
double CompJacobValwErr(double *P, unsigned int n, double x, double alpha, double beta, double * runerrbound)
{
    assert ( 0 < n);
    assert (fabs(x)<=1.0);
    assert (alpha>-1.0);
    assert (beta>-1.0);
    double  b1=0,b2=0;
    double  res,err_temp, err_temp1, err_temp2, err_temp3, errb1=0, errb2=0;
    dd_real temp1,temp2,temp3,temp4,temp5,temp6;
    int j;

    double  abst=0,absb1=0;
    int na,nb,ne;
    na=6;
    nb=5;
    ne=9;
    double errz,errz1,errz2, errztemp;
    errz1=0;
    errz2=0;
    errztemp=0;

    dd_real *A=(dd_real *) malloc(sizeof(dd_real)*(n+2));
    dd_real *B=(dd_real *) malloc(sizeof(dd_real)*(n+2));
    dd_real *C=(dd_real *) malloc(sizeof(dd_real)*(n+2));

    RecurCoefJacob_DD(n,alpha,beta,A,B,C);

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

/*----------------------- CompJacobDerwErr evaluates  the first derivative of a series of Jacobi polynomial  at the point x, which is in [-1, 1], with compensated method, and performs a running-error bound */
double CompJacobDerwErr(double *P, unsigned int n, double x, double alpha, double beta, double * runerrbound)
{
    assert ( 0 < n);
    assert (fabs(x)<=1.0);
    assert (alpha>-1.0);
    assert (beta>-1.0);
    double  b1=0,b2=0;
    double  res,err_temp, errb1=0, errb2=0;
    dd_real temp1,temp2,temp3,temp4,temp5;
    dd_real Ac, A1, A2;
    dd_real a10,a11,a12,a20,a21;
    dd_real u,u1,u2,u3,u4,u5,u6,u7;
    dd_real w,s1,s2,s3,s4,s5,s6,s7,s8;
    int i;
    double j;
    double C=0.5;

    double  abst=0,absb1=0;
    int na,nb,ne;
    na=7;
    nb=5;
    ne=8;
    double errz,errz1,errz2, errztemp, err_P;
    errz1=0;
    errz2=0;
    errztemp=0;

    u=two_sum(alpha,beta);
    w=two_sum(alpha,-beta);

    for(i=n-1; i>=0; i--)
    {
        j=1.0*i;
        u1=add_dd_d(u,j+2);
        u2=add_dd_d(u,j+3);
        u3=add_dd_d(u,j+4);
        u4=add_dd_d(u,2*j+2);
        u5=add_dd_d(u,2*j+3);
        u6=add_dd_d(u,2*j+4);
        u7=add_dd_d(u,2*j+6);
//-------------recurrence coefficients-----------------------//
        Ac=u1;

        s1=prod_dd_d(u2,(2*j+2));
        a10=div_dd_dd(u5,s1);
        a11=prod_dd_d(u6,x);
        s2=add_dd_d(u,2);
        s3=prod_dd_dd(w,s2);
        a12=div_dd_dd(s3,u4);
        s4=add_dd_dd(a11,a12);
        A1=prod_dd_dd(a10,s4);

        s5=two_sum(j+2,alpha);
        s6=two_sum(j+2,beta);
        s5.H=-s5.H;
        s5.L=-s5.L;
        s7=prod_dd_dd(s5,s6);
        s8=prod_dd_d(u3,j+2);
        a20=div_dd_dd(s7,s8);
        a21=div_dd_dd(u7,u6);
        A2=prod_dd_dd(a20,a21);
//--------------iteration-----------------------------------//
        temp1=two_prod(Ac.H,P[i+1]);
        temp2=two_prod(A1.H,b1);
        temp3=two_prod(A2.H,b2);
        temp4=two_sum(temp1.H,temp2.H);
        temp5=two_sum(temp3.H,temp4.H);
//--------------the compensated part------------------------//
        err_P=Ac.L*P[i+1]+A1.L*b1+A2.L*b2+temp1.L+temp2.L+temp3.L+temp4.L+temp5.L;
        err_temp=A1.H*errb1+A2.H*errb2+err_P;
//--------------the runninge error bound-------------------//
        abst=fabs(err_temp);
        errz=errz1*fabs(A1.H)+errz2*fabs(A2.H)+(ne+1)*fabs(err_P);
        errz2=errztemp+(nb+2)*absb1;
        errz1=errz+(na+3)*abst;

        b2=b1;
        b1=temp5.H;

        errb2=errb1;
        errb1=err_temp;

        errztemp=errz;
        absb1=abst;
    }
    res=C*(b1+errb1);
    *(runerrbound)=((errz1-(1+na)*abst)*unit*C+fabs(C*errb1-(res-C*b1)))/(1-unit*2);

    return  res;
}

/*----------------------- CompJacobDerKwErr evaluates  the k-th derivative of a series of Jacobi polynomial  at the point x, which is in [-1, 1], with compensated method, and performs a running-error bound */
double CompJacobDerKwErr(double *P, unsigned int n, double x, double alpha, double beta, double * runerrbound, unsigned int k)
{
    assert ( 0 < n);
    assert (fabs(x)<=1.0);
    assert (alpha>-1.0);
    assert (beta>-1.0);
    double  b1=0,b2=0;
    double  res,err_temp, errb1=0, errb2=0;
    dd_real temp1,temp2,temp3,temp4,temp5;
    dd_real A1, A2;
    dd_real a10,a11,a12,a20,a21;
    dd_real u,u1,u2,u3,u4,u5,u6,u7,u8,u9;
    dd_real w,s1,s2,s3,s4,s5,s6,s7,s8;
    int i,j;
    double C=1.0;
    for(i=k;i>0;i--)
    {
        C=C/2.0;
    }
    u=two_sum(alpha,beta);
    w=two_sum(alpha,-beta);

    double  abst=0,absb1=0;
    int na,nb,ne;
    na=7;
    nb=5;
    ne=8;
    if(k==0){
        na=6;
        ne=9;
    }
    double errz,errz1,errz2, errztemp, err_P;
    errz1=0;
    errz2=0;
    errztemp=0;

    u=two_sum(alpha,beta);
    w=two_sum(alpha,-beta);

    for(j=n-k; j>=0; j--)
    {
        u1=add_dd_d(u,j+k+1);
        u2=add_dd_d(u,j+2*k+1);
        u3=add_dd_d(u,j+2*k+2);
        u4=add_dd_d(u,2*j+2*k);
        u5=add_dd_d(u,2*j+2*k+1);
        u6=add_dd_d(u,2*j+2*k+2);
        u7=add_dd_d(u,2*j+2*k+4);
//-------------recurrence coefficients-----------------------//
        u9.H=1.0;
        u9.L=0.0;
        for(i=1;i<=k;i++)
        {
            u8=add_dd_d(u1,i-1);
            u9=prod_dd_dd(u9,u8);
        }

        s1=prod_dd_d(u2,(2*j+2));
        a10=div_dd_dd(u5,s1);
        a11=prod_dd_d(u6,x);
        s2=add_dd_d(u,2*k);
        s3=prod_dd_dd(w,s2);
        a12=div_dd_dd(s3,u4);
        s4=add_dd_dd(a11,a12);
        A1=prod_dd_dd(a10,s4);

        s5=two_sum(j+1+k,alpha);
        s6=two_sum(j+1+k,beta);
        s5.H=-s5.H;
        s5.L=-s5.L;
        s7=prod_dd_dd(s5,s6);
        s8=prod_dd_d(u3,j+2);
        a20=div_dd_dd(s7,s8);
        a21=div_dd_dd(u7,u6);
        A2=prod_dd_dd(a20,a21);
//--------------iteration-----------------------------------//
        temp1=two_prod(u9.H,P[j+k]);
        temp2=two_prod(A1.H,b1);
        temp3=two_prod(A2.H,b2);
        temp4=two_sum(temp1.H,temp2.H);
        temp5=two_sum(temp3.H,temp4.H);
//--------------the compensated part------------------------//
        err_P=u9.L*P[j+k]+A1.L*b1+A2.L*b2+temp1.L+temp2.L+temp3.L+temp4.L+temp5.L;
        err_temp=A1.H*errb1+A2.H*errb2+err_P;
//--------------the runninge error bound-------------------//
        abst=fabs(err_temp);
        errz=errz1*fabs(A1.H)+errz2*fabs(A2.H)+(ne+1)*fabs(err_P);

        errz2=errztemp+(nb+2)*absb1;
        errz1=errz+(na+3)*abst;

        b2=b1;
        b1=temp5.H;

        errb2=errb1;
        errb1=err_temp;

        errztemp=errz;
        absb1=abst;
    }
    res=C*(b1+errb1);
    *(runerrbound)=((errz1-(2+na)*abst)*unit*C+fabs(C*errb1-(res-C*b1)))/(1-unit*3);

    return  res;
}

/*----------------------- AccJacobValwErr evaluates a series of Jacobi polynomial  at the point x, which is in [-1, 1], with compensated method, and performs a running-error bound */
dd_real AccJacobValwErr(double *P, unsigned int n, double x, double alpha, double beta, double * runerrbound)
{
    assert ( 0 < n);
    assert (fabs(x)<=1.0);
    assert (alpha>-1.0);
    assert (beta>-1.0);
    double  b1=0,b2=0;
    double  err_temp, err_temp1, err_temp2, err_temp3, errb1=0, errb2=0;
    dd_real res,temp1,temp2,temp3,temp4,temp5,temp6;
    int j;

    double  abst=0,absb1=0;
    int na,nb,ne;
    na=6;
    nb=5;
    ne=9;
    double errz,errz1,errz2, errztemp;
    errz1=0;
    errz2=0;
    errztemp=0;

    dd_real *A=(dd_real *) malloc(sizeof(dd_real)*(n+2));
    dd_real *B=(dd_real *) malloc(sizeof(dd_real)*(n+2));
    dd_real *C=(dd_real *) malloc(sizeof(dd_real)*(n+2));

    RecurCoefJacob_DD(n,alpha,beta,A,B,C);

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

/*----------------------- AccJacobDerwErr evaluates  the first derivative of a series of Jacobi polynomial  at the point x, which is in [-1, 1], with compensated method, and performs a running-error bound */
dd_real AccJacobDerwErr(double *P, unsigned int n, double x, double alpha, double beta, double * runerrbound)
{
    assert ( 0 < n);
    assert (fabs(x)<=1.0);
    assert (alpha>-1.0);
    assert (beta>-1.0);
    double  b1=0,b2=0;
    double  err_temp, errb1=0, errb2=0;
    dd_real res,temp1,temp2,temp3,temp4,temp5;
    dd_real Ac, A1, A2;
    dd_real a10,a11,a12,a20,a21;
    dd_real u,u1,u2,u3,u4,u5,u6,u7;
    dd_real w,s1,s2,s3,s4,s5,s6,s7,s8;
    int i;
    double j;
    double C=0.5;

    double  abst=0,absb1=0;
    int na,nb,ne;
    na=7;
    nb=5;
    ne=8;
    double errz,errz1,errz2, errztemp, err_P;
    errz1=0;
    errz2=0;
    errztemp=0;

    u=two_sum(alpha,beta);
    w=two_sum(alpha,-beta);

    for(i=n-1; i>=0; i--)
    {
        j=1.0*i;
        u1=add_dd_d(u,j+2);
        u2=add_dd_d(u,j+3);
        u3=add_dd_d(u,j+4);
        u4=add_dd_d(u,2*j+2);
        u5=add_dd_d(u,2*j+3);
        u6=add_dd_d(u,2*j+4);
        u7=add_dd_d(u,2*j+6);
//-------------recurrence coefficients-----------------------//
        Ac=u1;

        s1=prod_dd_d(u2,(2*j+2));
        a10=div_dd_dd(u5,s1);
        a11=prod_dd_d(u6,x);
        s2=add_dd_d(u,2);
        s3=prod_dd_dd(w,s2);
        a12=div_dd_dd(s3,u4);
        s4=add_dd_dd(a11,a12);
        A1=prod_dd_dd(a10,s4);

        s5=two_sum(j+2,alpha);
        s6=two_sum(j+2,beta);
        s5.H=-s5.H;
        s5.L=-s5.L;
        s7=prod_dd_dd(s5,s6);
        s8=prod_dd_d(u3,j+2);
        a20=div_dd_dd(s7,s8);
        a21=div_dd_dd(u7,u6);
        A2=prod_dd_dd(a20,a21);
//--------------iteration-----------------------------------//
        temp1=two_prod(Ac.H,P[i+1]);
        temp2=two_prod(A1.H,b1);
        temp3=two_prod(A2.H,b2);
        temp4=two_sum(temp1.H,temp2.H);
        temp5=two_sum(temp3.H,temp4.H);
//--------------the compensated part------------------------//
        err_P=Ac.L*P[i+1]+A1.L*b1+A2.L*b2+temp1.L+temp2.L+temp3.L+temp4.L+temp5.L;
        err_temp=A1.H*errb1+A2.H*errb2+err_P;
//--------------the runninge error bound-------------------//
        abst=fabs(err_temp);
        errz=errz1*fabs(A1.H)+errz2*fabs(A2.H)+(ne+1)*fabs(err_P);

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
    res.H=C*res.H;
    res.L=C*res.L;
    *(runerrbound)=(errz1-(1+na)*abst)*unit*C;

    return  res;
}

/*----------------------- AccJacobDerKwErr evaluates  the k-th derivative of a series of Jacobi polynomial  at the point x, which is in [-1, 1], with compensated method, and performs a running-error bound */
dd_real AccJacobDerKwErr(double *P, unsigned int n, double x, double alpha, double beta, double * runerrbound, unsigned int k)
{
    assert ( 0 < n);
    assert (fabs(x)<=1.0);
    assert (alpha>-1.0);
    assert (beta>-1.0);
    double  b1=0,b2=0;
    double  err_temp, errb1=0, errb2=0;
    dd_real res,temp1,temp2,temp3,temp4,temp5;
    dd_real A1, A2;
    dd_real a10,a11,a12,a20,a21;
    dd_real u,u1,u2,u3,u4,u5,u6,u7,u8,u9;
    dd_real w,s1,s2,s3,s4,s5,s6,s7,s8;
    int i,j;
    double C=1.0;
    for(i=k;i>0;i--)
    {
        C=C/2.0;
    }
    u=two_sum(alpha,beta);
    w=two_sum(alpha,-beta);

    double  abst=0,absb1=0;
    int na,nb,ne;
    na=7;
    nb=5;
    ne=8;
    if(k==0){
        na=6;
        ne=9;
    }
   double errz,errz1,errz2, errztemp, err_P;
    errz1=0;
    errz2=0;
    errztemp=0;

    u=two_sum(alpha,beta);
    w=two_sum(alpha,-beta);

    for(j=n-k; j>=0; j--)
    {
        u1=add_dd_d(u,j+k+1);
        u2=add_dd_d(u,j+2*k+1);
        u3=add_dd_d(u,j+2*k+2);
        u4=add_dd_d(u,2*j+2*k);
        u5=add_dd_d(u,2*j+2*k+1);
        u6=add_dd_d(u,2*j+2*k+2);
        u7=add_dd_d(u,2*j+2*k+4);
//-------------recurrence coefficients-----------------------//
        u9.H=1.0;
        u9.L=0.0;
        for(i=1;i<=k;i++)
        {
            u8=add_dd_d(u1,i-1);
            u9=prod_dd_dd(u9,u8);
        }

        s1=prod_dd_d(u2,(2*j+2));
        a10=div_dd_dd(u5,s1);
        a11=prod_dd_d(u6,x);
        s2=add_dd_d(u,2*k);
        s3=prod_dd_dd(w,s2);
        a12=div_dd_dd(s3,u4);
        s4=add_dd_dd(a11,a12);
        A1=prod_dd_dd(a10,s4);

        s5=two_sum(j+1+k,alpha);
        s6=two_sum(j+1+k,beta);
        s5.H=-s5.H;
        s5.L=-s5.L;
        s7=prod_dd_dd(s5,s6);
        s8=prod_dd_d(u3,j+2);
        a20=div_dd_dd(s7,s8);
        a21=div_dd_dd(u7,u6);
        A2=prod_dd_dd(a20,a21);
//--------------iteration-----------------------------------//
        temp1=two_prod(u9.H,P[j+k]);
        temp2=two_prod(A1.H,b1);
        temp3=two_prod(A2.H,b2);
        temp4=two_sum(temp1.H,temp2.H);
        temp5=two_sum(temp3.H,temp4.H);
//--------------the compensated part------------------------//
        err_P=u9.L*P[j+k]+A1.L*b1+A2.L*b2+temp1.L+temp2.L+temp3.L+temp4.L+temp5.L;
        err_temp=A1.H*errb1+A2.H*errb2+err_P;
//--------------the runninge error bound-------------------//
        abst=fabs(err_temp);
        errz=errz1*fabs(A1.H)+errz2*fabs(A2.H)+(ne+1)*fabs(err_P);

        errz2=errztemp+(nb+2)*absb1;
        errz1=errz+(na+3)*abst;

        b2=b1;
        b1=temp5.H;

        errb2=errb1;
        errb1=err_temp;

        errztemp=errz;
        absb1=abst;
    }
    res=quick_two_sum(C*b1,C*errb1);
    *(runerrbound)=(errz1-(2+na)*abst)*unit*C;

    return  res;
}















