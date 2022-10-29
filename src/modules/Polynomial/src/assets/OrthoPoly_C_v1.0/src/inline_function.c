#include<inline.h>

#ifndef SPLITTER
#define SPLITTER 134217729.0               // = 2^27 + 1
#endif
//int const _splitter_ = (1<<27)+1;  



/*********** Basic Functions ************/
/* Computes fl(a+b) and err(a+b).  Assumes |a| >= |b|. */
inline  dd_real  quick_two_sum(double a, double b) {
        dd_real res;
        res.H = a + b;
        res.L = b - (res.H - a);
  return res;
}


/* Computes fl(a+b) and err(a+b).  */
 inline dd_real two_sum(double a, double b) {
        dd_real res;
res.H = a + b;
double bb = res.H - a;
res.L = (a - (res.H - bb)) + (b - bb);
 return res;
}

/* Computes high word and lo word of a */
 inline dd_real split(double a) {
double temp;
        dd_real res;
    temp = SPLITTER * a;
    res.H = temp - (temp - a);
    res.L = a - res.H;
 return res;
}


/* Computes fl(a*b) and err(a*b). */
 inline dd_real two_prod(double a, double b) {
        dd_real res,aa,bb;
res.H = a * b;
aa=split(a);
bb=split(b);
res.L = ((aa.H * bb.H - res.H) + aa.H * bb.L + aa.L * bb.H) + aa.L * bb.L;
 return res;
}


/*********** Additions ************/
/* double+double  equals to two_sum */
/* double-double + double */
 inline dd_real add_dd_d(const dd_real aa, double b) {
dd_real res,temp;
temp=two_sum(aa.H,b);
temp.L+=aa.L;
res=quick_two_sum(temp.H, temp.L);
return res;
}

/* double-double + double-double */
/* the sloppy version from QD library*/

 inline dd_real add_dd_dd(const dd_real aa, const dd_real bb) {
dd_real res,temp;
temp=two_sum(aa.H,bb.H);
temp.L+=(aa.L+bb.L);
res=quick_two_sum(temp.H, temp.L);
return res;
}

/*********** Multiplications ************/
/* double * double equals to two_prod */
/* double-double * double */
 inline dd_real prod_dd_d(const dd_real aa, double b) {
dd_real res,temp;
temp=two_prod(aa.H,b);
temp.L += (aa.L * b);
res=quick_two_sum(temp.H, temp.L);
return res;
}

/* double-double * double-double */
 inline dd_real prod_dd_dd(const dd_real aa, const dd_real bb) {
dd_real res,temp;
temp=two_prod(aa.H,bb.H);
temp.L += (aa.L * bb.H + aa.H * bb.L);
res=quick_two_sum(temp.H, temp.L);
return res;
}



/*********** Divisions ************/
/* double / double */
 inline dd_real div_d_d(double a, double b) {
double q1, q2;
dd_real res,temp1,temp2;
q1 = a / b;
  /* Compute  a - q1 * b */
temp1=two_prod(q1,b);
temp2=two_sum(a,-temp1.H);
temp2.L=temp2.L-temp1.L;
  /* get next approximation */
q2=(temp2.H+temp2.L)/b; 

res=quick_two_sum(q1, q2);
return res;
}

/* double-double / double */
 inline dd_real div_dd_d(const dd_real aa, double b) {
double q1, q2;
dd_real res,temp1,temp2;

q1 = aa.H / b;    
  /* Compute  this - q1 * d */
temp1=two_prod(q1,b);
temp2=two_sum(aa.H,-temp1.H);
temp2.L=temp2.L+aa.L;
temp2.L=temp2.L-temp1.L;
  /* get next approximation. */
q2=(temp2.H+temp2.L)/b; 

res=quick_two_sum(q1, q2);
return res;
}


/* double-double / double-double */
/* the sloppy version from QD library*/
 inline dd_real div_dd_dd(const dd_real aa, const dd_real bb) {
double q1, q2;
dd_real res,temp1,temp2;

q1 = aa.H / bb.H; 
temp1=prod_dd_d(bb,q1);
temp2=two_sum(aa.H,-temp1.H);
temp2.L=temp2.L-temp1.L;
temp2.L=temp2.L+aa.L;
  /* get next approximation */
q2=(temp2.H+temp2.L)/bb.H; 

res=quick_two_sum(q1, q2);
return res;
}
   

/* double-double / double-double */
/* the sloppy version not from QD library*/
 inline dd_real div_d_dd(double a, const dd_real bb) {
double q1, q2;
dd_real res,temp1,temp2;

q1 = a / bb.H; 
temp1=prod_dd_d(bb,q1);
temp2=two_sum(a,-temp1.H);
temp2.L=temp2.L-temp1.L;
  /* get next approximation */
q2=(temp2.H+temp2.L)/bb.H; 

res=quick_two_sum(q1, q2);
return res;
}



/* double-double -  double-double */
/* the sloppy version from QD library*/

 inline dd_real diff_dd_dd(const dd_real aa, const dd_real bb) {
dd_real res,temp;
temp=two_sum(aa.H,-bb.H);
temp.L+=(aa.L-bb.L);
res=quick_two_sum(temp.H, temp.L);
return res;
}


