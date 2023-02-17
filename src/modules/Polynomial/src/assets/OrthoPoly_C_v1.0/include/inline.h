/*
 * include/inline.h
 *
 * OrthoPoly is developed by
 *
 * Roberto Barrio, Hao Jiang, Sergio Serrano and Peibing Du
 *
 * This file contains basic error-free transformation functions.
 * These functions can be used in double-double algorithms 
 * and compensated algorithm.
 */
#ifndef _INLINE_H
#define _INLINE_H

#ifndef SPLITTER
	#define SPLITTER 134217729.0               // = 2^27 + 1
#endif

#ifndef unit
	#define unit  1.110223024625157e-16
#endif


//int const _splitter_ = (1<<27)+1;          // = 2^27+1

/*********** the struct *****************/
typedef struct doubledouble{
        double H;
        double L;
}dd_real;

/****************************************/

 inline dd_real quick_two_sum(double a, double b);
 inline dd_real two_sum(double a, double b);
 inline dd_real split(double a);
 inline dd_real two_prod(double a, double b);

 inline dd_real add_dd_d(const dd_real aa, double b);
 inline dd_real add_dd_dd(const dd_real aa, const dd_real bb);
 inline dd_real prod_dd_d(const dd_real aa, double b);
 inline dd_real prod_dd_dd(const dd_real aa, const dd_real bb);
 inline dd_real div_d_d(double a, double b);
 inline dd_real div_dd_d(const dd_real aa, double b);
 inline dd_real div_dd_dd(const dd_real aa, const dd_real bb);
 inline dd_real div_d_dd(double a, const dd_real bb);



 inline dd_real diff_dd_dd(const dd_real aa, const dd_real bb);

#endif
