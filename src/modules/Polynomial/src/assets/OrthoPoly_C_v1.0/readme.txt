%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Licensing:
%
%    This code is distributed under the GNU General Public License 3 (GPLv3).
%
%  Modified:
%
%   05 October 2017
%
%  Authors:
%
%   Roberto Barrio, Peibing Du, Hao Jiang and Sergio Serrano
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
Outline:

I.   Introduction
II.  Directories and Files
III. Functions
IV.  Example

%----------------------------------------------------------------------------------------

I. Introduction

ORTHOPOLY software permits to evaluate, efficiently and accurately, finite series of any 
classical family of orthogonal polynomials (Chebyshev polynomials of the first and second kind,
Legendre, ultraspherical or Gegenbauer, Jacobi, Hermite and Laguerre orthogonal polynomials) 
and their derivatives. The basic algorithm is the BCS-algorithm (Barrio-Clenshaw-Smith derivative algorithm), 
that permits to evaluate the k-th derivative of a finite series of orthogonal 
polynomials at any point without obtaining before the previous derivatives. The compensated 
BCS-algorithm, based on Error-Free techniques, permits to relegate the influence of the 
conditioning of the problem up to second order in the round-off unit of the computer. 
The running error bounds are also given.

%-------------------------------------------------------------------------------------------

II. Directories and Files

There are three directories and this file in the main directory of this distribution, 
described below

src  	This directory contains the source code of package OrthoPoly. This source code also includes
	inline functions (double-double arithmetic, and error free transformation function).

include  This directory contains the header files for each orthogonal polynomial and inline fuctions.

example	 This directory contains an example using the functions provided for Chebyshev polynomials
         and the output for that example.

%----------------------------------------------------------------------------------------

III. Functions

OrthoPoly_C: the functions as follows are provided and can be found in directory src.

1. Jacobi:
	1.1 JacobVal:
	1.2 CompJacobVal:
	1.3 AccJacobVal:
	1.4 JacobValwErr:
        1.5 CompJacobValwErr:
	1.6 AccJacobValwErr:

	1.7  JacobDer:
	1.8  CompJacobDer:
	1.9  AccJacobDer:
	1.10 JacobDerwErr:
	1.11 CompJacobDerwErr:
	1.12 AccJacobDerwErr:

	1.13 JacobDerK:
	1.14 CompJacobDerK:
	1.15 AccJacobDerK:
	1.16 JacobDerwErrK:
	1.17 CompJacobDerKwErr:
	1.18 AccJacobDerKwErr:

2. Gegenbauber:

	2.1 GegenVal:
	2.2 CompGegenVal:
	2.3 AccGegenVal:
	2.4 GegenValwErr:
	2.5 CompGegenValwErr:
	2.6 AccGegenValwErr:

	2.7  GegenDer:
	2.8  CompGegenDer:
	2.9  AccGegenDer:
	2.10 GegenDerwErr:
	2.11 CompGegenDerwErr:
	2.12 AccGegenDerwErr:

	2.13 GegenDerK:
	2.14 CompGegenDerK:
	2.15 AccGegenDerK:
	2.16 GegenDerwErrK:
	2.17 CompGegenDerKwErr:
	2.18 AccGegenDerKwErr:

3. Chebyshev:

	3.1 Cheb1Val:
	3.2 CompCheb1Val:
	3.3 AccCheb1Val:
	3.4 Cheb1ValwErr:
	3.5 CompCheb1ValwErr:
	3.6 AccCheb1ValwErr:

	3.7  Cheb2Val:
	3.8  CompCheb2Val:
	3.9  AccCheb2Val:
	3.10 Cheb2ValwErr:
	3.11 CompCheb2ValwErr:
	3.12 AccCheb2ValwErr:


	3.13 Cheb1Der:
	3.14 CompCheb1Der:
	3.15 AccCheb1Der:
	3.16 Cheb1DerwErr:
	3.17 CompCheb1DerwErr:
	3.18 AccCheb1DerwErr:

	3.19 Cheb2Der:
	3.20 CompCheb2Der:
	3.21 AccCheb2Der:
	3.22 Cheb2DerwErr:
	3.23 CompCheb2DerwErr:
	3.24 AccCheb2DerwErr:


	3.25 Cheb1DerK:
	3.26 CompCheb1DerK:
	3.27 AccCheb1DerK:
	3.28 Cheb1DerKwErr:
	3.29 CompCheb1DerKwErr:
	3.30 AccCheb1DerKwErr:

	3.31 Cheb2DerK:
	3.32 CompCheb2DerK:
	3.33 AccCheb2DerK:
	3.34 Cheb2DerKwErr:
	3.35 CompCheb2DerKwErr:
	3.36 AccCheb2DerKwErr:

4. Legendre:

	4.1 LegenVal:
	4.2 CompLegenVal:
	4.3 AccLegenVal:
	4.4 LegenValwErr:
	4.5 CompLegenValwErr:
	4.6 AccLegenValwErr:

	4.7  LegenDer:
	4.8  CompLegenDer:
	4.9  AccLegenDer:
	4.10 LegenDerwErr:
	4.11 CompLegenDerwErr:
	4.12 AccLegenDerwErr:

	4.13  LegenDerK:
	4.14  CompLegenDerK:
	4.15  AccLegenDerK:
	4.16  LegenDerwErrK:
	4.17  CompLegenDerKwErr:
	4.18  AccLegenDerKwErr:

5. Laguerre:

	5.1 LagueVal:
	5.2 CompLagueVal:
	5.3 AccLagueVal:
	5.4 LagueValwErr:
	5.5 CompLagueValwErr:
	5.6 AccLagueValwErr:

	5.7  LagueDer:
	5.8  CompLagueDer:
	5.9  AccLagueDer:
	5.10 LagueDerwErr:
	5.11 CompLagueDerwErr:
	5.12 AccLagueDerwErr:

	5.13  LagueDerK:
	5.14  CompLagueDerK:
	5.15  AccLagueDerK:
	5.16  LagueDerwErrK:
	5.17  CompLagueDerKwErr:
	5.18  AccLagueDerKwErr:

6. Hermite:

	6.1 Herm1Val:
	6.2 CompHerm1Val:
	6.3 AccHerm1Val:
	6.4 Herm1ValwErr:
	6.5 CompHerm1ValwErr:
	6.6 AccHerm1ValwErr:

	6.7  Herm2Val:
	6.8  CompHerm2Val:
	6.9  AccHerm2Val:
	6.10 Herm2ValwErr:
	6.11 CompHerm2ValwErr:
	6.12 AccHerm2ValwErr:

	6.13 Herm1Der:
	6.14 CompHerm1Der:
	6.15 AccHerm1Der:
	6.16 Herm1DerwErr:
	6.17 CompHerm1DerwErr:
	6.18 AccHerm1DerwErr:

	6.19 Herm2Der:
	6.20 CompHerm2Der:
	6.21 AccHerm2Der:
	6.22 Herm2DerwErr:
	6.23 CompHerm2DerwErr:
	6.24 AccHerm2DerwErr:

	6.25 Herm1DerK:
	6.26 CompHerm1DerK:
	6.27 AccHerm1DerK:
	6.28 Herm1DerKwErr:
	6.29 CompHerm1DerKwErr:
	6.30 AccHerm1DerKwErr:


	6.31 Herm2DerK:
	6.32 CompHerm2DerK:
	6.33 AccHerm2DerK:
	6.34 Herm2DerKwErr:
	6.35 CompHerm2DerKwErr:
	6.36 AccHerm2DerKwErr:

%----------------------------------------------------------------------------------------

IV.  Example

How to generate the programs for test_example_chebyshev.c (with chebyshev series) is presented:

gcc  -O2  -I ./include -Wall -c example/test_example_chebyshev.c -o example/test_example_chebyshev.o
gcc  -O2  -I ./include -Wall -c src/chebyshev_series.c -o example/chebyshev_series.o
gcc  -O2  -I ./include -Wall -c src/inline_function.c -o example/inline_function.o
gcc  -Wall example/test_example_chebyshev.o example/chebyshev_series.o example/inline_function.o -o example/test_example_chebyshev

If other orthogonal polynomials can be used, the only difference in compiling is the need to call the code file for that family. 
The call to functions of other families of polynomials is analogous, 
changing only 'Cheb1' in the name of the function by the five characters corresponding to the family of interest. 
Those families dependent on one parameter (two for Jacobi) have a fourth (and fifth) argument(s) for said parameter(s).

%----------------------------------------------------------------------------------------

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

