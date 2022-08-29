! This program is a part of EASIFEM library
! Copyright (C) 2020-2021  Vikas Sharma, Ph.D
!
! This program is free software: you can redistribute it and/or modify
! it under the terms of the GNU General Public License as published by
! the Free Software Foundation, either version 3 of the License, or
! (at your option) any later version.
!
! This program is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.
!
! You should have received a copy of the GNU General Public License
! along with this program.  If not, see <https: //www.gnu.org/licenses/>
!

MODULE Chebyshev1PolynomialUtility
USE GlobalData
IMPLICIT NONE

!----------------------------------------------------------------------------
!                                              GetChebyshev1RecurrenceCoeff
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2 Aug 2022
! summary: Return the recurrence coefficient for nth order Chebyshev1
! polynomial
!
!
!# Introduction
!
! These recurrence coefficients are for monic jacobi polynomials.

INTERFACE
MODULE PURE SUBROUTINE GetChebyshev1RecurrenceCoeff( n, alphaCoeff, &
  & betaCoeff )
  INTEGER( I4B ), INTENT( IN ) :: n
  REAL( DFP ), INTENT( OUT ) :: alphaCoeff(0:n-1)
  REAL( DFP ), INTENT( OUT ) :: betaCoeff(0:n-1)
END SUBROUTINE GetChebyshev1RecurrenceCoeff
END INTERFACE

PUBLIC :: GetChebyshev1RecurrenceCoeff

!----------------------------------------------------------------------------
!                                                    Chebyshev1LeadingCoeff
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2 Aug 2022
! summary: Leading coefficient of Chebyshev1 polynomial

INTERFACE
MODULE PURE FUNCTION Chebyshev1LeadingCoeff( n ) RESULT( ans )
  INTEGER( I4B ), INTENT( IN ) :: n
    !! order of Chebyshev1 polynomial
  REAL( DFP ) :: ans
    !! answer
END FUNCTION Chebyshev1LeadingCoeff
END INTERFACE

PUBLIC :: Chebyshev1LeadingCoeff

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2 Aug 2022
! summary: Square norm of Chebyshev1 polynomial

INTERFACE
MODULE PURE FUNCTION Chebyshev1NormSQR( n, alpha, beta ) RESULT( ans )
  INTEGER( I4B ), INTENT( IN ) :: n
  REAL( DFP ), INTENT( IN ) :: alpha
  REAL( DFP ), INTENT( IN ) :: beta
  REAL( DFP ) :: ans
END FUNCTION Chebyshev1NormSQR
END INTERFACE

!----------------------------------------------------------------------------
!                                                 Chebyshev1GaussQuadrature
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 3 Aug 2022
! summary: 	Returns the Gauss quadrature points for Chebyshev1 Polynomial

INTERFACE
MODULE SUBROUTINE Chebyshev1GaussQuadrature( n, pt, wt )
  INTEGER( I4B ), INTENT( IN ) :: n
  REAL( DFP ), INTENT( OUT ) :: pt(:)
    !! the size is 1 to n
  REAL( DFP ), INTENT( OUT ) :: wt(:)
    !! the size is 1 to n
END SUBROUTINE Chebyshev1GaussQuadrature
END INTERFACE

PUBLIC :: Chebyshev1GaussQuadrature

!----------------------------------------------------------------------------
!                                            Chebyshev1GaussRadauQuadrature
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 3 Aug 2022
! summary: 	Returns the GaussRadau quadrature points for Chebyshev1 Polynomial

INTERFACE
MODULE SUBROUTINE Chebyshev1GaussRadauQuadrature( n, a, pt, wt )
  INTEGER( I4B ), INTENT( IN ) :: n
  REAL( DFP ), INTENT( IN ) :: a
    !! +1.0 or -1.0
  REAL( DFP ), INTENT( OUT ) :: pt(:)
    !! the size is 1 to n+1
  REAL( DFP ), INTENT( OUT ) :: wt(:)
    !! the size is 1 to n+1
END SUBROUTINE Chebyshev1GaussRadauQuadrature
END INTERFACE

PUBLIC :: Chebyshev1GaussRadauQuadrature

!----------------------------------------------------------------------------
!                                            Chebyshev1GaussLobattoQuadrature
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 3 Aug 2022
! summary: 	Returns the GaussLobatto quadrature points for Chebyshev1
! Polynomial

INTERFACE
MODULE SUBROUTINE Chebyshev1GaussLobattoQuadrature( n, pt, wt )
  INTEGER( I4B ), INTENT( IN ) :: n
  REAL( DFP ), INTENT( OUT ) :: pt(:)
    !! the size is 1 to n+2
  REAL( DFP ), INTENT( OUT ) :: wt(:)
    !! the size is 1 to n+2
END SUBROUTINE Chebyshev1GaussLobattoQuadrature
END INTERFACE

PUBLIC :: Chebyshev1GaussLobattoQuadrature

!----------------------------------------------------------------------------
!                                                            Chebyshev1Zeros
!----------------------------------------------------------------------------

INTERFACE
MODULE FUNCTION Chebyshev1Zeros( n ) RESULT( ans )
  INTEGER( I4B ), INTENT( IN ) :: n
  REAL( DFP ) :: ans( n )
END FUNCTION Chebyshev1Zeros
END INTERFACE

PUBLIC :: Chebyshev1Zeros

END MODULE Chebyshev1PolynomialUtility