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

SUBMODULE(Chebyshev1PolynomialUtility) Methods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                              GetChebyshev1RecurrenceCoeff
!----------------------------------------------------------------------------

MODULE PROCEDURE GetChebyshev1RecurrenceCoeff
  IF( n .LE. 0 ) RETURN
  alphaCoeff = 0.0_DFP
  betaCoeff(0) = pi
  IF( n .EQ. 1 ) RETURN
  betaCoeff(1) = 0.5_DFP
  IF( n .EQ. 2 ) RETURN
  betaCoeff(2:) = 0.25_DFP
END PROCEDURE GetChebyshev1RecurrenceCoeff

!----------------------------------------------------------------------------
!                                                     Chebyshev1LeadingCoeff
!----------------------------------------------------------------------------

MODULE PROCEDURE Chebyshev1LeadingCoeff
  IF( n .EQ. 0_I4B ) THEN
    ans = 1.0_DFP
  ELSE
    ans = 2.0_DFP ** (n-1_I4B)
  END IF
END PROCEDURE Chebyshev1LeadingCoeff

!----------------------------------------------------------------------------
!                                                          Chebyshev1NormSQR
!----------------------------------------------------------------------------

MODULE PROCEDURE Chebyshev1NormSQR
  IF( n .EQ. 0_I4B ) THEN
    ans = pi
  ELSE
    ans = pi/2.0_DFP
  END IF
END PROCEDURE Chebyshev1NormSQR

!----------------------------------------------------------------------------
!                                                   Chebyshev1GaussQuadrature
!----------------------------------------------------------------------------

MODULE PROCEDURE Chebyshev1GaussQuadrature
  pt = Chebyshev1Zeros(n=n)
  wt = pi/n
END PROCEDURE Chebyshev1GaussQuadrature

!----------------------------------------------------------------------------
!                                             Chebyshev1GaussRadauQuadrature
!----------------------------------------------------------------------------

MODULE PROCEDURE Chebyshev1GaussRadauQuadrature
  INTEGER( I4B ) :: ii
  REAL( DFP ) :: avar, avar2
  !!
  avar = 2.0_DFP * pi / (2.0_DFP * n + 1.0_DFP)
  avar2 = pi / (2.0_DFP * n + 1.0_DFP)
  !!
  DO ii = 0, n
    pt( ii+1 ) = -COS( avar*ii )
    wt( ii+1 ) = avar2
  END DO
  !!
  wt( 1 ) = wt( 1 ) / 2.0_DFP
  !!
END PROCEDURE Chebyshev1GaussRadauQuadrature

!----------------------------------------------------------------------------
!                                           Chebyshev1GaussLobattoQuadrature
!----------------------------------------------------------------------------

MODULE PROCEDURE Chebyshev1GaussLobattoQuadrature
  INTEGER( I4B ) :: ii
  REAL( DFP ) :: avar
  !!
  avar = pi / (n + 1.0_DFP)
  !!
  DO ii = 0, n+1
    pt( ii+1 ) = -COS( avar*ii )
    wt( ii+1 ) = avar
  END DO
  !!
  wt( 1 ) = wt( 1 ) / 2.0_DFP
  wt( n+2 ) = wt( n+2 ) / 2.0_DFP
  !!
END PROCEDURE Chebyshev1GaussLobattoQuadrature

!----------------------------------------------------------------------------
!                                                           Chebyshev1Zeros
!----------------------------------------------------------------------------

MODULE PROCEDURE Chebyshev1Zeros
  INTEGER( I4B ) :: ii
  DO ii = 1, n
    ans( ii ) = -COS( (2.0_DFP*ii-1.0_DFP)*pi*0.5_DFP/n )
  END DO
END PROCEDURE Chebyshev1Zeros

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE Methods