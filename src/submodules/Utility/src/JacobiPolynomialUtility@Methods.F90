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

SUBMODULE(JacobiPolynomialUtility) Methods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                   GetJacobiRecurrenceCoeff
!----------------------------------------------------------------------------

MODULE PROCEDURE GetJacobiRecurrenceCoeff
  REAL( DFP ), PARAMETER :: two = 2.0_DFP, four=4.0_DFP
  REAL( DFP ) :: ab1, ab, ab2, abm1, bma,ab3, b2ma2, ab4
  INTEGER( I4B ) :: ii
  !!
  IF( n .LE. 0 ) RETURN
  !!
  ab = alpha + beta
  ab1 = ab + 1.0_DFP
  abm1 = ab - 1.0_DFP
  bma = beta - alpha
  ab2 = ab1 + 1.0_DFP
  ab3 = ab2 + 1.0_DFP
  ab4 = ab3 + 1.0_DFP
  b2ma2 = beta*beta - alpha*alpha
  !!
  !! beta 0
  !!
  betaCoeff(0) = two ** ( ab1 ) * GAMMA( alpha + 1.0_DFP ) &
    & * GAMMA( beta+1.0_DFP ) &
    & / GAMMA( ab1+1.0_DFP )
  !!
  !! alpha 0
  !!
  alphaCoeff( 0 ) =  bma / ab2
  !!
  !! Return if n = 1
  !!
  IF( n .EQ. 1 ) RETURN
  !!
  betaCoeff(1) = four * (1.0_DFP+alpha) * (1.0_DFP+beta) / (ab2*ab2*ab3)
  alphaCoeff(1) = b2ma2 / (ab2*ab4)
  !!
  !! Now it safe to compute other coefficients
  !!
  DO ii = 2, n-1
    !!
    betaCoeff( ii ) = four * ii * (ii+alpha) * (ii+beta) * (ii+ab) &
      & / (ab+2.0*ii)**2 / (ab1+2.0*ii) / (abm1 + 2.0*ii)
    !!
    alphaCoeff( ii ) = b2ma2 / ( ab+2.0*ii ) / ( ab2 + 2.0*ii )
    !!
  END DO
  !!
END PROCEDURE GetJacobiRecurrenceCoeff

!----------------------------------------------------------------------------
!                                                       JacobiLeadingCoeff
!----------------------------------------------------------------------------

MODULE PROCEDURE JacobiLeadingCoeff
  ans = GAMMA( 2.0_DFP*n + alpha + beta + 1.0_DFP) / GAMMA( n + 1.0_DFP ) / &
    & GAMMA( n + alpha + beta + 1.0_DFP  ) / 2.0_DFP ** n
END PROCEDURE JacobiLeadingCoeff

!----------------------------------------------------------------------------
!                                                             JacobiNormSqr
!----------------------------------------------------------------------------

MODULE PROCEDURE JacobiNormSqr
  REAL( DFP ) :: a1, a2, a3, b1, b2, b3
  a1 = 2.0 ** ( alpha + beta + 1.0_DFP )
  a2 = GAMMA( n + alpha + 1.0_DFP )
  a3 = GAMMA( n + beta + 1.0_DFP )
  b1 = 2.0_DFP * n + alpha + beta + 1.0_DFP
  b2 = Factorial( n )
  b3 = GAMMA( n + alpha + beta + 1.0_DFP )
  ans = a1 * a2 * a3 / b1 / b2 / b3
END PROCEDURE JacobiNormSqr

!----------------------------------------------------------------------------
!                                                         JacobiJacobiMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE JacobiJacobiMatrix
  REAL( DFP ), DIMENSION( 0:n-1 ) :: alphaCoeff0, betaCoeff0
  !!
  IF( n .LT. 1 ) RETURN
  !!
  CALL GetJacobiRecurrenceCoeff( n=n, alpha=alpha, beta=beta, &
    & alphaCoeff=alphaCoeff0, betaCoeff=betaCoeff0 )
  IF( PRESENT( alphaCoeff ) ) alphaCoeff( 0:n-1 ) = alphaCoeff0
  IF( PRESENT( betaCoeff ) ) betaCoeff( 0:n-1 ) = betaCoeff0
  CALL JacobiMatrix( alphaCoeff=alphaCoeff0, &
    & betaCoeff=betaCoeff0, D=D, E=E )
  !!
END PROCEDURE JacobiJacobiMatrix

!----------------------------------------------------------------------------
!                                                     JacobiGaussQuadrature
!----------------------------------------------------------------------------

MODULE PROCEDURE JacobiGaussQuadrature
  REAL( DFP ) :: beta0, Z(n, n), betaCoeff( 0:n-1 )
  INTEGER( I4B ) :: ii
  !!
  CALL JacobiJacobiMatrix( n=n, alpha=alpha, beta=beta, D=pt, &
    & E=wt, betaCoeff=betaCoeff )
  !!
#ifdef USE_LAPACK95
  CALL STEV( D=pt, E=wt, Z=Z )
  DO ii = 1, n
    wt( ii ) = betaCoeff( 0 ) * Z( 1, ii )**2
  END DO
  !!
#else
  CALL ErrorMsg( &
    & msg="The subroutine requires Lapack95 package", &
    & file = __FILE__, &
    & routine= "JacobiGaussQuadrature", &
    & line= __LINE__, &
    & unitno = stdout )
#endif
  !!
END PROCEDURE JacobiGaussQuadrature


!----------------------------------------------------------------------------
!                                                    JacobiJacobiRadauMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE JacobiJacobiRadauMatrix
  REAL( DFP ) :: avar, r1, r2, r3, ab,ab2
  !!
  IF( n .LT. 1 ) RETURN
  !!
  CALL JacobiJacobiMatrix( n=n, alpha=alpha, beta=beta, &
    & D=D, E=E, alphaCoeff=alphaCoeff, betaCoeff=betaCoeff )
  !!
  r1 = (1.0-a)*n*(n+alpha) - (1.0+a)*n*(n+beta)
  r2 = 2.0*n+alpha+beta
  r3 = r2 + 1.0
  avar = a + r1/r2/r3
  D(n+1) = avar
  !!
  ab = alpha + beta
  ab2 = ab + 2.0_DFP
  IF( n .EQ. 1 ) THEN
    avar = 4.0_DFP * (1.0_DFP+alpha) * (1.0_DFP+beta) / (ab2*ab2*(ab2+1.0))
  ELSE
    avar = 4.0_DFP * n * (n+alpha) * (n+beta) * (n+ab) &
      & / (ab+2.0*n)**2 / (ab+1.0+2.0*n) / (ab-1.0 + 2.0*n)
  END IF
  !!
  E( n ) = SQRT( avar )
  !!
END PROCEDURE JacobiJacobiRadauMatrix

!----------------------------------------------------------------------------
!                                                JacobiGaussRadauQuadrature
!----------------------------------------------------------------------------

MODULE PROCEDURE JacobiGaussRadauQuadrature
  !!
  REAL( DFP ) :: beta0, Z(n+1, n+1), betaCoeff( 0:n )
  INTEGER( I4B ) :: ii
  !!
  CALL JacobiJacobiRadauMatrix( a=a, n=n, alpha=alpha, beta=beta, D=pt, &
    & E=wt, betaCoeff=betaCoeff )
  !!
#ifdef USE_LAPACK95
  !!
  CALL STEV( D=pt, E=wt, Z=Z )
  DO ii = 1, n+1
    wt( ii ) = betaCoeff( 0 ) * Z( 1, ii )**2
  END DO
  !!
#else
  CALL ErrorMsg( &
    & msg="The subroutine requires Lapack95 package", &
    & file = __FILE__, &
    & routine= "JacobiGaussRadauQuadrature", &
    & line= __LINE__, &
    & unitno = stdout )
#endif
  !!
END PROCEDURE JacobiGaussRadauQuadrature

!----------------------------------------------------------------------------
!                                                  JacobiJacobiLobattoMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE JacobiJacobiLobattoMatrix
  !!
  REAL( DFP ) :: avar, r1, r2, r3, ab
  !!
  IF( n .LT. 1 ) RETURN
  !!
  CALL JacobiJacobiMatrix(  &
    & n=n+1, &
    & alpha=alpha, &
    & beta=beta, &
    & D=D, &
    & E=E, &
    & alphaCoeff=alphaCoeff, &
    & betaCoeff=betaCoeff )
  !!
  r1 = alpha - beta
  r2 = 2.0*n + alpha + beta + 2.0_DFP
  r3 = 1.0
  avar = r1/r2/r3
  D(n+2) = avar
  !!
  ab = alpha + beta
  r1 = 4.0_DFP * (n+alpha+1.0) * (n+beta+1.0) * (n+ab+1.0)
  r2 = 2.0*n + ab + 1.0
  r3 = ( r2 + 1.0 )**2
  !!
  E( n+1 ) = SQRT( r1 / r2 / r3 )
  !!
END PROCEDURE JacobiJacobiLobattoMatrix

!----------------------------------------------------------------------------
!                                              JacobiGaussLobattoQuadrature
!----------------------------------------------------------------------------

MODULE PROCEDURE JacobiGaussLobattoQuadrature
  !!
  REAL( DFP ) :: beta0, Z(n+2, n+2), betaCoeff(0:n+1)
  INTEGER( I4B ) :: ii
  !!
  CALL JacobiJacobiLobattoMatrix( n=n, alpha=alpha, beta=beta, D=pt, &
    & E=wt, betaCoeff=betaCoeff )
  !!
#ifdef USE_LAPACK95
  CALL STEV( D=pt, E=wt, Z=Z )
  DO ii = 1, n+2
    wt( ii ) = betaCoeff( 0 ) * Z( 1, ii )**2
  END DO
  !!
#else
  CALL ErrorMsg( &
    & msg="The subroutine requires Lapack95 package", &
    & file = __FILE__, &
    & routine= "JacobiGaussLobattoQuadrature", &
    & line= __LINE__, &
    & unitno = stdout )
#endif
  !!
END PROCEDURE JacobiGaussLobattoQuadrature

!----------------------------------------------------------------------------
!                                                               JacobiZeros
!----------------------------------------------------------------------------

MODULE PROCEDURE JacobiZeros
  !!
  REAL( DFP ) :: E( n )
  !!
  CALL JacobiJacobiMatrix( &
    & n=n, &
    & alpha=alpha, &
    & beta=beta, &
    & D=ans, &
    & E=E )
  !!
#ifdef USE_LAPACK95
  !!
  CALL STEV( D=ans, E=E )
  !!
#else
  !!
  CALL ErrorMsg( &
    & msg="The subroutine requires Lapack95 package", &
    & file = __FILE__, &
    & routine= "JacobiZeros", &
    & line= __LINE__, &
    & unitno = stdout )
  !!
#endif
  !!
END PROCEDURE JacobiZeros

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE Methods