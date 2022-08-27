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

SUBMODULE(TriangleInterpolationUtility) Methods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                   LagrangeDegree_Triangle
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeDegree_Triangle
  INTEGER( I4B ) ::  n, ii, jj, kk
  !!
  n = LagrangeDOF_Triangle( order=order )
  ALLOCATE( ans( n, 2 ) )
  !!
  kk = 0
  !!
  DO jj = 0, order
    DO ii = 0, order-jj
      kk = kk + 1
      ans(kk, 1) = ii
      ans(kk, 2) = jj
    END DO
  END DO
  !!
END PROCEDURE LagrangeDegree_Triangle

!----------------------------------------------------------------------------
!                                                      LagrangeDOF_Triangle
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeDOF_Triangle
  ans = (order+1)*(order+2)/2_I4B
END PROCEDURE LagrangeDOF_Triangle

!----------------------------------------------------------------------------
!                                                    LagrangeInDOF_Triangle
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeInDOF_Triangle
  ans = (order-1)*(order-2)/2_I4B
END PROCEDURE LagrangeInDOF_Triangle

!----------------------------------------------------------------------------
!                                                 EquidistancePoint_Triangle
!----------------------------------------------------------------------------

MODULE PROCEDURE EquidistancePoint_Triangle
  INTEGER( I4B ) :: nsd, n, ne, i1, i2
  REAL( DFP ) :: x( 3, 3 ), xin( 3, 3 ), e1(3), e2(3), lam, avar, mu
  !!
  x = 0.0_DFP; xin=0.0_DFP; e1=0.0_DFP; e2=0.0_DFP
  !!
  IF( PRESENT( xij ) ) THEN
    nsd = SIZE( xij, 1 )
    x(1:nsd, 1:3) = xij(1:nsd, 1:3)
  ELSE
    nsd = 3_I4B
    x(1:nsd,1) = [0.0,0.0,0.0]
    x(1:nsd,2) = [1.0,0.0,0.0]
    x(1:nsd,3) = [0.0,1.0,0.0]
  END IF
  !!
  n = LagrangeDOF_Triangle(order=order)
  ALLOCATE( ans( nsd, n ) )
  ans = 0.0_DFP
  !!
  !! points on vertex
  !!
  ans(1:nsd,1:3) = x(1:nsd, 1:3)
  !!
  !! points on edge
  !!
  ne = LagrangeInDOF_Line( order=order )
  !!
  i2=3
  IF( order .GT. 1_I4B ) THEN
    i1 = i2+1; i2=i1+ne-1
    ans(1:nsd, i1:i2 ) = EquidistanceInPoint_Line( &
      & order=order, &
      & xij=x(1:nsd, [1,2]) )
    !!
    i1 = i2+1; i2=i1+ne-1
    ans(1:nsd, i1:i2 ) = EquidistanceInPoint_Line( &
      & order=order, &
      & xij=x(1:nsd, [2,3]) )
    !!
    i1 = i2+1; i2=i1+ne-1
    ans(1:nsd, i1:i2 ) = EquidistanceInPoint_Line( &
      & order=order, &
      & xij=x(1:nsd, [3,1]) )
    !!
  END IF
  !!
  !! points on face
  !!
  IF( order .GT. 2_I4B ) THEN
    !!
    IF( order .EQ. 3_I4B ) THEN
      i1 = i2+1
      ans(1:nsd, i1) = (x(1:nsd,1)+x(1:nsd,2)+x(1:nsd,3))/3.0_DFP
    ELSE
      !!
      e1 = x(:,2)-x(:,1)
      avar = NORM2(e1)
      e1 = e1 / avar
      lam = avar / order
      e2 = x(:,3)-x(:,1)
      avar = NORM2(e2)
      e2 = e2 / avar
      mu = avar / order
      xin(1:nsd, 1) = x(1:nsd, 1) + lam*e1(1:nsd) + mu*e2(1:nsd)
      !!
      e1 = x(:,3)-x(:,2)
      avar = NORM2(e1)
      e1 = e1 / avar
      lam = avar / order
      e2 = x(:,1)-x(:,2)
      avar = NORM2(e2)
      e2 = e2 / avar
      mu = avar / order
      xin(1:nsd, 2) = x(1:nsd, 2) + lam*e1(1:nsd) + mu*e2(1:nsd)
      !!
      e1 = x(:,1)-x(:,3)
      avar = NORM2(e1)
      e1 = e1 / avar
      lam = avar / order
      e2 = x(:,2)-x(:,3)
      avar = NORM2(e2)
      e2 = e2 / avar
      mu = avar / order
      xin(1:nsd, 3) = x(1:nsd, 3) + lam*e1(1:nsd) + mu*e2(1:nsd)
      !!
      i1 = i2+1
      ans(1:nsd, i1: ) = EquidistancePoint_Triangle( &
        & order=order-3, &
        & xij=xin(1:nsd, 1:3) )
      !!
    END IF
  END IF
  !!
END PROCEDURE EquidistancePoint_Triangle

!----------------------------------------------------------------------------
!                                              EquidistanceInPoint_Triangle
!----------------------------------------------------------------------------

MODULE PROCEDURE EquidistanceInPoint_Triangle
  INTEGER( I4B ) :: nsd, n, ne
  REAL( DFP ) :: x( 3, 3 ), xin( 3, 3 ), e1(3), e2(3), lam, avar, mu
  !!
  IF( order .LT. 3_I4B ) THEN
    ALLOCATE( ans( 0, 0 ) )
    RETURN
  END IF
  !!
  x = 0.0_DFP; xin=0.0_DFP; e1=0.0_DFP; e2=0.0_DFP
  !!
  IF( PRESENT( xij ) ) THEN
    nsd = SIZE( xij, 1 )
    x(1:nsd, 1:3) = xij(1:nsd, 1:3)
  ELSE
    nsd = 3_I4B
    x(1:nsd,1) = [0.0,0.0,0.0]
    x(1:nsd,2) = [1.0,0.0,0.0]
    x(1:nsd,3) = [0.0,1.0,0.0]
  END IF
  !!
  n = LagrangeInDOF_Triangle(order=order)
  ALLOCATE( ans( nsd, n ) )
  ans = 0.0_DFP
  !!
  !! points on face
  !!
  IF( order .EQ. 3_I4B ) THEN
    ans(1:nsd, 1) = (x(1:nsd,1)+x(1:nsd,2)+x(1:nsd,3))/3.0_DFP
  ELSE
    !!
    e1 = x(:,2)-x(:,1)
    avar = NORM2(e1)
    e1 = e1 / avar
    lam = avar / order
    e2 = x(:,3)-x(:,1)
    avar = NORM2(e2)
    e2 = e2 / avar
    mu = avar / order
    xin(1:nsd, 1) = x(1:nsd, 1) + lam*e1(1:nsd) + mu*e2(1:nsd)
    !!
    e1 = x(:,3)-x(:,2)
    avar = NORM2(e1)
    e1 = e1 / avar
    lam = avar / order
    e2 = x(:,1)-x(:,2)
    avar = NORM2(e2)
    e2 = e2 / avar
    mu = avar / order
    xin(1:nsd, 2) = x(1:nsd, 2) + lam*e1(1:nsd) + mu*e2(1:nsd)
    !!
    e1 = x(:,1)-x(:,3)
    avar = NORM2(e1)
    e1 = e1 / avar
    lam = avar / order
    e2 = x(:,2)-x(:,3)
    avar = NORM2(e2)
    e2 = e2 / avar
    mu = avar / order
    xin(1:nsd, 3) = x(1:nsd, 3) + lam*e1(1:nsd) + mu*e2(1:nsd)
    !!
    ans(1:nsd, 1: ) = EquidistancePoint_Triangle( &
      & order=order-3, &
      & xij=xin(1:nsd, 1:3) )
    !!
  END IF
  !!
END PROCEDURE EquidistanceInPoint_Triangle

!----------------------------------------------------------------------------
!                                               InterpolationPoint_Triangle
!----------------------------------------------------------------------------

MODULE PROCEDURE InterpolationPoint_Triangle
  SELECT CASE( ipType )
  CASE( Equidistance )
    nodecoord = EquidistancePoint_Triangle( xij=xij, order=order )
  CASE( GaussLegendre )
  CASE( GaussLobatto )
  CASE( Chebyshev )
  END SELECT
END PROCEDURE InterpolationPoint_Triangle

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE Methods