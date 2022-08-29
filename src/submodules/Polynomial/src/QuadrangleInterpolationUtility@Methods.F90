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

SUBMODULE(QuadrangleInterpolationUtility) Methods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                 LagrangeDegree_Quadrangle
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeDegree_Quadrangle
INTEGER(I4B) :: n, ii, jj, kk
  !!
n = LagrangeDOF_Quadrangle(order=order)
ALLOCATE (ans(n, 2))
  !!
kk = 0
  !!
DO jj = 0, order
  DO ii = 0, order
    kk = kk + 1
    ans(kk, 1) = ii
    ans(kk, 2) = jj
  END DO
END DO
END PROCEDURE LagrangeDegree_Quadrangle

!----------------------------------------------------------------------------
!                                                     LagrangeDOF_Quadrangle
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeDOF_Quadrangle
ans = (order + 1)**2
END PROCEDURE LagrangeDOF_Quadrangle

!----------------------------------------------------------------------------
!                                                   LagrangeInDOF_Quadrangle
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeInDOF_Quadrangle
ans = (order - 1)**2
END PROCEDURE LagrangeInDOF_Quadrangle

!----------------------------------------------------------------------------
!                                               EquidistancePoint_Quadrangle
!----------------------------------------------------------------------------

MODULE PROCEDURE EquidistancePoint_Quadrangle
INTEGER(I4B) :: nsd, n, ne, i1, i2
REAL(DFP) :: x(3, 4), xin(3, 4), e1(3), e2(3), lam, avar, mu
  !!
x = 0.0_DFP; xin = 0.0_DFP; e1 = 0.0_DFP; e2 = 0.0_DFP
  !!
IF (PRESENT(xij)) THEN
  nsd = SIZE(xij, 1)
  x(1:nsd, 1:4) = xij(1:nsd, 1:4)
ELSE
  nsd = 3_I4B
  x(1:nsd, 1) = [-1.0, -1.0, 0.0]
  x(1:nsd, 2) = [1.0, -1.0, 0.0]
  x(1:nsd, 3) = [1.0, 1.0, 0.0]
  x(1:nsd, 4) = [-1.0, 1.0, 0.0]
END IF
  !!
n = LagrangeDOF_Quadrangle(order=order)
ALLOCATE (ans(nsd, n))
ans = 0.0_DFP
  !!
  !! points on vertex
  !!
ans(1:nsd, 1:4) = x(1:nsd, 1:4)
  !!
  !! points on edge
  !!
ne = LagrangeInDOF_Line(order=order)
  !!
i2 = 4
IF (order .GT. 1_I4B) THEN
  i1 = i2 + 1; i2 = i1 + ne - 1
  ans(1:nsd, i1:i2) = EquidistanceInPoint_Line( &
    & order=order, &
    & xij=x(1:nsd, [1, 2]))
    !!
  i1 = i2 + 1; i2 = i1 + ne - 1
  ans(1:nsd, i1:i2) = EquidistanceInPoint_Line( &
    & order=order, &
    & xij=x(1:nsd, [2, 3]))
    !!
  i1 = i2 + 1; i2 = i1 + ne - 1
  ans(1:nsd, i1:i2) = EquidistanceInPoint_Line( &
    & order=order, &
    & xij=x(1:nsd, [3, 4]))
    !!
  i1 = i2 + 1; i2 = i1 + ne - 1
  ans(1:nsd, i1:i2) = EquidistanceInPoint_Line( &
    & order=order, &
    & xij=x(1:nsd, [4, 1]))
    !!
END IF
  !!
  !! points on face
  !!
IF (order .GT. 1_I4B) THEN
    !!
  IF (order .EQ. 2_I4B) THEN
    i1 = i2 + 1
    ans(1:nsd, i1) = SUM(x(1:nsd, :), dim=2_I4B) / 4.0_DFP
  ELSE
      !!
    e1 = x(:, 2) - x(:, 1)
    avar = NORM2(e1)
    e1 = e1 / avar
    lam = avar / order
    e2 = x(:, 4) - x(:, 1)
    avar = NORM2(e2)
    e2 = e2 / avar
    mu = avar / order
    xin(1:nsd, 1) = x(1:nsd, 1) + lam * e1(1:nsd) + mu * e2(1:nsd)
      !!
    e1 = x(:, 3) - x(:, 2)
    avar = NORM2(e1)
    e1 = e1 / avar
    lam = avar / order
    e2 = x(:, 1) - x(:, 2)
    avar = NORM2(e2)
    e2 = e2 / avar
    mu = avar / order
    xin(1:nsd, 2) = x(1:nsd, 2) + lam * e1(1:nsd) + mu * e2(1:nsd)
      !!
    e1 = x(:, 2) - x(:, 3)
    avar = NORM2(e1)
    e1 = e1 / avar
    lam = avar / order
    e2 = x(:, 4) - x(:, 3)
    avar = NORM2(e2)
    e2 = e2 / avar
    mu = avar / order
    xin(1:nsd, 3) = x(1:nsd, 3) + lam * e1(1:nsd) + mu * e2(1:nsd)
      !!
    e1 = x(:, 3) - x(:, 4)
    avar = NORM2(e1)
    e1 = e1 / avar
    lam = avar / order
    e2 = x(:, 1) - x(:, 4)
    avar = NORM2(e2)
    e2 = e2 / avar
    mu = avar / order
    xin(1:nsd, 4) = x(1:nsd, 4) + lam * e1(1:nsd) + mu * e2(1:nsd)
      !!
    i1 = i2 + 1
    ans(1:nsd, i1:) = EquidistancePoint_Quadrangle( &
      & order=order - 2, &
      & xij=xin(1:nsd, 1:4))
      !!
  END IF
END IF
  !!
END PROCEDURE EquidistancePoint_Quadrangle

!----------------------------------------------------------------------------
!                                            EquidistanceInPoint_Quadrangle
!----------------------------------------------------------------------------

MODULE PROCEDURE EquidistanceInPoint_Quadrangle
INTEGER(I4B) :: nsd, n, ne, i1, i2
REAL(DFP) :: x(3, 4), xin(3, 4), e1(3), e2(3), lam, avar, mu
  !!
IF (order .LT. 2_I4B) THEN
  ALLOCATE (ans(0, 0))
  RETURN
END IF
  !!
x = 0.0_DFP; xin = 0.0_DFP; e1 = 0.0_DFP; e2 = 0.0_DFP
  !!
IF (PRESENT(xij)) THEN
  nsd = SIZE(xij, 1)
  x(1:nsd, 1:4) = xij(1:nsd, 1:4)
ELSE
  nsd = 3_I4B
  x(1:nsd, 1) = [-1.0, -1.0, 0.0]
  x(1:nsd, 2) = [1.0, -1.0, 0.0]
  x(1:nsd, 3) = [1.0, 1.0, 0.0]
  x(1:nsd, 4) = [-1.0, 1.0, 0.0]
END IF
  !!
n = LagrangeInDOF_Quadrangle(order=order)
ALLOCATE (ans(nsd, n))
ans = 0.0_DFP
  !!
  !! points on face
  !!
IF (order .EQ. 2_I4B) THEN
  ans(1:nsd, 1) = SUM(x, dim=2_I4B) / 4.0_DFP
ELSE
    !!
  e1 = x(:, 2) - x(:, 1)
  avar = NORM2(e1)
  e1 = e1 / avar
  lam = avar / order
  e2 = x(:, 4) - x(:, 1)
  avar = NORM2(e2)
  e2 = e2 / avar
  mu = avar / order
  xin(1:nsd, 1) = x(1:nsd, 1) + lam * e1(1:nsd) + mu * e2(1:nsd)
    !!
  e1 = x(:, 3) - x(:, 2)
  avar = NORM2(e1)
  e1 = e1 / avar
  lam = avar / order
  e2 = x(:, 1) - x(:, 2)
  avar = NORM2(e2)
  e2 = e2 / avar
  mu = avar / order
  xin(1:nsd, 2) = x(1:nsd, 2) + lam * e1(1:nsd) + mu * e2(1:nsd)
    !!
  e1 = x(:, 2) - x(:, 3)
  avar = NORM2(e1)
  e1 = e1 / avar
  lam = avar / order
  e2 = x(:, 4) - x(:, 3)
  avar = NORM2(e2)
  e2 = e2 / avar
  mu = avar / order
  xin(1:nsd, 3) = x(1:nsd, 3) + lam * e1(1:nsd) + mu * e2(1:nsd)
    !!
  e1 = x(:, 3) - x(:, 4)
  avar = NORM2(e1)
  e1 = e1 / avar
  lam = avar / order
  e2 = x(:, 1) - x(:, 4)
  avar = NORM2(e2)
  e2 = e2 / avar
  mu = avar / order
  xin(1:nsd, 4) = x(1:nsd, 4) + lam * e1(1:nsd) + mu * e2(1:nsd)
    !!
  ans(1:nsd, 1:) = EquidistancePoint_Quadrangle( &
    & order=order - 2, &
    & xij=xin(1:nsd, 1:4))
    !!
END IF
  !!
END PROCEDURE EquidistanceInPoint_Quadrangle

!----------------------------------------------------------------------------
!                                              InterpolationPoint_Quadrangle
!----------------------------------------------------------------------------

MODULE PROCEDURE InterpolationPoint_Quadrangle
SELECT CASE (ipType)
CASE (Equidistance)
  nodecoord = EquidistancePoint_Quadrangle(xij=xij, order=order)
CASE (GaussLegendre)
CASE (GaussLobatto)
CASE (Chebyshev)
END SELECT
END PROCEDURE InterpolationPoint_Quadrangle

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE Methods
