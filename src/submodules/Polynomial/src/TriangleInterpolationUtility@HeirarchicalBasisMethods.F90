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

SUBMODULE(TriangleInterpolationUtility) HeirarchicalBasisMethods
USE BaseMethod

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                            BarycentricVertexBasis_Triangle
!----------------------------------------------------------------------------

MODULE PROCEDURE BarycentricVertexBasis_Triangle
INTEGER(I4B) :: a(2)
a = SHAPE(lambda)
ans(1:a(2), 1:a(1)) = TRANSPOSE(lambda)
END PROCEDURE BarycentricVertexBasis_Triangle

!----------------------------------------------------------------------------
!                                             BarycentricEdgeBasis_Triangle
!----------------------------------------------------------------------------

MODULE PROCEDURE BarycentricEdgeBasis_Triangle
REAL(DFP) :: d_lambda(3 * SIZE(lambda, 2))
REAL(DFP) :: phi(1:3 * SIZE(lambda, 2), 0:MAX(pe1 - 2, pe2 - 2, pe3 - 2))
INTEGER(I4B) :: maxP, tPoints, ii

tPoints = SIZE(lambda, 2)
maxP = MAX(pe1 - 2, pe2 - 2, pe3 - 2)

DO CONCURRENT(ii=1:tpoints)
  ! edge 1 -> 2
  d_lambda(ii) = lambda(2, ii) - lambda(1, ii)
  ! edge 2 -> 3
  d_lambda(ii + tPoints) = lambda(3, ii) - lambda(2, ii)
  ! edge 3 -> 1
  d_lambda(ii + 2 * tPoints) = lambda(1, ii) - lambda(3, ii)
END DO

phi = LobattoKernelEvalAll(n=maxP, x=d_lambda)

CALL BarycentricEdgeBasis_Triangle2(pe1=pe1, pe2=pe2, pe3=pe3, &
                                    lambda=lambda, phi=phi, ans=ans)

END PROCEDURE BarycentricEdgeBasis_Triangle

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 30 Oct 2022
! summary: Evaluate the edge basis on triangle using barycentric coordinate
! (internal only)

MODULE PURE SUBROUTINE BarycentricEdgeBasis_Triangle2(pe1, pe2, pe3, &
                                                      lambda, phi, ans)
  INTEGER(I4B), INTENT(IN) :: pe1
    !! order on  edge (e1)
  INTEGER(I4B), INTENT(IN) :: pe2
    !! order on edge (e2)
  INTEGER(I4B), INTENT(IN) :: pe3
    !! order on edge (e3)
  REAL(DFP), INTENT(IN) :: lambda(:, :)
    !! point of evaluation in terms of barycentric coordinates
    !! size(lambda,1) = 3
    !! size(lambda,2) = number of points of evaluation
  REAL(DFP), INTENT(IN) :: phi(1:, 0:)
    !! lobatto kernel values
    !! size(phi1, 1) = 3*number of points (lambda2-lambda1),
    !! (lambda3-lambda1), (lambda3-lambda2)
    !! size(phi1, 2) = max(pe1-2, pe2-2, pe3-2)+1
  REAL(DFP), INTENT(INOUT) :: ans(:, :)
  ! REAL(DFP), INTENT(INOUT) :: ans(SIZE(lambda, 2), pe1 + pe2 + pe3 - 3)

  INTEGER(I4B) :: tPoints, a, ii
  REAL(DFP) :: temp(SIZE(lambda, 2))

  ans = 0.0_DFP
  tPoints = SIZE(lambda, 2)
  a = 0

! edge(1) = 1 -> 2
  temp = lambda(1, :) * lambda(2, :)
  DO ii = 1, pe1 - 1
    ans(:, a + ii) = temp * phi(1:tPoints, ii - 1)
  END DO

! edge(2) = 2 -> 3
  a = pe1 - 1
  temp = lambda(2, :) * lambda(3, :)
  DO ii = 1, pe2 - 1
    ans(:, a + ii) = temp &
                     * phi(1 + tPoints:2 * tPoints, ii - 1)
  END DO

! edge(3) = 3 -> 1
  a = pe1 - 1 + pe2 - 1
  temp = lambda(3, :) * lambda(1, :)
  DO ii = 1, pe3 - 1
    ans(:, a + ii) = temp &
                     * phi(1 + 2 * tPoints:3 * tPoints, ii - 1)
  END DO
END SUBROUTINE BarycentricEdgeBasis_Triangle2

!----------------------------------------------------------------------------
!                                   BarycentricEdgeBasisGradient_Triangle2
!----------------------------------------------------------------------------

MODULE PROCEDURE BarycentricEdgeBasisGradient_Triangle
REAL(DFP) :: d_lambda(3 * SIZE(lambda, 2))
REAL(DFP) :: phi(1:3 * SIZE(lambda, 2), 0:MAX(pe1 - 2, pe2 - 2, pe3 - 2))
REAL(DFP) :: gradientPhi(1:3 * SIZE(lambda, 2), 0:MAX(pe1 - 2, pe2 - 2, pe3 - 2))
INTEGER(I4B) :: maxP, tPoints, ii

tPoints = SIZE(lambda, 2)
maxP = MAX(pe1 - 2, pe2 - 2, pe3 - 2)

DO CONCURRENT(ii=1:tpoints)
  ! edge 1 -> 2
  d_lambda(ii) = lambda(2, ii) - lambda(1, ii)
  ! edge 2 -> 3
  d_lambda(ii + tPoints) = lambda(3, ii) - lambda(2, ii)
  ! edge 3 -> 1
  d_lambda(ii + 2 * tPoints) = lambda(1, ii) - lambda(3, ii)
END DO

phi = LobattoKernelEvalAll(n=maxP, x=d_lambda)
gradientPhi = LobattoKernelGradientEvalAll(n=maxP, x=d_lambda)

ans = BarycentricEdgeBasisGradient_Triangle2(pe1=pe1, pe2=pe2, pe3=pe3, &
                              lambda=lambda, phi=phi, gradientPhi=gradientPhi)

END PROCEDURE BarycentricEdgeBasisGradient_Triangle

!----------------------------------------------------------------------------
!                                   BarycentricEdgeBasisGradient_Triangle2
!----------------------------------------------------------------------------

MODULE PROCEDURE BarycentricEdgeBasisGradient_Triangle2
INTEGER(I4B) :: tPoints, a, ii
REAL(DFP) :: temp(SIZE(lambda, 2))

ans = 0.0_DFP
tPoints = SIZE(lambda, 2)
a = 0

! edge(1) = 1 -> 2
temp = lambda(1, :) * lambda(2, :)
DO ii = 1, pe1 - 1
  ans(:, a + ii, 1) = lambda(2, :) * phi(1:tPoints, ii - 1) - &
                      temp * gradientPhi(1:tPoints, ii - 1)
  ans(:, a + ii, 3) = lambda(1, :) * phi(1:tPoints, ii - 1) + &
                      temp * gradientPhi(1:tPoints, ii - 1)
END DO

! edge(2) = 2 -> 3
a = pe1 - 1
temp = lambda(2, :) * lambda(3, :)
DO ii = 1, pe2 - 1
  ans(:, a + ii, 2) = lambda(3, :) * &
                      phi(1 + tPoints:2 * tPoints, ii - 1) - &
                      temp * gradientPhi(1 + tPoints:2 * tPoints, ii - 1)
  ans(:, a + ii, 3) = lambda(2, :) * &
                      phi(1 + tPoints:2 * tPoints, ii - 1) + &
                      temp * gradientPhi(1 + tPoints:2 * tPoints, ii - 1)
END DO

! edge(3) = 3 -> 1
a = pe1 - 1 + pe2 - 1
temp = lambda(3, :) * lambda(1, :)
DO ii = 1, pe3 - 1
  ans(:, a + ii, 1) = lambda(3, :) * &
                      phi(1 + 2 * tPoints:3 * tPoints, ii - 1) + &
                      temp * gradientPhi(1 + 2 * tPoints:3 * tPoints, ii - 1)
  ans(:, a + ii, 3) = lambda(1, :) * &
                      phi(1 + 2 * tPoints:3 * tPoints, ii - 1) - &
                      temp * gradientPhi(1 + 2 * tPoints:3 * tPoints, ii - 1)
END DO

END PROCEDURE BarycentricEdgeBasisGradient_Triangle2

!----------------------------------------------------------------------------
!                                              BarycentricCellBasis_Triangle
!----------------------------------------------------------------------------

MODULE PROCEDURE BarycentricCellBasis_Triangle2
INTEGER(I4B) :: tPoints, k1, k2, cnt
REAL(DFP) :: temp(SIZE(lambda, 2))

tPoints = SIZE(lambda, 2)
temp = lambda(1, :) * lambda(2, :) * lambda(3, :)
cnt = 0

DO k1 = 1, order - 2
  DO k2 = 1, order - 1 - k1
    cnt = cnt + 1
    ans(:, cnt) = temp * phi(1:tPoints, k1 - 1) * &
      & phi(1 + 2 * tPoints:3 * tPoints, k2 - 1)
  END DO
END DO

END PROCEDURE BarycentricCellBasis_Triangle2

!----------------------------------------------------------------------------
!                                       BarycentricCellBasisGradient_Triangle
!----------------------------------------------------------------------------

MODULE PROCEDURE BarycentricCellBasisGradient_Triangle2
INTEGER(I4B) :: tPoints, k1, k2, cnt
REAL(DFP) :: temp1(SIZE(lambda, 2)), temp2(SIZE(lambda, 2))
REAL(DFP) :: temp3(SIZE(lambda, 2)), temp4(SIZE(lambda, 2))

tPoints = SIZE(lambda, 2)
temp1 = lambda(1, :) * lambda(2, :) * lambda(3, :)
temp2 = lambda(2, :) * lambda(3, :)
temp3 = lambda(1, :) * lambda(3, :)
temp4 = lambda(1, :) * lambda(2, :)
cnt = 0

DO k1 = 1, order - 2
  DO k2 = 1, order - 1 - k1
    cnt = cnt + 1
    ans(:, cnt, 1) = temp2 * phi(1:tPoints, k1 - 1) * &
                     phi(1 + 2 * tPoints:3 * tPoints, k2 - 1) - &
                     temp1 * (gradientPhi(1:tPoints, k1 - 1) * &
                              phi(1 + 2 * tPoints:3 * tPoints, k2 - 1) - &
                              phi(1:tPoints, k1 - 1) * &
                             gradientPhi(1 + 2 * tPoints:3 * tPoints, k2 - 1))
    ans(:, cnt, 2) = (temp3 * phi(1:tPoints, k1 - 1) + &
                      temp1 * gradientPhi(1:tPoints, k1 - 1)) * &
                     phi(1 + 2 * tPoints:3 * tPoints, k2 - 1)
    ans(:, cnt, 3) = (temp4 * phi(1 + 2 * tPoints:3 * tPoints, k2 - 1) - &
                 temp1 * gradientPhi(1 + 2 * tPoints:3 * tPoints, k2 - 1)) * &
                     phi(1:tPoints, k1 - 1)
  END DO
END DO

END PROCEDURE BarycentricCellBasisGradient_Triangle2

!----------------------------------------------------------------------------
!                                     BarycentricHeirarchicalBasis_Triangle
!----------------------------------------------------------------------------

MODULE PROCEDURE BarycentricHeirarchicalBasis_Triangle1
INTEGER(I4B) :: a, b, ii
INTEGER(I4B) :: maxP, tPoints
REAL(DFP) :: phi(1:3 * SIZE(lambda, 2), &
                 0:MAX(pe1 - 2, pe2 - 2, pe3 - 2, order - 2))
REAL(DFP) :: d_lambda(3 * SIZE(lambda, 2))

tPoints = SIZE(lambda, 2)
maxP = MAX(pe1 - 2, pe2 - 2, pe3 - 2, order - 2)

DO CONCURRENT(ii=1:tpoints)
  ! edge 1 -> 2
  d_lambda(ii) = lambda(2, ii) - lambda(1, ii)
  ! edge 2 -> 3
  d_lambda(ii + tPoints) = lambda(3, ii) - lambda(2, ii)
  ! edge 3 -> 1
  d_lambda(ii + 2 * tPoints) = lambda(1, ii) - lambda(3, ii)
END DO

! d_lambda(1:tPoints) = lambda(2, :) - lambda(1, :)
! d_lambda(1 + tPoints:2 * tPoints) = lambda(3, :) - lambda(2, :)
! d_lambda(1 + 2 * tPoints:3 * tPoints) = lambda(1, :) - lambda(3, :)

phi = LobattoKernelEvalAll(n=maxP, x=d_lambda)

! Vertex basis function
ans = 0.0_DFP
CALL BarycentricVertexBasis_Triangle(lambda=lambda, ans=ans(:, 1:3))

! Edge basis function
b = 3

IF (pe1 .GE. 2_I4B .OR. pe2 .GE. 2_I4B .OR. pe3 .GE. 2_I4B) THEN
  a = b + 1
  b = a - 1 + pe1 + pe2 + pe3 - 3 !!4+qe1 + qe2 - 2
  CALL BarycentricEdgeBasis_Triangle2(pe1=pe1, pe2=pe2, pe3=pe3, &
                                      lambda=lambda, phi=phi, ans=ans(:, a:b))
END IF

! Cell basis function
IF (order .GT. 2_I4B) THEN
  a = b + 1
  b = a - 1 + INT((order - 1) * (order - 2) / 2)
  ans(:, a:b) = BarycentricCellBasis_Triangle2(order=order, &
                                               lambda=lambda, phi=phi)
END IF

END PROCEDURE BarycentricHeirarchicalBasis_Triangle1

!----------------------------------------------------------------------------
!                                     BarycentricHeirarchicalBasis_Triangle
!----------------------------------------------------------------------------

MODULE PROCEDURE BarycentricHeirarchicalBasis_Triangle2
ans = BarycentricHeirarchicalBasis_Triangle1(order=order, pe1=order, &
                 pe2=order, pe3=order, lambda=lambda, refTriangle=refTriangle)
END PROCEDURE BarycentricHeirarchicalBasis_Triangle2

!----------------------------------------------------------------------------
!                                     BarycentricHeirarchicalBasis_Triangle
!----------------------------------------------------------------------------

MODULE PROCEDURE BarycentricHeirarchicalBasisGradient_Triangle1
INTEGER(I4B) :: a, b, ii
INTEGER(I4B) :: maxP, tPoints
REAL(DFP) :: phi(1:3 * SIZE(lambda, 2), &
                 0:MAX(pe1 - 2, pe2 - 2, pe3 - 2, order - 2))
REAL(DFP) :: gradientPhi(1:3 * SIZE(lambda, 2), &
                         0:MAX(pe1 - 2, pe2 - 2, pe3 - 2, order - 2))
REAL(DFP) :: d_lambda(3 * SIZE(lambda, 2))

tPoints = SIZE(lambda, 2)
maxP = MAX(pe1 - 2, pe2 - 2, pe3 - 2, order - 2)

DO CONCURRENT(ii=1:tpoints)
  ! edge 1 -> 2
  d_lambda(ii) = lambda(2, ii) - lambda(1, ii)
  ! edge 2 -> 3
  d_lambda(ii + tPoints) = lambda(3, ii) - lambda(2, ii)
  ! edge 3 -> 1
  d_lambda(ii + 2 * tPoints) = lambda(1, ii) - lambda(3, ii)
END DO

phi = LobattoKernelEvalAll(n=maxP, x=d_lambda)
gradientPhi = LobattoKernelGradientEvalAll(n=maxP, x=d_lambda)

ans = 0.0_DFP

! gradient of vertex basis
ans(:, 1, 1) = 1.0_DFP
ans(:, 2, 2) = 1.0_DFP
ans(:, 3, 3) = 1.0_DFP

! gradient of Edge basis function
b = 3
IF (pe1 .GE. 2_I4B .OR. pe2 .GE. 2_I4B .OR. pe3 .GE. 2_I4B) THEN
  a = b + 1
  b = a - 1 + pe1 + pe2 + pe3 - 3 !!4+qe1 + qe2 - 2
  ans(:, a:b, :) = BarycentricEdgeBasisGradient_Triangle2( &
                   pe1=pe1, pe2=pe2, pe3=pe3, lambda=lambda, &
                   phi=phi, gradientPhi=gradientPhi)
END IF

! gradient of Cell basis function
IF (order .GT. 2_I4B) THEN
  a = b + 1
  b = a - 1 + INT((order - 1) * (order - 2) / 2)
  ans(:, a:b, :) = BarycentricCellBasisGradient_Triangle2(order=order, &
                                                     lambda=lambda, phi=phi, &
                                                      gradientPhi=gradientPhi)
END IF

END PROCEDURE BarycentricHeirarchicalBasisGradient_Triangle1

!----------------------------------------------------------------------------
!                                                      VertexBasis_Triangle
!----------------------------------------------------------------------------

MODULE PROCEDURE VertexBasis_Triangle
REAL(DFP) :: lambda(SIZE(xij, 1), SIZE(xij, 2))
CALL BarycentricCoordTriangle_(ans=lambda, refTriangle=refTriangle, xin=xij)
CALL BarycentricVertexBasis_Triangle(lambda=lambda, ans=ans)
END PROCEDURE VertexBasis_Triangle

!----------------------------------------------------------------------------
!                                                    VertexBasis_Triangle2
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 Oct 2022
! summary: Returns the vertex basis functions on Triangle (internal only)

MODULE PURE SUBROUTINE VertexBasis_Triangle2(Lo1, Lo2, ans)
  REAL(DFP), INTENT(IN) :: Lo1(1:, 0:)
  REAL(DFP), INTENT(IN) :: Lo2(1:, 0:)
    !! coordinates on biunit square
  REAL(DFP), INTENT(INOUT) :: ans(:, :)
  ! ans(SIZE(Lo1, 1), 3)
    !! ans(:,v1) basis function of vertex v1 at all points

  INTEGER(I4B) :: ii, tpoints

  tpoints = SIZE(ans, 1)

  DO CONCURRENT(ii=1:tpoints)
    ans(ii, 1) = Lo1(ii, 0) * Lo2(ii, 0)
    ans(ii, 2) = Lo1(ii, 1) * Lo2(ii, 0)
    ans(ii, 3) = Lo1(ii, 1) * Lo2(ii, 1) + Lo1(ii, 0) * Lo2(ii, 1)
  END DO

END SUBROUTINE VertexBasis_Triangle2

!----------------------------------------------------------------------------
!                                             VertexBasisGradient_Triangle
!----------------------------------------------------------------------------

MODULE PROCEDURE VertexBasisGradient_Triangle2
ans(:, 1, 1) = dLo1(:, 0) * Lo2(:, 0)
ans(:, 1, 2) = Lo1(:, 0) * dLo2(:, 0)
ans(:, 2, 1) = dLo1(:, 1) * Lo2(:, 0)
ans(:, 2, 2) = Lo1(:, 1) * dLo2(:, 0)
ans(:, 3, 1) = dLo1(:, 1) * Lo2(:, 1) + dLo1(:, 0) * Lo2(:, 1)
ans(:, 3, 2) = Lo1(:, 1) * dLo2(:, 1) + Lo1(:, 0) * dLo2(:, 1)
END PROCEDURE VertexBasisGradient_Triangle2

!----------------------------------------------------------------------------
!                                                         EdgeBasis_Triangle
!----------------------------------------------------------------------------

MODULE PROCEDURE EdgeBasis_Triangle
REAL(DFP) :: x(SIZE(xij, 1), SIZE(xij, 2))
REAL(DFP) :: L1(SIZE(xij, 2), 0:MAX(pe1, pe2, pe3))
REAL(DFP) :: L2(SIZE(xij, 2), 0:MAX(pe1, pe2, pe3))
REAL(DFP) :: Lo1(SIZE(xij, 2), 0:1)
REAL(DFP) :: Lo2(SIZE(xij, 2), 0:1)
INTEGER(I4B) :: maxP

SELECT CASE (refTriangle)
CASE ("BIUNIT", "biunit")
  x = FromBiUnitTriangle2BiUnitSqr(xin=xij)
CASE ("UNIT", "unit")
  x = FromUnitTriangle2BiUnitSqr(xin=xij)
END SELECT

maxP = MAX(pe1, pe2, pe3)
L1 = JacobiEvalAll(n=maxP, x=x(1, :), alpha=1.0_DFP, beta=1.0_DFP)
L2 = JacobiEvalAll(n=maxP, x=x(2, :), alpha=1.0_DFP, beta=1.0_DFP)

Lo1(:, 0) = 0.5_DFP * (1.0 - x(1, :))
Lo1(:, 1) = 0.5_DFP * (1.0 + x(1, :))
Lo2(:, 0) = 0.5_DFP * (1.0 - x(2, :))
Lo2(:, 1) = 0.5_DFP * (1.0 + x(2, :))

ans = EdgeBasis_Triangle2(pe1=pe1, pe2=pe2, pe3=pe3, L1=L1, L2=L2, Lo1=Lo1, &
                          Lo2=Lo2)
END PROCEDURE EdgeBasis_Triangle

!----------------------------------------------------------------------------
!                                                         EdgeBasis_Triangle
!----------------------------------------------------------------------------

MODULE PROCEDURE EdgeBasis_Triangle2
INTEGER(I4B) :: maxP, k1, k2, a
REAL(DFP) :: asign

maxP = MAX(pe1, pe2, pe3)
! edge(1) = 1 -> 2
a = 0

DO k1 = 2, pe1
  ans(:, k1 - 1) = Lo1(:, 0) * Lo1(:, 1) * L1(:, k1 - 2) * (Lo2(:, 0)**k1)
END DO

! edge(2) = 2 -> 3
a = pe1 - 1
DO k2 = 2, pe2
  ans(:, a + k2 - 1) = Lo1(:, 1) * Lo2(:, 0) * Lo2(:, 1) * L2(:, k2 - 2)
END DO

! edge(3) = 3 -> 1
a = pe1 - 1 + pe2 - 1
DO k2 = 2, pe3
  asign = (-1.0_DFP)**(k2 - 2)
ans(:, a + k2 - 1) = asign * Lo1(:, 0) * Lo2(:, 0) * Lo2(:, 1) * L2(:, k2 - 2)
END DO
END PROCEDURE EdgeBasis_Triangle2

!----------------------------------------------------------------------------
!                                                EdgeBasisGradient_Triangle
!----------------------------------------------------------------------------

MODULE PROCEDURE EdgeBasisGradient_Triangle2
INTEGER(I4B) :: maxP, k1, k2, a
REAL(DFP), DIMENSION(SIZE(Lo1, 1)) :: avec

maxP = MAX(pe1, pe2, pe3)
! edge(1)
a = 0

DO k1 = 2, pe1
  avec = dLo1(:, 0) * Lo1(:, 1) * L1(:, k1 - 2) &
      & + Lo1(:, 0) * dLo1(:, 1) * L1(:, k1 - 2) &
      & + Lo1(:, 0) * Lo1(:, 1) * dL1(:, k1 - 2)

  ans(:, k1 - 1, 1) = avec * (Lo2(:, 0)**k1)

  ans(:, k1 - 1, 2) = Lo1(:, 0) * Lo1(:, 1)  &
                     & * L1(:, k1 - 2)  &
                     & * REAL(k1, DFP)  &
                     & * (Lo2(:, 0)**(k1 - 1))  &
                     & * dLo2(:, 0)
END DO

! edge(2)
a = pe1 - 1
DO k2 = 2, pe2
  avec = dLo2(:, 0) * Lo2(:, 1) * L2(:, k2 - 2) &
      &+ Lo2(:, 0) * dLo2(:, 1) * L2(:, k2 - 2) &
      &+ Lo2(:, 0) * Lo2(:, 1) * dL2(:, k2 - 2)
  ans(:, a + k2 - 1, 1) = dLo1(:, 0) * Lo2(:, 0) * Lo2(:, 1) * L2(:, k2 - 2)
  ans(:, a + k2 - 1, 2) = Lo1(:, 0) * avec
END DO

! edge(3)
a = pe1 - 1 + pe2 - 1
DO k2 = 2, pe3
  avec = dLo2(:, 0) * Lo2(:, 1) * L2(:, k2 - 2)  &
      & + Lo2(:, 0) * dLo2(:, 1) * L2(:, k2 - 2)  &
      & + Lo2(:, 0) * Lo2(:, 1) * dL2(:, k2 - 2)
  ans(:, a + k2 - 1, 1) = dLo1(:, 1) * Lo2(:, 0) * Lo2(:, 1) * L2(:, k2 - 2)
  ans(:, a + k2 - 1, 2) = Lo1(:, 1) * avec
END DO
END PROCEDURE EdgeBasisGradient_Triangle2

!----------------------------------------------------------------------------
!                                              CellBasis_Triangle
!----------------------------------------------------------------------------

MODULE PROCEDURE CellBasis_Triangle
REAL(DFP) :: x(SIZE(xij, 1), SIZE(xij, 2))
REAL(DFP) :: L1(SIZE(xij, 2), 0:order)
REAL(DFP) :: Lo1(SIZE(xij, 2), 0:1)
REAL(DFP) :: Lo2(SIZE(xij, 2), 0:1)

SELECT CASE (refTriangle)
CASE ("BIUNIT", "biunit")
  x = FromBiUnitTriangle2BiUnitSqr(xin=xij)
CASE ("UNIT", "unit")
  x = FromUnitTriangle2BiUnitSqr(xin=xij)
END SELECT

Lo1(:, 0) = 0.5_DFP * (1.0 - x(1, :))
Lo1(:, 1) = 0.5_DFP * (1.0 + x(1, :))
Lo2(:, 0) = 0.5_DFP * (1.0 - x(2, :))
Lo2(:, 1) = 0.5_DFP * (1.0 + x(2, :))

L1 = JacobiEvalAll(n=order, x=x(1, :), alpha=1.0_DFP, beta=1.0_DFP)

ans = CellBasis_Triangle2(order=order, L1=L1, Lo1=Lo1, &
                          Lo2=Lo2, eta_ij=x)
END PROCEDURE CellBasis_Triangle

!----------------------------------------------------------------------------
!                                                        CellBasis_Triangle
!----------------------------------------------------------------------------

MODULE PROCEDURE CellBasis_Triangle2
REAL(DFP) :: P2(SIZE(eta_ij, 2), 0:order)
REAL(DFP) :: avec(SIZE(eta_ij, 2)), alpha, beta
INTEGER(I4B) :: k1, k2, max_k2, cnt

alpha = 0.0_DFP
beta = 1.0_DFP
cnt = 0

DO k1 = 2, order - 1
  avec = (Lo2(:, 0)**k1) * Lo2(:, 1) * Lo1(:, 0) * Lo1(:, 1)
  alpha = 2.0_DFP * k1 - 1.0_DFP
  max_k2 = MAX(order - k1 - 1, 0)
  P2(:, 0:max_k2) = JacobiEvalAll(n=max_k2, x=eta_ij(2, :), &
    & alpha=alpha, beta=beta)
  DO k2 = 2, order - k1 + 1
    cnt = cnt + 1
    ans(:, cnt) = L1(:, k1 - 2) * avec * P2(:, k2 - 2)
  END DO
END DO

END PROCEDURE CellBasis_Triangle2

!----------------------------------------------------------------------------
!                                                 CellBasisGradinet_Triangle
!----------------------------------------------------------------------------

MODULE PROCEDURE CellBasisGradient_Triangle2
REAL(DFP) :: P2(SIZE(eta_ij, 2), 0:order)
REAL(DFP) :: dP2(SIZE(eta_ij, 2), 0:order)

REAL(DFP) :: temp(SIZE(eta_ij, 2), 13)

REAL(DFP) :: alpha, beta
INTEGER(I4B) :: k1, k2, max_k2, cnt

alpha = 0.0_DFP
beta = 1.0_DFP
cnt = 0
temp(:, 5) = dLo1(:, 0) * Lo1(:, 1)
temp(:, 6) = Lo1(:, 0) * dLo1(:, 1)
temp(:, 7) = Lo1(:, 0) * Lo1(:, 1)
temp(:, 9) = dLo2(:, 0) * Lo2(:, 1)
temp(:, 12) = Lo2(:, 0) * Lo2(:, 1)
temp(:, 13) = Lo2(:, 0) * dLo2(:, 1)

DO k1 = 2, order - 1
  alpha = 2.0_DFP * k1 - 1.0_DFP
  max_k2 = MAX(order - k1 - 1, 0)
  P2(:, 0:max_k2) = JacobiEvalAll(n=max_k2, x=eta_ij(2, :), &
    & alpha=alpha, beta=beta)
  dP2(:, 0:max_k2) = JacobiGradientEvalAll(n=max_k2, x=eta_ij(2, :), &
    & alpha=alpha, beta=beta)

  temp(:, 1) = (temp(:, 5) + temp(:, 6)) * L1(:, k1 - 2)  &
    & + temp(:, 7) * dL1(:, k1 - 2)
  temp(:, 11) = Lo2(:, 0)**(k1 - 1)
  temp(:, 2) = temp(:, 11) * temp(:, 12)
  temp(:, 3) = temp(:, 7) * L1(:, k1 - 2)

  temp(:, 10) = REAL(k1, dfp) * temp(:, 9) + temp(:, 13)
  temp(:, 8) = temp(:, 11) * temp(:, 10)

  DO k2 = 2, order - k1 + 1
    cnt = cnt + 1
    temp(:, 4) = temp(:, 8) * P2(:, k2 - 2) + temp(:, 2) * dP2(:, k2 - 2)

    ans(:, cnt, 1) = temp(:, 1) * temp(:, 2) * P2(:, k2 - 2)
    ans(:, cnt, 2) = temp(:, 3) * temp(:, 4)
  END DO

END DO
END PROCEDURE CellBasisGradient_Triangle2

!----------------------------------------------------------------------------
!                                                 HeirarchicalBasis_Triangle
!----------------------------------------------------------------------------

MODULE PROCEDURE HeirarchicalBasis_Triangle1
CHARACTER(20) :: layout
REAL(DFP) :: x(SIZE(xij, 1), SIZE(xij, 2))
REAL(DFP) :: L1(SIZE(xij, 2), 0:MAX(pe1, pe2, pe3, order))
REAL(DFP) :: L2(SIZE(xij, 2), 0:MAX(pe1, pe2, pe3, order))
REAL(DFP) :: Lo1(SIZE(xij, 2), 0:1)
REAL(DFP) :: Lo2(SIZE(xij, 2), 0:1)
INTEGER(I4B) :: maxP, a, b

layout = TRIM(UpperCase(refTriangle))
IF (layout .EQ. "BIUNIT") THEN
  x = FromBiUnitTriangle2BiUnitSqr(xin=xij)
ELSE
  x = FromUnitTriangle2BiUnitSqr(xin=xij)
END IF

Lo1(:, 0) = 0.5_DFP * (1.0 - x(1, :))
Lo1(:, 1) = 0.5_DFP * (1.0 + x(1, :))
Lo2(:, 0) = 0.5_DFP * (1.0 - x(2, :))
Lo2(:, 1) = 0.5_DFP * (1.0 + x(2, :))

!! Vertex basis function
ans = 0.0_DFP
CALL VertexBasis_Triangle2(Lo1=Lo1, Lo2=Lo2, ans=ans(:, 1:3))

maxP = MAX(pe1, pe2, pe3, order)
L1 = JacobiEvalAll(n=maxP, x=x(1, :), alpha=1.0_DFP, beta=1.0_DFP)
L2 = JacobiEvalAll(n=maxP, x=x(2, :), alpha=1.0_DFP, beta=1.0_DFP)

!! Edge basis function
b = 3
IF (pe1 .GE. 2_I4B .OR. pe2 .GE. 2_I4B .OR. pe3 .GE. 2_I4B) THEN
  a = b + 1
  b = a - 1 + pe1 + pe2 + pe3 - 3 !!4+qe1 + qe2 - 2
  ans(:, a:b) = EdgeBasis_Triangle2( &
    & pe1=pe1, pe2=pe2, pe3=pe3, L1=L1, L2=L2, Lo1=Lo1, &
    & Lo2=Lo2)
END IF

!! Cell basis function
IF (order .GT. 2_I4B) THEN
  a = b + 1
  b = a - 1 + INT((order - 1) * (order - 2) / 2)
  ans(:, a:b) = CellBasis_Triangle2(order=order, L1=L1, &
    & Lo1=Lo1, Lo2=Lo2, eta_ij=x)
END IF
END PROCEDURE HeirarchicalBasis_Triangle1

!----------------------------------------------------------------------------
!                                         HeirarchicalBasisGradient_Triangle
!----------------------------------------------------------------------------

MODULE PROCEDURE HeirarchicalBasisGradient_Triangle1
CHARACTER(20) :: layout
REAL(DFP) :: x(SIZE(xij, 1), SIZE(xij, 2))
REAL(DFP) :: L1(SIZE(xij, 2), 0:MAX(pe1, pe2, pe3, order))
REAL(DFP) :: L2(SIZE(xij, 2), 0:MAX(pe1, pe2, pe3, order))
REAL(DFP) :: dL1(SIZE(xij, 2), 0:MAX(pe1, pe2, pe3, order))
REAL(DFP) :: dL2(SIZE(xij, 2), 0:MAX(pe1, pe2, pe3, order))
REAL(DFP) :: Lo1(SIZE(xij, 2), 0:1)
REAL(DFP) :: Lo2(SIZE(xij, 2), 0:1)
REAL(DFP) :: dLo1(SIZE(xij, 2), 0:1)
REAL(DFP) :: dLo2(SIZE(xij, 2), 0:1)

INTEGER(I4B) :: maxP, a, b

layout = TRIM(UpperCase(refTriangle))

IF (layout .EQ. "BIUNIT") THEN
  x = FromBiUnitTriangle2BiUnitSqr(xin=xij)
ELSE
  x = FromUnitTriangle2BiUnitSqr(xin=xij)
END IF

Lo1(:, 0) = 0.5_DFP * (1.0 - x(1, :))
Lo1(:, 1) = 0.5_DFP * (1.0 + x(1, :))
Lo2(:, 0) = 0.5_DFP * (1.0 - x(2, :))
Lo2(:, 1) = 0.5_DFP * (1.0 + x(2, :))
dLo1(:, 0) = -0.5_DFP
dLo1(:, 1) = 0.5_DFP
dLo2(:, 0) = -0.5_DFP
dLo2(:, 1) = 0.5_DFP

!! Vertex basis function
ans = 0.0_DFP
ans(:, 1:3, 1:2) = VertexBasisGradient_Triangle2( &
  & Lo1=Lo1, &
  & Lo2=Lo2,  &
  & dLo1=dLo1, &
  & dLo2=dLo2  &
  & )

maxP = MAX(pe1, pe2, pe3, order)
L1 = JacobiEvalAll(n=maxP, x=x(1, :), alpha=1.0_DFP, beta=1.0_DFP)
L2 = JacobiEvalAll(n=maxP, x=x(2, :), alpha=1.0_DFP, beta=1.0_DFP)
dL1 = JacobiGradientEvalAll(n=maxP, x=x(1, :), alpha=1.0_DFP, beta=1.0_DFP)
dL2 = JacobiGradientEvalAll(n=maxP, x=x(2, :), alpha=1.0_DFP, beta=1.0_DFP)

!! Edge basis function
b = 3
IF (pe1 .GE. 2_I4B .OR. pe2 .GE. 2_I4B .OR. pe3 .GE. 2_I4B) THEN
  a = b + 1
  b = a - 1 + pe1 + pe2 + pe3 - 3 !!4+qe1 + qe2 - 2
  ans(:, a:b, 1:2) = EdgeBasisGradient_Triangle2( &
    & pe1=pe1, &
    & pe2=pe2, &
    & pe3=pe3, &
    & L1=L1, &
    & L2=L2, &
    & Lo1=Lo1, &
    & Lo2=Lo2,  &
    & dL1=dL1, &
    & dL2=dL2, &
    & dLo1=dLo1, &
    & dLo2=dLo2)
END IF

!! Cell basis function
IF (order .GT. 2_I4B) THEN
  a = b + 1
  b = a - 1 + INT((order - 1) * (order - 2) / 2)
  ans(:, a:b, 1:2) = CellBasisGradient_Triangle2( &
    & order=order, &
    & L1=L1, &
    & Lo1=Lo1, &
    & Lo2=Lo2, &
    & dL1=dL1, &
    & dLo1=dLo1, &
    & dLo2=dLo2, &
    & eta_ij=x)
END IF
END PROCEDURE HeirarchicalBasisGradient_Triangle1

!----------------------------------------------------------------------------
!                                         HeirarchicalBasisGradient_Triangle
!----------------------------------------------------------------------------

MODULE PROCEDURE HeirarchicalBasisGradient_Triangle2
CHARACTER(20) :: layout
REAL(DFP) :: lambda(3, SIZE(xij, 2))
REAL(DFP) :: dPhi(SIZE(xij, 2), pe1 + pe2 + pe3 + &
                  INT((order - 1) * (order - 2) / 2), 3)
REAL(DFP) :: jacobian(3, 2)

INTEGER(I4B) :: a, b

lambda = BarycentricCoordTriangle(xij, refTriangle)
dPhi = BarycentricHeirarchicalBasisGradient_Triangle1( &
       order=order, pe1=pe1, pe2=pe2, pe3=pe3, lambda=lambda, &
       refTriangle=refTriangle)

layout = TRIM(UpperCase(refTriangle))
IF (layout .EQ. "BIUNIT") THEN
  jacobian(1, :) = [-0.50_DFP, -0.50_DFP]
  jacobian(2, :) = [0.50_DFP, 0.0_DFP]
  jacobian(3, :) = [0.0_DFP, 0.50_DFP]
ELSE
  jacobian(1, :) = [-1.0_DFP, -1.0_DFP]
  jacobian(2, :) = [1.0_DFP, 0.0_DFP]
  jacobian(3, :) = [0.0_DFP, 1.0_DFP]
END IF

ans = 0.0_DFP
ans(:, 1, 1) = jacobian(1, 1)
ans(:, 1, 2) = jacobian(1, 2)
ans(:, 2, 1) = jacobian(2, 1)
ans(:, 2, 2) = jacobian(2, 2)
ans(:, 3, 1) = jacobian(3, 1)
ans(:, 3, 2) = jacobian(3, 2)

!! Edge basis function
b = 3
IF (pe1 .GE. 2_I4B .OR. pe2 .GE. 2_I4B .OR. pe3 .GE. 2_I4B) THEN
  a = b + 1
  b = a - 1 + pe1 + pe2 + pe3 - 3 !!4+qe1 + qe2 - 2
  ans(:, a:b, 1:2) = MATMUL(dPhi(:, a:b, :), jacobian)
END IF

!! Cell basis function
IF (order .GT. 2_I4B) THEN
  a = b + 1
  b = a - 1 + INT((order - 1) * (order - 2) / 2)
  ans(:, a:b, 1:2) = MATMUL(dPhi(:, a:b, :), jacobian)
END IF

END PROCEDURE HeirarchicalBasisGradient_Triangle2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE HeirarchicalBasisMethods
