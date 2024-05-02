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
USE LobattoPolynomialUtility, ONLY: LobattoKernelEvalAll_, &
                                    LobattoKernelGradientEvalAll_
USE MappingUtility, ONLY: BarycentricCoordTriangle_

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
!                                                      VertexBasis_Triangle
!----------------------------------------------------------------------------

MODULE PROCEDURE VertexBasis_Triangle
REAL(DFP) :: lambda(3, SIZE(xij, 2))
CALL BarycentricCoordTriangle_(ans=lambda, refTriangle=refTriangle, xin=xij)
CALL BarycentricVertexBasis_Triangle(lambda=lambda, ans=ans)
END PROCEDURE VertexBasis_Triangle

!----------------------------------------------------------------------------
!                                             BarycentricEdgeBasis_Triangle
!----------------------------------------------------------------------------

MODULE PROCEDURE BarycentricEdgeBasis_Triangle
REAL(DFP) :: d_lambda(3 * SIZE(lambda, 2))
REAL(DFP) :: phi(1:3 * SIZE(lambda, 2), 0:MAX(pe1 - 2, pe2 - 2, pe3 - 2))
INTEGER(I4B) :: maxP, tPoints, ii, jj

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

CALL LobattoKernelEvalAll_(n=maxP, x=d_lambda, ans=phi, nrow=ii, ncol=jj)

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
  !FIXME: Remove this temp, I want no allocation in this routine

  ans = 0.0_DFP
  tPoints = SIZE(lambda, 2)
  a = 0

  !FIXME: Make these loop parallel

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
!                                                         EdgeBasis_Triangle
!----------------------------------------------------------------------------

MODULE PROCEDURE EdgeBasis_Triangle
REAL(DFP) :: lambda(3, SIZE(xij, 2))
CALL BarycentricCoordTriangle_(ans=lambda, refTriangle=refTriangle, xin=xij)
CALL BarycentricEdgeBasis_Triangle(lambda=lambda, ans=ans, pe1=pe1, &
                                   pe2=pe2, pe3=pe3)
END PROCEDURE EdgeBasis_Triangle

!----------------------------------------------------------------------------
!                                             BarycentricEdgeBasis_Triangle
!----------------------------------------------------------------------------

MODULE PROCEDURE BarycentricCellBasis_Triangle
REAL(DFP) :: d_lambda(3 * SIZE(lambda, 2))
REAL(DFP) :: phi(1:3 * SIZE(lambda, 2), 0:order - 2)
INTEGER(I4B) :: maxP, tPoints, ii, nrow, ncol

tPoints = SIZE(lambda, 2)
maxP = order - 2

DO CONCURRENT(ii=1:tpoints)
  ! Cell 1 -> 2
  d_lambda(ii) = lambda(2, ii) - lambda(1, ii)
  ! Cell 2 -> 3
  d_lambda(ii + tPoints) = lambda(3, ii) - lambda(2, ii)
  ! Cell 3 -> 1
  d_lambda(ii + 2 * tPoints) = lambda(1, ii) - lambda(3, ii)
END DO

CALL LobattoKernelEvalAll_(n=maxP, x=d_lambda, ans=ans, nrow=nrow, ncol=ncol)

CALL BarycentricCellBasis_Triangle2(order=order, lambda=lambda, phi=phi, &
                                    ans=ans)

END PROCEDURE BarycentricCellBasis_Triangle

!----------------------------------------------------------------------------
!                                              BarycentricCellBasis_Triangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 Oct 2022
! summary: Eval basis in the cell of reference triangle (internal only)

PURE SUBROUTINE BarycentricCellBasis_Triangle2(order, lambda, phi, ans)
  INTEGER(I4B), INTENT(IN) :: order
    !! order in the cell of triangle, it should be greater than 2
  REAL(DFP), INTENT(IN) :: lambda(:, :)
    !! point of evaluation in terms of barcentric coordinates
  REAL(DFP), INTENT(IN) :: phi(1:, 0:)
    !! lobatto kernel values
    !! size(phi1, 1) = 3*number of points
    !! (lambda2-lambda1),
    !! (lambda3-lambda2),
    !! (lambda1-lambda3)
    !! size(phi1, 2) = max(pe1-2, pe2-2, pe3-2)+1
  REAL(DFP), INTENT(INOUT) :: ans(:, :)
  ! REAL(DFP) :: ans(SIZE(lambda, 2), INT((order - 1) * (order - 2) / 2))

  INTEGER(I4B) :: tp, k1, k2, cnt
  REAL(DFP) :: temp(SIZE(lambda, 2))
  !! FIXME: Remove this temp from there, no allocation is our goal

  tp = SIZE(lambda, 2)
  temp = lambda(1, :) * lambda(2, :) * lambda(3, :)
  cnt = 0

  ! FIXME: Make this loop parallel

  DO k1 = 1, order - 2
    DO k2 = 1, order - 1 - k1
      cnt = cnt + 1
      ans(:, cnt) = temp * phi(1:tp, k1 - 1) * &
        & phi(1 + 2 * tp:3 * tp, k2 - 1)
    END DO
  END DO

END SUBROUTINE BarycentricCellBasis_Triangle2

!----------------------------------------------------------------------------
!                                              CellBasis_Triangle
!----------------------------------------------------------------------------

MODULE PROCEDURE CellBasis_Triangle
REAL(DFP) :: lambda(3, SIZE(xij, 2))
CALL BarycentricCoordTriangle_(ans=lambda, refTriangle=refTriangle, xin=xij)
CALL BarycentricCellBasis_Triangle(lambda=lambda, ans=ans, order=order)
END PROCEDURE CellBasis_Triangle

!----------------------------------------------------------------------------
!                                     BarycentricHeirarchicalBasis_Triangle
!----------------------------------------------------------------------------

MODULE PROCEDURE BarycentricHeirarchicalBasis_Triangle1
INTEGER(I4B) :: a, b, ii
INTEGER(I4B) :: maxP
REAL(DFP) :: phi(1:3 * SIZE(lambda, 2), &
                 0:MAX(pe1 - 2, pe2 - 2, pe3 - 2, order - 2))
REAL(DFP) :: d_lambda(3 * SIZE(lambda, 2))
LOGICAL(LGT) :: isok

nrow = SIZE(lambda, 2)
ncol = pe1 + pe2 + pe3 + INT((order - 1) * (order - 2) / 2)

maxP = MAX(pe1 - 2, pe2 - 2, pe3 - 2, order - 2)

DO CONCURRENT(ii=1:nrow)
  ! edge 1 -> 2
  d_lambda(ii) = lambda(2, ii) - lambda(1, ii)
  ! edge 2 -> 3
  d_lambda(ii + nrow) = lambda(3, ii) - lambda(2, ii)
  ! edge 3 -> 1
  d_lambda(ii + 2 * nrow) = lambda(1, ii) - lambda(3, ii)
END DO

CALL LobattoKernelEvalAll_(n=maxP, x=d_lambda, ans=phi, nrow=a, ncol=b)

! Vertex basis function
ans = 0.0_DFP
CALL BarycentricVertexBasis_Triangle(lambda=lambda, ans=ans(:, 1:3))

! Edge basis function
b = 3

isok = ANY([pe1, pe2, pe3] .GE. 2_I4B)
IF (isok) THEN
  a = b + 1
  b = a - 1 + pe1 + pe2 + pe3 - 3 !!4+qe1 + qe2 - 2
  CALL BarycentricEdgeBasis_Triangle2(pe1=pe1, pe2=pe2, pe3=pe3, &
                                      lambda=lambda, phi=phi, ans=ans(:, a:b))
END IF

! Cell basis function
IF (order .GT. 2_I4B) THEN
  a = b + 1
  b = a - 1 + INT((order - 1) * (order - 2) / 2)
  CALL BarycentricCellBasis_Triangle2(order=order, lambda=lambda, phi=phi, &
                                      ans=ans(:, a:b))
END IF

END PROCEDURE BarycentricHeirarchicalBasis_Triangle1

!----------------------------------------------------------------------------
!                                     BarycentricHeirarchicalBasis_Triangle
!----------------------------------------------------------------------------

MODULE PROCEDURE BarycentricHeirarchicalBasis_Triangle2
CALL BarycentricHeirarchicalBasis_Triangle1(order=order, pe1=order, &
                                        pe2=order, pe3=order, lambda=lambda, &
                       refTriangle=refTriangle, ans=ans, nrow=nrow, ncol=ncol)
END PROCEDURE BarycentricHeirarchicalBasis_Triangle2

!----------------------------------------------------------------------------
!                                                 HeirarchicalBasis_Triangle
!----------------------------------------------------------------------------

MODULE PROCEDURE HeirarchicalBasis_Triangle1
INTEGER(I4B) :: nrow, ncol
CALL HeirarchicalBasis_Triangle1_(order=order, pe1=pe1, pe2=pe2, pe3=pe3, &
              xij=xij, refTriangle=refTriangle, ans=ans, nrow=nrow, ncol=ncol)
END PROCEDURE HeirarchicalBasis_Triangle1

!----------------------------------------------------------------------------
!                                                 HeirarchicalBasis_Triangle
!----------------------------------------------------------------------------

MODULE PROCEDURE HeirarchicalBasis_Triangle1_
REAL(DFP) :: lambda(3, SIZE(xij, 2))
CALL BarycentricCoordTriangle_(ans=lambda, refTriangle=refTriangle, xin=xij)
CALL BarycentricHeirarchicalBasis_Triangle(order=order, pe1=pe1, pe2=pe2, &
        pe3=pe3, lambda=lambda, refTriangle=refTriangle, ans=ans, nrow=nrow, &
                                           ncol=ncol)
END PROCEDURE HeirarchicalBasis_Triangle1_

!----------------------------------------------------------------------------
!                                                 HeirarchicalBasis_Triangle
!----------------------------------------------------------------------------

MODULE PROCEDURE HeirarchicalBasis_Triangle2
INTEGER(I4B) :: nrow, ncol
CALL HeirarchicalBasis_Triangle2_(order=order, xij=xij, &
                       refTriangle=refTriangle, ans=ans, nrow=nrow, ncol=ncol)
END PROCEDURE HeirarchicalBasis_Triangle2

!----------------------------------------------------------------------------
!                                                 HeirarchicalBasis_Triangle
!----------------------------------------------------------------------------

MODULE PROCEDURE HeirarchicalBasis_Triangle2_
REAL(DFP) :: lambda(3, SIZE(xij, 2))
CALL BarycentricCoordTriangle_(ans=lambda, refTriangle=refTriangle, xin=xij)
CALL BarycentricHeirarchicalBasis_Triangle(order=order, lambda=lambda, &
                       refTriangle=refTriangle, ans=ans, nrow=nrow, ncol=ncol)
END PROCEDURE HeirarchicalBasis_Triangle2_

!----------------------------------------------------------------------------
!                                   BarycentricVertexBasisGradient_Triangle
!----------------------------------------------------------------------------

MODULE PROCEDURE BarycentricVertexBasisGradient_Triangle
INTEGER(I4B) :: ii, tp

tp = SIZE(lambda, 2)
ans(1:tp, 1:3, 1:3) = 0.0_DFP
DO CONCURRENT(ii=1:3)
  ans(1:tp, ii, ii) = 1.0_DFP
END DO

END PROCEDURE BarycentricVertexBasisGradient_Triangle

!----------------------------------------------------------------------------
!                                   BarycentricEdgeBasisGradient_Triangle2
!----------------------------------------------------------------------------

MODULE PROCEDURE BarycentricEdgeBasisGradient_Triangle
REAL(DFP) :: d_lambda(3 * SIZE(lambda, 2))
REAL(DFP) :: phi(1:3 * SIZE(lambda, 2), 0:MAX(pe1 - 2, pe2 - 2, pe3 - 2))
REAL(DFP) :: gradientPhi(1:3 * SIZE(lambda, 2), 0:MAX(pe1 - 2, pe2 - 2, pe3 - 2))
INTEGER(I4B) :: maxP, tPoints, ii, a, b

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

CALL LobattoKernelEvalAll_(n=maxP, x=d_lambda, ans=phi, nrow=a, ncol=b)

CALL LobattoKernelGradientEvalAll_(n=maxP, x=d_lambda, ans=gradientPhi, &
                                   nrow=a, ncol=b)

CALL BarycentricEdgeBasisGradient_Triangle2(pe1=pe1, pe2=pe2, pe3=pe3, &
                     lambda=lambda, phi=phi, gradientPhi=gradientPhi, ans=ans)

END PROCEDURE BarycentricEdgeBasisGradient_Triangle

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Shion Shimizu and Vikas Sharma, Ph. D.
! date:   2024-04-21
! summary: Evaluate the gradient of the edge basis on triangle
! using barycentric coordinate

PURE SUBROUTINE BarycentricEdgeBasisGradient_Triangle2(pe1, pe2, pe3, &
                                                lambda, phi, gradientPhi, ans)
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
  REAL(DFP), INTENT(IN) :: gradientPhi(1:, 0:)
    !! gradients of lobatto kernel functions
  REAL(DFP), INTENT(INOUT) :: ans(:, :, :)
  ! REAL(DFP) :: ans(SIZE(lambda, 2), pe1 + pe2 + pe3 - 3, 3)

  INTEGER(I4B) :: tp, a, ii
  REAL(DFP) :: temp(SIZE(lambda, 2))
  ! FIXME: Remove this temp

  tp = SIZE(lambda, 2)

  !FIXME: Make these loop parallel

  a = 0
  ! edge(1) = 1 -> 2
  temp = lambda(1, :) * lambda(2, :)
  DO ii = 1, pe1 - 1
    ans(1:tp, a + ii, 1) = lambda(2, :) * phi(1:tp, ii - 1) - &
                           temp * gradientPhi(1:tp, ii - 1)
    ans(1:tp, a + ii, 2) = lambda(1, :) * phi(1:tp, ii - 1) + &
                           temp * gradientPhi(1:tp, ii - 1)
    ans(1:tp, a + ii, 3) = 0.0_DFP
  END DO

  ! edge(2) = 2 -> 3
  a = pe1 - 1
  temp = lambda(2, :) * lambda(3, :)
  DO ii = 1, pe2 - 1
    ans(1:tp, a + ii, 1) = 0.0_DFP

    ans(1:tp, a + ii, 2) = lambda(3, :) * &
                           phi(1 + tp:2 * tp, ii - 1) - &
                           temp * gradientPhi(1 + tp:2 * tp, ii - 1)

    ans(1:tp, a + ii, 3) = lambda(2, :) * &
                           phi(1 + tp:2 * tp, ii - 1) + &
                           temp * gradientPhi(1 + tp:2 * tp, ii - 1)
  END DO

  ! edge(3) = 3 -> 1
  a = pe1 - 1 + pe2 - 1
  temp = lambda(3, :) * lambda(1, :)
  DO ii = 1, pe3 - 1
    ans(1:tp, a + ii, 1) = lambda(3, :) * &
                           phi(1 + 2 * tp:3 * tp, ii - 1) + &
                           temp * gradientPhi(1 + 2 * tp:3 * tp, ii - 1)

    ans(1:tp, a + ii, 2) = 0.0_DFP

    ans(1:tp, a + ii, 3) = lambda(1, :) * &
                           phi(1 + 2 * tp:3 * tp, ii - 1) - &
                           temp * gradientPhi(1 + 2 * tp:3 * tp, ii - 1)
  END DO
END SUBROUTINE BarycentricEdgeBasisGradient_Triangle2

!----------------------------------------------------------------------------
!                                   BarycentricVertexBasisGradient_Triangle
!----------------------------------------------------------------------------

MODULE PROCEDURE BarycentricCellBasisGradient_Triangle
INTEGER(I4B) :: a, b, ii, maxP, tp
REAL(DFP), ALLOCATABLE :: phi(:, :), gradientPhi(:, :), d_lambda(:)

tp = SIZE(lambda, 2)
maxP = order - 2

a = 3 * tp; b = maxP
ALLOCATE (phi(a, b), gradientPhi(a, b), d_lambda(a))

DO CONCURRENT(ii=1:tp)
  ! edge 1 -> 2
  d_lambda(ii) = lambda(2, ii) - lambda(1, ii)
  ! edge 2 -> 3
  d_lambda(ii + tp) = lambda(3, ii) - lambda(2, ii)
  ! edge 3 -> 1
  d_lambda(ii + 2 * tp) = lambda(1, ii) - lambda(3, ii)
END DO

CALL LobattoKernelEvalAll_(n=maxP, x=d_lambda, ans=phi, nrow=a, ncol=b)

CALL LobattoKernelGradientEvalAll_(n=maxP, x=d_lambda, ans=gradientPhi, &
                                   nrow=a, ncol=b)

CALL BarycentricCellBasisGradient_Triangle2(order=order, lambda=lambda, &
                                    phi=phi, gradientPhi=gradientPhi, ans=ans)
END PROCEDURE BarycentricCellBasisGradient_Triangle

!----------------------------------------------------------------------------
!                                       BarycentricCellBasisGradient_Triangle
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:   2024-04-21
! summary:  Evaluate the gradient of the cell basis on triangle

PURE SUBROUTINE BarycentricCellBasisGradient_Triangle2(order, lambda, phi, &
                                                       gradientPhi, ans)
  INTEGER(I4B), INTENT(IN) :: order
    !! order in the cell of triangle, it should be greater than 2
  REAL(DFP), INTENT(IN) :: lambda(:, :)
    !! point of evaluation
  REAL(DFP), INTENT(IN) :: phi(1:, 0:)
    !! lobatto kernel values
    !! size(phi1, 1) = 3*number of points (lambda2-lambda1),
    !! (lambda3-lambda1), (lambda3-lambda2)
    !! size(phi1, 2) = max(pe1-2, pe2-2, pe3-2)+1
  REAL(DFP), INTENT(IN) :: gradientPhi(1:, 0:)
    !! gradients of lobatto kernel functions
  REAL(DFP), INTENT(INOUT) :: ans(:, :, :)
  ! REAL(DFP) :: ans(SIZE(lambda, 2), INT((order - 1) * (order - 2) / 2), 3)

  ! internal variables
  INTEGER(I4B) :: tPoints, k1, k2, cnt
  REAL(DFP) :: temp1(SIZE(lambda, 2)), temp2(SIZE(lambda, 2))
  REAL(DFP) :: temp3(SIZE(lambda, 2)), temp4(SIZE(lambda, 2))

  ! FIXME: Remove these temps

  tPoints = SIZE(lambda, 2)
  temp1 = lambda(1, :) * lambda(2, :) * lambda(3, :)
  temp2 = lambda(2, :) * lambda(3, :)
  temp3 = lambda(1, :) * lambda(3, :)
  temp4 = lambda(1, :) * lambda(2, :)
  cnt = 0

  ! FIXME: make these loop parallel

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
END SUBROUTINE BarycentricCellBasisGradient_Triangle2

!----------------------------------------------------------------------------
!                                     BarycentricHeirarchicalBasis_Triangle
!----------------------------------------------------------------------------

MODULE PROCEDURE BarycentricHeirarchicalBasisGradient_Triangle1
INTEGER(I4B) :: a, b, ii, maxP, tp
REAL(DFP), ALLOCATABLE :: phi(:, :), gradientPhi(:, :), d_lambda(:)
LOGICAL(LGT) :: isok

tp = SIZE(lambda, 2)
maxP = MAX(pe1 - 2, pe2 - 2, pe3 - 2, order - 2)

a = 3 * tp; b = maxP
ALLOCATE (phi(a, 0:b), gradientPhi(a, 0:b), d_lambda(a))

DO CONCURRENT(ii=1:tp)
  ! edge 1 -> 2
  d_lambda(ii) = lambda(2, ii) - lambda(1, ii)
  ! edge 2 -> 3
  d_lambda(ii + tp) = lambda(3, ii) - lambda(2, ii)
  ! edge 3 -> 1
  d_lambda(ii + 2 * tp) = lambda(1, ii) - lambda(3, ii)
END DO

CALL LobattoKernelEvalAll_(n=maxP, x=d_lambda, ans=phi, nrow=a, ncol=b)

CALL LobattoKernelGradientEvalAll_(n=maxP, x=d_lambda, ans=gradientPhi, &
                                   nrow=a, ncol=b)

! gradient of vertex basis
ans(1:tp, 1:3, 1:3) = 0.0_DFP
DO CONCURRENT(ii=1:3)
  ans(1:tp, ii, ii) = 1.0_DFP
END DO

! gradient of Edge basis function
b = 3
isok = ANY([pe1, pe2, pe3] .GE. 2_I4B)
IF (isok) THEN
  a = b + 1
  b = a - 1 + pe1 + pe2 + pe3 - 3 !!4+qe1 + qe2 - 2
  CALL BarycentricEdgeBasisGradient_Triangle2( &
    pe1=pe1, pe2=pe2, pe3=pe3, lambda=lambda, &
    phi=phi, gradientPhi=gradientPhi, ans=ans(:, a:b, :))
END IF

! gradient of Cell basis function
IF (order .GT. 2_I4B) THEN
  a = b + 1
  b = a - 1 + INT((order - 1) * (order - 2) / 2)
  CALL BarycentricCellBasisGradient_Triangle2(order=order, lambda=lambda, &
                         phi=phi, gradientPhi=gradientPhi, ans=ans(:, a:b, :))
END IF

DEALLOCATE (phi, gradientPhi, d_lambda)
END PROCEDURE BarycentricHeirarchicalBasisGradient_Triangle1

!----------------------------------------------------------------------------
!                                         HeirarchicalBasisGradient_Triangle
!----------------------------------------------------------------------------

MODULE PROCEDURE HeirarchicalBasisGradient_Triangle1
INTEGER(I4B) :: s(3)
CALL HeirarchicalBasisGradient_Triangle1_(order=order, pe1=pe1, &
   pe2=pe2, pe3=pe3, xij=xij, refTriangle=refTriangle, ans=ans, tsize1=s(1), &
                                          tsize2=s(2), tsize3=s(3))
END PROCEDURE HeirarchicalBasisGradient_Triangle1

!----------------------------------------------------------------------------
!                                         HeirarchicalBasisGradient_Triangle
!----------------------------------------------------------------------------

MODULE PROCEDURE HeirarchicalBasisGradient_Triangle1_
REAL(DFP) :: jac(3, 2)
REAL(DFP), ALLOCATABLE :: lambda(:, :), dPhi(:, :, :)
INTEGER(I4B) :: ii, jj, kk

ii = SIZE(xij, 2)
jj = pe1 + pe2 + pe3 + INT((order - 1) * (order - 2) / 2)
ALLOCATE (lambda(3, ii), dPhi(ii, jj, 3))
tsize1 = SIZE(xij, 2)
tsize2 = pe1 + pe2 + pe3 + INT((order - 1) * (order - 2) / 2)
tsize3 = 2

CALL BarycentricCoordTriangle_(xin=xij, refTriangle=refTriangle, ans=lambda)
CALL BarycentricHeirarchicalBasisGradient_Triangle( &
  order=order, pe1=pe1, pe2=pe2, pe3=pe3, lambda=lambda, &
  refTriangle=refTriangle, ans=dPhi)

SELECT CASE (refTriangle(1:1))
CASE ("B", "b")
  jac(1, :) = [-0.50_DFP, -0.50_DFP]
  jac(2, :) = [0.50_DFP, 0.0_DFP]
  jac(3, :) = [0.0_DFP, 0.50_DFP]
CASE ("U", "u")
  jac(1, :) = [-1.0_DFP, -1.0_DFP]
  jac(2, :) = [1.0_DFP, 0.0_DFP]
  jac(3, :) = [0.0_DFP, 1.0_DFP]
END SELECT

DO CONCURRENT(ii=1:tsize1, jj=1:tsize2, kk=1:tsize3)
  ans(ii, jj, kk) = dPhi(ii, jj, 1) * jac(1, kk) &
                    + dPhi(ii, jj, 2) * jac(2, kk) &
                    + dPhi(ii, jj, 3) * jac(3, kk)
END DO

DEALLOCATE (lambda, dPhi)

END PROCEDURE HeirarchicalBasisGradient_Triangle1_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE HeirarchicalBasisMethods
