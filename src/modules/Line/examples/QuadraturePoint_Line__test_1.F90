!> author: Vikas Sharma, Ph.D.
! date: 2026-11-20
! summary: Quadrature points and weights on a line written into a buffer.
!
! The last active row of ans holds the weights. With xij the points are
! mapped to the segment and the weights are scaled by its half length.

PROGRAM main
USE BaseType, ONLY: qpopt => TypeQuadratureOpt
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE LineInterpolationUtility, ONLY: QuadraturePoint_Line_
USE Test_Method, ONLY: OK
IMPLICIT NONE
REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP

! Test 1: two Gauss-Legendre points on the biunit line
BLOCK
  REAL(DFP) :: ans(3, 5), a
  INTEGER(I4B) :: nrow, ncol
  a = 1.0_DFP / SQRT(3.0_DFP)
  CALL QuadraturePoint_Line_(nips=[2_I4B], quadType=qpopt%GaussLegendre, &
                             layout="INCREASING", ans=ans, nrow=nrow, &
                             ncol=ncol)
  CALL OK(nrow == 2 .AND. ncol == 2, "Two rows and two points")
  CALL OK(MAXVAL(ABS(ans(1, 1:2) - [-a, a])) < tol, "Points are -a, a")
  CALL OK(MAXVAL(ABS(ans(2, 1:2) - 1.0_DFP)) < tol, "Weights are 1")
END BLOCK

! Test 2: three Lobatto points in VEFC layout
BLOCK
  REAL(DFP) :: ans(2, 3)
  INTEGER(I4B) :: nrow, ncol
  CALL QuadraturePoint_Line_(nips=[3_I4B], &
                             quadType=qpopt%GaussLegendreLobatto, &
                             layout="VEFC", ans=ans, nrow=nrow, ncol=ncol)
  CALL OK(MAXVAL(ABS(ans(1, :) - [-1.0_DFP, 1.0_DFP, 0.0_DFP])) < tol, &
          "End points first")
  CALL OK(MAXVAL(ABS(ans(2, :) - [1.0_DFP, 1.0_DFP, 4.0_DFP] / 3.0_DFP)) &
          < tol, "Weights follow the points")
END BLOCK

! Test 3: 2D segment from (1,1) to (4,5) of length 5
BLOCK
  REAL(DFP) :: ans(3, 3), xij(2, 2)
  INTEGER(I4B) :: nrow, ncol
  xij(:, 1) = [1.0_DFP, 1.0_DFP]
  xij(:, 2) = [4.0_DFP, 5.0_DFP]
  CALL QuadraturePoint_Line_(nips=[3_I4B], quadType=qpopt%GaussLegendre, &
                             layout="INCREASING", xij=xij, ans=ans, &
                             nrow=nrow, ncol=ncol)
  CALL OK(nrow == 3 .AND. ncol == 3, "Two coordinate rows and weights")
  CALL OK(ABS(SUM(ans(3, :)) - 5.0_DFP) < tol, "Weights sum to 5")
  CALL OK(MAXVAL(ABS(4.0_DFP * (ans(1, :) - 1.0_DFP) - &
                     3.0_DFP * (ans(2, :) - 1.0_DFP))) < tol, &
          "Points lie on the segment")
  CALL OK(MAXVAL(ABS(ans(1:2, 2) - [2.5_DFP, 3.0_DFP])) < tol, &
          "Middle point is the midpoint")
END BLOCK

END PROGRAM main
