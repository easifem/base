!> author: Vikas Sharma, Ph.D.
! date: 2026-11-20
! summary: Hierarchical line basis with orientation and on the unit line.
!
! The modes are L0 = (1-x)/2, L1 = (1+x)/2,
! L2 = 1.5 (x**2 - 1)/SQRT(6) and L3 = 2.5 (x**3 - x)/SQRT(10).
! Reversing the orientation multiplies mode k by (-1)**k.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE LineInterpolationUtility, ONLY: HeirarchicalBasis_Line
USE LineInterpolationUtility, ONLY: HeirarchicalBasis_Line_
USE Test_Method, ONLY: OK
IMPLICIT NONE
REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
REAL(DFP) :: xb(1, 3), l2(3), l3(3)

xb(1, :) = [-0.5_DFP, 0.0_DFP, 0.5_DFP]
l2 = 1.5_DFP * (xb(1, :)**2 - 1.0_DFP) / SQRT(6.0_DFP)
l3 = 2.5_DFP * (xb(1, :)**3 - xb(1, :)) / SQRT(10.0_DFP)

! Test 1: vertex modes and the cubic mode with orient = 1
BLOCK
  REAL(DFP) :: y(3, 4)
  INTEGER(I4B) :: nrow, ncol
  CALL HeirarchicalBasis_Line_(order=3_I4B, xij=xb, refLine="BIUNIT", &
                               orient=1_I4B, ans=y, nrow=nrow, ncol=ncol)
  CALL OK(nrow == 3 .AND. ncol == 4, "Three points, four modes")
  CALL OK(MAXVAL(ABS(y(:, 1) - 0.5_DFP * (1.0_DFP - xb(1, :)))) < tol, &
          "Mode L0")
  CALL OK(MAXVAL(ABS(y(:, 2) - 0.5_DFP * (1.0_DFP + xb(1, :)))) < tol, &
          "Mode L1")
  CALL OK(MAXVAL(ABS(y(:, 4) - l3)) < tol, "Mode L3")
END BLOCK

! Test 2: orient = -1 flips the sign of odd interior modes only
BLOCK
  REAL(DFP) :: y(3, 4)
  INTEGER(I4B) :: nrow, ncol
  CALL HeirarchicalBasis_Line_(order=3_I4B, xij=xb, refLine="BIUNIT", &
                               orient=-1_I4B, ans=y, nrow=nrow, ncol=ncol)
  CALL OK(MAXVAL(ABS(y(:, 3) - l2)) < tol, "orient -1: L2 unchanged")
  CALL OK(MAXVAL(ABS(y(:, 4) + l3)) < tol, "orient -1: L3 negated")
END BLOCK

! Test 3: points on the unit line are mapped to the biunit line
BLOCK
  REAL(DFP) :: xu(1, 3), y(3, 3)
  xu(1, :) = [0.25_DFP, 0.5_DFP, 0.75_DFP]
  y = HeirarchicalBasis_Line(order=2_I4B, xij=xu, refLine="UNIT")
  CALL OK(MAXVAL(ABS(y(:, 3) - l2)) < tol, "UNIT points give same L2")
END BLOCK

END PROGRAM main
