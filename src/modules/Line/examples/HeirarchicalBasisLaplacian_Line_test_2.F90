!> author: Vikas Sharma, Ph.D.
! date: 2026-11-20
! summary: Second derivative of the hierarchical line basis.
!
! On the biunit line L0'' = L1'' = 0, L2'' = 3/SQRT(6) and
! L3'' = 15x/SQRT(10). On the unit line the chain rule gives a factor 4.
! Reversing the orientation multiplies mode k by (-1)**k.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE LineInterpolationUtility, ONLY: HeirarchicalBasisLaplacian_Line
USE LineInterpolationUtility, ONLY: HeirarchicalBasisLaplacian_Line_
USE Test_Method, ONLY: OK
IMPLICIT NONE
REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
REAL(DFP) :: xb(1, 3), d2l2(3), d2l3(3)

xb(1, :) = [-0.5_DFP, 0.0_DFP, 0.5_DFP]
d2l2 = 3.0_DFP / SQRT(6.0_DFP)
d2l3 = 15.0_DFP * xb(1, :) / SQRT(10.0_DFP)

! Test 1: function, biunit line
BLOCK
  REAL(DFP) :: y(3, 4)
  y = HeirarchicalBasisLaplacian_Line(order=3_I4B, xij=xb, refLine="BIUNIT")
  CALL OK(MAXVAL(ABS(y(:, 1:2))) < tol, "Vertex modes are linear")
  CALL OK(MAXVAL(ABS(y(:, 3) - d2l2)) < tol, "Second derivative of L2")
  CALL OK(MAXVAL(ABS(y(:, 4) - d2l3)) < tol, "Second derivative of L3")
END BLOCK

! Test 2: subroutine with orient = -1
BLOCK
  REAL(DFP) :: y(3, 4)
  INTEGER(I4B) :: nrow, ncol
  CALL HeirarchicalBasisLaplacian_Line_(order=3_I4B, xij=xb, &
                                        refLine="BIUNIT", orient=-1_I4B, &
                                        ans=y, nrow=nrow, ncol=ncol)
  CALL OK(nrow == 3 .AND. ncol == 4, "Subroutine dimensions")
  CALL OK(MAXVAL(ABS(y(:, 3) - d2l2)) < tol, "orient -1: L2'' unchanged")
  CALL OK(MAXVAL(ABS(y(:, 4) + d2l3)) < tol, "orient -1: L3'' negated")
END BLOCK

! Test 3: unit line; chain rule gives a factor 4
BLOCK
  REAL(DFP) :: xu(1, 3), y(3, 3)
  xu(1, :) = [0.25_DFP, 0.5_DFP, 0.75_DFP]
  y = HeirarchicalBasisLaplacian_Line(order=2_I4B, xij=xu, refLine="UNIT")
  CALL OK(MAXVAL(ABS(y(:, 3) - 4.0_DFP * d2l2)) < tol, &
          "UNIT: second derivative of L2 scaled by 4")
END BLOCK

END PROGRAM main
