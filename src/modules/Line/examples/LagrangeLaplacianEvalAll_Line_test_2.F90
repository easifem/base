!> author: Vikas Sharma, Ph.D.
! date: 2026-11-20
! summary: Second derivative of the quadratic Lagrange basis with caller
! work arrays.
!
! Nodes are -1, 1, 0. The second derivatives are l1'' = 1, l2'' = 1,
! l3'' = -2 at every point.

PROGRAM main
USE BaseType, ONLY: poly => TypePolynomialOpt
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE LineInterpolationUtility, ONLY: LagrangeLaplacianEvalAll_Line_
USE Test_Method, ONLY: OK
IMPLICIT NONE
REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
REAL(DFP) :: nodes(1, 3), x(1, 2), d2(3)

nodes(1, :) = [-1.0_DFP, 1.0_DFP, 0.0_DFP]
x(1, :) = [0.5_DFP, -0.25_DFP]
d2 = [1.0_DFP, 1.0_DFP, -2.0_DFP]

! Test 1: subroutine with caller work arrays coeff and xx
BLOCK
  REAL(DFP) :: y(2, 3), coeff(3, 3), xx(2, 3)
  INTEGER(I4B) :: nrow, ncol
  CALL LagrangeLaplacianEvalAll_Line_(order=2_I4B, x=x, xij=nodes, ans=y, &
                                      nrow=nrow, ncol=ncol, coeff=coeff, &
                                      xx=xx, firstCall=.TRUE., &
                                      basisType=poly%Monomial)
  CALL OK(nrow == 2 .AND. ncol == 3, "Work array form: 2 by 3")
  CALL OK(MAXVAL(ABS(y(1, :) - d2)) < tol .AND. &
          MAXVAL(ABS(y(2, :) - d2)) < tol, "Constant second derivatives")
END BLOCK

! Test 2: reuse the coefficients with firstCall false
BLOCK
  REAL(DFP) :: y(2, 3), coeff(3, 3), xx(2, 3)
  INTEGER(I4B) :: nrow, ncol
  CALL LagrangeLaplacianEvalAll_Line_(order=2_I4B, x=x, xij=nodes, ans=y, &
                                      nrow=nrow, ncol=ncol, coeff=coeff, &
                                      xx=xx, firstCall=.TRUE., &
                                      basisType=poly%Monomial)
  y = 0.0_DFP
  CALL LagrangeLaplacianEvalAll_Line_(order=2_I4B, x=x, xij=nodes, ans=y, &
                                      nrow=nrow, ncol=ncol, coeff=coeff, &
                                      xx=xx, firstCall=.FALSE., &
                                      basisType=poly%Monomial)
  CALL OK(MAXVAL(ABS(y(1, :) - d2)) < tol, "Stored coefficients reused")
END BLOCK

END PROGRAM main
