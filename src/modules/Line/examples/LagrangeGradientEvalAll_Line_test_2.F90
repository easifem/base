!> author: Vikas Sharma, Ph.D.
! date: 2026-11-20
! summary: Gradient of the quadratic Lagrange basis, with caller work
! arrays and with a Legendre basis.
!
! Nodes are -1, 1, 0. The gradients are l1' = x - 1/2, l2' = x + 1/2,
! l3' = -2x.

PROGRAM main
USE BaseType, ONLY: poly => TypePolynomialOpt
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE LineInterpolationUtility, ONLY: LagrangeGradientEvalAll_Line
USE LineInterpolationUtility, ONLY: LagrangeGradientEvalAll_Line_
USE Test_Method, ONLY: OK
IMPLICIT NONE
REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
REAL(DFP) :: nodes(1, 3), x(1, 2), gp(3), gm(3)

nodes(1, :) = [-1.0_DFP, 1.0_DFP, 0.0_DFP]
x(1, :) = [0.5_DFP, -0.5_DFP]
gp = [0.0_DFP, 1.0_DFP, -1.0_DFP]
gm = [-1.0_DFP, 0.0_DFP, 1.0_DFP]

! Test 1: subroutine with caller work arrays coeff and xx
BLOCK
  REAL(DFP) :: g(2, 3, 1), coeff(3, 3), xx(2, 3)
  INTEGER(I4B) :: dim1, dim2, dim3
  CALL LagrangeGradientEvalAll_Line_(order=2_I4B, x=x, xij=nodes, ans=g, &
                                     dim1=dim1, dim2=dim2, dim3=dim3, &
                                     coeff=coeff, xx=xx, firstCall=.TRUE., &
                                     basisType=poly%Monomial)
  CALL OK(dim1 == 2 .AND. dim2 == 3 .AND. dim3 == 1, &
          "Work array form: 2 by 3 by 1")
  CALL OK(MAXVAL(ABS(g(1, :, 1) - gp)) < tol .AND. &
          MAXVAL(ABS(g(2, :, 1) - gm)) < tol, "Work array form values")
END BLOCK

! Test 2: Legendre basis gives the same Lagrange gradients
BLOCK
  REAL(DFP) :: g(2, 3, 1)
  g = LagrangeGradientEvalAll_Line(order=2_I4B, x=x, xij=nodes, &
                                   basisType=poly%Legendre)
  CALL OK(MAXVAL(ABS(g(1, :, 1) - gp)) < tol .AND. &
          MAXVAL(ABS(g(2, :, 1) - gm)) < tol, "Legendre basis gradients")
END BLOCK

! Test 3: gradients of a partition of unity sum to zero
BLOCK
  REAL(DFP) :: g(2, 3, 1)
  g = LagrangeGradientEvalAll_Line(order=2_I4B, x=x, xij=nodes)
  CALL OK(MAXVAL(ABS(SUM(g(:, :, 1), DIM=2))) < tol, &
          "Gradients sum to zero")
END BLOCK

END PROGRAM main
