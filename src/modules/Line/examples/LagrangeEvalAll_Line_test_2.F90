!> author: Vikas Sharma, Ph.D.
! date: 2026-11-20
! summary: Quadratic Lagrange basis at one point and with stored
! coefficients.
!
! Nodes are -1, 1, 0. The basis is l1 = (x**2 - x)/2,
! l2 = (x**2 + x)/2, l3 = 1 - x**2.

PROGRAM main
USE BaseType, ONLY: poly => TypePolynomialOpt
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE LineInterpolationUtility, ONLY: LagrangeEvalAll_Line
USE LineInterpolationUtility, ONLY: LagrangeEvalAll_Line_
USE Test_Method, ONLY: OK
IMPLICIT NONE
REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
REAL(DFP) :: nodes(1, 3), yp(3), ym(3)

nodes(1, :) = [-1.0_DFP, 1.0_DFP, 0.0_DFP]
! basis values at x = 0.5 and x = -0.5
yp = [-0.125_DFP, 0.375_DFP, 0.75_DFP]
ym = [0.375_DFP, -0.125_DFP, 0.75_DFP]

! Test 1: one point, monomial basis by default
BLOCK
  REAL(DFP) :: y(3)
  y = LagrangeEvalAll_Line(order=2_I4B, x=0.5_DFP, xij=nodes)
  CALL OK(MAXVAL(ABS(y - yp)) < tol, "One point: values at x = 0.5")
END BLOCK

! Test 2: one point, Legendre basis gives the same Lagrange values
BLOCK
  REAL(DFP) :: y(3)
  y = LagrangeEvalAll_Line(order=2_I4B, x=0.5_DFP, xij=nodes, &
                           basisType=poly%Legendre)
  CALL OK(MAXVAL(ABS(y - yp)) < tol, "One point: Legendre basis")
END BLOCK

! Test 3: compute coefficients once, then reuse them
BLOCK
  REAL(DFP) :: y(3), coeff(3, 3)
  y = LagrangeEvalAll_Line(order=2_I4B, x=0.5_DFP, xij=nodes, &
                           coeff=coeff, firstCall=.TRUE.)
  CALL OK(MAXVAL(ABS(y - yp)) < tol, "firstCall true fills coeff")
  y = LagrangeEvalAll_Line(order=2_I4B, x=-0.5_DFP, xij=nodes, &
                           coeff=coeff, firstCall=.FALSE.)
  CALL OK(MAXVAL(ABS(y - ym)) < tol, "firstCall false reuses coeff")
END BLOCK

! Test 4: subroutine at one point
BLOCK
  REAL(DFP) :: y(4)
  INTEGER(I4B) :: tsize
  CALL LagrangeEvalAll_Line_(order=2_I4B, x=0.5_DFP, xij=nodes, ans=y, &
                             tsize=tsize)
  CALL OK(tsize == 3, "One point subroutine: three values")
  CALL OK(MAXVAL(ABS(y(1:3) - yp)) < tol, "One point subroutine values")
END BLOCK

! Test 5: subroutine with caller work arrays coeff and xx
BLOCK
  REAL(DFP) :: x(1, 2), y(2, 3), coeff(3, 3), xx(2, 3)
  INTEGER(I4B) :: nrow, ncol
  x(1, :) = [0.5_DFP, -0.5_DFP]
  CALL LagrangeEvalAll_Line_(order=2_I4B, x=x, xij=nodes, ans=y, &
                             nrow=nrow, ncol=ncol, coeff=coeff, xx=xx, &
                             firstCall=.TRUE.)
  CALL OK(nrow == 2 .AND. ncol == 3, "Work array form: 2 by 3")
  CALL OK(MAXVAL(ABS(y(1, :) - yp)) < tol .AND. &
          MAXVAL(ABS(y(2, :) - ym)) < tol, "Work array form values")
END BLOCK

END PROGRAM main
