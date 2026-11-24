!> author: Vikas Sharma, Ph. D.
! date: 2026-11-20
! summary: Testing HeirarchicalBasisGradient_Triangle against central
! differences of HeirarchicalBasis_Triangle, cell modes included

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE Test_Method, ONLY: OK
USE TriangleInterpolationUtility, ONLY: HeirarchicalBasis_Triangle
USE TriangleInterpolationUtility, ONLY: HeirarchicalBasisGradient_Triangle
IMPLICIT NONE

INTEGER(I4B), PARAMETER :: np = 3
REAL(DFP), PARAMETER :: h = 1.0E-6_DFP, tol = 1.0E-6_DFP
REAL(DFP) :: x(2, np)

x(:, 1) = [0.2_DFP, 0.3_DFP]
x(:, 2) = [0.5_DFP, 0.2_DFP]
x(:, 3) = [0.1_DFP, 0.6_DFP]

! Test 1: order 3 (one cell mode)
BLOCK
  CALL check(3_I4B, 3_I4B, 3_I4B, 3_I4B)
END BLOCK

! Test 2: order 4 (three cell modes)
BLOCK
  CALL check(4_I4B, 4_I4B, 4_I4B, 4_I4B)
END BLOCK

! Test 3: order 5 with mixed edge orders
BLOCK
  CALL check(5_I4B, 2_I4B, 3_I4B, 4_I4B)
END BLOCK

CONTAINS

SUBROUTINE check(order, pe1, pe2, pe3)
  INTEGER(I4B), INTENT(IN) :: order, pe1, pe2, pe3

  REAL(DFP), ALLOCATABLE :: g(:, :, :), fp(:, :), fm(:, :)
  REAL(DFP) :: xp(2, np), xm(2, np)
  INTEGER(I4B) :: dim, n
  CHARACTER(1) :: astr

  n = pe1 + pe2 + pe3 + (order - 1) * (order - 2) / 2
  ALLOCATE (g(np, n, 2), fp(np, n), fm(np, n))
  WRITE (astr, "(I1)") order

  g = HeirarchicalBasisGradient_Triangle(order=order, pe1=pe1, pe2=pe2, &
                                         pe3=pe3, xij=x, refTriangle="UNIT")
  DO dim = 1, 2
    xp = x
    xm = x
    xp(dim, :) = xp(dim, :) + h
    xm(dim, :) = xm(dim, :) - h
    fp = HeirarchicalBasis_Triangle(order=order, pe1=pe1, pe2=pe2, &
                                    pe3=pe3, xij=xp, refTriangle="UNIT")
    fm = HeirarchicalBasis_Triangle(order=order, pe1=pe1, pe2=pe2, &
                                    pe3=pe3, xij=xm, refTriangle="UNIT")
    CALL OK(MAXVAL(ABS(g(:, :, dim) - (fp - fm) / (2.0_DFP * h))) &
            .LT. tol, "order "//astr//": all modes match differences")
  END DO

  DEALLOCATE (g, fp, fm)
END SUBROUTINE check

END PROGRAM main
