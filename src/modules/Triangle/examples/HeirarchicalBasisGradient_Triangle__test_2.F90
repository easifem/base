!> author: Vikas Sharma, Ph. D.
! date: 2026-11-20
! summary: Testing HeirarchicalBasisGradient_Triangle_ with edge and
! face orientation

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE Test_Method, ONLY: OK
USE TriangleInterpolationUtility, ONLY: HeirarchicalBasisGradient_Triangle
USE TriangleInterpolationUtility, ONLY: HeirarchicalBasisGradient_Triangle_
IMPLICIT NONE

INTEGER(I4B), PARAMETER :: order = 3, n = 10, np = 3
REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
REAL(DFP) :: x(2, np), expected(np, n, 2)

x(:, 1) = [-0.6_DFP, -0.4_DFP]
x(:, 2) = [0.0_DFP, -0.6_DFP]
x(:, 3) = [-0.8_DFP, 0.2_DFP]

expected = HeirarchicalBasisGradient_Triangle(order=order, pe1=order, &
                                              pe2=order, pe3=order, xij=x, &
                                              refTriangle="BIUNIT")

! Test 1: default orientation (subroutine without orientation)
BLOCK
  REAL(DFP) :: ans(np, n, 2)
  INTEGER(I4B) :: s(3)

  CALL HeirarchicalBasisGradient_Triangle_(order=order, pe1=order, &
                                           pe2=order, pe3=order, xij=x, &
                                           refTriangle="BIUNIT", ans=ans, &
                                           tsize1=s(1), tsize2=s(2), &
                                           tsize3=s(3))
  CALL OK(ALL(s .EQ. [np, n, 2]), "default orientation: size")
  CALL OK(MAXVAL(ABS(ans - expected)) .LT. tol, &
          "default orientation: values")
END BLOCK

! Test 2: positive orientation gives the default gradients
BLOCK
  REAL(DFP) :: ans(np, n, 2)
  INTEGER(I4B) :: s(3)

  CALL HeirarchicalBasisGradient_Triangle_(order=order, pe1=order, &
                         pe2=order, pe3=order, xij=x, edgeOrient1=1_I4B, &
                         edgeOrient2=1_I4B, edgeOrient3=1_I4B, &
                         faceOrient=[0_I4B, 1_I4B], refTriangle="BIUNIT", &
                         ans=ans, tsize1=s(1), tsize2=s(2), tsize3=s(3))
  CALL OK(ALL(s .EQ. [np, n, 2]), "orientation +1: size")
  CALL OK(MAXVAL(ABS(ans - expected)) .LT. tol, "orientation +1: values")
END BLOCK

! Test 3: reversed edge 2 negates the gradient of its odd mode only
! columns 6 and 7 belong to edge 2 (modes 1 and 2)
BLOCK
  REAL(DFP) :: ans(np, n, 2)
  INTEGER(I4B) :: s(3)

  CALL HeirarchicalBasisGradient_Triangle_(order=order, pe1=order, &
                         pe2=order, pe3=order, xij=x, edgeOrient1=1_I4B, &
                         edgeOrient2=-1_I4B, edgeOrient3=1_I4B, &
                         faceOrient=[0_I4B, 1_I4B], refTriangle="BIUNIT", &
                         ans=ans, tsize1=s(1), tsize2=s(2), tsize3=s(3))
  CALL OK(MAXVAL(ABS(ans(:, 6, :) - expected(:, 6, :))) .LT. tol, &
          "edge 2 reversed: even mode unchanged")
  CALL OK(MAXVAL(ABS(ans(:, 7, :) + expected(:, 7, :))) .LT. tol, &
          "edge 2 reversed: odd mode changes sign")
END BLOCK

END PROGRAM main
