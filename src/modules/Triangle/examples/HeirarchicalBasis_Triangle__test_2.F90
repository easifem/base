!> author: Vikas Sharma, Ph. D.
! date: 2026-11-20
! summary: Testing HeirarchicalBasis_Triangle_ with one order and with
! edge orientation

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE Test_Method, ONLY: OK
USE TriangleInterpolationUtility, ONLY: HeirarchicalBasis_Triangle
USE TriangleInterpolationUtility, ONLY: HeirarchicalBasis_Triangle_
IMPLICIT NONE

INTEGER(I4B), PARAMETER :: order = 3, n = 10
REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
REAL(DFP) :: x(2, 3), expected(3, n)

x(:, 1) = [-0.6_DFP, -0.4_DFP]
x(:, 2) = [0.0_DFP, -0.6_DFP]
x(:, 3) = [-0.8_DFP, 0.2_DFP]

expected = HeirarchicalBasis_Triangle(order=order, xij=x, &
                                      refTriangle="BIUNIT")

! Test 1: one order for all entities
BLOCK
  REAL(DFP) :: ans(3, n + 2)
  INTEGER(I4B) :: nrow, ncol

  CALL HeirarchicalBasis_Triangle_(order=order, xij=x, &
                                   refTriangle="BIUNIT", ans=ans, &
                                   nrow=nrow, ncol=ncol)
  CALL OK(nrow .EQ. 3 .AND. ncol .EQ. n, "one order: size")
  CALL OK(MAXVAL(ABS(ans(1:nrow, 1:ncol) - expected)) .LT. tol, &
          "one order: values")
END BLOCK

! Test 2: positive orientation gives the default basis
BLOCK
  REAL(DFP) :: ans(3, n)
  INTEGER(I4B) :: nrow, ncol

  CALL HeirarchicalBasis_Triangle_(order=order, pe1=order, pe2=order, &
                                   pe3=order, xij=x, refTriangle="BIUNIT", &
                                   edgeOrient1=1_I4B, edgeOrient2=1_I4B, &
                                   edgeOrient3=1_I4B, &
                                   faceOrient=[0_I4B, 1_I4B], ans=ans, &
                                   nrow=nrow, ncol=ncol)
  CALL OK(nrow .EQ. 3 .AND. ncol .EQ. n, "orientation +1: size")
  CALL OK(MAXVAL(ABS(ans - expected)) .LT. tol, "orientation +1: values")
END BLOCK

! Test 3: reversed edge 1 negates its odd modes only
! columns 4 and 5 belong to edge 1 (modes 1 and 2)
BLOCK
  REAL(DFP) :: ans(3, n)
  INTEGER(I4B) :: nrow, ncol

  CALL HeirarchicalBasis_Triangle_(order=order, pe1=order, pe2=order, &
                                   pe3=order, xij=x, refTriangle="BIUNIT", &
                                   edgeOrient1=-1_I4B, edgeOrient2=1_I4B, &
                                   edgeOrient3=1_I4B, &
                                   faceOrient=[0_I4B, 1_I4B], ans=ans, &
                                   nrow=nrow, ncol=ncol)
  CALL OK(MAXVAL(ABS(ans(:, 4) - expected(:, 4))) .LT. tol, &
          "edge 1 reversed: even mode unchanged")
  CALL OK(MAXVAL(ABS(ans(:, 5) + expected(:, 5))) .LT. tol, &
          "edge 1 reversed: odd mode changes sign")
  CALL OK(MAXVAL(ABS(ans(:, 6:n) - expected(:, 6:n))) .LT. tol, &
          "edge 1 reversed: other modes unchanged")
END BLOCK

END PROGRAM main
