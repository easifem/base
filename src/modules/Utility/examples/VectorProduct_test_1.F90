!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: Vector (cross) product

PROGRAM main
USE GlobalData, ONLY: DFP
USE ProductUtility, ONLY: VectorProduct
USE ProductUtility, ONLY: Vector_Product
USE ProductUtility, ONLY: Cross_Product
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
REAL(DFP) :: e1(3), e2(3), e3(3), a(3), b(3), c(3)

e1 = [1.0_DFP, 0.0_DFP, 0.0_DFP]
e2 = [0.0_DFP, 1.0_DFP, 0.0_DFP]
e3 = [0.0_DFP, 0.0_DFP, 1.0_DFP]

! unit vectors
CALL OK(ALL(ABS(VectorProduct(e1, e2) - e3) .LT. tol), "e1 x e2 = e3")
CALL OK(ALL(ABS(VectorProduct(e2, e3) - e1) .LT. tol), "e2 x e3 = e1")
CALL OK(ALL(ABS(VectorProduct(e2, e1) + e3) .LT. tol), "e2 x e1 = -e3")

! general vectors
a = [1.0_DFP, 2.0_DFP, 3.0_DFP]
b = [4.0_DFP, 5.0_DFP, 6.0_DFP]
c = VectorProduct(a, b)
CALL OK(ALL(ABS(c - [-3.0_DFP, 6.0_DFP, -3.0_DFP]) .LT. tol), "a x b")
CALL OK(ABS(DOT_PRODUCT(a, c)) .LT. tol, "a x b is orthogonal to a")
CALL OK(ABS(DOT_PRODUCT(b, c)) .LT. tol, "a x b is orthogonal to b")
CALL OK(ALL(ABS(VectorProduct(a, a)) .LT. tol), "a x a = 0")

! the three names call the same function
CALL OK(ALL(ABS(Vector_Product(a, b) - c) .LT. tol), "Vector_Product")
CALL OK(ALL(ABS(Cross_Product(a, b) - c) .LT. tol), "Cross_Product")

END PROGRAM main
