!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: OuterProd of two vectors with sym

PROGRAM main
USE GlobalData, ONLY: DFP
USE ProductUtility, ONLY: OuterProd
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
REAL(DFP) :: a(3), b(3), ans(3, 3)

a = [1.0_DFP, 2.0_DFP, 3.0_DFP]
b = [4.0_DFP, 5.0_DFP, 6.0_DFP]

! sym = .FALSE. gives the ordinary outer product
ans = OuterProd(a, b, sym=.FALSE.)
CALL OK(ALL(ABS(ans - OuterProd(a, b)) .LT. tol), "sym = .FALSE.")
CALL OK(ABS(ans(1, 2) - 5.0_DFP) .LT. tol, "ans(1,2) = a(1) * b(2)")

! sym = .TRUE. gives the symmetric part
ans = OuterProd(a, b, sym=.TRUE.)
CALL OK(ALL(ABS(ans - TRANSPOSE(ans)) .LT. tol), "ans is symmetric")
CALL OK(ALL(ABS(ans - 0.5_DFP * (OuterProd(a, b) + OuterProd(b, a))) &
            .LT. tol), "ans = (a x b + b x a) / 2")

! entries computed by hand
CALL OK(ABS(ans(1, 1) - 4.0_DFP) .LT. tol, "ans(1,1) = 1 * 4")
CALL OK(ABS(ans(1, 2) - 6.5_DFP) .LT. tol, "ans(1,2) = (1*5 + 4*2) / 2")
CALL OK(ABS(ans(3, 3) - 18.0_DFP) .LT. tol, "ans(3,3) = 3 * 6")

END PROGRAM main
