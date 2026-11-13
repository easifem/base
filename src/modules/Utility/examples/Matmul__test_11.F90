!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: Matmul_ for a vector and a vector

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE MatmulUtility, ONLY: Matmul_
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
REAL(DFP) :: a1(3), a2(3)
REAL(DFP) :: ans
INTEGER(I4B) :: ii

a1 = [(REAL(ii, DFP), ii = 1, 3)]
a2 = [(0.5_DFP * REAL(ii, DFP), ii = 1, 3)]

ans = 0.0_DFP
CALL Matmul_(a1, a2, ans)

! a1 . a2 = 0.5 * (1 + 4 + 9)
CALL OK(ABS(ans - 7.0_DFP) .LT. tol, "value of the result")
CALL OK(ABS(ans - DOT_PRODUCT(a1, a2)) .LT. tol, &
        "same as DOT_PRODUCT")

END PROGRAM main
