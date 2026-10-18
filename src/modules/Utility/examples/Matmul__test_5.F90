!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: Matmul_ for a rank-3 array and a vector

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE MatmulUtility, ONLY: Matmul
USE MatmulUtility, ONLY: Matmul_
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
REAL(DFP) :: a1(2, 3, 4), a2(4)
REAL(DFP) :: ans(3, 4)
REAL(DFP) :: ref(2, 3)
INTEGER(I4B) :: nrow, ncol
INTEGER(I4B) :: ii

a1 = RESHAPE([(REAL(ii, DFP), ii = 1, 24)], &
             [2_I4B, 3_I4B, 4_I4B])
a2 = [(0.5_DFP * REAL(ii, DFP), ii = 1, 4)]

! ans is larger than needed, only a part of it is written
ans = 0.0_DFP
CALL Matmul_(a1, a2, ans, nrow, ncol)

! the size of the written part
CALL OK(ALL([nrow, ncol] .EQ. [2, 3]), &
        "size of the result")

! same as the function version
ref = Matmul(a1, a2)
CALL OK(ALL(ABS(ans(1:nrow, 1:ncol) - ref) .LT. tol), &
        "same as Matmul")

! first and last entries computed by hand
CALL OK(ABS(ans(1, 1) - 65.0_DFP) .LT. tol, &
        "first entry")
CALL OK(ABS(ans(2, 3) - 90.0_DFP) .LT. tol, &
        "last entry")

! the rest of ans is not changed
CALL OK(ABS(SUM(ans) - SUM(ref)) .LT. tol, "rest is untouched")

END PROGRAM main
