!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: Matmul_ for a vector and a rank-4 array

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE MatmulUtility, ONLY: Matmul
USE MatmulUtility, ONLY: Matmul_
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
REAL(DFP) :: a1(3), a2(3, 2, 2, 2)
REAL(DFP) :: ans(3, 3, 3)
REAL(DFP) :: ref(2, 2, 2)
INTEGER(I4B) :: dim1, dim2, dim3
INTEGER(I4B) :: ii

a1 = [(REAL(ii, DFP), ii = 1, 3)]
a2 = RESHAPE([(0.5_DFP * REAL(ii, DFP), ii = 1, 24)], &
             [3_I4B, 2_I4B, 2_I4B, 2_I4B])

! ans is larger than needed, only a part of it is written
ans = 0.0_DFP
CALL Matmul_(a1, a2, ans, dim1, dim2, dim3)

! the size of the written part
CALL OK(ALL([dim1, dim2, dim3] .EQ. [2, 2, 2]), &
        "size of the result")

! same as the function version
ref = Matmul(a1, a2)
CALL OK(ALL(ABS(ans(1:dim1, 1:dim2, 1:dim3) - ref) .LT. tol), &
        "same as Matmul")

! first and last entries computed by hand
CALL OK(ABS(ans(1, 1, 1) - 7.0_DFP) .LT. tol, &
        "first entry")
CALL OK(ABS(ans(2, 2, 2) - 70.0_DFP) .LT. tol, &
        "last entry")

! the rest of ans is not changed
CALL OK(ABS(SUM(ans) - SUM(ref)) .LT. tol, "rest is untouched")

END PROGRAM main
