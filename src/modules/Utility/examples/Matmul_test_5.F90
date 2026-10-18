!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: Matmul for a rank-3 array and a vector

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE MatmulUtility, ONLY: Matmul
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
REAL(DFP) :: a1(2, 3, 4), a2(4)
REAL(DFP) :: ans(2, 3)
REAL(DFP) :: ref(2, 3)
INTEGER(I4B) :: ii

a1 = RESHAPE([(REAL(ii, DFP), ii = 1, 24)], &
             [2_I4B, 3_I4B, 4_I4B])
a2 = [(0.5_DFP * REAL(ii, DFP), ii = 1, 4)]

ans = Matmul(a1, a2)

! reference: contract the last index of a1 with the first
! index of a2 by using the intrinsic MATMUL on reshaped arrays
ref = RESHAPE(MATMUL(RESHAPE(a1, [6_I4B, 4_I4B]), a2), SHAPE(ref))
CALL OK(ALL(ABS(ans - ref) .LT. tol), "same as the reference")

! first and last entries computed by hand
CALL OK(ABS(ans(1, 1) - 65.0_DFP) .LT. tol, &
        "first entry")
CALL OK(ABS(ans(2, 3) - 90.0_DFP) .LT. tol, &
        "last entry")

! the shape of the result
CALL OK(ALL(SHAPE(Matmul(a1, a2)) .EQ. [2, 3]), &
        "shape of the result")

END PROGRAM main
