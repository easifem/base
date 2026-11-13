!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: Matmul for a vector and a rank-4 array

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE MatmulUtility, ONLY: Matmul
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
REAL(DFP) :: a1(3), a2(3, 2, 2, 2)
REAL(DFP) :: ans(2, 2, 2)
REAL(DFP) :: ref(2, 2, 2)
INTEGER(I4B) :: ii

a1 = [(REAL(ii, DFP), ii = 1, 3)]
a2 = RESHAPE([(0.5_DFP * REAL(ii, DFP), ii = 1, 24)], &
             [3_I4B, 2_I4B, 2_I4B, 2_I4B])

ans = Matmul(a1, a2)

! reference: contract the last index of a1 with the first
! index of a2 by using the intrinsic MATMUL on reshaped arrays
ref = RESHAPE(MATMUL(a1, RESHAPE(a2, [3_I4B, 8_I4B])), SHAPE(ref))
CALL OK(ALL(ABS(ans - ref) .LT. tol), "same as the reference")

! first and last entries computed by hand
CALL OK(ABS(ans(1, 1, 1) - 7.0_DFP) .LT. tol, &
        "first entry")
CALL OK(ABS(ans(2, 2, 2) - 70.0_DFP) .LT. tol, &
        "last entry")

! the shape of the result
CALL OK(ALL(SHAPE(Matmul(a1, a2)) .EQ. [2, 2, 2]), &
        "shape of the result")

END PROGRAM main
