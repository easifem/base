!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: Determinant of a batch of 2 x 2 matrices

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE InvUtility, ONLY: DET
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
REAL(DFP) :: a(2, 2, 3), b(2, 2, 1)
REAL(DFP), ALLOCATABLE :: ans(:)
INTEGER(I4B) :: k

! the last index is the batch index
a(:, :, 1) = RESHAPE(REAL([1, 0, 0, 1], KIND=DFP), [2, 2], &
                     ORDER=[2, 1])
a(:, :, 2) = RESHAPE(REAL([3, 8, 4, 6], KIND=DFP), [2, 2], &
                     ORDER=[2, 1])
a(:, :, 3) = RESHAPE(REAL([1, 2, 3, 4], KIND=DFP), [2, 2], &
                     ORDER=[2, 1])

ans = DET(a)
CALL OK(ALLOCATED(ans), "ans is allocated")
CALL OK(SIZE(ans) .EQ. 3_I4B, "size of ans equals the batch size")
CALL OK(ABS(ans(1) - 1.0_DFP) .LT. tol, "det of identity")
CALL OK(ABS(ans(2) + 14.0_DFP) .LT. tol, "det of a(:,:,2)")
CALL OK(ABS(ans(3) + 2.0_DFP) .LT. tol, "det of a(:,:,3)")

! each entry equals the determinant of the corresponding slice
DO k = 1, 3
  CALL OK(ABS(ans(k) - DET(a(:, :, k))) .LT. tol, "slice consistency")
END DO

! batch of size one
b(:, :, 1) = a(:, :, 2)
ans = DET(b)
CALL OK(SIZE(ans) .EQ. 1_I4B, "size of ans for a single matrix")
CALL OK(ABS(ans(1) + 14.0_DFP) .LT. tol, "det of a single matrix")

END PROGRAM main
