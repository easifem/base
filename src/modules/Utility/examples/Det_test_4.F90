!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: Determinant of batches of 3 x 3 and 4 x 4 matrices

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE InvUtility, ONLY: DET
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
REAL(DFP) :: a3(3, 3, 3), a4(4, 4, 3)
REAL(DFP), ALLOCATABLE :: ans(:)
INTEGER(I4B) :: i

! batch of 3 x 3 matrices
a3(:, :, 1) = 0.0_DFP
DO i = 1, 3
  a3(i, i, 1) = 2.0_DFP
END DO
a3(:, :, 2) = RESHAPE(REAL([1, 2, 3, 0, 1, 4, 5, 6, 0], KIND=DFP), &
                      [3, 3], ORDER=[2, 1])
a3(:, :, 3) = RESHAPE(REAL([2, -1, 0, -1, 2, -1, 0, -1, 2], &
                           KIND=DFP), [3, 3], ORDER=[2, 1])

ans = DET(a3)
CALL OK(SIZE(ans) .EQ. 3_I4B, "size of ans, 3x3 batch")
CALL OK(ABS(ans(1) - 8.0_DFP) .LT. tol, "det of 2*I (3x3)")
CALL OK(ABS(ans(2) - 1.0_DFP) .LT. tol, "det of a3(:,:,2)")
CALL OK(ABS(ans(3) - 4.0_DFP) .LT. tol, "det of tridiagonal (3x3)")

! batch of 4 x 4 matrices
a4(:, :, 1) = 0.0_DFP
DO i = 1, 4
  a4(i, i, 1) = REAL(i, KIND=DFP)
END DO
a4(:, :, 2) = RESHAPE(REAL([1, 1, 1, 1, 1, 2, 3, 4, 1, 3, 6, 10, 1, 4, &
                            10, 20], KIND=DFP), [4, 4], ORDER=[2, 1])
a4(:, :, 3) = RESHAPE(REAL([1, 2, 0, 1, 0, 1, 3, 0, 2, 0, 1, 1, 1, 1, &
                            0, 2], KIND=DFP), [4, 4], ORDER=[2, 1])

ans = DET(a4)
CALL OK(SIZE(ans) .EQ. 3_I4B, "size of ans, 4x4 batch")
CALL OK(ABS(ans(1) - 24.0_DFP) .LT. tol, "det of diagonal (4x4)")
CALL OK(ABS(ans(2) - 1.0_DFP) .LT. tol, "det of Pascal matrix (4x4)")
CALL OK(ABS(ans(3) - 16.0_DFP) .LT. tol, "det of a4(:,:,3)")

END PROGRAM main
