!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: Inverse of a batch of 3 x 3 matrices

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE InvUtility, ONLY: INV
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
REAL(DFP) :: a(3, 3, 3), b(3, 3, 3), c(3, 3), eye3(3, 3)
INTEGER(I4B) :: i, k

eye3 = 0.0_DFP
DO i = 1, 3
  eye3(i, i) = 1.0_DFP
END DO

! the last index is the batch index
a(:, :, 1) = 2.0_DFP * eye3
a(:, :, 2) = RESHAPE(REAL([1, 2, 3, 0, 1, 4, 5, 6, 0], KIND=DFP), &
                     [3, 3], ORDER=[2, 1])
a(:, :, 3) = RESHAPE(REAL([2, -1, 0, -1, 2, -1, 0, -1, 2], KIND=DFP), &
                     [3, 3], ORDER=[2, 1])

CALL INV(invA=b, A=a)

CALL OK(MAXVAL(ABS(b(:, :, 1) - 0.5_DFP * eye3)) .LT. tol, &
        "inverse of 2*I")

! each slice is the inverse of the corresponding slice of a
DO k = 1, 3
  CALL OK(MAXVAL(ABS(MATMUL(a(:, :, k), b(:, :, k)) - eye3)) .LT. tol, &
          "A(:,:,k) * invA(:,:,k) = I")
END DO

! each slice equals the inverse computed one matrix at a time
DO k = 1, 3
  CALL INV(invA=c, A=a(:, :, k))
  CALL OK(MAXVAL(ABS(b(:, :, k) - c)) .LT. tol, "slice consistency")
END DO

END PROGRAM main
