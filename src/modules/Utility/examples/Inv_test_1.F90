!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: Inverse of matrices of size 1 to 3

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE InvUtility, ONLY: INV
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
REAL(DFP) :: a1(1, 1), b1(1, 1), a2(2, 2), b2(2, 2), ex2(2, 2)
REAL(DFP) :: a3(3, 3), b3(3, 3), ex3(3, 3), eye3(3, 3)
INTEGER(I4B) :: i

! 1 x 1 matrix
a1 = 4.0_DFP
CALL INV(invA=b1, A=a1)
CALL OK(ABS(b1(1, 1) - 0.25_DFP) .LT. tol, "inverse of 1x1 matrix")

! 2 x 2 matrix, det = 10
a2 = RESHAPE(REAL([4, 7, 2, 6], KIND=DFP), [2, 2], ORDER=[2, 1])
ex2 = RESHAPE([0.6_DFP, -0.7_DFP, -0.2_DFP, 0.4_DFP], [2, 2], &
              ORDER=[2, 1])
CALL INV(invA=b2, A=a2)
CALL OK(MAXVAL(ABS(b2 - ex2)) .LT. tol, "inverse of 2x2 matrix")

! 3 x 3 matrix, det = 1, the inverse has integer entries
a3 = RESHAPE(REAL([1, 2, 3, 0, 1, 4, 5, 6, 0], KIND=DFP), [3, 3], &
             ORDER=[2, 1])
ex3 = RESHAPE(REAL([-24, 18, 5, 20, -15, -4, -5, 4, 1], KIND=DFP), &
              [3, 3], ORDER=[2, 1])
CALL INV(invA=b3, A=a3)
CALL OK(MAXVAL(ABS(b3 - ex3)) .LT. tol, "inverse of 3x3 matrix")

! A * inv(A) = inv(A) * A = I
eye3 = 0.0_DFP
DO i = 1, 3
  eye3(i, i) = 1.0_DFP
END DO
CALL OK(MAXVAL(ABS(MATMUL(a3, b3) - eye3)) .LT. tol, "A * inv(A) = I")
CALL OK(MAXVAL(ABS(MATMUL(b3, a3) - eye3)) .LT. tol, "inv(A) * A = I")

END PROGRAM main
