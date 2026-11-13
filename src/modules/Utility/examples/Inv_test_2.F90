!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: Inverse of 4 x 4 matrices and of singular matrices

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE InvUtility, ONLY: INV
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
REAL(DFP) :: a2(2, 2), b2(2, 2), a3(3, 3), b3(3, 3)
REAL(DFP) :: a4(4, 4), b4(4, 4), eye4(4, 4)
INTEGER(I4B) :: i

eye4 = 0.0_DFP
DO i = 1, 4
  eye4(i, i) = 1.0_DFP
END DO

! 4 x 4 Pascal matrix, det = 1
a4 = RESHAPE(REAL([1, 1, 1, 1, 1, 2, 3, 4, 1, 3, 6, 10, 1, 4, 10, 20], &
                  KIND=DFP), [4, 4], ORDER=[2, 1])
CALL INV(invA=b4, A=a4)
CALL OK(MAXVAL(ABS(MATMUL(a4, b4) - eye4)) .LT. tol, &
        "A * inv(A) = I, 4x4 Pascal matrix")
CALL OK(MAXVAL(ABS(MATMUL(b4, a4) - eye4)) .LT. tol, &
        "inv(A) * A = I, 4x4 Pascal matrix")

! 4 x 4 non-symmetric matrix, det = 16
a4 = RESHAPE(REAL([1, 2, 0, 1, 0, 1, 3, 0, 2, 0, 1, 1, 1, 1, 0, 2], &
                  KIND=DFP), [4, 4], ORDER=[2, 1])
CALL INV(invA=b4, A=a4)
CALL OK(MAXVAL(ABS(MATMUL(a4, b4) - eye4)) .LT. tol, &
        "A * inv(A) = I, 4x4 non-symmetric matrix")

! the inverse of the identity is the identity
CALL INV(invA=b4, A=eye4)
CALL OK(MAXVAL(ABS(b4 - eye4)) .LT. tol, "inverse of 4x4 identity")

! a singular matrix gives a zero matrix
a2 = RESHAPE(REAL([1, 2, 2, 4], KIND=DFP), [2, 2], ORDER=[2, 1])
b2 = 1.0_DFP
CALL INV(invA=b2, A=a2)
CALL OK(ALL(b2 .EQ. 0.0_DFP), "singular 2x2 matrix gives zero")

a3 = RESHAPE(REAL([1, 2, 3, 4, 5, 6, 7, 8, 9], KIND=DFP), [3, 3], &
             ORDER=[2, 1])
b3 = 1.0_DFP
CALL INV(invA=b3, A=a3)
CALL OK(ALL(b3 .EQ. 0.0_DFP), "singular 3x3 matrix gives zero")

END PROGRAM main
