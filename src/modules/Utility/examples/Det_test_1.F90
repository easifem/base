!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: Determinant of matrices of size 1 to 4

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE InvUtility, ONLY: DET
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
REAL(DFP) :: a1(1, 1), a2(2, 2), a3(3, 3), a4(4, 4), eye(4, 4)
REAL(DFP) :: ans
INTEGER(I4B) :: i

! 1 x 1 matrix
a1 = 5.0_DFP
ans = DET(a1)
CALL OK(ABS(ans - 5.0_DFP) .LT. tol, "det of 1x1 matrix")

! 2 x 2 matrix, det = 3*6 - 8*4 = -14
a2 = RESHAPE(REAL([3, 8, 4, 6], KIND=DFP), [2, 2], ORDER=[2, 1])
ans = DET(a2)
CALL OK(ABS(ans + 14.0_DFP) .LT. tol, "det of 2x2 matrix")

! 3 x 3 matrix, det = 1
a3 = RESHAPE(REAL([1, 2, 3, 0, 1, 4, 5, 6, 0], KIND=DFP), [3, 3], &
             ORDER=[2, 1])
ans = DET(a3)
CALL OK(ABS(ans - 1.0_DFP) .LT. tol, "det of 3x3 matrix")

! 3 x 3 tridiagonal matrix, det = 4
a3 = RESHAPE(REAL([2, -1, 0, -1, 2, -1, 0, -1, 2], KIND=DFP), [3, 3], &
             ORDER=[2, 1])
ans = DET(a3)
CALL OK(ABS(ans - 4.0_DFP) .LT. tol, "det of 3x3 tridiagonal matrix")

! 4 x 4 identity matrix
eye = 0.0_DFP
DO i = 1, 4
  eye(i, i) = 1.0_DFP
END DO
ans = DET(eye)
CALL OK(ABS(ans - 1.0_DFP) .LT. tol, "det of 4x4 identity")

! 4 x 4 Pascal matrix, det = 1
a4 = RESHAPE(REAL([1, 1, 1, 1, 1, 2, 3, 4, 1, 3, 6, 10, 1, 4, 10, 20], &
                  KIND=DFP), [4, 4], ORDER=[2, 1])
ans = DET(a4)
CALL OK(ABS(ans - 1.0_DFP) .LT. tol, "det of 4x4 Pascal matrix")

! 4 x 4 non-symmetric matrix, det = 16
a4 = RESHAPE(REAL([1, 2, 0, 1, 0, 1, 3, 0, 2, 0, 1, 1, 1, 1, 0, 2], &
                  KIND=DFP), [4, 4], ORDER=[2, 1])
ans = DET(a4)
CALL OK(ABS(ans - 16.0_DFP) .LT. tol, "det of 4x4 non-symmetric matrix")

END PROGRAM main
