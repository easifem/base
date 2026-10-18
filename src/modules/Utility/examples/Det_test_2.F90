!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: Properties of the determinant, singular and 5 x 5 matrices

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE InvUtility, ONLY: DET
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
REAL(DFP) :: a2(2, 2), a3(3, 3), b3(3, 3), a4(4, 4), a5(5, 5)
REAL(DFP) :: ans
INTEGER(I4B) :: i

! diagonal matrix, det = 1*2*3*4 = 24
a4 = 0.0_DFP
DO i = 1, 4
  a4(i, i) = REAL(i, KIND=DFP)
END DO
ans = DET(a4)
CALL OK(ABS(ans - 24.0_DFP) .LT. tol, "det of 4x4 diagonal matrix")

! det(2A) = 2**3 det(A) for a 3 x 3 matrix
a3 = RESHAPE(REAL([1, 2, 3, 0, 1, 4, 5, 6, 0], KIND=DFP), [3, 3], &
             ORDER=[2, 1])
ans = DET(2.0_DFP * a3)
CALL OK(ABS(ans - 8.0_DFP) .LT. tol, "det of scaled 3x3 matrix")

! det(A^T) = det(A)
ans = DET(TRANSPOSE(a3))
CALL OK(ABS(ans - 1.0_DFP) .LT. tol, "det of transpose of 3x3 matrix")

! swapping two rows changes the sign of the determinant
b3 = a3
b3(1, :) = a3(2, :)
b3(2, :) = a3(1, :)
ans = DET(b3)
CALL OK(ABS(ans + 1.0_DFP) .LT. tol, "det changes sign on row swap")

! singular 2 x 2 matrix
a2 = RESHAPE(REAL([1, 2, 2, 4], KIND=DFP), [2, 2], ORDER=[2, 1])
ans = DET(a2)
CALL OK(ABS(ans) .LT. tol, "det of singular 2x2 matrix")

! singular 3 x 3 matrix
a3 = RESHAPE(REAL([1, 2, 3, 4, 5, 6, 7, 8, 9], KIND=DFP), [3, 3], &
             ORDER=[2, 1])
ans = DET(a3)
CALL OK(ABS(ans) .LT. tol, "det of singular 3x3 matrix")

! singular 4 x 4 matrix, two equal rows
a4 = RESHAPE(REAL([1, 2, 3, 4, 1, 2, 3, 4, 0, 1, 0, 1, 2, 0, 1, 0], &
                  KIND=DFP), [4, 4], ORDER=[2, 1])
ans = DET(a4)
CALL OK(ABS(ans) .LT. tol, "det of singular 4x4 matrix")

! 5 x 5 tridiagonal matrix, det = 6
a5 = 0.0_DFP
DO i = 1, 5
  a5(i, i) = 2.0_DFP
END DO
DO i = 1, 4
  a5(i, i + 1) = -1.0_DFP
  a5(i + 1, i) = -1.0_DFP
END DO
ans = DET(a5)
CALL OK(ABS(ans - 6.0_DFP) .LT. tol, "det of 5x5 tridiagonal matrix")

END PROGRAM main
