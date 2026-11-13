!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: HilbertMatrix entries, symmetry and size

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: LGT
USE LinearAlgebraUtility, ONLY: HilbertMatrix
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-14_DFP
REAL(DFP) :: h(4, 4), h1(1, 1)
INTEGER(I4B) :: i, j
LOGICAL(LGT) :: isok

h = HilbertMatrix(4_I4B)

! entries are 1/(i+j-1)
isok = .TRUE.
DO j = 1, 4
  DO i = 1, 4
    IF (ABS(h(i, j) - 1.0_DFP / REAL(i + j - 1, DFP)) .GT. tol) THEN
      isok = .FALSE.
    END IF
  END DO
END DO
CALL OK(isok, "entries are 1/(i+j-1)")

CALL OK(ABS(h(1, 1) - 1.0_DFP) .LE. tol, "h(1,1) = 1")
CALL OK(ABS(h(2, 3) - 0.25_DFP) .LE. tol, "h(2,3) = 1/4")
CALL OK(ABS(h(4, 4) - 1.0_DFP / 7.0_DFP) .LE. tol, "h(4,4) = 1/7")

! the Hilbert matrix is symmetric
CALL OK(ALL(ABS(h - TRANSPOSE(h)) .LE. tol), "symmetric")

! shape of the result
CALL OK(ALL(SHAPE(HilbertMatrix(3_I4B)) .EQ. [3_I4B, 3_I4B]), &
        "shape is (n, n)")

! n = 1
h1 = HilbertMatrix(1_I4B)
CALL OK(ABS(h1(1, 1) - 1.0_DFP) .LE. tol, "n = 1")

END PROGRAM main
