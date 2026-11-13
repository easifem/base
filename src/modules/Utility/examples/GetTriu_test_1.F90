!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: GetTriu returns the upper triangular part as a full matrix

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE TriagUtility, ONLY: GetTriu
USE TriagUtility, ONLY: Triu
USE Test_Method, ONLY: OK
IMPLICIT NONE

INTEGER(I4B) :: a(3, 3), lu(3, 3), i, j
REAL(DFP) :: b(3, 5), lu2(3, 5)
REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP

DO j = 1, 3
  DO i = 1, 3
    a(i, j) = 10 * i + j
  END DO
END DO

! entries outside the triangle are set to zero
lu = 99_I4B
CALL GetTriu(A=a, lu=lu)
CALL OK(ALL(lu .EQ. Triu(a)), "integer, default diagNo")

CALL GetTriu(A=a, diagNo=-1_I4B, lu=lu)
CALL OK(ALL(lu .EQ. RESHAPE([11, 21, 0, 12, 22, 32, 13, 23, 33], &
                            [3, 3])), "integer, diagNo=-1")

DO j = 1, 5
  DO i = 1, 3
    b(i, j) = REAL(10 * i + j, DFP)
  END DO
END DO

CALL GetTriu(A=b, diagNo=1_I4B, lu=lu2)
CALL OK(ALL(ABS(lu2 - Triu(b, diagNo=1_I4B)) .LT. tol), &
        "real (3,5), diagNo=1")

END PROGRAM main
