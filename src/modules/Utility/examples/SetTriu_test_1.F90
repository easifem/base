!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: SetTriu copies the upper triangular part of lu into A

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE TriagUtility, ONLY: SetTriu
USE TriagUtility, ONLY: Triu
USE Test_Method, ONLY: OK
IMPLICIT NONE

INTEGER(I4B) :: a(3, 3), c(3, 3), i, j
REAL(DFP) :: b(3, 5), d(3, 5)
REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP

DO j = 1, 3
  DO i = 1, 3
    a(i, j) = 10 * i + j
  END DO
END DO

! entries outside the triangle keep their value
c = -1_I4B
CALL SetTriu(A=c, lu=a)
CALL OK(ALL(c .EQ. RESHAPE([11, -1, -1, 12, 22, -1, 13, 23, 33], &
                           [3, 3])), "integer, default diagNo")

c = -1_I4B
CALL SetTriu(A=c, lu=a, diagNo=-1_I4B)
CALL OK(ALL(c .EQ. RESHAPE([11, 21, -1, 12, 22, 32, 13, 23, 33], &
                           [3, 3])), "integer, diagNo=-1")

DO j = 1, 5
  DO i = 1, 3
    b(i, j) = REAL(10 * i + j, DFP)
  END DO
END DO

! setting into a zero matrix gives Triu
d = 0.0_DFP
CALL SetTriu(A=d, lu=b, diagNo=1_I4B)
CALL OK(ALL(ABS(d - Triu(b, diagNo=1_I4B)) .LT. tol), &
        "real (3,5), diagNo=1")

END PROGRAM main
