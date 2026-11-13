!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: SetTril copies the lower triangular part of lu into A

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE TriagUtility, ONLY: SetTril
USE TriagUtility, ONLY: Tril
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
CALL SetTril(A=c, lu=a)
CALL OK(ALL(c .EQ. RESHAPE([11, 21, 31, -1, 22, 32, -1, -1, 33], &
                           [3, 3])), "integer, default diagNo")

c = -1_I4B
CALL SetTril(A=c, lu=a, diagNo=1_I4B)
CALL OK(ALL(c .EQ. RESHAPE([11, 21, 31, 12, 22, 32, -1, 23, 33], &
                           [3, 3])), "integer, diagNo=1")

DO j = 1, 5
  DO i = 1, 3
    b(i, j) = REAL(10 * i + j, DFP)
  END DO
END DO

! setting into a zero matrix gives Tril
d = 0.0_DFP
CALL SetTril(A=d, lu=b, diagNo=-1_I4B)
CALL OK(ALL(ABS(d - Tril(b, diagNo=-1_I4B)) .LT. tol), &
        "real (3,5), diagNo=-1")

END PROGRAM main
