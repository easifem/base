!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: SetTril sets the lower triangle to a scalar value

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE TriagUtility, ONLY: SetTril
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP) :: c(3, 3), e(3, 3), d(3, 5)
INTEGER(I4B) :: i, j
REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP

! real value on the triangle, then an integer value further away
c = 0.0_DFP
CALL SetTril(A=c, val=5.0_DFP)
CALL SetTril(A=c, val=2_I4B, diagNo=-1_I4B)

DO j = 1, 3
  DO i = 1, 3
    e(i, j) = 0.0_DFP
    IF (j - i .EQ. 0) e(i, j) = 5.0_DFP
    IF (j - i .LE. -1) e(i, j) = 2.0_DFP
  END DO
END DO
CALL OK(ALL(ABS(c - e) .LT. tol), "real (3,3), real and integer val")

! rectangular matrix
d = 0.0_DFP
CALL SetTril(A=d, val=1.0_DFP, diagNo=1_I4B)
CALL OK(COUNT(d .GT. 0.5_DFP) .EQ. 9_I4B, "real (3,5), diagNo=1")

END PROGRAM main
