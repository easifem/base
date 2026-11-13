!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: SetTril with flate sets the lower triangle from a vector

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE TriagUtility, ONLY: SetTril
USE TriagUtility, ONLY: Tril
USE Test_Method, ONLY: OK
IMPLICIT NONE

INTEGER(I4B) :: c(3, 3), v(6), i, j
REAL(DFP) :: b(3, 5), d(3, 5)
REAL(DFP), ALLOCATABLE :: w(:)
REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP

! lu is used in the same order as Tril with flate
v = [1, 2, 3, 4, 5, 6]
c = 0_I4B
CALL SetTril(A=c, flate=.TRUE., lu=v)
CALL OK(ALL(c .EQ. RESHAPE([1, 4, 6, 0, 2, 5, 0, 0, 3], &
                           [3, 3])), "integer, diagNo=0")

DO j = 1, 5
  DO i = 1, 3
    b(i, j) = REAL(10 * i + j, DFP)
  END DO
END DO

! round trip: Tril with flate, then SetTril with flate
w = Tril(b, flate=.TRUE., diagNo=-1_I4B)
d = 0.0_DFP
CALL SetTril(A=d, flate=.TRUE., lu=w, diagNo=-1_I4B)
CALL OK(ALL(ABS(d - Tril(b, diagNo=-1_I4B)) .LT. tol), &
        "real (3,5), round trip, diagNo=-1")

END PROGRAM main
