!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: GetTriu with flate returns the upper triangular part as a vector

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE TriagUtility, ONLY: GetTriu
USE TriagUtility, ONLY: Triu
USE Test_Method, ONLY: OK
IMPLICIT NONE

INTEGER(I4B) :: a(3, 3), i, j
INTEGER(I4B), ALLOCATABLE :: v(:)
REAL(DFP) :: b(3, 5)
REAL(DFP), ALLOCATABLE :: w(:)
REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP

DO j = 1, 3
  DO i = 1, 3
    a(i, j) = 10 * i + j
  END DO
END DO

! lu is reallocated to the number of entries
ALLOCATE (v(2))
CALL GetTriu(A=a, flate=.TRUE., lu=v)
CALL OK(SIZE(v) .EQ. 6_I4B, "integer, diagNo=0: size")
CALL OK(ALL(v .EQ. [11, 22, 33, 12, 23, 13]), "integer, diagNo=0")

CALL GetTriu(A=a, flate=.TRUE., diagNo=1_I4B, lu=v)
CALL OK(ALL(v .EQ. [12, 23, 13]), "integer, diagNo=1")

DO j = 1, 5
  DO i = 1, 3
    b(i, j) = REAL(10 * i + j, DFP)
  END DO
END DO

! same result as Triu with flate
CALL GetTriu(A=b, flate=.TRUE., diagNo=-1_I4B, lu=w)
CALL OK(SIZE(w) .EQ. 14_I4B, "real (3,5), diagNo=-1: size")
CALL OK(ALL(ABS(w - Triu(b, flate=.TRUE., diagNo=-1_I4B)) .LT. tol), &
        "real (3,5), same as Triu")

END PROGRAM main
