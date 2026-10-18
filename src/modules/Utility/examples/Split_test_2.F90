!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: Split for real vectors

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE SplitUtility, ONLY: Split
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP) :: areal(10), breal(9)
REAL(DFP), ALLOCATABLE :: b(:)
REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
INTEGER(I4B) :: ii

DO ii = 1, 10
  areal(ii) = 0.5_DFP * REAL(ii, DFP)
END DO
breal = areal(1:9)

! even size 10: 5 + 5
b = Split(areal, 1)
CALL OK(SIZE(b) .EQ. 5_I4B, "even size, size of section 1")
CALL OK(ALL(ABS(b - areal(:5)) .LT. tol), "even size, section 1")
b = Split(areal, 2)
CALL OK(SIZE(b) .EQ. 5_I4B, "even size, size of section 2")
CALL OK(ALL(ABS(b - areal(6:)) .LT. tol), "even size, section 2")

! odd size 9: 4 + 5
b = Split(breal, 1)
CALL OK(SIZE(b) .EQ. 4_I4B, "odd size, size of section 1")
CALL OK(ALL(ABS(b - breal(:4)) .LT. tol), "odd size, section 1")
b = Split(breal, 2)
CALL OK(SIZE(b) .EQ. 5_I4B, "odd size, size of section 2")
CALL OK(ALL(ABS(b - breal(5:)) .LT. tol), "odd size, section 2")

! size 1
b = Split(areal(3:3), 1)
CALL OK(SIZE(b) .EQ. 0_I4B, "size 1, section 1 is empty")
b = Split(areal(3:3), 2)
CALL OK(ABS(b(1) - 1.5_DFP) .LT. tol, "size 1, section 2 is x(1)")

END PROGRAM main
