!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: Swap two scalars with a mask

PROGRAM main
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: LGT
USE SwapUtility, ONLY: Swap
USE Test_Method, ONLY: OK
IMPLICIT NONE

INTEGER(I4B) :: ia, ib
REAL(DFP) :: ra, rb
REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP

! mask is true: the values are swapped
ia = 3_I4B
ib = 4_I4B
CALL Swap(ia, ib, .TRUE._LGT)
CALL OK(ia .EQ. 4_I4B .AND. ib .EQ. 3_I4B, "integer, mask true")

! mask is false: nothing changes
CALL Swap(ia, ib, .FALSE._LGT)
CALL OK(ia .EQ. 4_I4B .AND. ib .EQ. 3_I4B, "integer, mask false")

! a typical use: make sure that a >= b
ra = 3.0_DFP
rb = 4.0_DFP
CALL Swap(ra, rb, ra .LE. rb)
CALL OK(ABS(ra - 4.0_DFP) .LT. tol .AND. ABS(rb - 3.0_DFP) .LT. tol, &
        "real, a <= b")

! the second call does nothing because now a > b
CALL Swap(ra, rb, ra .LE. rb)
CALL OK(ABS(ra - 4.0_DFP) .LT. tol .AND. ABS(rb - 3.0_DFP) .LT. tol, &
        "real, a > b")

END PROGRAM main
