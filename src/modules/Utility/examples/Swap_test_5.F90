!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: Swap two vectors with a mask

PROGRAM main
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: LGT
USE SwapUtility, ONLY: Swap
USE Test_Method, ONLY: OK
IMPLICIT NONE

INTEGER(I4B) :: ia(4), ib(4)
REAL(DFP) :: ra(3), rb(3)
LOGICAL(LGT) :: mask(4)
REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP

! only the entries where mask is true are swapped
ia = [1_I4B, 2_I4B, 3_I4B, 4_I4B]
ib = [10_I4B, 20_I4B, 30_I4B, 40_I4B]
mask = [.TRUE., .FALSE., .TRUE., .FALSE.]
CALL Swap(ia, ib, mask)
CALL OK(ALL(ia .EQ. [10_I4B, 2_I4B, 30_I4B, 4_I4B]), "integer: a")
CALL OK(ALL(ib .EQ. [1_I4B, 20_I4B, 3_I4B, 40_I4B]), "integer: b")

! all false: nothing changes
mask = .FALSE.
CALL Swap(ia, ib, mask)
CALL OK(ALL(ia .EQ. [10_I4B, 2_I4B, 30_I4B, 4_I4B]), "all false")

! a typical use: put the larger entry in a
ra = [1.0_DFP, 5.0_DFP, 2.0_DFP]
rb = [3.0_DFP, 4.0_DFP, 2.0_DFP]
CALL Swap(ra, rb, ra .LT. rb)
CALL OK(ALL(ABS(ra - [3.0_DFP, 5.0_DFP, 2.0_DFP]) .LT. tol), "real: a")
CALL OK(ALL(ABS(rb - [1.0_DFP, 4.0_DFP, 2.0_DFP]) .LT. tol), "real: b")

END PROGRAM main
