!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: Swap two matrices with a mask

PROGRAM main
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: LGT
USE SwapUtility, ONLY: Swap
USE Test_Method, ONLY: OK
IMPLICIT NONE

INTEGER(I4B) :: ia(2, 2), ib(2, 2)
REAL(DFP) :: ra(2, 2), rb(2, 2), ra0(2, 2), rb0(2, 2)
LOGICAL(LGT) :: mask(2, 2)
REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP

! swap only the diagonal
ia = RESHAPE([1_I4B, 2_I4B, 3_I4B, 4_I4B], [2, 2])
ib = RESHAPE([10_I4B, 20_I4B, 30_I4B, 40_I4B], [2, 2])
mask = RESHAPE([.TRUE., .FALSE., .FALSE., .TRUE.], [2, 2])
CALL Swap(ia, ib, mask)
CALL OK(ALL(ia .EQ. RESHAPE([10_I4B, 2_I4B, 3_I4B, 40_I4B], [2, 2])), &
        "integer: a")
CALL OK(ALL(ib .EQ. RESHAPE([1_I4B, 20_I4B, 30_I4B, 4_I4B], [2, 2])), &
        "integer: b")

! put the larger entry in a
ra0 = RESHAPE([1.0_DFP, 6.0_DFP, 2.0_DFP, 8.0_DFP], [2, 2])
rb0 = RESHAPE([5.0_DFP, 3.0_DFP, 7.0_DFP, 4.0_DFP], [2, 2])
ra = ra0
rb = rb0
CALL Swap(ra, rb, ra .LT. rb)
CALL OK(ALL(ABS(ra - MAX(ra0, rb0)) .LT. tol), "real: a = max")
CALL OK(ALL(ABS(rb - MIN(ra0, rb0)) .LT. tol), "real: b = min")

END PROGRAM main
