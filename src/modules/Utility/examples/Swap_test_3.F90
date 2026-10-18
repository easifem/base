!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: Swap two matrices (integer, real, complex)

PROGRAM main
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: DFPC
USE SwapUtility, ONLY: Swap
USE Test_Method, ONLY: OK
IMPLICIT NONE

INTEGER(I4B) :: ia(2, 3), ib(2, 3), ia0(2, 3), ib0(2, 3)
REAL(DFP) :: ra(3, 2), rb(3, 2), ra0(3, 2), rb0(3, 2)
COMPLEX(DFPC) :: ca(2, 2), cb(2, 2), ca0(2, 2), cb0(2, 2)
REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP

! integer matrices
ia0 = RESHAPE([1_I4B, 2_I4B, 3_I4B, 4_I4B, 5_I4B, 6_I4B], [2, 3])
ib0 = -ia0
ia = ia0
ib = ib0
CALL Swap(ia, ib)
CALL OK(ALL(ia .EQ. ib0), "integer: a")
CALL OK(ALL(ib .EQ. ia0), "integer: b")

! real matrices
ra0 = RESHAPE([1.0_DFP, 2.0_DFP, 3.0_DFP, 4.0_DFP, 5.0_DFP, 6.0_DFP], &
              [3, 2])
rb0 = 0.5_DFP * ra0
ra = ra0
rb = rb0
CALL Swap(ra, rb)
CALL OK(ALL(ABS(ra - rb0) .LT. tol), "real: a")
CALL OK(ALL(ABS(rb - ra0) .LT. tol), "real: b")

! complex matrices
ca0 = RESHAPE([(1.0_DFP, 0.0_DFP), (0.0_DFP, 1.0_DFP), &
               (2.0_DFP, 2.0_DFP), (3.0_DFP, -1.0_DFP)], [2, 2])
cb0 = CONJG(ca0)
ca = ca0
cb = cb0
CALL Swap(ca, cb)
CALL OK(ALL(ABS(ca - cb0) .LT. tol), "complex: a")
CALL OK(ALL(ABS(cb - ca0) .LT. tol), "complex: b")

END PROGRAM main
