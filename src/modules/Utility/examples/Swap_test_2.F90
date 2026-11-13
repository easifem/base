!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: Swap two vectors (integer, real, complex)
!
! With USE_BLAS95 the real and complex vector calls go to the
! SWAP routine of F95_BLAS, which has the same effect.

PROGRAM main
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: DFPC
USE SwapUtility, ONLY: Swap
USE Test_Method, ONLY: OK
IMPLICIT NONE

INTEGER(I4B) :: ia(4), ib(4)
REAL(DFP) :: ra(3), rb(3)
COMPLEX(DFPC) :: ca(2), cb(2)
REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP

! integer vectors
ia = [1_I4B, 2_I4B, 3_I4B, 4_I4B]
ib = [10_I4B, 20_I4B, 30_I4B, 40_I4B]
CALL Swap(ia, ib)
CALL OK(ALL(ia .EQ. [10_I4B, 20_I4B, 30_I4B, 40_I4B]), "integer: a")
CALL OK(ALL(ib .EQ. [1_I4B, 2_I4B, 3_I4B, 4_I4B]), "integer: b")

! array sections
CALL Swap(ia(1:2), ib(3:4))
CALL OK(ALL(ia .EQ. [3_I4B, 4_I4B, 30_I4B, 40_I4B]), "section: a")
CALL OK(ALL(ib .EQ. [1_I4B, 2_I4B, 10_I4B, 20_I4B]), "section: b")

! real vectors
ra = [1.0_DFP, 2.0_DFP, 3.0_DFP]
rb = [-1.0_DFP, -2.0_DFP, -3.0_DFP]
CALL Swap(ra, rb)
CALL OK(ALL(ABS(ra - [-1.0_DFP, -2.0_DFP, -3.0_DFP]) .LT. tol), &
        "real: a")
CALL OK(ALL(ABS(rb - [1.0_DFP, 2.0_DFP, 3.0_DFP]) .LT. tol), "real: b")

! complex vectors
ca = [(1.0_DFP, 1.0_DFP), (2.0_DFP, 2.0_DFP)]
cb = [(0.0_DFP, -1.0_DFP), (5.0_DFP, 0.0_DFP)]
CALL Swap(ca, cb)
CALL OK(ABS(ca(1) - (0.0_DFP, -1.0_DFP)) .LT. tol .AND. &
        ABS(ca(2) - (5.0_DFP, 0.0_DFP)) .LT. tol, "complex: a")
CALL OK(ABS(cb(1) - (1.0_DFP, 1.0_DFP)) .LT. tol .AND. &
        ABS(cb(2) - (2.0_DFP, 2.0_DFP)) .LT. tol, "complex: b")

END PROGRAM main
