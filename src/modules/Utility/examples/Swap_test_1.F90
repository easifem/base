!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: Swap two scalars (integer, real, complex)

PROGRAM main
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: DFPC
USE SwapUtility, ONLY: Swap
USE Test_Method, ONLY: OK
IMPLICIT NONE

INTEGER(I4B) :: ia, ib
REAL(DFP) :: ra, rb
COMPLEX(DFPC) :: ca, cb
REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP

! integer scalars
ia = 3_I4B
ib = 4_I4B
CALL Swap(ia, ib)
CALL OK(ia .EQ. 4_I4B .AND. ib .EQ. 3_I4B, "swap integers")

! swapping twice gives back the input
CALL Swap(ia, ib)
CALL OK(ia .EQ. 3_I4B .AND. ib .EQ. 4_I4B, "swap integers twice")

! real scalars
ra = 1.5_DFP
rb = -2.5_DFP
CALL Swap(ra, rb)
CALL OK(ABS(ra + 2.5_DFP) .LT. tol .AND. ABS(rb - 1.5_DFP) .LT. tol, &
        "swap reals")

! equal values stay equal
ra = 7.0_DFP
rb = 7.0_DFP
CALL Swap(ra, rb)
CALL OK(ABS(ra - 7.0_DFP) .LT. tol .AND. ABS(rb - 7.0_DFP) .LT. tol, &
        "swap equal reals")

! complex scalars
ca = (1.0_DFP, 2.0_DFP)
cb = (3.0_DFP, -4.0_DFP)
CALL Swap(ca, cb)
CALL OK(ABS(ca - (3.0_DFP, -4.0_DFP)) .LT. tol, "swap complex: a")
CALL OK(ABS(cb - (1.0_DFP, 2.0_DFP)) .LT. tol, "swap complex: b")

END PROGRAM main
