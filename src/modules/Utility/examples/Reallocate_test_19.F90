!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: Reallocate a real vector and an integer vector together

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE ReallocateUtility, ONLY: Reallocate
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), ALLOCATABLE :: A(:)
INTEGER(I4B), ALLOCATABLE :: IA(:)

CALL Reallocate(A, 3_I4B, IA, 4_I4B)
CALL OK(SIZE(A) .EQ. 3_I4B, "size of A is 3")
CALL OK(SIZE(IA) .EQ. 4_I4B, "size of IA is 4")
CALL OK(ALL(A .EQ. 0.0_DFP), "A is zero")
CALL OK(ALL(IA .EQ. 0_I4B), "IA is zero")

! new sizes, the old values are not kept
A = 2.0_DFP
CALL Reallocate(A, 3_I4B, IA, 2_I4B)
CALL OK(ALL(A .EQ. 0.0_DFP), "A is reset")
CALL OK(SIZE(IA) .EQ. 2_I4B, "size of IA is 2")

END PROGRAM main
