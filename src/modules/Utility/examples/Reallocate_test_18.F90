!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: Reallocate the A, IA, JA vectors of a sparse matrix

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE ReallocateUtility, ONLY: Reallocate
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), ALLOCATABLE :: A(:)
INTEGER(I4B), ALLOCATABLE :: IA(:), JA(:)

! CSR storage of a 3x3 matrix with 5 nonzeros
CALL Reallocate(A, 5_I4B, IA, 4_I4B, JA, 5_I4B)
CALL OK(SIZE(A) .EQ. 5_I4B, "size of A is 5")
CALL OK(SIZE(IA) .EQ. 4_I4B, "size of IA is 4")
CALL OK(SIZE(JA) .EQ. 5_I4B, "size of JA is 5")
CALL OK(ALL(A .EQ. 0.0_DFP), "A is zero")
CALL OK(ALL(IA .EQ. 0_I4B) .AND. ALL(JA .EQ. 0_I4B), "IA, JA are zero")

! new sizes, the old values are not kept
A = 1.0_DFP
IA = 1_I4B
CALL Reallocate(A, 7_I4B, IA, 4_I4B, JA, 7_I4B)
CALL OK(SIZE(A) .EQ. 7_I4B, "size of A is 7")
CALL OK(ALL(IA .EQ. 0_I4B), "IA is reset")
CALL OK(SIZE(JA) .EQ. 7_I4B, "size of JA is 7")

END PROGRAM main
