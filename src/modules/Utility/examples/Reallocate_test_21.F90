!> author: Vikas Sharma, Ph. D.
! date: 2026-10-19
! summary: Reallocate sparse storage vectors with isExpand
!
! Description:
!   Regression test for bug H-06 in bug-reports/Utility_bug_report.md.
!   The sparse forms Reallocate(A, nA, IA, nIA, JA, nJA) and
!   Reallocate(A, nA, IA, nIA) accepted isExpand and expandFactor but
!   did not use them. Each vector now follows the rule of the rank 2
!   and rank 3 overloads (see Reallocate_test_20).

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE ReallocateUtility, ONLY: Reallocate
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), ALLOCATABLE :: A(:)
INTEGER(I4B), ALLOCATABLE :: IA(:), JA(:)

! A, IA, JA
CALL Reallocate(A, 5_I4B, IA, 4_I4B, JA, 5_I4B)
A = 1.0_DFP
IA = 1_I4B
JA = 1_I4B
CALL Reallocate(A, 3_I4B, IA, 4_I4B, JA, 7_I4B, isExpand=.TRUE., &
                expandFactor=2_I4B)
CALL OK(SIZE(A) .EQ. 5_I4B, "AIJ isExpand: A keeps size 5")
CALL OK(ALL(A(1:3) .EQ. 0.0_DFP), "AIJ isExpand: A(1:3) is zero")
CALL OK(ALL(A(4:5) .EQ. 1.0_DFP), "AIJ isExpand: A(4:5) is not changed")
CALL OK(SIZE(IA) .EQ. 4_I4B, "AIJ isExpand: IA keeps size 4")
CALL OK(ALL(IA .EQ. 0_I4B), "AIJ isExpand: IA is zero")
CALL OK(SIZE(JA) .EQ. 14_I4B, "AIJ isExpand: JA grows to 7 * 2")
CALL OK(ALL(JA(1:7) .EQ. 0_I4B), "AIJ isExpand: JA(1:7) is zero")

! A, IA
CALL Reallocate(A, 2_I4B, IA, 6_I4B, isExpand=.TRUE.)
CALL OK(SIZE(A) .EQ. 5_I4B, "AI isExpand: A keeps size 5")
CALL OK(SIZE(IA) .EQ. 6_I4B, "AI isExpand: IA grows to 6")

! without isExpand the sizes are exact
CALL Reallocate(A, 2_I4B, IA, 2_I4B)
CALL OK(SIZE(A) .EQ. 2_I4B .AND. SIZE(IA) .EQ. 2_I4B, &
        "AI no isExpand: exact sizes")

END PROGRAM main
