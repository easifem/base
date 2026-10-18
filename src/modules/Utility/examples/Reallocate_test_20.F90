!> author: Vikas Sharma, Ph. D.
! date: 2026-10-19
! summary: Reallocate several integer vectors with isExpand
!
! Description:
!   Regression test for bug H-06 in bug-reports/Utility_bug_report.md.
!   The multi-vector form accepted isExpand and expandFactor but did
!   not use them. Each vector now follows the rule of the rank 2 and
!   rank 3 overloads:
!
!   - not allocated: allocate exactly n entries;
!   - allocated, isExpand false or absent: reallocate if the size is
!     not n;
!   - allocated, isExpand true: keep the vector if it has at least n
!     entries, else allocate n * expandFactor entries;
!   - in all cases only the first n entries are set to zero.

PROGRAM main
USE GlobalData, ONLY: I4B
USE ReallocateUtility, ONLY: Reallocate
USE Test_Method, ONLY: OK
IMPLICIT NONE

INTEGER(I4B), ALLOCATABLE :: v1(:), v2(:), v3(:)

CALL Reallocate(v1, 5_I4B, v2, 5_I4B)
v1 = 7_I4B
v2 = 7_I4B

! v1 is large enough and is kept, v2 grows to 8 * 2
CALL Reallocate(v1, 2_I4B, v2, 8_I4B, isExpand=.TRUE., &
                expandFactor=2_I4B)
CALL OK(SIZE(v1) .EQ. 5_I4B, "isExpand: v1 keeps size 5")
CALL OK(ALL(v1(1:2) .EQ. 0_I4B), "isExpand: v1(1:2) is zero")
CALL OK(ALL(v1(3:5) .EQ. 7_I4B), "isExpand: v1(3:5) is not changed")
CALL OK(SIZE(v2) .EQ. 16_I4B, "isExpand: v2 grows to 16")
CALL OK(ALL(v2(1:8) .EQ. 0_I4B), "isExpand: v2(1:8) is zero")

! a vector that is not allocated gets exactly n entries
CALL Reallocate(v1, 2_I4B, v2, 2_I4B, v3, 4_I4B, isExpand=.TRUE., &
                expandFactor=3_I4B)
CALL OK(SIZE(v3) .EQ. 4_I4B, "isExpand: new v3 has size 4")
CALL OK(SIZE(v1) .EQ. 5_I4B .AND. SIZE(v2) .EQ. 16_I4B, &
        "isExpand: v1 and v2 are kept")

! without isExpand every vector gets exactly n entries
CALL Reallocate(v1, 2_I4B, v2, 3_I4B, v3, 1_I4B)
CALL OK(SIZE(v1) .EQ. 2_I4B .AND. SIZE(v2) .EQ. 3_I4B .AND. &
        SIZE(v3) .EQ. 1_I4B, "no isExpand: exact sizes")

END PROGRAM main
