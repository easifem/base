!> author: Vikas Sharma, Ph. D.
! date: 2026-10-19
! summary: Reallocate with isExpand zeros all of a new, larger array
!
! Description:
!   Regression test for bug M-12 in bug-reports/Utility_bug_report.md.
!   With isExpand, a too-small array is reallocated with
!   n * expandFactor entries, but only the requested n entries were
!   set to zero; the rest of the new array was undefined. Now a newly
!   allocated array is zero everywhere.
!
!   Before each call a scratch array of the same size is filled with
!   7 and deallocated, so that the new allocation is likely to reuse
!   that memory; without the fix the extra entries would then be 7.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE ReallocateUtility, ONLY: Reallocate
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), ALLOCATABLE :: a(:), m4(:, :, :, :), junk(:), A1(:)
INTEGER(I4B), ALLOCATABLE :: m2(:, :), v1(:), v2(:), ijunk(:), IA(:)

! rank 1: 2 -> 5 * 3 entries
CALL Reallocate(a, 2_I4B)
ALLOCATE (junk(15))
junk = 7.0_DFP
DEALLOCATE (junk)
CALL Reallocate(a, 5_I4B, isExpand=.TRUE., expandFactor=3_I4B)
CALL OK(SIZE(a) .EQ. 15_I4B, "rank 1: size is 5 * 3")
CALL OK(ALL(a .EQ. 0.0_DFP), "rank 1: all entries are zero")

! rank 2: (2, 2) -> (3 * 2, 2)
CALL Reallocate(m2, 2_I4B, 2_I4B)
ALLOCATE (ijunk(12))
ijunk = 7_I4B
DEALLOCATE (ijunk)
CALL Reallocate(m2, 3_I4B, 2_I4B, isExpand=.TRUE., expandFactor=2_I4B)
CALL OK(ALL(SHAPE(m2) .EQ. [6_I4B, 2_I4B]), "rank 2: shape is (6, 2)")
CALL OK(ALL(m2 .EQ. 0_I4B), "rank 2: all entries are zero")

! rank 4: last extent 2 -> 3 * 2
CALL Reallocate(m4, 2_I4B, 2_I4B, 2_I4B, 2_I4B)
ALLOCATE (junk(48))
junk = 7.0_DFP
DEALLOCATE (junk)
CALL Reallocate(m4, 1_I4B, 1_I4B, 1_I4B, 3_I4B, isExpand=.TRUE., &
                expandFactor=2_I4B)
CALL OK(SIZE(m4, 4) .EQ. 6_I4B, "rank 4: last extent is 3 * 2")
CALL OK(ALL(m4 .EQ. 0.0_DFP), "rank 4: all entries are zero")

! several vectors: v2 grows from 2 to 4 * 2
CALL Reallocate(v1, 2_I4B, v2, 2_I4B)
ALLOCATE (ijunk(8))
ijunk = 7_I4B
DEALLOCATE (ijunk)
CALL Reallocate(v1, 1_I4B, v2, 4_I4B, isExpand=.TRUE., &
                expandFactor=2_I4B)
CALL OK(SIZE(v2) .EQ. 8_I4B, "vectors: v2 has 4 * 2 entries")
CALL OK(ALL(v2 .EQ. 0_I4B), "vectors: all of v2 is zero")

! sparse A, IA: A grows from 2 to 3 * 2
CALL Reallocate(A1, 2_I4B, IA, 2_I4B)
ALLOCATE (junk(6))
junk = 7.0_DFP
DEALLOCATE (junk)
CALL Reallocate(A1, 3_I4B, IA, 2_I4B, isExpand=.TRUE., &
                expandFactor=2_I4B)
CALL OK(SIZE(A1) .EQ. 6_I4B, "sparse: A has 3 * 2 entries")
CALL OK(ALL(A1 .EQ. 0.0_DFP), "sparse: all of A is zero")

! a kept, larger array still keeps the entries after the request
a = 1.0_DFP
CALL Reallocate(a, 4_I4B, isExpand=.TRUE.)
CALL OK(SIZE(a) .EQ. 15_I4B, "kept: size is still 15")
CALL OK(ALL(a(1:4) .EQ. 0.0_DFP) .AND. ALL(a(5:) .EQ. 1.0_DFP), &
        "kept: only a(1:4) is reset")

END PROGRAM main
