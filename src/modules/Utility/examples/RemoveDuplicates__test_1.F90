!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: RemoveDuplicates_ for an integer vector, no allocation

PROGRAM main
USE GlobalData, ONLY: I4B
USE IntegerUtility, ONLY: RemoveDuplicates_
USE Test_Method, ONLY: OK
IMPLICIT NONE

INTEGER(I4B) :: avec(11), bvec(6)
INTEGER(I4B) :: tsize

! unsorted vector: set isSorted to false
avec = [5_I4B, 4_I4B, 4_I4B, 4_I4B, 3_I4B, 3_I4B, 2_I4B, 2_I4B, &
        1_I4B, 1_I4B, 1_I4B]
CALL RemoveDuplicates_(avec, tsize, .FALSE.)
CALL OK(tsize .EQ. 5_I4B, "number of unique entries")
CALL OK(ALL(avec(1:tsize) .EQ. [1_I4B, 2_I4B, 3_I4B, 4_I4B, 5_I4B]), &
        "unique entries")
CALL OK(ALL(avec(tsize + 1:) .EQ. 0_I4B), "remaining entries are zero")

! sorted vector: set isSorted to true to skip sorting
bvec = [1_I4B, 1_I4B, 2_I4B, 3_I4B, 3_I4B, 3_I4B]
CALL RemoveDuplicates_(obj=bvec, tsize=tsize, isSorted=.TRUE.)
CALL OK(tsize .EQ. 3_I4B, "sorted: number of unique entries")
CALL OK(ALL(bvec(1:tsize) .EQ. [1_I4B, 2_I4B, 3_I4B]), &
        "sorted: unique entries")

END PROGRAM main
