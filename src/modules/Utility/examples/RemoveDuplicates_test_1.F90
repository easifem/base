!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: RemoveDuplicates for an allocatable integer vector

PROGRAM main
USE GlobalData, ONLY: I4B
USE IntegerUtility, ONLY: RemoveDuplicates
USE Test_Method, ONLY: OK
IMPLICIT NONE

INTEGER(I4B), ALLOCATABLE :: avec(:)

! duplicates are removed and the result is sorted
avec = [5_I4B, 4_I4B, 4_I4B, 4_I4B, 3_I4B, 3_I4B, 2_I4B, 2_I4B, &
        1_I4B, 1_I4B, 1_I4B]
CALL RemoveDuplicates(avec)
CALL OK(SIZE(avec) .EQ. 5_I4B, "size after removing duplicates")
CALL OK(ALL(avec .EQ. [1_I4B, 2_I4B, 3_I4B, 4_I4B, 5_I4B]), &
        "unique entries")

! no duplicates: the vector is only sorted
avec = [3_I4B, 1_I4B, 2_I4B]
CALL RemoveDuplicates(avec)
CALL OK(ALL(avec .EQ. [1_I4B, 2_I4B, 3_I4B]), "no duplicates")

! all entries are the same
avec = [7_I4B, 7_I4B, 7_I4B]
CALL RemoveDuplicates(avec)
CALL OK(SIZE(avec) .EQ. 1_I4B, "all entries are the same")
CALL OK(avec(1) .EQ. 7_I4B, "the only entry")

! an unallocated vector is not changed
DEALLOCATE (avec)
CALL RemoveDuplicates(avec)
CALL OK(.NOT. ALLOCATED(avec), "unallocated vector")

END PROGRAM main
