PROGRAM main
USE GlobalData, ONLY: I4B, LGT, DFP, CHAR_LF
USE AppendUtility, ONLY: RowConcat
USE Display_Method, ONLY: Display
USE Test_Method, ONLY: ok
IMPLICIT NONE

! Concat two integer vectors
CALL Display(RowConcat(a=[1, 2], b=[3, 4]), "RowConcat=")
CALL OK(ALL(RowConcat(a=[1, 2], b=[3, 4]) &
            .EQ. RESHAPE([1, 3, 2, 4], [2, 2])), "RowConcat")

! RowConcat=
! ----------
!    1  2
!    3  4

! Concat two integer vectors of different length
CALL Display(RowConcat(a=[1, 2], b=[3, 4, 5]), "RowConcat=")
CALL OK(ALL(RowConcat(a=[1, 2], b=[3, 4, 5]) &
            .EQ. RESHAPE([1, 3, 2, 4, 0, 5], [2, 3])), "RowConcat")

! RowConcat=
! ----------
!  1  2  0
!  3  4  5
END PROGRAM main
