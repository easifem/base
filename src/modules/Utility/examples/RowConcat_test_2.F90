PROGRAM main
USE GlobalData, ONLY: I4B, LGT, DFP, CHAR_LF
USE AppendUtility, ONLY: RowConcat
USE Display_Method, ONLY: Display
IMPLICIT NONE

! Concat columns of a rank2 array and rank 1 array.
CALL Display(RowConcat(a=RESHAPE([1, 2, 4, 5], [2, 2]), &
                       b=[3, 6]), "RowConcat=")

! RowConcat=
! ----------
!    1  4
!    2  5
!    3  6

! RowConcat works fine even when number of columns in rank-2 array
! is not the same as the size of rank-1 array.

CALL Display(RowConcat(a=RESHAPE([1, 2, 4, 5], [2, 2]), &
                       b=[3, 6, 7]), "RowConcat=")

! RowConcat=
! ----------
!  1  4  0
!  2  5  0
!  3  6  7

! We can also concat the columns of a vector and a matrix.
CALL Display(RowConcat(b=RESHAPE([1, 2, 4, 5], [2, 2]), &
                       a=[3, 6]), "RowConcat=")
! RowConcat=
! ----------
!    3  6
!    1  4
!    2  5

CALL Display(RowConcat(b=RESHAPE([1, 2, 4, 5], [2, 2]), &
                       a=[3, 6, 7]), "RowConcat=")
! RowConcat=
! ----------
!  3  6  7
!  1  4  0
!  2  5  0
END PROGRAM main
