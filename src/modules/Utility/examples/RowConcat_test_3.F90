PROGRAM main
USE GlobalData, ONLY: I4B, LGT, DFP, CHAR_LF
USE AppendUtility, ONLY: RowConcat
USE Display_Method, ONLY: Display
USE ArangeUtility, ONLY: Arange
IMPLICIT NONE
!
! We can also concat the columns of two matrices.
CALL Display(RowConcat(a=RESHAPE([1, 2, 3, 4], [2, 2]), &
                       b=RESHAPE([5, 6, 7, 8], [2, 2])), "RowConcat=")
!
! RowConcat=
! ----------
!    1  3
!    2  4
!    5  7
!    6  8
!
CALL Display(RowConcat(a=RESHAPE(Arange(1, 9), [3, 3]), &
                       b=RESHAPE(Arange(10, 13), [2, 2])), "RowConcat=")
!
! RowConcat=
! ----------
!  1   4   7
!  2   5   8
!  3   6   9
! 10  12   0
! 11  13   0
!
CALL Display(RowConcat(a=RESHAPE(Arange(1, 4), [2, 2]), &
                       b=RESHAPE(Arange(5, 13), [3, 3])), "RowConcat=")
!
! RowConcat=
! ----------
!  1   3   0
!  2   4   0
!  5   8  11
!  6   9  12
!  7  10  13
END PROGRAM main
