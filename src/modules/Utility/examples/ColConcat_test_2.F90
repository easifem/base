PROGRAM main
USE GlobalData, ONLY: I4B, LGT, DFP, CHAR_LF
USE AppendUtility, ONLY: ColConcat
USE MdEncode_Method, ONLY: MdEnCode
USE Display_Method, ONLY: Display
IMPLICIT NONE

! Concat columns of a rank2 array and rank 1 array.
CALL Display(ColConcat(a=RESHAPE([1, 2, 3, 4], [2, 2]), &
                       b=[5, 6]), "colconcat=")

! ColConcat works fine even when number of rows in rank-2
! array is not the same as the size of rank-1 array.

CALL Display(ColConcat(a=RESHAPE([1, 2, 3, 4], [2, 2]), &
                       b=[5, 6, 7]), "colconcat=")

! Results
!
! colconcat=
! ----------
!  1  3  5
!  2  4  6
!  0  0  7

! Concat columns of a rank2 array and rank 1 array.
CALL Display(ColConcat(b=RESHAPE([1, 2, 3, 4], [2, 2]), &
                       a=[5, 6]), "colconcat=")

! colconcat=
! ----------
!  5  1  3
!  6  2  4

! ColConcat works fine even when number of rows in rank-2 array is not the
! same as the size of rank-1 array.
CALL Display(ColConcat(b=RESHAPE([1, 2, 3, 4], [2, 2]), &
                       a=[5, 6, 7]), "colconcat=")

! colconcat=
! ----------
!  5  1  3
!  6  2  4
!  7  0  0
END PROGRAM main
