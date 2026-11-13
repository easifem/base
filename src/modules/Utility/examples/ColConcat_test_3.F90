PROGRAM main
USE GlobalData, ONLY: I4B, LGT, DFP, CHAR_LF
USE AppendUtility, ONLY: ColConcat
USE Display_Method, ONLY: Display
USE ArangeUtility, ONLY: Arange
IMPLICIT NONE

CALL Display(ColConcat(a=RESHAPE([1, 2, 3, 4], [2, 2]), &
                       b=RESHAPE([5, 6, 7, 8], [2, 2])), "colconcat=")

! colconcat=
! ----------
! 1  3  5  7
! 2  4  6  8

CALL Display(ColConcat(a=RESHAPE(arange(1, 9), [3, 3]), &
                       b=RESHAPE(arange(10, 13), [2, 2])), "colconcat=")

! ```txt title="results"
!     colconcat=
! ------------------
!  1   4   7  10  12
!  2   5   8  11  13
!  3   6   9   0   0

CALL Display(ColConcat(a=RESHAPE(arange(1, 4), [2, 2]), &
                       b=RESHAPE(arange(5, 13), [3, 3])), "colconcat=")

! ```txt title="results"
!     colconcat=
! ------------------
!  1   3   5   8  11
!  2   4   6   9  12
!  0   0   7  10  13
! ```

END PROGRAM main
