PROGRAM main
USE ArangeUtility, ONLY: Arange
USE Display_Method, ONLY: Display
IMPLICIT NONE

CALL Display(arange(1.0, 10.0, 3.0), "ans = ", &
             full=.TRUE., orient="row")
CALL Display(arange(1, 10, 2), "ans = ", &
             full=.TRUE., orient="row")
END PROGRAM main
!               ans =
! ----------------------------------
!  1.0000   4.0000   7.0000  10.0000
!     ans =
! -------------
! 1  3  5  7  9
