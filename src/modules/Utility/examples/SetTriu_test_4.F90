!> author: Vikas Sharma, Ph. D.
! date: 2026-10-19
! summary: SetTriu and SetTril with an lu that is too small
!
! Description:
!   Regression test for bug M-41 in bug-reports/Utility_bug_report.md.
!   A matrix lu smaller than A, or a flat lu with fewer entries than the
!   triangle, was read out of bounds. Now only the entries that lu has
!   are set.

PROGRAM main
USE GlobalData, ONLY: I4B
USE TriagUtility, ONLY: SetTriu
USE TriagUtility, ONLY: SetTril
USE Test_Method, ONLY: OK
IMPLICIT NONE

INTEGER(I4B) :: c(3, 3), lu(2, 2)

lu = 9_I4B

! matrix lu smaller than A: only the triangle entries inside lu
c = 0_I4B
CALL SetTriu(A=c, lu=lu)
CALL OK(ALL(c .EQ. RESHAPE([9, 0, 0, 9, 9, 0, 0, 0, 0], [3, 3])), &
        "SetTriu: matrix lu is 2 x 2")

c = 0_I4B
CALL SetTril(A=c, lu=lu)
CALL OK(ALL(c .EQ. RESHAPE([9, 9, 0, 0, 9, 0, 0, 0, 0], [3, 3])), &
        "SetTril: matrix lu is 2 x 2")

! flat lu shorter than the triangle: the diagonal comes first
c = 0_I4B
CALL SetTriu(A=c, flate=.TRUE., lu=[1_I4B, 2_I4B, 3_I4B])
CALL OK(ALL(c .EQ. RESHAPE([1, 0, 0, 0, 2, 0, 0, 0, 3], [3, 3])), &
        "SetTriu: flat lu has 3 entries")

c = 0_I4B
CALL SetTril(A=c, flate=.TRUE., lu=[1_I4B, 2_I4B, 3_I4B])
CALL OK(ALL(c .EQ. RESHAPE([1, 0, 0, 0, 2, 0, 0, 0, 3], [3, 3])), &
        "SetTril: flat lu has 3 entries")

END PROGRAM main
