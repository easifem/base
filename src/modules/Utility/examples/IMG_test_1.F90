!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: IMG returns the imaginary part of a complex number
!
! Description:
!   Regression test for bug H-02 in bug-reports/Utility_bug_report.md.
!   IMG computed REAL(x * i), which is -AIMAG(x).

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: DFPC
USE MiscUtility, ONLY: IMG
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
COMPLEX(DFPC) :: z(3)

CALL OK(ABS(IMG((1.0_DFP, 2.0_DFP)) - 2.0_DFP) .LT. tol, &
        "IMG((1,2)) = 2")
CALL OK(ABS(IMG((0.0_DFP, -4.0_DFP)) + 4.0_DFP) .LT. tol, &
        "IMG((0,-4)) = -4")
CALL OK(ABS(IMG((3.0_DFP, 0.0_DFP))) .LT. tol, "real number gives 0")

! IMG is elemental and equals the intrinsic AIMAG
z = [(1.0_DFP, 1.0_DFP), (0.0_DFP, -4.0_DFP), (2.0_DFP, 0.5_DFP)]
CALL OK(ALL(ABS(IMG(z) - AIMAG(z)) .LT. tol), "IMG(z) = AIMAG(z)")

END PROGRAM main
