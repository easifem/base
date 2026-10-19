!> author: Vikas Sharma, Ph. D.
! date: 2026-10-19
! summary: FromTriangle2Triangle_ with BT, nsd = 4, missing x1 and BB
!
! Description:
!   Regression test for bugs M-21 and M-22 in
!   bug-reports/Utility_bug_report.md.
!   Only nsd <= 3 worked, x1, x2, x3 were not checked, the biunit to
!   physical case was missing, and some names left nrow unset.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE MappingUtility, ONLY: FromTriangle2Triangle_
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
REAL(DFP) :: xb(2, 3), xu(2, 3), ans(4, 3)
REAL(DFP) :: x1(4), x2(4), x3(4)
INTEGER(I4B) :: nrow, ncol

! the vertices of the biunit and unit triangles
xb = RESHAPE([-1.0_DFP, -1.0_DFP, 1.0_DFP, -1.0_DFP, -1.0_DFP, 1.0_DFP], &
             [2_I4B, 3_I4B])
xu = RESHAPE([0.0_DFP, 0.0_DFP, 1.0_DFP, 0.0_DFP, 0.0_DFP, 1.0_DFP], &
             [2_I4B, 3_I4B])

! a triangle in 4D
x1 = [1.0_DFP, 2.0_DFP, 3.0_DFP, 4.0_DFP]
x2 = [5.0_DFP, 6.0_DFP, 7.0_DFP, 8.0_DFP]
x3 = [9.0_DFP, 1.0_DFP, 2.0_DFP, 3.0_DFP]

! biunit to a physical triangle
CALL FromTriangle2Triangle_(xin=xb, ans=ans, nrow=nrow, ncol=ncol, &
                            from="biunit", to="triangle", x1=x1, x2=x2, &
                            x3=x3)
CALL OK(nrow .EQ. 4_I4B .AND. ncol .EQ. 3_I4B, "BT: nrow and ncol")
CALL OK(ALL(ABS(ans(:, 1) - x1) .LT. tol), "BT: vertex 1 goes to x1")
CALL OK(ALL(ABS(ans(:, 2) - x2) .LT. tol), "BT: vertex 2 goes to x2")
CALL OK(ALL(ABS(ans(:, 3) - x3) .LT. tol), "BT: vertex 3 goes to x3")

! unit to a physical triangle with nsd = 4
CALL FromTriangle2Triangle_(xin=xu, ans=ans, nrow=nrow, ncol=ncol, &
                            from="Unit", to="Triangle", x1=x1, x2=x2, &
                            x3=x3)
CALL OK(nrow .EQ. 4_I4B, "UT: nrow is 4")
CALL OK(ALL(ABS(ans(:, 3) - x3) .LT. tol), "UT: vertex 3 goes to x3")

! x1 is missing: nothing is written
CALL FromTriangle2Triangle_(xin=xu, ans=ans, nrow=nrow, ncol=ncol, &
                            from="UNIT", to="TRIANGLE", x2=x2, x3=x3)
CALL OK(nrow .EQ. 0_I4B .AND. ncol .EQ. 0_I4B, "UT without x1")

! an unknown name: nothing is written
CALL FromTriangle2Triangle_(xin=xu, ans=ans, nrow=nrow, ncol=ncol, &
                            from="X", to="UNIT")
CALL OK(nrow .EQ. 0_I4B .AND. ncol .EQ. 0_I4B, "unknown name")

! the same triangle: xin is copied
ans = 0.0_DFP
CALL FromTriangle2Triangle_(xin=xb, ans=ans, nrow=nrow, ncol=ncol, &
                            from="Biunit", to="bIUNIT")
CALL OK(nrow .EQ. 2_I4B .AND. ncol .EQ. 3_I4B, "BB: nrow and ncol")
CALL OK(ALL(ABS(ans(1:2, 1:3) - xb) .LT. tol), "BB: xin is copied")

END PROGRAM main
