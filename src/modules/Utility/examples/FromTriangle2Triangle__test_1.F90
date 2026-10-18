!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: FromTriangle2Triangle_ maps between triangles

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE MappingUtility, ONLY: FromTriangle2Triangle_
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
REAL(DFP), PARAMETER :: third = 1.0_DFP / 3.0_DFP
REAL(DFP) :: xb(2, 4), xu(2, 4), ans(3, 4)
REAL(DFP) :: x1(2), x2(2), x3(2)
INTEGER(I4B) :: nrow, ncol

! vertices and centroid of the biunit and unit triangles
xb = RESHAPE([-1.0_DFP, -1.0_DFP, 1.0_DFP, -1.0_DFP, -1.0_DFP, 1.0_DFP, &
              -third, -third], [2_I4B, 4_I4B])
xu = RESHAPE([0.0_DFP, 0.0_DFP, 1.0_DFP, 0.0_DFP, 0.0_DFP, 1.0_DFP, &
              third, third], [2_I4B, 4_I4B])

! biunit to unit
CALL FromTriangle2Triangle_(xin=xb, ans=ans, nrow=nrow, ncol=ncol, &
                            from="BIUNIT", to="UNIT")
CALL OK(nrow .EQ. 2_I4B .AND. ncol .EQ. 4_I4B, "nrow and ncol")
CALL OK(ALL(ABS(ans(1:2, 1:4) - xu) .LT. tol), "BIUNIT to UNIT")

! unit to biunit
CALL FromTriangle2Triangle_(xin=xu, ans=ans, nrow=nrow, ncol=ncol, &
                            from="UNIT", to="BIUNIT")
CALL OK(ALL(ABS(ans(1:2, 1:4) - xb) .LT. tol), "UNIT to BIUNIT")

! unit to a general triangle, x1, x2, x3 are needed
x1 = [1.0_DFP, 1.0_DFP]
x2 = [4.0_DFP, 1.0_DFP]
x3 = [1.0_DFP, 3.0_DFP]
CALL FromTriangle2Triangle_(xin=xu, ans=ans, nrow=nrow, ncol=ncol, &
                            from="UNIT", to="TRIANGLE", x1=x1, x2=x2, x3=x3)
CALL OK(nrow .EQ. 2_I4B, "nrow is SIZE(x1)")
CALL OK(ALL(ABS(ans(1:2, 2) - x2) .LT. tol), "(1,0) goes to x2")
CALL OK(ALL(ABS(ans(1:2, 4) - (x1 + x2 + x3) / 3.0_DFP) .LT. tol), &
        "centroid goes to centroid")

END PROGRAM main
