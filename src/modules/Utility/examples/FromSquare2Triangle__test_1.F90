!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: FromSquare2Triangle_ maps the biunit square to a triangle

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE MappingUtility, ONLY: FromSquare2Triangle_
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
REAL(DFP), PARAMETER :: third = 1.0_DFP / 3.0_DFP
REAL(DFP) :: xin(2, 3), ans(2, 5), eb(2, 3), eu(2, 3)
INTEGER(I4B) :: nrow, ncol

! vertex 1, vertex 2 and the image of the centroid in the biunit square
xin = RESHAPE([-1.0_DFP, -1.0_DFP, 1.0_DFP, -1.0_DFP, 0.0_DFP, -third], &
              [2_I4B, 3_I4B])
eb = RESHAPE([-1.0_DFP, -1.0_DFP, 1.0_DFP, -1.0_DFP, -third, -third], &
             [2_I4B, 3_I4B])
eu = RESHAPE([0.0_DFP, 0.0_DFP, 1.0_DFP, 0.0_DFP, third, third], &
             [2_I4B, 3_I4B])

! biunit square to biunit triangle
CALL FromSquare2Triangle_(xin=xin, ans=ans, from="BIUNIT", to="BIUNIT", &
                          nrow=nrow, ncol=ncol)
CALL OK(nrow .EQ. 2_I4B, "nrow is 2")
CALL OK(ncol .EQ. 3_I4B, "ncol is SIZE(xin, 2)")
CALL OK(ALL(ABS(ans(1:nrow, 1:ncol) - eb) .LT. tol), "to BIUNIT triangle")

! biunit square to unit triangle, lower case is accepted
CALL FromSquare2Triangle_(xin=xin, ans=ans, from="biunit", to="unit", &
                          nrow=nrow, ncol=ncol)
CALL OK(ALL(ABS(ans(1:nrow, 1:ncol) - eu) .LT. tol), "to UNIT triangle")

END PROGRAM main
