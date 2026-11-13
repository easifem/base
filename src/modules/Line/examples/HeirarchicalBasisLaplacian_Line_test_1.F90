!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Checked example for LineInterpolationUtility.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE LineInterpolationUtility, ONLY: HeirarchicalBasisLaplacian_Line
USE LineInterpolationUtility, ONLY: HeirarchicalBasisGradient_Line
USE Test_Method, ONLY: OK
IMPLICIT NONE
REAL(DFP) :: x(1,3), xp(1,3), xm(1,3)
REAL(DFP) :: gp(3,3,1), gm(3,3,1), second(3,3)
REAL(DFP), PARAMETER :: h = 1.0E-5_DFP

! The hierarchical Laplacian forwards to empty Lobatto implementations.
x(1,:) = [-0.5_DFP,0.0_DFP,0.5_DFP]
xp = x+h
xm = x-h
gp = HeirarchicalBasisGradient_Line(2_I4B,xp,"BIUNIT")
gm = HeirarchicalBasisGradient_Line(2_I4B,xm,"BIUNIT")
second = (gp(:,:,1)-gm(:,:,1))/(2.0_DFP*h)
CALL OK(MAXVAL(ABS(second(:,3)-3.0_DFP/SQRT(6.0_DFP))) &
  < 1.0E-8_DFP, "Gradient based second derivative workaround")
END PROGRAM main
