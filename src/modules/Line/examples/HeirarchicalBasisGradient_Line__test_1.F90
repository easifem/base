!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Checked example for LineInterpolationUtility.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE LineInterpolationUtility, ONLY: HeirarchicalBasisGradient_Line_
USE Test_Method, ONLY: OK
IMPLICIT NONE
REAL(DFP) :: x(1,3), y(3,3,1)
INTEGER(I4B) :: nr,nc,nd

x(1,:) = [-0.5_DFP,0.0_DFP,0.5_DFP]
CALL HeirarchicalBasisGradient_Line_(order=2_I4B, xij=x, &
  refLine="BIUNIT", ans=y, dim1=nr,dim2=nc,dim3=nd)
CALL OK(MAXVAL(ABS(y(:,3,1)- &
  (3.0_DFP*x(1,:)/SQRT(6.0_DFP)))) < 1.0E-10_DFP, "Analytic quadratic mode")
END PROGRAM main
