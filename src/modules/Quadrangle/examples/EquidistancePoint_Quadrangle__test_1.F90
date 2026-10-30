!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Checked example for QuadrangleInterpolationUtility.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE QuadrangleInterpolationUtility, ONLY: EquidistancePoint_Quadrangle_
USE Test_Method, ONLY: OK
IMPLICIT NONE
REAL(DFP) :: nodes(2,9)
INTEGER(I4B) :: nr,nc

CALL EquidistancePoint_Quadrangle_(order=2_I4B,ans=nodes,nrow=nr,ncol=nc)
CALL OK(nr == 2 .AND. nc == 9, &
  "Interpolation coordinate dimensions")
CALL OK(MAXVAL(ABS(nodes)) <= 1.0_DFP, "Reference coordinate bounds")
END PROGRAM main
