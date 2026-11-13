!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Checked example for TetrahedronInterpolationUtility.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE TetrahedronInterpolationUtility, ONLY: HeirarchicalBasis_Tetrahedron_
USE Test_Method, ONLY: OK
IMPLICIT NONE
REAL(DFP) :: x(3,2),v(2,4)
INTEGER(I4B) :: nr,nc

x(:,1) = [0.1_DFP,0.2_DFP,0.1_DFP]
x(:,2) = [0.2_DFP,0.1_DFP,0.2_DFP]
CALL HeirarchicalBasis_Tetrahedron_(order=1_I4B,xij=x, &
  refTetrahedron="UNIT",ans=v,nrow=nr,ncol=nc)
CALL OK(nr == 2 .AND. nc == 4, "Active dimensions")
CALL OK(MAXVAL(ABS(SUM(v,DIM=2)-1.0_DFP)) < 1.0E-11_DFP, &
  "Linear hierarchical partition")
END PROGRAM main
