!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Checked example for TetrahedronInterpolationUtility.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: Equidistance
USE TetrahedronInterpolationUtility, ONLY: InterpolationPoint_Tetrahedron_
USE Test_Method, ONLY: OK
IMPLICIT NONE
REAL(DFP) :: nodes(3,10)
INTEGER(I4B) :: nr,nc

CALL InterpolationPoint_Tetrahedron_(order=2_I4B,ipType=Equidistance, &
  layout="VEFC",ans=nodes,nrow=nr,ncol=nc)
CALL OK(nr == 3 .AND. nc == 10, &
  "Interpolation coordinate dimensions")
CALL OK(MAXVAL(ABS(nodes)) <= 1.0_DFP, "Reference coordinate bounds")
END PROGRAM main
