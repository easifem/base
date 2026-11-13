!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Construct recursive nodes on a 3D simplex.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: Equidistance
USE RecursiveNodesUtility, ONLY: RecursiveNode3D_
USE Test_Method, ONLY: OK
IMPLICIT NONE
REAL(DFP) :: nodes(4, 10)
INTEGER(I4B) :: nr, nc

CALL RecursiveNode3D_(order=2_I4B, ipType=Equidistance, &
  domain="BARYCENTRIC", ans=nodes, nrow=nr, ncol=nc)
CALL OK(nr == 4 .AND. nc == 10, "Active node dimensions")
CALL OK(MAXVAL(ABS(SUM(nodes, DIM=1)-1.0_DFP)) &
  < 1.0E-11_DFP, "Barycentric coordinates sum to one")
CALL OK(MINVAL(nodes) >= -1.0E-11_DFP, "Nodes lie in simplex")
END PROGRAM main
