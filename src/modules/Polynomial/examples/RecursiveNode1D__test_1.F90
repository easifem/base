!> author: OpenAI Codex
! date: 2026-10-30
! summary: Construct recursive nodes on a 1D simplex.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: Equidistance
USE RecursiveNodesUtility, ONLY: RecursiveNode1D_
USE Test_Method, ONLY: OK
IMPLICIT NONE
REAL(DFP) :: nodes(2, 3)
INTEGER(I4B) :: nr, nc

CALL RecursiveNode1D_(order=2_I4B, ipType=Equidistance, &
  domain="BARYCENTRIC", ans=nodes, nrow=nr, ncol=nc)
CALL OK(nr == 2 .AND. nc == 3, "Active node dimensions")
CALL OK(MAXVAL(ABS(SUM(nodes, DIM=1)-1.0_DFP)) &
  < 1.0E-11_DFP, "Barycentric coordinates sum to one")
CALL OK(MINVAL(nodes) >= -1.0E-11_DFP, "Nodes lie in simplex")
END PROGRAM main
