!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Checked example for TetrahedronInterpolationUtility.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE TetrahedronInterpolationUtility, ONLY: GetVertexDOF_Tetrahedron
USE TetrahedronInterpolationUtility, ONLY: GetEdgeDOF_Tetrahedron
USE TetrahedronInterpolationUtility, ONLY: GetFacetDOF_Tetrahedron
USE TetrahedronInterpolationUtility, ONLY: GetCellDOF_Tetrahedron
USE Test_Method, ONLY: OK
IMPLICIT NONE
INTEGER(I4B) :: n

CALL OK(GetVertexDOF_Tetrahedron() == 4, "Four vertex modes")
CALL OK(GetEdgeDOF_Tetrahedron(2_I4B) == 6, "Quadratic edge modes")
CALL OK(GetFacetDOF_Tetrahedron(3_I4B) == 4, "Cubic facet bubbles")
CALL OK(GetCellDOF_Tetrahedron(4_I4B) == 1, "Quartic cell bubble")
n = GetVertexDOF_Tetrahedron()+GetEdgeDOF_Tetrahedron(4_I4B) &
  +GetFacetDOF_Tetrahedron(4_I4B)+GetCellDOF_Tetrahedron(4_I4B)
CALL OK(n == 35, "Entity counts recover quartic polynomial dimension")
END PROGRAM main
