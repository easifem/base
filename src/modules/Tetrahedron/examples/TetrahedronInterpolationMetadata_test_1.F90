!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Checked example for TetrahedronInterpolationUtility.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE TetrahedronInterpolationUtility, ONLY: LagrangeDOF_Tetrahedron
USE TetrahedronInterpolationUtility, ONLY: LagrangeInDOF_Tetrahedron
USE TetrahedronInterpolationUtility, ONLY: GetTotalDOF_Tetrahedron
USE TetrahedronInterpolationUtility, ONLY: GetTotalInDOF_Tetrahedron
USE TetrahedronInterpolationUtility, ONLY: LagrangeDegree_Tetrahedron
USE TetrahedronInterpolationUtility, ONLY: RefElemDomain_Tetrahedron
USE String_Class, ONLY: String
USE Test_Method, ONLY: OK
IMPLICIT NONE
INTEGER(I4B), ALLOCATABLE :: degree(:,:)
TYPE(String) :: domain

CALL OK(LagrangeDOF_Tetrahedron(4_I4B) == 35, "Total nodal DOFs")
CALL OK(LagrangeInDOF_Tetrahedron(4_I4B) == 1, "One interior mode")
CALL OK(GetTotalDOF_Tetrahedron(4_I4B,"H1","Lagrange") == 35, &
  "Total count dispatcher")
CALL OK(GetTotalInDOF_Tetrahedron(4_I4B,"H1","Lagrange") == 1, &
  "Interior count dispatcher")
degree = LagrangeDegree_Tetrahedron(4_I4B)
CALL OK(ALL(SHAPE(degree) == [35,3]), "Exponent table layout")
domain = RefElemDomain_Tetrahedron("H1","Lagrange")
CALL OK(domain%chars() == "UNIT", "Reference domain")
END PROGRAM main
