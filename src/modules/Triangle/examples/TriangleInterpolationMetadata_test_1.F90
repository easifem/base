!> author: OpenAI Codex
! date: 2026-10-30
! summary: Checked example for TriangleInterpolationUtility.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE TriangleInterpolationUtility, ONLY: LagrangeDOF_Triangle
USE TriangleInterpolationUtility, ONLY: LagrangeInDOF_Triangle
USE TriangleInterpolationUtility, ONLY: GetTotalDOF_Triangle
USE TriangleInterpolationUtility, ONLY: GetTotalInDOF_Triangle
USE TriangleInterpolationUtility, ONLY: LagrangeDegree_Triangle
USE TriangleInterpolationUtility, ONLY: RefElemDomain_Triangle
USE String_Class, ONLY: String
USE Test_Method, ONLY: OK
IMPLICIT NONE
INTEGER(I4B), ALLOCATABLE :: degree(:, :)
TYPE(String) :: domain

CALL OK(LagrangeDOF_Triangle(3_I4B) == 10, "Cubic total DOFs")
CALL OK(LagrangeInDOF_Triangle(3_I4B) == 1, "Cubic interior DOFs")
CALL OK(GetTotalDOF_Triangle(3_I4B,"H1","Lagrange") == &
  LagrangeDOF_Triangle(3_I4B), "Total DOF APIs agree")
CALL OK(GetTotalInDOF_Triangle(3_I4B,"H1","Lagrange") == &
  LagrangeInDOF_Triangle(3_I4B), "Interior DOF APIs agree")
degree = LagrangeDegree_Triangle(3_I4B)
CALL OK(SIZE(degree,1) == 10 .AND. &
  SIZE(degree,2) == 2, "Monomial rows and coordinate columns")
domain = RefElemDomain_Triangle("H1","Lagrange")
CALL OK(domain%chars() == "UNIT", "Reference domain")
END PROGRAM main
