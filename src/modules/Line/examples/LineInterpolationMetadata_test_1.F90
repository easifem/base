!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Checked example for LineInterpolationUtility.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE LineInterpolationUtility, ONLY: LagrangeDOF_Line
USE LineInterpolationUtility, ONLY: LagrangeInDOF_Line
USE LineInterpolationUtility, ONLY: GetTotalDOF_Line
USE LineInterpolationUtility, ONLY: GetTotalInDOF_Line
USE LineInterpolationUtility, ONLY: LagrangeDegree_Line
USE LineInterpolationUtility, ONLY: RefElemDomain_Line
USE LineInterpolationUtility, ONLY: LagrangeDOF_Point
USE String_Class, ONLY: String
USE Test_Method, ONLY: OK
IMPLICIT NONE
INTEGER(I4B), ALLOCATABLE :: degree(:, :)
TYPE(String) :: domain

CALL OK(LagrangeDOF_Line(3_I4B) == 4, "Cubic total DOFs")
CALL OK(LagrangeInDOF_Line(3_I4B) == 2, "Cubic interior DOFs")
CALL OK(GetTotalDOF_Line(3_I4B,"H1","Lagrange") == &
  LagrangeDOF_Line(3_I4B), "Total DOF APIs agree")
CALL OK(GetTotalInDOF_Line(3_I4B,"H1","Lagrange") == &
  LagrangeInDOF_Line(3_I4B), "Interior DOF APIs agree")
degree = LagrangeDegree_Line(3_I4B)
CALL OK(SIZE(degree,1) == 4 .AND. &
  SIZE(degree,2) == 1, "Monomial rows and coordinate columns")
domain = RefElemDomain_Line("H1","Lagrange")
CALL OK(domain%chars() == "BIUNIT", "Reference domain")
CALL OK(LagrangeDOF_Point(3_I4B) == 1, "Point has one DOF")
END PROGRAM main
