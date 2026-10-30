!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Checked example for QuadrangleInterpolationUtility.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE QuadrangleInterpolationUtility, ONLY: LagrangeDOF_Quadrangle
USE QuadrangleInterpolationUtility, ONLY: LagrangeInDOF_Quadrangle
USE QuadrangleInterpolationUtility, ONLY: GetTotalDOF_Quadrangle
USE QuadrangleInterpolationUtility, ONLY: GetTotalInDOF_Quadrangle
USE QuadrangleInterpolationUtility, ONLY: LagrangeDegree_Quadrangle
USE QuadrangleInterpolationUtility, ONLY: RefElemDomain_Quadrangle
USE String_Class, ONLY: String
USE Test_Method, ONLY: OK
IMPLICIT NONE
INTEGER(I4B), ALLOCATABLE :: degree(:,:)
TYPE(String) :: domain

CALL OK(LagrangeDOF_Quadrangle(2_I4B) == 9, "Total nodal DOFs")
CALL OK(LagrangeInDOF_Quadrangle(2_I4B) == 1, "One interior mode")
CALL OK(GetTotalDOF_Quadrangle(2_I4B,"H1","Lagrange") == 9, &
  "Total count dispatcher")
CALL OK(GetTotalInDOF_Quadrangle(2_I4B,"H1","Lagrange") == 1, &
  "Interior count dispatcher")
degree = LagrangeDegree_Quadrangle(2_I4B)
CALL OK(ALL(SHAPE(degree) == [9,2]), "Exponent table layout")
domain = RefElemDomain_Quadrangle("H1","Lagrange")
CALL OK(domain%chars() == "BIUNIT", "Reference domain")
CALL OK(LagrangeDOF_Quadrangle(p=2_I4B,q=3_I4B) == 12, &
  "Anisotropic total count")
END PROGRAM main
