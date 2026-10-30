!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Checked example for TriangleInterpolationUtility.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE TriangleInterpolationUtility, ONLY: GetHierarchicalDOF_Triangle
USE Test_Method, ONLY: OK
IMPLICIT NONE
INTEGER(I4B) :: n

n = GetHierarchicalDOF_Triangle(3_I4B,2_I4B,3_I4B,4_I4B,"H")
CALL OK(n == 10, "Three vertices, six edge modes and one cell mode")
CALL OK(GetHierarchicalDOF_Triangle(3_I4B,2_I4B,3_I4B,4_I4B,"C") &
  == 1, "One cubic bubble mode")
END PROGRAM main
