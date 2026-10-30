!> author: OpenAI Codex
! date: 2026-10-30
! summary: Checked example for QuadrangleInterpolationUtility.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE QuadrangleInterpolationUtility, ONLY: GetHierarchicalDOF_Quadrangle
USE Test_Method, ONLY: OK
IMPLICIT NONE
INTEGER(I4B) :: n

n = GetHierarchicalDOF_Quadrangle(2_I4B,3_I4B, &
  2_I4B,3_I4B,2_I4B,3_I4B,"H")
CALL OK(n == 12, "Four vertices, six edge modes and two cell modes")
CALL OK(GetHierarchicalDOF_Quadrangle(2_I4B,3_I4B, &
  2_I4B,3_I4B,2_I4B,3_I4B,"C") == 2, "Anisotropic cell count")
END PROGRAM main
