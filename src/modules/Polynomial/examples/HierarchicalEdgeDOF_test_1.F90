!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Count hierarchical edge interior degrees of freedom.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: Tetrahedron
USE HierarchicalPolynomialUtility, ONLY: HierarchicalEdgeDOF
USE Test_Method, ONLY: OK
IMPLICIT NONE
INTEGER(I4B) :: ndof

ndof = HierarchicalEdgeDOF(elemType=Tetrahedron, &
  order=[2_I4B, 2_I4B, 2_I4B, 2_I4B, 2_I4B, 2_I4B])
CALL OK(ndof == 6, "Hierarchical entity degree count")
END PROGRAM main
