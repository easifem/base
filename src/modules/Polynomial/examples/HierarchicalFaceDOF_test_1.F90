!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Count hierarchical face interior degrees of freedom.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: Triangle
USE HierarchicalPolynomialUtility, ONLY: HierarchicalFaceDOF
USE Test_Method, ONLY: OK
IMPLICIT NONE
INTEGER(I4B) :: ndof

ndof = HierarchicalFaceDOF(elemType=Triangle, &
  order=RESHAPE([2_I4B, 2_I4B, 2_I4B], [1, 3]))
CALL OK(ndof == 3, "Hierarchical entity degree count")
END PROGRAM main
