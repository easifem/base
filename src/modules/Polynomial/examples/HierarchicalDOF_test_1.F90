!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Count hierarchical total degrees of freedom.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: Line
USE HierarchicalPolynomialUtility, ONLY: HierarchicalDOF
USE Test_Method, ONLY: OK
IMPLICIT NONE
INTEGER(I4B) :: ndof

ndof = HierarchicalDOF(elemType=Line, cellOrder=[3_I4B])
CALL OK(ndof == 4, "Hierarchical entity degree count")
END PROGRAM main
