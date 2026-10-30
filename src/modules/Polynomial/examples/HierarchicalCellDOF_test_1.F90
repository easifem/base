!> author: OpenAI Codex
! date: 2026-10-30
! summary: Count hierarchical cell interior degrees of freedom.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: Line
USE HierarchicalPolynomialUtility, ONLY: HierarchicalCellDOF
USE Test_Method, ONLY: OK
IMPLICIT NONE
INTEGER(I4B) :: ndof

ndof = HierarchicalCellDOF(elemType=Line, order=[3_I4B])
CALL OK(ndof == 2, "Hierarchical entity degree count")
END PROGRAM main
