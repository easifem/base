!> author: OpenAI Codex
! date: 2026-10-30
! summary: Return the reference domain name for an interpolation family.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: Line
USE InterpolationUtility, ONLY: RefElemDomain
USE String_Class, ONLY: String
USE Test_Method, ONLY: OK
IMPLICIT NONE
TYPE(String) :: domain

domain = RefElemDomain(elemType=Line, baseContinuity="H1", &
  baseInterpol="Lagrange")
CALL OK(domain%chars() == "BIUNIT", "Line reference domain")
END PROGRAM main
