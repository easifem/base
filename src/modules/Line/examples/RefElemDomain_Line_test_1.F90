!> author: Vikas Sharma, Ph.D.
! date: 2026-11-20
! summary: Reference domain of a line element is always BIUNIT.

PROGRAM main
USE LineInterpolationUtility, ONLY: RefElemDomain_Line
USE String_Class, ONLY: String
USE Test_Method, ONLY: OK
IMPLICIT NONE

! Test 1: H1 Lagrange
BLOCK
  TYPE(String) :: domain
  domain = RefElemDomain_Line("H1", "Lagrange")
  CALL OK(domain%chars() == "BIUNIT", "H1 Lagrange gives BIUNIT")
END BLOCK

! Test 2: the arguments do not change the result
BLOCK
  TYPE(String) :: domain
  domain = RefElemDomain_Line("DG", "Hierarchy")
  CALL OK(domain%chars() == "BIUNIT", "DG Hierarchy gives BIUNIT")
END BLOCK

END PROGRAM main
