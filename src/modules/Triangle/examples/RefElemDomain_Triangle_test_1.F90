!> author: Vikas Sharma, Ph. D.
! date: 2026-11-20
! summary: Testing RefElemDomain_Triangle for H1 basis families

PROGRAM main
USE String_Class, ONLY: String
USE Test_Method, ONLY: OK
USE TriangleInterpolationUtility, ONLY: RefElemDomain_Triangle
IMPLICIT NONE

TYPE(String) :: ans

! Test 1: Lagrange and Serendipity bases live on the UNIT triangle
BLOCK
  ans = RefElemDomain_Triangle(baseContinuity="H1", &
                               baseInterpol="Lagrange")
  CALL OK(ans%chars() .EQ. "UNIT", "H1 Lagrange is UNIT")

  ans = RefElemDomain_Triangle(baseContinuity="H1", &
                               baseInterpol="Serendipity")
  CALL OK(ans%chars() .EQ. "UNIT", "H1 Serendipity is UNIT")
END BLOCK

! Test 2: Hierarchical and Orthogonal bases live on the BIUNIT triangle
BLOCK
  ans = RefElemDomain_Triangle(baseContinuity="H1", &
                               baseInterpol="Hierarchical")
  CALL OK(ans%chars() .EQ. "BIUNIT", "H1 Hierarchical is BIUNIT")

  ans = RefElemDomain_Triangle(baseContinuity="H1", &
                               baseInterpol="Orthogonal")
  CALL OK(ans%chars() .EQ. "BIUNIT", "H1 Orthogonal is BIUNIT")
END BLOCK

! Test 3: the arguments are not case sensitive
BLOCK
  ans = RefElemDomain_Triangle(baseContinuity="h1", &
                               baseInterpol="lagrange")
  CALL OK(ans%chars() .EQ. "UNIT", "h1 lagrange is UNIT")
END BLOCK

END PROGRAM main
