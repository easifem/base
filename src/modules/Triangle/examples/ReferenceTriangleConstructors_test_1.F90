!> author: OpenAI Codex
! date: 2026-10-30
! summary: Checked example for ReferenceTriangle_Method.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE BaseType, ONLY: ReferenceTriangle_
USE ReferenceTriangle_Method, ONLY: Initiate
USE ReferenceTriangle_Method, ONLY: ReferenceTriangle
USE ReferenceTriangle_Method, ONLY: ReferenceTriangle_Pointer
USE ReferenceTriangle_Method, ONLY: RefTriangleCoord
USE ReferenceTriangle_Method, ONLY: RefCoord_Triangle
USE Test_Method, ONLY: OK
IMPLICIT NONE
TYPE(ReferenceTriangle_) :: a, b
CLASS(ReferenceTriangle_), POINTER :: p
REAL(DFP) :: x(2, 3)

CALL Initiate(obj=a, nsd=2_I4B, domainName="UNIT")
b = ReferenceTriangle(nsd=2_I4B, domainName="UNIT")
p => ReferenceTriangle_Pointer(nsd=2_I4B, domainName="UNIT")
CALL OK(a%order == 1 .AND. b%order == 1, "Linear reference objects")
CALL OK(ASSOCIATED(p), "Pointer constructor allocates its result")
x = RefTriangleCoord("UNIT")
CALL OK(MAXVAL(ABS(x-RefCoord_Triangle("UNIT"))) < 1.0E-12_DFP, &
  "Coordinate aliases agree")
CALL OK(MAXVAL(ABS(a%xij-x)) < 1.0E-12_DFP, "Stored reference coordinates")
DEALLOCATE(p)
END PROGRAM main
