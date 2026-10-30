!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Checked example for ReferenceLine_Method.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE BaseType, ONLY: ReferenceLine_
USE ReferenceLine_Method, ONLY: Initiate
USE ReferenceLine_Method, ONLY: ReferenceLine
USE ReferenceLine_Method, ONLY: ReferenceLine_Pointer
USE ReferenceLine_Method, ONLY: RefLineCoord
USE ReferenceLine_Method, ONLY: RefCoord_Line
USE Test_Method, ONLY: OK
IMPLICIT NONE
TYPE(ReferenceLine_) :: a, b
CLASS(ReferenceLine_), POINTER :: p
REAL(DFP) :: x(1, 2)

CALL Initiate(obj=a, nsd=1_I4B, domainName="UNIT")
b = ReferenceLine(nsd=1_I4B, domainName="UNIT")
p => ReferenceLine_Pointer(nsd=1_I4B, domainName="UNIT")
CALL OK(a%order == 1 .AND. b%order == 1, "Linear reference objects")
CALL OK(ASSOCIATED(p), "Pointer constructor allocates its result")
x = RefLineCoord("UNIT")
CALL OK(MAXVAL(ABS(x-RefCoord_Line("UNIT"))) < 1.0E-12_DFP, &
  "Coordinate aliases agree")
CALL OK(MAXVAL(ABS(a%xij-x)) < 1.0E-12_DFP, "Stored reference coordinates")
DEALLOCATE(p)
END PROGRAM main
