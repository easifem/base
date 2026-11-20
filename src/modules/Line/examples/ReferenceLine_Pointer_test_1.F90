!> author: Vikas Sharma, Ph.D.
! date: 2026-11-20
! summary: Allocate a linear reference line through a pointer.

PROGRAM main
USE BaseType, ONLY: ReferenceLine_
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: Line2
USE ReferenceLine_Method, ONLY: ReferenceLine_Pointer
USE Test_Method, ONLY: OK
IMPLICIT NONE
REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP

! Test 1: pointer to a UNIT line
BLOCK
  CLASS(ReferenceLine_), POINTER :: p
  p => ReferenceLine_Pointer(nsd=1_I4B, domainName="unit")
  CALL OK(ASSOCIATED(p), "Pointer is associated")
  CALL OK(TRIM(p%domainName) == "UNIT", "Domain name in upper case")
  CALL OK(MAXVAL(ABS(p%xij(1, :) - [0.0_DFP, 1.0_DFP])) < tol, &
          "UNIT coordinates")
  CALL OK(p%name == Line2, "Element type is Line2")
  DEALLOCATE (p)
END BLOCK

! Test 2: pointer from general coordinates
BLOCK
  CLASS(ReferenceLine_), POINTER :: p
  REAL(DFP) :: xij(1, 2)
  xij(1, :) = [1.0_DFP, 3.0_DFP]
  p => ReferenceLine_Pointer(nsd=1_I4B, xij=xij)
  CALL OK(TRIM(p%domainName) == "GENERAL", "General coordinates")
  CALL OK(MAXVAL(ABS(p%xij - xij)) < tol, "Coordinates are stored")
  DEALLOCATE (p)
END BLOCK

END PROGRAM main
