!> author: Vikas Sharma, Ph.D.
! date: 2026-11-20
! summary: End point coordinates of the unit and biunit reference lines.

PROGRAM main
USE GlobalData, ONLY: DFP
USE ReferenceLine_Method, ONLY: RefCoord_Line
USE ReferenceLine_Method, ONLY: RefLineCoord
USE Test_Method, ONLY: OK
IMPLICIT NONE
REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP

! Test 1: UNIT gives [0, 1]
BLOCK
  REAL(DFP) :: x(1, 2)
  x = RefLineCoord("UNIT")
  CALL OK(MAXVAL(ABS(x(1, :) - [0.0_DFP, 1.0_DFP])) < tol, "UNIT")
END BLOCK

! Test 2: BIUNIT gives [-1, 1]
BLOCK
  REAL(DFP) :: x(1, 2)
  x = RefLineCoord("BIUNIT")
  CALL OK(MAXVAL(ABS(x(1, :) - [-1.0_DFP, 1.0_DFP])) < tol, "BIUNIT")
END BLOCK

! Test 3: lower case names are accepted
BLOCK
  REAL(DFP) :: x(1, 2)
  x = RefLineCoord("unit")
  CALL OK(MAXVAL(ABS(x(1, :) - [0.0_DFP, 1.0_DFP])) < tol, "unit")
END BLOCK

! Test 4: the generic name RefCoord_Line gives the same result
BLOCK
  CALL OK(MAXVAL(ABS(RefCoord_Line("BIUNIT") - RefLineCoord("BIUNIT"))) &
          < tol, "RefCoord_Line equals RefLineCoord")
END BLOCK

END PROGRAM main
