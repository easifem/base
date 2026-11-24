!> author: Vikas Sharma, Ph.D.
! date: 2026-11-20
! summary: Length of a line in 1D, 2D and 3D.

PROGRAM main
USE BaseType, ONLY: ReferenceLine_
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE ReferenceLine_Method, ONLY: Measure_Simplex_Line
USE ReferenceLine_Method, ONLY: ReferenceLine
USE Test_Method, ONLY: OK
IMPLICIT NONE
REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
TYPE(ReferenceLine_) :: ref

ref = ReferenceLine(nsd=1_I4B, domainName="BIUNIT")

! Test 1: 1D line from 3 to -1
BLOCK
  REAL(DFP) :: xij(1, 2)
  xij(1, :) = [3.0_DFP, -1.0_DFP]
  CALL OK(ABS(Measure_Simplex_Line(ref, xij) - 4.0_DFP) < tol, &
          "1D length is 4")
END BLOCK

! Test 2: 2D line from (0,0) to (3,4)
BLOCK
  REAL(DFP) :: xij(2, 2)
  xij(:, 1) = [0.0_DFP, 0.0_DFP]
  xij(:, 2) = [3.0_DFP, 4.0_DFP]
  CALL OK(ABS(Measure_Simplex_Line(ref, xij) - 5.0_DFP) < tol, &
          "2D length is 5")
END BLOCK

! Test 3: 3D line from (1,1,1) to (2,3,3)
BLOCK
  REAL(DFP) :: xij(3, 2)
  xij(:, 1) = [1.0_DFP, 1.0_DFP, 1.0_DFP]
  xij(:, 2) = [2.0_DFP, 3.0_DFP, 3.0_DFP]
  CALL OK(ABS(Measure_Simplex_Line(ref, xij) - 3.0_DFP) < tol, &
          "3D length is 3")
END BLOCK

END PROGRAM main
