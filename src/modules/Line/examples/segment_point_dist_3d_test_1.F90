!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Checked example for Line_Method.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE Line_Method, ONLY: segment_point_dist_3d
USE Test_Method, ONLY: OK
IMPLICIT NONE
REAL(DFP) :: p1(2), p2(2)
REAL(DFP) :: d

p1 = [0.0_DFP, 0.0_DFP]
p2 = [2.0_DFP, 0.0_DFP]
d = segment_point_dist_3d([0.0_DFP, 0.0_DFP, 0.0_DFP], &
  [2.0_DFP, 0.0_DFP, 0.0_DFP], [1.0_DFP, 0.0_DFP, 1.0_DFP])
CALL OK(ABS(d-1.0_DFP) < 1.0E-12_DFP, "Spatial segment distance")
END PROGRAM main
