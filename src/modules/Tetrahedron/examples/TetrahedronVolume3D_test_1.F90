!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Checked example for ReferenceTetrahedron_Method.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE ReferenceTetrahedron_Method, ONLY: TetrahedronVolume3D
USE Test_Method, ONLY: OK
IMPLICIT NONE
REAL(DFP) :: x(3,4),v

x = 0.0_DFP
x(1,2) = 1.0_DFP
x(2,3) = 1.0_DFP
x(3,4) = 1.0_DFP
CALL TetrahedronVolume3D(x,v)
CALL OK(ABS(v-1.0_DFP/6.0_DFP) < 1.0E-11_DFP, "Unit simplex volume")
x(:,[2,3]) = x(:,[3,2])
CALL TetrahedronVolume3D(x,v)
CALL OK(ABS(v-1.0_DFP/6.0_DFP) < 1.0E-11_DFP, &
  "Volume is independent of orientation")
END PROGRAM main
