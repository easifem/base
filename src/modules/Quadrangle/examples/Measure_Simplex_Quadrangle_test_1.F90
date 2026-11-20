!> author: Vikas Sharma, Ph.D.
! date: 2026-11-20
! summary: Testing Measure_Simplex_Quadrangle in 2D and 3D

PROGRAM main
USE BaseType, ONLY: ReferenceQuadrangle_
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE ReferenceQuadrangle_Method, ONLY: Measure_Simplex_Quadrangle
USE ReferenceQuadrangle_Method, ONLY: ReferenceQuadrangle
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP

! Test 1: biunit square in 2D has area 4
BLOCK
  TYPE(ReferenceQuadrangle_) :: ref

  ref = ReferenceQuadrangle(nsd=2_I4B)
  CALL OK(ABS(Measure_Simplex_Quadrangle(ref, ref%xij) - 4.0_DFP) &
          .LT. tol, "2D: biunit square has area 4")
END BLOCK

! Test 2: tilted unit square in 3D (plane z = x) has area SQRT(2)
BLOCK
  TYPE(ReferenceQuadrangle_) :: ref
  REAL(DFP) :: xij(3, 4)

  ref = ReferenceQuadrangle(nsd=3_I4B)
  xij(:, 1) = [0.0_DFP, 0.0_DFP, 0.0_DFP]
  xij(:, 2) = [1.0_DFP, 0.0_DFP, 1.0_DFP]
  xij(:, 3) = [1.0_DFP, 1.0_DFP, 1.0_DFP]
  xij(:, 4) = [0.0_DFP, 1.0_DFP, 0.0_DFP]
  CALL OK(ABS(Measure_Simplex_Quadrangle(ref, xij) - SQRT(2.0_DFP)) &
          .LT. tol, "3D: tilted unit square has area SQRT(2)")
END BLOCK

! Test 3: a general convex quadrangle in 2D
BLOCK
  TYPE(ReferenceQuadrangle_) :: ref
  REAL(DFP) :: xij(2, 4)

  ref = ReferenceQuadrangle(nsd=2_I4B)
  xij(:, 1) = [0.0_DFP, 0.0_DFP]
  xij(:, 2) = [2.0_DFP, 0.0_DFP]
  xij(:, 3) = [1.0_DFP, 1.0_DFP]
  xij(:, 4) = [0.0_DFP, 1.0_DFP]
  CALL OK(ABS(Measure_Simplex_Quadrangle(ref, xij) - 1.5_DFP) .LT. tol, &
          "2D: trapezoid has area 3/2")
END BLOCK

END PROGRAM main
