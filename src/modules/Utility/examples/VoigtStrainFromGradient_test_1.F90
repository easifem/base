!> author: Shion Shimizu
! date: 2026-11-17
! summary: VoigtStrainFromGradient in 1D, 2D and 3D

PROGRAM main
USE GlobalData, ONLY: DFP
USE Test_Method, ONLY: OK
USE VoigtUtility, ONLY: VoigtStrainFromGradient
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-14_DFP
REAL(DFP), PARAMETER :: grad(3, 3) = RESHAPE( &
                        [1.0_DFP, 2.0_DFP, 3.0_DFP, &
                         4.0_DFP, 5.0_DFP, 6.0_DFP, &
                         7.0_DFP, 8.0_DFP, 9.0_DFP], [3, 3])
!! grad(i, j) = du_i / dx_j

! Test 1: 3D, engineering shear strain
BLOCK
  REAL(DFP) :: eps(6), expected(6)

  CALL VoigtStrainFromGradient(grad=grad, nsd=3, ans=eps)
  expected = [1.0_DFP, 5.0_DFP, 9.0_DFP, 6.0_DFP, 14.0_DFP, 10.0_DFP]
  CALL OK(MAXVAL(ABS(eps - expected)) .LT. tol, &
          "VoigtStrainFromGradient: 3D")
END BLOCK

! Test 2: 2D (plane strain), out-of-plane components are zero
BLOCK
  REAL(DFP) :: eps(6), expected(6)

  CALL VoigtStrainFromGradient(grad=grad, nsd=2, ans=eps)
  expected = [1.0_DFP, 5.0_DFP, 0.0_DFP, 6.0_DFP, 0.0_DFP, 0.0_DFP]
  CALL OK(MAXVAL(ABS(eps - expected)) .LT. tol, &
          "VoigtStrainFromGradient: 2D")
END BLOCK

! Test 3: 1D (uniaxial strain)
BLOCK
  REAL(DFP) :: eps(6), expected(6)

  CALL VoigtStrainFromGradient(grad=grad, nsd=1, ans=eps)
  expected = [1.0_DFP, 0.0_DFP, 0.0_DFP, 0.0_DFP, 0.0_DFP, 0.0_DFP]
  CALL OK(MAXVAL(ABS(eps - expected)) .LT. tol, &
          "VoigtStrainFromGradient: 1D")
END BLOCK

END PROGRAM main
