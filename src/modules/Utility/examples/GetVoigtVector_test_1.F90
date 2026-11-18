!> author: Shion Shimizu
! date: 2026-11-17
! summary: GetVoigtVector(v, voigtForm) of a 3D Voigt array

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: VoigtForm3D
USE GlobalData, ONLY: VoigtFormPlaneStressFull
USE GlobalData, ONLY: VoigtFormUniaxialStrainFull
USE Test_Method, ONLY: OK
USE VoigtUtility, ONLY: GetVoigtVector
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-14_DFP
REAL(DFP), PARAMETER :: v(6) = [1.0_DFP, 2.0_DFP, 3.0_DFP, &
                                4.0_DFP, 5.0_DFP, 6.0_DFP]

! Test 1: 3D form returns all 6 entries
BLOCK
  REAL(DFP), ALLOCATABLE :: ans(:)

  ans = GetVoigtVector(v=v, voigtForm=VoigtForm3D)
  CALL OK(SIZE(ans) .EQ. 6, "3D: size")
  CALL OK(MAXVAL(ABS(ans - v)) .LT. tol, "3D: values")
END BLOCK

! Test 2: plane stress full keeps the 33 component, [11, 22, 12, 33]
BLOCK
  REAL(DFP), ALLOCATABLE :: ans(:)

  ans = GetVoigtVector(v=v, voigtForm=VoigtFormPlaneStressFull)
  CALL OK(SIZE(ans) .EQ. 4, "plane stress full: size")
  CALL OK(MAXVAL(ABS(ans - [1.0_DFP, 2.0_DFP, 4.0_DFP, 3.0_DFP])) &
          .LT. tol, "plane stress full: values")
END BLOCK

! Test 3: uniaxial strain full returns the normal components
BLOCK
  REAL(DFP), ALLOCATABLE :: ans(:)

  ans = GetVoigtVector(v=v, voigtForm=VoigtFormUniaxialStrainFull)
  CALL OK(SIZE(ans) .EQ. 3, "uniaxial strain full: size")
  CALL OK(MAXVAL(ABS(ans - [1.0_DFP, 2.0_DFP, 3.0_DFP])) .LT. tol, &
          "uniaxial strain full: values")
END BLOCK

END PROGRAM main
