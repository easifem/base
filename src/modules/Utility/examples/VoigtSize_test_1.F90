!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: VoigtSize of each Voigt form

PROGRAM main
USE GlobalData, ONLY: VoigtForm3D
USE GlobalData, ONLY: VoigtFormAxisymmetric
USE GlobalData, ONLY: VoigtFormPlaneStrain
USE GlobalData, ONLY: VoigtFormPlaneStrainFull
USE GlobalData, ONLY: VoigtFormPlaneStress
USE GlobalData, ONLY: VoigtFormPlaneStressFull
USE GlobalData, ONLY: VoigtFormUniaxialStrain
USE GlobalData, ONLY: VoigtFormUniaxialStrainFull
USE Test_Method, ONLY: OK
USE VoigtUtility, ONLY: VoigtSize
IMPLICIT NONE

! Test 1: size of each Voigt form
BLOCK
  CALL OK(VoigtSize(VoigtForm3D) .EQ. 6, "3D: 6")
  CALL OK(VoigtSize(VoigtFormPlaneStrain) .EQ. 3, "plane strain: 3")
  CALL OK(VoigtSize(VoigtFormPlaneStress) .EQ. 3, "plane stress: 3")
  CALL OK(VoigtSize(VoigtFormAxisymmetric) .EQ. 4, "axisymmetric: 4")
  CALL OK(VoigtSize(VoigtFormUniaxialStrain) .EQ. 1, "uniaxial strain: 1")
END BLOCK

! Test 2: size of the Full forms
BLOCK
  CALL OK(VoigtSize(VoigtFormPlaneStrainFull) .EQ. 4, &
          "plane strain full: 4")
  CALL OK(VoigtSize(VoigtFormPlaneStressFull) .EQ. 4, &
          "plane stress full: 4")
  CALL OK(VoigtSize(VoigtFormUniaxialStrainFull) .EQ. 3, &
          "uniaxial strain full: 3")
END BLOCK

END PROGRAM main
