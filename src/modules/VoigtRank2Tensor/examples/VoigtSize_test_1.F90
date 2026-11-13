!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: VoigtSize of each Voigt form

PROGRAM main
USE GlobalData, ONLY: VoigtForm3D
USE GlobalData, ONLY: VoigtFormAxisymmetric
USE GlobalData, ONLY: VoigtFormPlaneStrain
USE GlobalData, ONLY: VoigtFormPlaneStress
USE Test_Method, ONLY: OK
USE VoigtRank2Tensor_Method, ONLY: VoigtSize
IMPLICIT NONE

! Test 1: size of each Voigt form
BLOCK
  CALL OK(VoigtSize(VoigtForm3D) .EQ. 6, "3D: 6")
  CALL OK(VoigtSize(VoigtFormPlaneStrain) .EQ. 3, "plane strain: 3")
  CALL OK(VoigtSize(VoigtFormPlaneStress) .EQ. 3, "plane stress: 3")
  CALL OK(VoigtSize(VoigtFormAxisymmetric) .EQ. 4, "axisymmetric: 4")
END BLOCK

END PROGRAM main
