!> author: Shion Shimizu
! date: 2026-11-17
! summary: GetVoigtIndex_(voigtForm, ans, tsize)

PROGRAM main
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: VoigtForm3D
USE GlobalData, ONLY: VoigtFormAxisymmetric
USE GlobalData, ONLY: VoigtFormPlaneStrain
USE GlobalData, ONLY: VoigtFormPlaneStrainFull
USE GlobalData, ONLY: VoigtFormPlaneStress
USE GlobalData, ONLY: VoigtFormPlaneStressFull
USE GlobalData, ONLY: VoigtFormUniaxialStrain
USE GlobalData, ONLY: VoigtFormUniaxialStrainFull
USE Test_Method, ONLY: OK
USE VoigtUtility, ONLY: GetVoigtIndex_
IMPLICIT NONE

! Test 1: 3D Voigt indices of each form
CALL Check(VoigtForm3D, [1, 2, 3, 4, 5, 6], "3D")
CALL Check(VoigtFormPlaneStrain, [1, 2, 4], "plane strain")
CALL Check(VoigtFormPlaneStress, [1, 2, 4], "plane stress")
CALL Check(VoigtFormAxisymmetric, [1, 2, 4, 3], "axisymmetric")
CALL Check(VoigtFormUniaxialStrain, [1], "uniaxial strain")
CALL Check(VoigtFormPlaneStrainFull, [1, 2, 4, 3], "plane strain full")
CALL Check(VoigtFormPlaneStressFull, [1, 2, 4, 3], "plane stress full")
CALL Check(VoigtFormUniaxialStrainFull, [1, 2, 3], &
           "uniaxial strain full")

! Test 2: an unknown form is treated as 3D
CALL Check(-1_I4B, [1, 2, 3, 4, 5, 6], "unknown form")

CONTAINS

! Compares the indices of a form with the expected ones
SUBROUTINE Check(voigtForm, expected, name)
  INTEGER(I4B), INTENT(IN) :: voigtForm
  INTEGER(I4B), INTENT(IN) :: expected(:)
  CHARACTER(*), INTENT(IN) :: name

  INTEGER(I4B) :: ans(6), tsize

  ans = 0
  CALL GetVoigtIndex_(voigtForm=voigtForm, ans=ans, tsize=tsize)
  CALL OK(tsize .EQ. SIZE(expected), name//": tsize")
  CALL OK(ALL(ans(1:SIZE(expected)) .EQ. expected), name//": index")
END SUBROUTINE Check

END PROGRAM main
