!> author: Shion Shimizu
! date: 2026-11-17
! summary: GetVoigtSize(obj, voigtForm)

PROGRAM main
USE BaseType, ONLY: VoigtRank2Tensor_
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: StressTypeVoigt
USE GlobalData, ONLY: VoigtForm3D
USE GlobalData, ONLY: VoigtFormPlaneStrain
USE GlobalData, ONLY: VoigtFormPlaneStrainFull
USE Test_Method, ONLY: OK
USE VoigtRank2Tensor_Method, ONLY: GetVoigtSize
USE VoigtRank2Tensor_Method, ONLY: GetVoigtVector
USE VoigtRank2Tensor_Method, ONLY: VoigtRank2Tensor
IMPLICIT NONE

! Test 1: size of the vector of obj in a Voigt form
BLOCK
  TYPE(VoigtRank2Tensor_) :: obj

  obj = VoigtRank2Tensor([1.0_DFP, 2.0_DFP, 3.0_DFP, 4.0_DFP, 5.0_DFP, &
                          6.0_DFP], StressTypeVoigt)
  CALL OK(GetVoigtSize(obj, VoigtForm3D) .EQ. 6, "3D: 6")
  CALL OK(GetVoigtSize(obj, VoigtFormPlaneStrain) .EQ. 3, &
          "plane strain: 3")
  CALL OK(GetVoigtSize(obj, VoigtFormPlaneStrainFull) .EQ. &
          SIZE(GetVoigtVector(obj, VoigtFormPlaneStrainFull)), &
          "plane strain full: same as SIZE(GetVoigtVector)")
END BLOCK

END PROGRAM main
