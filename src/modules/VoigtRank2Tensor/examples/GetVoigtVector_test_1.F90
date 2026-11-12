!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: GetVoigtVector in the 3D and reduced 2D Voigt forms

PROGRAM main
USE BaseType, ONLY: VoigtRank2Tensor_
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: StressTypeVoigt
USE GlobalData, ONLY: VoigtForm3D
USE GlobalData, ONLY: VoigtFormAxisymmetric
USE GlobalData, ONLY: VoigtFormPlaneStrain
USE GlobalData, ONLY: VoigtFormPlaneStress
USE Test_Method, ONLY: OK
USE VoigtRank2Tensor_Method, ONLY: GetVoigtVector
USE VoigtRank2Tensor_Method, ONLY: VoigtRank2Tensor
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
REAL(DFP), PARAMETER :: v6(6) = [1.0_DFP, 2.0_DFP, 3.0_DFP, 4.0_DFP, &
                                 5.0_DFP, 6.0_DFP]
TYPE(VoigtRank2Tensor_) :: obj

obj = VoigtRank2Tensor(v6, StressTypeVoigt)

! Test 1: 3D form returns all 6 entries
BLOCK
  REAL(DFP), ALLOCATABLE :: v(:)

  v = GetVoigtVector(obj, VoigtForm3D)
  CALL OK(SIZE(v) .EQ. 6, "3D: size")
  CALL OK(MAXVAL(ABS(v - v6)) .LT. tol, "3D: values")
END BLOCK

! Test 2: plane strain and plane stress return [11,22,12]
BLOCK
  REAL(DFP), ALLOCATABLE :: v(:)

  v = GetVoigtVector(obj, VoigtFormPlaneStrain)
  CALL OK(SIZE(v) .EQ. 3, "plane strain: size")
  CALL OK(MAXVAL(ABS(v - [1.0_DFP, 2.0_DFP, 4.0_DFP])) .LT. tol, &
          "plane strain: values")
  v = GetVoigtVector(obj, VoigtFormPlaneStress)
  CALL OK(MAXVAL(ABS(v - [1.0_DFP, 2.0_DFP, 4.0_DFP])) .LT. tol, &
          "plane stress: values")
END BLOCK

! Test 3: axisymmetric returns [11,22,12,33]
BLOCK
  REAL(DFP), ALLOCATABLE :: v(:)

  v = GetVoigtVector(obj, VoigtFormAxisymmetric)
  CALL OK(SIZE(v) .EQ. 4, "axisymmetric: size")
  CALL OK(MAXVAL(ABS(v - [1.0_DFP, 2.0_DFP, 4.0_DFP, 3.0_DFP])) &
          .LT. tol, "axisymmetric: values")
END BLOCK

END PROGRAM main
