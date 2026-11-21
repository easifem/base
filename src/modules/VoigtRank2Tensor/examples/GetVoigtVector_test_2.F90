!> author: Vikas Sharma, Ph. D.
! date: 2026-11-20
! summary: GetVoigtVector of a strain-like tensor and of an unknown form

PROGRAM main
USE BaseType, ONLY: VoigtRank2Tensor_
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: StrainTypeVoigt
USE GlobalData, ONLY: VoigtFormAxisymmetric
USE GlobalData, ONLY: VoigtFormPlaneStress
USE Test_Method, ONLY: OK
USE VoigtRank2Tensor_Method, ONLY: GetVoigtVector
USE VoigtRank2Tensor_Method, ONLY: VoigtRank2Tensor
USE VoigtRank2Tensor_Method, ONLY: VoigtSize
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
REAL(DFP), PARAMETER :: T(3, 3) = RESHAPE([1.0_DFP, 4.0_DFP, 6.0_DFP, &
                                           4.0_DFP, 2.0_DFP, 5.0_DFP, &
                                           6.0_DFP, 5.0_DFP, 3.0_DFP], &
                                          [3, 3])
TYPE(VoigtRank2Tensor_) :: obj

obj = VoigtRank2Tensor(T, StrainTypeVoigt)

! Test 1: plane stress keeps the engineering shear 2*e12
BLOCK
  REAL(DFP), ALLOCATABLE :: v(:)

  v = GetVoigtVector(obj, VoigtFormPlaneStress)
  CALL OK(SIZE(v) .EQ. 3, "plane stress: size")
  CALL OK(MAXVAL(ABS(v - [1.0_DFP, 2.0_DFP, 8.0_DFP])) .LT. tol, &
          "plane stress: values")
END BLOCK

! Test 2: axisymmetric is [11,22,12,33]
BLOCK
  REAL(DFP), ALLOCATABLE :: v(:)

  v = GetVoigtVector(obj, VoigtFormAxisymmetric)
  CALL OK(SIZE(v) .EQ. VoigtSize(VoigtFormAxisymmetric), &
          "axisymmetric: size is VoigtSize")
  CALL OK(MAXVAL(ABS(v - [1.0_DFP, 2.0_DFP, 8.0_DFP, 3.0_DFP])) &
          .LT. tol, "axisymmetric: values")
END BLOCK

! Test 3: an unknown form gives the 3D vector
BLOCK
  REAL(DFP), ALLOCATABLE :: v(:)

  v = GetVoigtVector(obj, 99)
  CALL OK(SIZE(v) .EQ. 6, "unknown: size")
  CALL OK(MAXVAL(ABS(v - obj%V)) .LT. tol, "unknown: values")
END BLOCK

END PROGRAM main
