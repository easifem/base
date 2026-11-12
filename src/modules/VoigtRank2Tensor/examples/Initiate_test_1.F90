!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Initiate a VoigtRank2Tensor from a vector, a matrix or a
!   reduced vector

PROGRAM main
USE BaseType, ONLY: VoigtRank2Tensor_
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: StrainTypeVoigt
USE GlobalData, ONLY: StressTypeVoigt
USE GlobalData, ONLY: VoigtFormPlaneStrain
USE Test_Method, ONLY: OK
USE VoigtRank2Tensor_Method, ONLY: Initiate
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
REAL(DFP), PARAMETER :: T(3, 3) = RESHAPE([1.0_DFP, 4.0_DFP, 6.0_DFP, &
                                           4.0_DFP, 2.0_DFP, 5.0_DFP, &
                                           6.0_DFP, 5.0_DFP, 3.0_DFP], &
                                          [3, 3])

! Test 1: from a Voigt vector
BLOCK
  TYPE(VoigtRank2Tensor_) :: obj
  REAL(DFP) :: vec(6)

  vec = [1.0_DFP, 2.0_DFP, 3.0_DFP, 4.0_DFP, 5.0_DFP, 6.0_DFP]
  CALL Initiate(obj, vec, StressTypeVoigt)
  CALL OK(MAXVAL(ABS(obj%V - vec)) .LT. tol, "vector: V")
  CALL OK(obj%voigtType .EQ. StressTypeVoigt, "vector: voigtType")
  CALL OK(ABS(obj%scale - 1.0_DFP) .LT. tol, "vector: scale is 1")
END BLOCK

! Test 2: from a matrix, strain-like
BLOCK
  TYPE(VoigtRank2Tensor_) :: obj

  CALL Initiate(obj, T, StrainTypeVoigt)
  CALL OK(MAXVAL(ABS(obj%V - [1.0_DFP, 2.0_DFP, 3.0_DFP, 8.0_DFP, &
                              10.0_DFP, 12.0_DFP])) .LT. tol, &
          "matrix: engineering shear")
  CALL OK(ABS(obj%scale - 0.5_DFP) .LT. tol, "matrix: scale is 0.5")
END BLOCK

! Test 3: a non-symmetric matrix uses its symmetric part
BLOCK
  TYPE(VoigtRank2Tensor_) :: obj
  REAL(DFP) :: T2(3, 3)

  T2 = T
  T2(1, 2) = 2.0_DFP
  CALL Initiate(obj, T2, StrainTypeVoigt)
  CALL OK(ABS(obj%V(4) - 6.0_DFP) .LT. tol, "non-symmetric: V4")
END BLOCK

! Test 4: an unknown Voigt type is treated as stress-like
BLOCK
  TYPE(VoigtRank2Tensor_) :: obj

  CALL Initiate(obj, T, 99)
  CALL OK(ABS(obj%scale - 1.0_DFP) .LT. tol, "unknown type: scale is 1")
END BLOCK

! Test 5: from a plane strain vector [11,22,12]
BLOCK
  TYPE(VoigtRank2Tensor_) :: obj

  CALL Initiate(obj, [1.0_DFP, 2.0_DFP, 4.0_DFP], StressTypeVoigt, &
                VoigtFormPlaneStrain)
  CALL OK(MAXVAL(ABS(obj%V - [1.0_DFP, 2.0_DFP, 0.0_DFP, 4.0_DFP, &
                              0.0_DFP, 0.0_DFP])) .LT. tol, &
          "plane strain: V")
END BLOCK

END PROGRAM main
