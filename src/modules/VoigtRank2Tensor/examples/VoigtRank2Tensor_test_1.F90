!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: VoigtRank2Tensor constructor from a matrix or a Voigt vector

PROGRAM main
USE BaseType, ONLY: VoigtRank2Tensor_
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: StrainTypeVoigt
USE GlobalData, ONLY: StressTypeVoigt
USE GlobalData, ONLY: VoigtFormAxisymmetric
USE Test_Method, ONLY: OK
USE VoigtRank2Tensor_Method, ONLY: ASSIGNMENT(=)
USE VoigtRank2Tensor_Method, ONLY: Display
USE VoigtRank2Tensor_Method, ONLY: VoigtRank2Tensor
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
REAL(DFP), PARAMETER :: T(3, 3) = RESHAPE([1.0_DFP, 4.0_DFP, 6.0_DFP, &
                                           4.0_DFP, 2.0_DFP, 5.0_DFP, &
                                           6.0_DFP, 5.0_DFP, 3.0_DFP], &
                                          [3, 3])

! Test 1: stress-like tensor from a matrix, shear entries are T_ij
BLOCK
  TYPE(VoigtRank2Tensor_) :: obj
  REAL(DFP) :: vec(6), T2(3, 3)

  obj = VoigtRank2Tensor(T, StressTypeVoigt)
  CALL Display(obj, "stress-like tensor")
  vec = obj
  CALL OK(MAXVAL(ABS(vec - [1.0_DFP, 2.0_DFP, 3.0_DFP, 4.0_DFP, &
                            5.0_DFP, 6.0_DFP])) .LT. tol, &
          "stress: Voigt vector")
  CALL OK(ABS(obj%scale - 1.0_DFP) .LT. tol, "stress: scale is 1")
  T2 = obj
  CALL OK(MAXVAL(ABS(T2 - T)) .LT. tol, "stress: round trip")
END BLOCK

! Test 2: strain-like tensor from a matrix, shear entries are 2*e_ij
BLOCK
  TYPE(VoigtRank2Tensor_) :: obj
  REAL(DFP) :: vec(6), T2(3, 3)

  obj = VoigtRank2Tensor(T, StrainTypeVoigt)
  vec = obj
  CALL OK(MAXVAL(ABS(vec - [1.0_DFP, 2.0_DFP, 3.0_DFP, 8.0_DFP, &
                            10.0_DFP, 12.0_DFP])) .LT. tol, &
          "strain: engineering shear")
  CALL OK(ABS(obj%scale - 0.5_DFP) .LT. tol, "strain: scale is 0.5")
  T2 = obj
  CALL OK(MAXVAL(ABS(T2 - T)) .LT. tol, "strain: round trip")
END BLOCK

! Test 3: strain-like tensor from a Voigt vector
BLOCK
  TYPE(VoigtRank2Tensor_) :: obj
  REAL(DFP) :: T2(3, 3)

  obj = VoigtRank2Tensor([1.0_DFP, 2.0_DFP, 3.0_DFP, 8.0_DFP, &
                          10.0_DFP, 12.0_DFP], StrainTypeVoigt)
  T2 = obj
  CALL OK(MAXVAL(ABS(T2 - T)) .LT. tol, "vector: tensor components")
END BLOCK

! Test 4: strain-like tensor from an axisymmetric vector [11,22,12,33]
BLOCK
  TYPE(VoigtRank2Tensor_) :: obj
  REAL(DFP) :: T2(3, 3)

  obj = VoigtRank2Tensor([1.0_DFP, 2.0_DFP, 4.0_DFP, 3.0_DFP], &
                         StrainTypeVoigt, VoigtFormAxisymmetric)
  T2 = obj
  CALL OK(ABS(T2(1, 2) - 2.0_DFP) .LT. tol, "axisymmetric: T12")
  CALL OK(ABS(T2(3, 3) - 3.0_DFP) .LT. tol, "axisymmetric: T33")
  CALL OK(ABS(T2(2, 3)) .LT. tol, "axisymmetric: T23 is zero")
END BLOCK

END PROGRAM main
