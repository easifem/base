!> author: Vikas Sharma, Ph. D.
! date: 2026-11-20
! summary: Matrix and vector assignment from a VoigtRank2Tensor

PROGRAM main
USE BaseType, ONLY: VoigtRank2Tensor_
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: StrainTypeVoigt
USE GlobalData, ONLY: StressTypeVoigt
USE Test_Method, ONLY: OK
USE VoigtRank2Tensor_Method, ONLY: ASSIGNMENT(=)
USE VoigtRank2Tensor_Method, ONLY: Initiate
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
REAL(DFP), PARAMETER :: v6(6) = [1.0_DFP, 2.0_DFP, 3.0_DFP, 8.0_DFP, &
                                 10.0_DFP, 12.0_DFP]

! Test 1: T = obj for a stress-like tensor, shear entries are V(4:6)
BLOCK
  TYPE(VoigtRank2Tensor_) :: obj
  REAL(DFP) :: T(3, 3)

  CALL Initiate(obj, v6, StressTypeVoigt)
  T = obj
  CALL OK(ABS(T(1, 1) - 1.0_DFP) .LT. tol, "stress: T11")
  CALL OK(ABS(T(3, 3) - 3.0_DFP) .LT. tol, "stress: T33")
  CALL OK(ABS(T(1, 2) - 8.0_DFP) .LT. tol, "stress: T12")
  CALL OK(ABS(T(2, 3) - 10.0_DFP) .LT. tol, "stress: T23")
  CALL OK(ABS(T(1, 3) - 12.0_DFP) .LT. tol, "stress: T13")
  CALL OK(MAXVAL(ABS(T - TRANSPOSE(T))) .LT. tol, "stress: symmetric")
END BLOCK

! Test 2: T = obj for a strain-like tensor, shear entries are V(4:6)/2
BLOCK
  TYPE(VoigtRank2Tensor_) :: obj
  REAL(DFP) :: T(3, 3)

  CALL Initiate(obj, v6, StrainTypeVoigt)
  T = obj
  CALL OK(ABS(T(1, 2) - 4.0_DFP) .LT. tol, "strain: T12")
  CALL OK(ABS(T(3, 2) - 5.0_DFP) .LT. tol, "strain: T32")
  CALL OK(ABS(T(3, 1) - 6.0_DFP) .LT. tol, "strain: T31")
  CALL OK(ABS(T(2, 2) - 2.0_DFP) .LT. tol, "strain: T22 not scaled")
END BLOCK

! Test 3: vec = obj returns the stored vector (engineering shear)
BLOCK
  TYPE(VoigtRank2Tensor_) :: obj
  REAL(DFP) :: vec(6)

  CALL Initiate(obj, v6, StrainTypeVoigt)
  vec = obj
  CALL OK(MAXVAL(ABS(vec - v6)) .LT. tol, "strain: vec is V")
END BLOCK

! Test 4: a default tensor gives a zero matrix and a zero vector
BLOCK
  TYPE(VoigtRank2Tensor_) :: obj
  REAL(DFP) :: T(3, 3), vec(6)

  T = obj
  vec = obj
  CALL OK(MAXVAL(ABS(T)) .LT. tol, "default: T is zero")
  CALL OK(MAXVAL(ABS(vec)) .LT. tol, "default: vec is zero")
END BLOCK

END PROGRAM main
