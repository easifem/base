!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Initiate a Rank2Tensor, a VoigtRank2Tensor and a Stress

PROGRAM main
USE BaseType, ONLY: Rank2Tensor_
USE BaseType, ONLY: Stress_
USE BaseType, ONLY: VoigtRank2Tensor_
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: StrainTypeVoigt
USE GlobalData, ONLY: StressMeasureCauchy
USE GlobalData, ONLY: StressMeasurePK1
USE GlobalData, ONLY: StressTypeVoigt
USE Rank2Tensor_Method, ONLY: Initiate
USE Test_Method, ONLY: OK
USE VoigtRank2Tensor_Method, ONLY: VoigtRank2Tensor
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
REAL(DFP), PARAMETER :: S0(3, 3) = RESHAPE([1.0_DFP, 4.0_DFP, 6.0_DFP, &
                                            4.0_DFP, 2.0_DFP, 5.0_DFP, &
                                            6.0_DFP, 5.0_DFP, 3.0_DFP], &
                                           [3, 3])

! Test 1: from a matrix, with and without the symmetry flag
BLOCK
  TYPE(Rank2Tensor_) :: obj

  CALL Initiate(obj, S0)
  CALL OK(MAXVAL(ABS(obj%T - S0)) .LT. tol, "matrix: T")
  CALL OK(.NOT. obj%isSym, "matrix: isSym defaults to false")
  CALL Initiate(obj, S0, .TRUE.)
  CALL OK(obj%isSym, "matrix: isSym is true")
END BLOCK

! Test 2: from another Rank2Tensor
BLOCK
  TYPE(Rank2Tensor_) :: obj, obj2

  CALL Initiate(obj2, S0, .TRUE.)
  CALL Initiate(obj, obj2)
  CALL OK(MAXVAL(ABS(obj%T - S0)) .LT. tol, "copy: T")
  CALL OK(obj%isSym, "copy: isSym")
END BLOCK

! Test 3: from a strain-like Voigt tensor
BLOCK
  TYPE(Rank2Tensor_) :: obj

  CALL Initiate(obj, VoigtRank2Tensor(S0, StrainTypeVoigt))
  CALL OK(MAXVAL(ABS(obj%T - S0)) .LT. tol, "from Voigt: T")
  CALL OK(obj%isSym, "from Voigt: isSym")
END BLOCK

! Test 4: a Voigt tensor from a Rank2Tensor
BLOCK
  TYPE(Rank2Tensor_) :: obj
  TYPE(VoigtRank2Tensor_) :: v

  CALL Initiate(obj, S0, .TRUE.)
  CALL Initiate(v, obj, StrainTypeVoigt)
  CALL OK(MAXVAL(ABS(v%V - [1.0_DFP, 2.0_DFP, 3.0_DFP, 8.0_DFP, &
                            10.0_DFP, 12.0_DFP])) .LT. tol, &
          "to Voigt, strain-like: V")
  CALL OK(ABS(v%scale - 0.5_DFP) .LT. tol, "to Voigt, strain-like: scale")
  CALL Initiate(v, obj, StressTypeVoigt)
  CALL OK(MAXVAL(ABS(v%V - [1.0_DFP, 2.0_DFP, 3.0_DFP, 4.0_DFP, &
                            5.0_DFP, 6.0_DFP])) .LT. tol, &
          "to Voigt, stress-like: V")
  CALL OK(v%voigtType .EQ. StressTypeVoigt, "to Voigt: voigtType")
END BLOCK

! Test 5: a Stress from a matrix and a stress measure
BLOCK
  TYPE(Stress_) :: sig
  REAL(DFP) :: mat(3, 3)

  CALL Initiate(sig, S0, StressMeasureCauchy)
  CALL OK(MAXVAL(ABS(sig%T - S0)) .LT. tol, "stress: T")
  CALL OK(sig%isSym, "stress: symmetric matrix gives isSym")
  CALL OK(sig%stressType .EQ. StressMeasureCauchy, "stress: stressType")
  mat = S0
  mat(1, 2) = 0.0_DFP
  CALL Initiate(sig, mat, StressMeasurePK1)
  CALL OK(.NOT. sig%isSym, "stress: non-symmetric matrix")
  CALL OK(sig%stressType .EQ. StressMeasurePK1, "stress: PK1")
END BLOCK

END PROGRAM main
