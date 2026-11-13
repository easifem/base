!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Double contraction of Rank2Tensors and VoigtRank2Tensors

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: StrainTypeVoigt
USE GlobalData, ONLY: StressTypeVoigt
USE Rank2Tensor_Method, ONLY: Contraction
USE Rank2Tensor_Method, ONLY: Rank2Tensor
USE Test_Method, ONLY: OK
USE VoigtRank2Tensor_Method, ONLY: VoigtRank2Tensor
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
REAL(DFP), PARAMETER :: A0(3, 3) = RESHAPE([2.0_DFP, 1.0_DFP, 0.5_DFP, &
                                            -1.0_DFP, 3.0_DFP, 0.25_DFP, &
                                            0.0_DFP, 1.5_DFP, 4.0_DFP], &
                                           [3, 3])
REAL(DFP), PARAMETER :: S0(3, 3) = RESHAPE([1.0_DFP, 4.0_DFP, 6.0_DFP, &
                                            4.0_DFP, 2.0_DFP, 5.0_DFP, &
                                            6.0_DFP, 5.0_DFP, 3.0_DFP], &
                                           [3, 3])

! Test 1: two Rank2Tensors
BLOCK
  REAL(DFP) :: ans

  ans = Contraction(Rank2Tensor(A0), Rank2Tensor(S0))
  CALL OK(ABS(ans - 31.75_DFP) .LT. tol, "obj : obj")
  CALL OK(ABS(ans - SUM(A0 * S0)) .LT. tol, "obj : obj = SUM(A*B)")
END BLOCK

! Test 2: a Rank2Tensor and a Voigt tensor
BLOCK
  REAL(DFP) :: ans

  ans = Contraction(Rank2Tensor(A0), &
                    VoigtRank2Tensor(S0, StressTypeVoigt))
  CALL OK(ABS(ans - 31.75_DFP) .LT. tol, "obj : Voigt stress-like")
  ans = Contraction(Rank2Tensor(A0), &
                    VoigtRank2Tensor(S0, StrainTypeVoigt))
  CALL OK(ABS(ans - 31.75_DFP) .LT. tol, "obj : Voigt strain-like")
END BLOCK

! Test 3: a Voigt tensor and a Rank2Tensor
BLOCK
  REAL(DFP) :: ans

  ans = Contraction(VoigtRank2Tensor(S0, StressTypeVoigt), &
                    Rank2Tensor(A0))
  CALL OK(ABS(ans - 31.75_DFP) .LT. tol, "Voigt stress-like : obj")
  ans = Contraction(VoigtRank2Tensor(S0, StrainTypeVoigt), &
                    Rank2Tensor(A0))
  CALL OK(ABS(ans - 31.75_DFP) .LT. tol, "Voigt strain-like : obj")
END BLOCK

! Test 4: two Voigt tensors of any type
BLOCK
  REAL(DFP) :: ans

  ans = Contraction(VoigtRank2Tensor(S0, StressTypeVoigt), &
                    VoigtRank2Tensor(S0, StressTypeVoigt))
  CALL OK(ABS(ans - SUM(S0 * S0)) .LT. tol, "Voigt : Voigt, stress")
  ans = Contraction(VoigtRank2Tensor(S0, StressTypeVoigt), &
                    VoigtRank2Tensor(S0, StrainTypeVoigt))
  CALL OK(ABS(ans - SUM(S0 * S0)) .LT. tol, "Voigt : Voigt, mixed")
  ans = Contraction(VoigtRank2Tensor(S0, StrainTypeVoigt), &
                    VoigtRank2Tensor(S0, StrainTypeVoigt))
  CALL OK(ABS(ans - SUM(S0 * S0)) .LT. tol, "Voigt : Voigt, strain")
END BLOCK

END PROGRAM main
