!> author: Vikas Sharma, Ph. D.
! date: 2026-11-20
! summary: Contraction of an anisotropic tangent with a strain
!
! The stress is checked against the rank-4 sum
! sigma_ij = C_ijkl eps_kl. A non-symmetric strain is contracted
! through its symmetric part.

PROGRAM main
USE BaseType, ONLY: MaterialJacobian_
USE BaseType, ONLY: Rank2Tensor_
USE BaseType, ONLY: VoigtRank2Tensor_
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: StrainTypeVoigt
USE MaterialJacobian_Method, ONLY: Contraction
USE MaterialJacobian_Method, ONLY: GetRank4
USE MaterialJacobian_Method, ONLY: MaterialJacobian
USE Rank2Tensor_Method, ONLY: Rank2Tensor
USE Test_Method, ONLY: OK
USE VoigtRank2Tensor_Method, ONLY: ASSIGNMENT(=)
USE VoigtRank2Tensor_Method, ONLY: VoigtRank2Tensor
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
REAL(DFP), PARAMETER :: e0(3, 3) = RESHAPE( &
                        [1.0_DFP, 0.6_DFP, -0.3_DFP, &
                         -0.2_DFP, 2.0_DFP, 0.1_DFP, &
                         0.5_DFP, 0.7_DFP, -1.0_DFP], [3, 3])
TYPE(MaterialJacobian_) :: obj
REAL(DFP) :: C(6, 6), C4(3, 3, 3, 3), esym(3, 3), expected(3, 3)
INTEGER(I4B) :: i, j

! symmetric anisotropic tangent with all entries non-zero
DO j = 1, 6
  DO i = 1, 6
    C(i, j) = REAL(MIN(i, j), DFP) + 0.1_DFP * REAL(i + j, DFP)
  END DO
END DO
obj = MaterialJacobian(C)
C4 = GetRank4(obj)
esym = 0.5_DFP * (e0 + TRANSPOSE(e0))
DO j = 1, 3
  DO i = 1, 3
    expected(i, j) = SUM(C4(i, j, :, :) * esym)
  END DO
END DO

! Test 1: symmetric strain as a Rank2Tensor_
BLOCK
  TYPE(Rank2Tensor_) :: sig

  sig = Contraction(obj, Rank2Tensor(esym, .TRUE.))
  CALL OK(MAXVAL(ABS(sig%T - expected)) .LT. tol, "symmetric strain")
END BLOCK

! Test 2: a non-symmetric strain gives the stress of its symmetric part
BLOCK
  TYPE(Rank2Tensor_) :: sig

  sig = Contraction(obj, Rank2Tensor(e0, .FALSE.))
  CALL OK(MAXVAL(ABS(sig%T - expected)) .LT. tol, &
          "non-symmetric strain: symmetric part used")
  CALL OK(sig%isSym, "non-symmetric strain: stress is symmetric")
END BLOCK

! Test 3: Voigt strain gives the same stress as the tensor strain
BLOCK
  TYPE(VoigtRank2Tensor_) :: vsig
  REAL(DFP) :: T(3, 3)

  vsig = Contraction(obj, VoigtRank2Tensor(esym, StrainTypeVoigt))
  T = vsig
  CALL OK(MAXVAL(ABS(T - expected)) .LT. tol, "Voigt strain")
END BLOCK

END PROGRAM main
