!> author: Shion Shimizu
! date: 2026-11-02
! summary: Allocation-free GetVoigtVector_ and ReduceVoigtTangent_
!
! The results are compared with the allocatable versions
! GetVoigtVector and ReduceVoigtTangent for all Voigt forms.

PROGRAM main
USE GlobalData, ONLY: DFP, I4B, StressTypeVoigt, VoigtForm3D, &
                      VoigtFormPlaneStrain, VoigtFormPlaneStress, &
                      VoigtFormAxisymmetric
USE BaseType, ONLY: VoigtRank2Tensor_
USE VoigtRank2Tensor_Method, ONLY: VoigtRank2Tensor, GetVoigtVector, &
                                   GetVoigtVector_, VoigtSize
USE MaterialJacobian_Method, ONLY: IsotropicTangent, ReduceVoigtTangent, &
                                   ReduceVoigtTangent_
IMPLICIT NONE

TYPE(VoigtRank2Tensor_) :: sig
REAL(DFP) :: vec(6), C(6, 6), Cr(6, 6)
REAL(DFP), ALLOCATABLE :: vref(:), Cref(:, :)
INTEGER(I4B) :: forms(4), ii, tsize, nrow, ncol

forms = [VoigtForm3D, VoigtFormPlaneStrain, VoigtFormPlaneStress, &
         VoigtFormAxisymmetric]

sig = VoigtRank2Tensor([1.0_DFP, 2.0_DFP, 3.0_DFP, 4.0_DFP, 5.0_DFP, &
                        6.0_DFP], StressTypeVoigt)
C = IsotropicTangent(lambda=1.5_DFP, mu=0.5_DFP)
C(1, 4) = 0.1_DFP ! make it unsymmetric
C(5, 2) = 0.2_DFP

DO ii = 1, SIZE(forms)
  vref = GetVoigtVector(sig, forms(ii))
  CALL GetVoigtVector_(sig, forms(ii), vec, tsize)
  IF (tsize .NE. VoigtSize(forms(ii))) STOP "FAILED: tsize"
  IF (ANY(vec(1:tsize) .NE. vref)) STOP "FAILED: GetVoigtVector_"

  Cref = ReduceVoigtTangent(C, forms(ii))
  CALL ReduceVoigtTangent_(C, forms(ii), Cr, nrow, ncol)
  IF (nrow .NE. SIZE(Cref, 1) .OR. ncol .NE. SIZE(Cref, 2)) &
    STOP "FAILED: nrow, ncol"
  IF (MAXVAL(ABS(Cr(1:nrow, 1:ncol) - Cref)) .GT. 1.0E-14) &
    STOP "FAILED: ReduceVoigtTangent_"
END DO

WRITE (*, "(A)") "MaterialJacobian_test_7 PASSED"

END PROGRAM main
