!> author: Shion Shimizu
! date: 2026-11-17
! summary: Dev (deviatoric part) of a VoigtRank2Tensor_

PROGRAM main
USE BaseType, ONLY: VoigtRank2Tensor_
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: StrainTypeVoigt
USE Test_Method, ONLY: OK
USE VoigtRank2Tensor_Method, ONLY: Dev
USE VoigtRank2Tensor_Method, ONLY: Trace
USE VoigtRank2Tensor_Method, ONLY: VoigtRank2Tensor
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-14_DFP

! Test 1: the mean is removed, the shear entries and the Voigt type are
! kept (an engineering strain stays an engineering strain)
BLOCK
  TYPE(VoigtRank2Tensor_) :: eps, e
  REAL(DFP) :: expected(6)

  eps = VoigtRank2Tensor([1.0_DFP, 2.0_DFP, 6.0_DFP, 4.0_DFP, 5.0_DFP, &
                          7.0_DFP], StrainTypeVoigt)
  e = Dev(eps)
  expected = [-2.0_DFP, -1.0_DFP, 3.0_DFP, 4.0_DFP, 5.0_DFP, 7.0_DFP]
  CALL OK(MAXVAL(ABS(e%V - expected)) .LT. tol, "Dev: values")
  CALL OK(ABS(Trace(e)) .LT. tol, "Dev: trace is zero")
  CALL OK(e%VoigtType .EQ. StrainTypeVoigt, "Dev: Voigt type is kept")
END BLOCK

END PROGRAM main
