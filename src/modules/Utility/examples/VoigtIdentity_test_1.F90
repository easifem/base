!> author: Shion Shimizu
! date: 2026-11-17
! summary: VoigtIdentity and the rank 4 tensors of MaterialJacobian

PROGRAM main
USE GlobalData, ONLY: DFP
USE MaterialJacobian_Method, ONLY: Rank4DeviatoricProjector
USE MaterialJacobian_Method, ONLY: Rank4IdentityOtimesIdentity
USE Test_Method, ONLY: OK
USE VoigtUtility, ONLY: VoigtDev
USE VoigtUtility, ONLY: VoigtIdentity
USE VoigtUtility, ONLY: VoigtOuter
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-14_DFP

! Test 1: identity in Voigt form
BLOCK
  REAL(DFP) :: expected(6)

  expected = [1.0_DFP, 1.0_DFP, 1.0_DFP, 0.0_DFP, 0.0_DFP, 0.0_DFP]
  CALL OK(MAXVAL(ABS(VoigtIdentity() - expected)) .LT. tol, &
          "VoigtIdentity: values")
END BLOCK

! Test 2: I otimes I
BLOCK
  CALL OK(MAXVAL(ABS(VoigtOuter(VoigtIdentity(), VoigtIdentity()) &
                     - Rank4IdentityOtimesIdentity())) .LT. tol, &
          "VoigtIdentity: I otimes I")
END BLOCK

! Test 3: the deviatoric projector maps an engineering strain to the
! deviator of the strain tensor
BLOCK
  REAL(DFP) :: eps(6), epsTensor(6)

  eps = [1.0_DFP, 2.0_DFP, 6.0_DFP, 4.0_DFP, 5.0_DFP, 7.0_DFP]
  epsTensor = [eps(1:3), 0.5_DFP * eps(4:6)]
  CALL OK(MAXVAL(ABS(MATMUL(Rank4DeviatoricProjector(), eps) &
                     - VoigtDev(epsTensor))) .LT. tol, &
          "VoigtIdentity: deviator of the strain tensor")
END BLOCK

END PROGRAM main
