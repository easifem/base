!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Rate of deformation D and spin W of a velocity gradient

PROGRAM main
USE BaseType, ONLY: ContinuumSpin_
USE BaseType, ONLY: StrainRate_
USE BaseType, ONLY: VelocityGradient_
USE GlobalData, ONLY: DFP
USE Rank2Tensor_Method, ONLY: ContinuumSpin
USE Rank2Tensor_Method, ONLY: ContinuumSpin_Pointer
USE Rank2Tensor_Method, ONLY: StrainRate
USE Rank2Tensor_Method, ONLY: StrainRate_Pointer
USE Rank2Tensor_Method, ONLY: VelocityGradient
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
REAL(DFP), PARAMETER :: L0(3, 3) = RESHAPE( &
                        [0.2_DFP, 0.1_DFP, 0.0_DFP, &
                         0.3_DFP, -0.1_DFP, 0.2_DFP, &
                         0.4_DFP, -0.1_DFP, 0.1_DFP], [3, 3])

! Test 1: D is the symmetric part of L
BLOCK
  TYPE(StrainRate_) :: D
  REAL(DFP) :: expected(3, 3)

  expected = RESHAPE([0.2_DFP, 0.2_DFP, 0.2_DFP, &
                      0.2_DFP, -0.1_DFP, 0.05_DFP, &
                      0.2_DFP, 0.05_DFP, 0.1_DFP], [3, 3])
  D = StrainRate(VelocityGradient(L0))
  CALL OK(MAXVAL(ABS(D%T - expected)) .LT. tol, "D: components")
  CALL OK(D%isSym, "D: symmetric flag")
END BLOCK

! Test 2: W is the skew part of L
BLOCK
  TYPE(ContinuumSpin_) :: W
  REAL(DFP) :: expected(3, 3)

  expected = RESHAPE([0.0_DFP, -0.1_DFP, -0.2_DFP, &
                      0.1_DFP, 0.0_DFP, 0.15_DFP, &
                      0.2_DFP, -0.15_DFP, 0.0_DFP], [3, 3])
  W = ContinuumSpin(VelocityGradient(L0))
  CALL OK(MAXVAL(ABS(W%T - expected)) .LT. tol, "W: components")
  CALL OK(MAXVAL(ABS(W%T + TRANSPOSE(W%T))) .LT. tol, "W: skew")
  CALL OK(.NOT. W%isSym, "W: not symmetric flag")
END BLOCK

! Test 3: L = D + W
BLOCK
  TYPE(VelocityGradient_) :: L
  TYPE(StrainRate_) :: D
  TYPE(ContinuumSpin_) :: W

  L = VelocityGradient(L0)
  D = StrainRate(L)
  W = ContinuumSpin(L)
  CALL OK(MAXVAL(ABS(D%T + W%T - L0)) .LT. tol, "L = D + W")
END BLOCK

! Test 4: pointers to D and W
BLOCK
  TYPE(VelocityGradient_) :: L
  CLASS(StrainRate_), POINTER :: pD
  CLASS(ContinuumSpin_), POINTER :: pW

  L = VelocityGradient(L0)
  pD => StrainRate_Pointer(L)
  pW => ContinuumSpin_Pointer(L)
  CALL OK(MAXVAL(ABS(pD%T - 0.5_DFP * (L0 + TRANSPOSE(L0)))) .LT. tol, &
          "pointer: D")
  CALL OK(pD%isSym, "pointer: D symmetric flag")
  CALL OK(MAXVAL(ABS(pW%T - 0.5_DFP * (L0 - TRANSPOSE(L0)))) .LT. tol, &
          "pointer: W")
  CALL OK(.NOT. pW%isSym, "pointer: W not symmetric flag")
  DEALLOCATE (pD)
  DEALLOCATE (pW)
END BLOCK

END PROGRAM main
