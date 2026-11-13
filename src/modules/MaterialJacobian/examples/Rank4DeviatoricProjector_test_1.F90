!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Rank4DeviatoricProjector in Voigt form

PROGRAM main
USE BaseType, ONLY: Rank2Tensor_
USE GlobalData, ONLY: DFP
USE MaterialJacobian_Method, ONLY: Contraction
USE MaterialJacobian_Method, ONLY: MaterialJacobian
USE MaterialJacobian_Method, ONLY: Rank4DeviatoricProjector
USE MaterialJacobian_Method, ONLY: Rank4IdentityOtimesIdentity
USE MaterialJacobian_Method, ONLY: Rank4SymIdentity
USE Rank2Tensor_Method, ONLY: Dev
USE Rank2Tensor_Method, ONLY: Rank2Tensor
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP

! Test 1: Pdev = Isym - I x I / 3
BLOCK
  REAL(DFP) :: P(6, 6)

  P = Rank4DeviatoricProjector()
  CALL OK(MAXVAL(ABS(P - Rank4SymIdentity() &
                     + Rank4IdentityOtimesIdentity() / 3.0_DFP)) &
          .LT. tol, "Pdev = Isym - I x I / 3")
END BLOCK

! Test 2: Pdev : eps = dev(eps)
BLOCK
  TYPE(Rank2Tensor_) :: eps, sig, D
  REAL(DFP) :: e0(3, 3)

  e0 = RESHAPE([1.0_DFP, 0.2_DFP, -0.3_DFP, &
                0.2_DFP, 2.0_DFP, 0.4_DFP, &
                -0.3_DFP, 0.4_DFP, -1.0_DFP], [3, 3])
  eps = Rank2Tensor(e0, .TRUE.)
  sig = Contraction(MaterialJacobian(Rank4DeviatoricProjector()), eps)
  D = Dev(eps)
  CALL OK(MAXVAL(ABS(sig%T - D%T)) .LT. tol, "Pdev : eps = dev eps")
END BLOCK

END PROGRAM main
