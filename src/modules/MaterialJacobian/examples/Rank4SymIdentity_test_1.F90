!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Rank4SymIdentity in Voigt form

PROGRAM main
USE BaseType, ONLY: Rank2Tensor_
USE GlobalData, ONLY: DFP
USE MaterialJacobian_Method, ONLY: Contraction
USE MaterialJacobian_Method, ONLY: MaterialJacobian
USE MaterialJacobian_Method, ONLY: Rank4SymIdentity
USE Rank2Tensor_Method, ONLY: Rank2Tensor
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP

! Test 1: diagonal matrix diag(1, 1, 1, 1/2, 1/2, 1/2)
BLOCK
  REAL(DFP) :: S(6, 6)

  S = Rank4SymIdentity()
  CALL OK(ABS(S(1, 1) - 1.0_DFP) .LT. tol, "S11 = 1")
  CALL OK(ABS(S(4, 4) - 0.5_DFP) .LT. tol, "S44 = 1/2")
  CALL OK(ABS(SUM(S) - 4.5_DFP) .LT. tol, "off-diagonal entries zero")
END BLOCK

! Test 2: Isym : eps = eps
BLOCK
  TYPE(Rank2Tensor_) :: sig
  REAL(DFP) :: e0(3, 3)

  e0 = RESHAPE([1.0_DFP, 0.2_DFP, -0.3_DFP, &
                0.2_DFP, 2.0_DFP, 0.4_DFP, &
                -0.3_DFP, 0.4_DFP, -1.0_DFP], [3, 3])
  sig = Contraction(MaterialJacobian(Rank4SymIdentity()), &
                    Rank2Tensor(e0, .TRUE.))
  CALL OK(MAXVAL(ABS(sig%T - e0)) .LT. tol, "Isym : eps = eps")
END BLOCK

END PROGRAM main
