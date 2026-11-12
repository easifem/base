!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: IsotropicTangent in Voigt form

PROGRAM main
USE GlobalData, ONLY: DFP
USE MaterialJacobian_Method, ONLY: IsotropicTangent
USE MaterialJacobian_Method, ONLY: Rank4IdentityOtimesIdentity
USE MaterialJacobian_Method, ONLY: Rank4SymIdentity
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP

! Test 1: lambda = 2 and mu = 1
BLOCK
  REAL(DFP) :: C(6, 6)

  C = IsotropicTangent(lambda=2.0_DFP, mu=1.0_DFP)
  CALL OK(ABS(C(1, 1) - 4.0_DFP) .LT. tol, "C11 = lambda + 2 mu")
  CALL OK(ABS(C(1, 2) - 2.0_DFP) .LT. tol, "C12 = lambda")
  CALL OK(ABS(C(4, 4) - 1.0_DFP) .LT. tol, "C44 = mu")
  CALL OK(MAXVAL(ABS(C - 2.0_DFP * Rank4IdentityOtimesIdentity() &
                     - 2.0_DFP * Rank4SymIdentity())) .LT. tol, &
          "C = lambda I x I + 2 mu Isym")
END BLOCK

END PROGRAM main
