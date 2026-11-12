!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Cijkl components of an isotropic tangent

PROGRAM main
USE BaseType, ONLY: MaterialJacobian_
USE GlobalData, ONLY: DFP
USE MaterialJacobian_Method, ONLY: Cijkl
USE MaterialJacobian_Method, ONLY: IsotropicTangent
USE MaterialJacobian_Method, ONLY: MaterialJacobian
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP

! Test 1: lambda = 2 and mu = 3
BLOCK
  TYPE(MaterialJacobian_) :: obj

  obj = MaterialJacobian(IsotropicTangent(2.0_DFP, 3.0_DFP))
  CALL OK(ABS(Cijkl(obj, 1, 1, 1, 1) - 8.0_DFP) .LT. tol, &
          "C1111 = lambda + 2 mu")
  CALL OK(ABS(Cijkl(obj, 1, 1, 2, 2) - 2.0_DFP) .LT. tol, &
          "C1122 = lambda")
  CALL OK(ABS(Cijkl(obj, 1, 2, 1, 2) - 3.0_DFP) .LT. tol, "C1212 = mu")
  CALL OK(ABS(Cijkl(obj, 2, 1, 1, 2) - Cijkl(obj, 1, 2, 2, 1)) .LT. tol, &
          "minor symmetry")
END BLOCK

END PROGRAM main
