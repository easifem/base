!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: GetRank4 of an isotropic tangent and its round trip

PROGRAM main
USE BaseType, ONLY: MaterialJacobian_
USE GlobalData, ONLY: DFP
USE MaterialJacobian_Method, ONLY: GetRank4
USE MaterialJacobian_Method, ONLY: IsotropicTangent
USE MaterialJacobian_Method, ONLY: MaterialJacobian
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP

! Test 1: entries of the rank-4 array and the round trip
BLOCK
  TYPE(MaterialJacobian_) :: obj, obj2
  REAL(DFP) :: C4(3, 3, 3, 3)

  obj = MaterialJacobian(IsotropicTangent(2.0_DFP, 3.0_DFP))
  C4 = GetRank4(obj)
  CALL OK(ABS(C4(1, 2, 2, 1) - 3.0_DFP) .LT. tol, "C4(1,2,2,1) = mu")
  CALL OK(ABS(C4(3, 3, 1, 1) - 2.0_DFP) .LT. tol, &
          "C4(3,3,1,1) = lambda")
  obj2 = MaterialJacobian(C4)
  CALL OK(MAXVAL(ABS(obj2%C - obj%C)) .LT. tol, "round trip")
END BLOCK

END PROGRAM main
