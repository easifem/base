!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Rank4IdentityOtimesIdentity in Voigt form

PROGRAM main
USE BaseType, ONLY: MaterialJacobian_
USE BaseType, ONLY: Rank2Tensor_
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: Eye3
USE MaterialJacobian_Method, ONLY: MaterialJacobian
USE MaterialJacobian_Method, ONLY: Rank4IdentityOtimesIdentity
USE Rank2Tensor_Method, ONLY: Otimes
USE Rank2Tensor_Method, ONLY: Rank2Tensor
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP

! Test 1: ones in the normal 3x3 block, zero elsewhere
BLOCK
  REAL(DFP) :: A(6, 6)

  A = Rank4IdentityOtimesIdentity()
  CALL OK(ALL(ABS(A(1:3, 1:3) - 1.0_DFP) .LT. tol), "normal block")
  CALL OK(ABS(SUM(A) - 9.0_DFP) .LT. tol, "other entries zero")
END BLOCK

! Test 2: equals Otimes(I, I) of Rank2Tensor_
BLOCK
  TYPE(Rank2Tensor_) :: I2
  TYPE(MaterialJacobian_) :: obj

  I2 = Rank2Tensor(Eye3, .TRUE.)
  obj = MaterialJacobian(Otimes(I2, I2))
  CALL OK(MAXVAL(ABS(obj%C - Rank4IdentityOtimesIdentity())) .LT. tol, &
          "Otimes(I, I)")
END BLOCK

END PROGRAM main
