!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Identity, ones, zeros and isotropic rank-2 tensors

PROGRAM main
USE BaseType, ONLY: Rank2Tensor_
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: Eye3
USE Rank2Tensor_Method, ONLY: getOnes
USE Rank2Tensor_Method, ONLY: getZeros
USE Rank2Tensor_Method, ONLY: IdentityTensor
USE Rank2Tensor_Method, ONLY: IsotropicTensor
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP

! Test 1: identity tensor
BLOCK
  TYPE(Rank2Tensor_) :: obj

  CALL IdentityTensor(obj)
  CALL OK(MAXVAL(ABS(obj%T - Eye3)) .LT. tol, "IdentityTensor: T")
  CALL OK(obj%isSym, "IdentityTensor: isSym")
END BLOCK

! Test 2: all components equal to one
BLOCK
  TYPE(Rank2Tensor_) :: obj

  CALL getOnes(obj)
  CALL OK(MAXVAL(ABS(obj%T - 1.0_DFP)) .LT. tol, "getOnes: T")
  CALL OK(obj%isSym, "getOnes: isSym")
END BLOCK

! Test 3: all components equal to zero
BLOCK
  TYPE(Rank2Tensor_) :: obj

  CALL getOnes(obj)
  CALL getZeros(obj)
  CALL OK(MAXVAL(ABS(obj%T)) .LT. tol, "getZeros: T")
  CALL OK(obj%isSym, "getZeros: isSym")
END BLOCK

! Test 4: isotropic tensor lambda * I
BLOCK
  TYPE(Rank2Tensor_) :: obj

  CALL IsotropicTensor(obj, 2.5_DFP)
  CALL OK(MAXVAL(ABS(obj%T - 2.5_DFP * Eye3)) .LT. tol, &
          "IsotropicTensor: T")
  CALL OK(obj%isSym, "IsotropicTensor: isSym")
END BLOCK

END PROGRAM main
