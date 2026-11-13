!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Second and third invariants of the deviatoric part

PROGRAM main
USE BaseType, ONLY: Rank2Tensor_
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: Eye3
USE Rank2Tensor_Method, ONLY: Dev
USE Rank2Tensor_Method, ONLY: J2
USE Rank2Tensor_Method, ONLY: J3
USE Rank2Tensor_Method, ONLY: Rank2Tensor
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP

! Test 1: uniaxial stress 3 plus a pressure, J2 = 3 and J3 = 2
BLOCK
  TYPE(Rank2Tensor_) :: obj
  REAL(DFP) :: mat(3, 3)

  mat = 5.0_DFP * Eye3
  mat(1, 1) = mat(1, 1) + 3.0_DFP
  obj = Rank2Tensor(mat, .TRUE.)
  CALL OK(ABS(J2(obj) - 3.0_DFP) .LT. tol, "uniaxial: J2 = 3")
  CALL OK(ABS(J3(obj) - 2.0_DFP) .LT. tol, "uniaxial: J3 = 2")
END BLOCK

! Test 2: isDeviatoric with a traceless tensor
BLOCK
  TYPE(Rank2Tensor_) :: obj
  REAL(DFP) :: mat(3, 3)

  mat = 5.0_DFP * Eye3
  mat(1, 1) = mat(1, 1) + 3.0_DFP
  obj = Dev(Rank2Tensor(mat, .TRUE.))
  CALL OK(ABS(J2(obj, isDeviatoric=.TRUE.) - 3.0_DFP) .LT. tol, &
          "deviatoric: J2 = 3")
  CALL OK(ABS(J3(obj, isDeviatoric=.TRUE.) - 2.0_DFP) .LT. tol, &
          "deviatoric: J3 = 2")
END BLOCK

! Test 3: pure shear 3, J2 = 9 and J3 = 0
BLOCK
  TYPE(Rank2Tensor_) :: obj
  REAL(DFP) :: mat(3, 3)

  mat = 0.0_DFP
  mat(1, 2) = 3.0_DFP
  mat(2, 1) = 3.0_DFP
  obj = Rank2Tensor(mat, .TRUE.)
  CALL OK(ABS(J2(obj) - 9.0_DFP) .LT. tol, "shear: J2 = 9")
  CALL OK(ABS(J3(obj)) .LT. tol, "shear: J3 = 0")
  CALL OK(ABS(J2(obj, isDeviatoric=.TRUE.) - 9.0_DFP) .LT. tol, &
          "shear deviatoric: J2 = 9")
END BLOCK

! Test 4: an isotropic tensor has J2 = J3 = 0
BLOCK
  TYPE(Rank2Tensor_) :: obj

  obj = Rank2Tensor(2.0_DFP * Eye3, .TRUE.)
  CALL OK(ABS(J2(obj)) .LT. tol, "isotropic: J2 = 0")
  CALL OK(ABS(J3(obj)) .LT. tol, "isotropic: J3 = 0")
END BLOCK

END PROGRAM main
