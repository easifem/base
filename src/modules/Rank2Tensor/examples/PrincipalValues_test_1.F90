!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Principal values and spectral radius of a tensor

PROGRAM main
USE BaseType, ONLY: Rank2Tensor_
USE GlobalData, ONLY: DFP
USE Rank2Tensor_Method, ONLY: PrincipalValues
USE Rank2Tensor_Method, ONLY: Rank2Tensor
USE Rank2Tensor_Method, ONLY: SpectralRadius
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
REAL(DFP), PARAMETER :: S0(3, 3) = RESHAPE([5.0_DFP, 0.0_DFP, 0.0_DFP, &
                                            0.0_DFP, -6.0_DFP, -12.0_DFP, &
                                            0.0_DFP, -12.0_DFP, 1.0_DFP], &
                                           [3, 3])

! Test 1: principal values of a symmetric tensor, descending order
BLOCK
  TYPE(Rank2Tensor_) :: obj
  REAL(DFP) :: pv(3)

  obj = Rank2Tensor(S0, .TRUE.)
  pv = PrincipalValues(obj)
  CALL OK(MAXVAL(ABS(pv - [10.0_DFP, 5.0_DFP, -15.0_DFP])) .LT. tol, &
          "PrincipalValues = 10, 5, -15")
END BLOCK

! Test 2: spectral radius of a symmetric tensor
BLOCK
  TYPE(Rank2Tensor_) :: obj

  obj = Rank2Tensor(S0, .TRUE.)
  CALL OK(ABS(SpectralRadius(obj) - 15.0_DFP) .LT. tol, &
          "SpectralRadius symmetric = 15")
END BLOCK

! Test 3: non-symmetric tensor with eigenvalues 1, 2i, -2i
BLOCK
  TYPE(Rank2Tensor_) :: obj
  REAL(DFP) :: mat(3, 3), pv(3)

  mat = 0.0_DFP
  mat(1, 2) = -2.0_DFP
  mat(2, 1) = 2.0_DFP
  mat(3, 3) = 1.0_DFP
  obj = Rank2Tensor(mat)
  pv = PrincipalValues(obj)
  CALL OK(MAXVAL(ABS(pv - [1.0_DFP, 0.0_DFP, 0.0_DFP])) .LT. tol, &
          "PrincipalValues non-symmetric = real parts 1, 0, 0")
  CALL OK(ABS(SpectralRadius(obj) - 2.0_DFP) .LT. tol, &
          "SpectralRadius complex = 2")
END BLOCK

END PROGRAM main
