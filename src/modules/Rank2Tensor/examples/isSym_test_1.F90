!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Symmetry flag and traceless check of a Rank2Tensor

PROGRAM main
USE BaseType, ONLY: Rank2Tensor_
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: Eye3
USE Rank2Tensor_Method, ONLY: isDeviatoric
USE Rank2Tensor_Method, ONLY: isSym
USE Rank2Tensor_Method, ONLY: Rank2Tensor
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: A0(3, 3) = RESHAPE([2.0_DFP, 1.0_DFP, 0.5_DFP, &
                                            -1.0_DFP, 3.0_DFP, 0.25_DFP, &
                                            0.0_DFP, 1.5_DFP, 4.0_DFP], &
                                           [3, 3])

! Test 1: isSym returns the symmetry flag
BLOCK
  TYPE(Rank2Tensor_) :: obj

  obj = Rank2Tensor(A0)
  CALL OK(.NOT. isSym(obj), "isSym: false by default")
  obj = Rank2Tensor(Eye3, .TRUE.)
  CALL OK(isSym(obj), "isSym: true when the flag is set")
END BLOCK

! Test 2: isDeviatoric checks the trace
BLOCK
  TYPE(Rank2Tensor_) :: obj
  REAL(DFP) :: mat(3, 3)

  obj = Rank2Tensor(A0)
  CALL OK(.NOT. isDeviatoric(obj), "isDeviatoric: trace is 9")
  mat = A0 - 3.0_DFP * Eye3
  obj = Rank2Tensor(mat)
  CALL OK(isDeviatoric(obj), "isDeviatoric: traceless")
  obj = Rank2Tensor(1.0E+6_DFP * mat)
  CALL OK(isDeviatoric(obj), "isDeviatoric: scale independent")
  obj = Rank2Tensor(1.0E-6_DFP * Eye3)
  CALL OK(.NOT. isDeviatoric(obj), "isDeviatoric: small identity")
END BLOCK

END PROGRAM main
