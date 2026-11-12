!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: MaterialJacobian_Pointer from a Voigt matrix or a rank-4 array

PROGRAM main
USE BaseType, ONLY: MaterialJacobian_
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: StressRateJaumann
USE MaterialJacobian_Method, ONLY: GetRank4
USE MaterialJacobian_Method, ONLY: IsotropicTangent
USE MaterialJacobian_Method, ONLY: MaterialJacobian
USE MaterialJacobian_Method, ONLY: MaterialJacobian_Pointer
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP

! Test 1: pointer from a 6x6 Voigt matrix with a rate tag
BLOCK
  CLASS(MaterialJacobian_), POINTER :: ptr

  ptr => MaterialJacobian_Pointer(IsotropicTangent(2.0_DFP, 3.0_DFP), &
                                  rateType=StressRateJaumann)
  CALL OK(ASSOCIATED(ptr), "matrix: associated")
  CALL OK(MAXVAL(ABS(ptr%C - IsotropicTangent(2.0_DFP, 3.0_DFP))) &
          .LT. tol, "matrix: C")
  CALL OK(ptr%rateType .EQ. StressRateJaumann, "matrix: rateType")
  DEALLOCATE (ptr)
END BLOCK

! Test 2: pointer from a rank-4 array
BLOCK
  CLASS(MaterialJacobian_), POINTER :: ptr
  TYPE(MaterialJacobian_) :: obj

  obj = MaterialJacobian(IsotropicTangent(2.0_DFP, 3.0_DFP))
  ptr => MaterialJacobian_Pointer(GetRank4(obj))
  CALL OK(MAXVAL(ABS(ptr%C - obj%C)) .LT. tol, "rank-4: C")
  DEALLOCATE (ptr)
END BLOCK

END PROGRAM main
