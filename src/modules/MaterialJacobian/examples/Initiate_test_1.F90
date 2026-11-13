!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Initiate a MaterialJacobian from a Voigt matrix or a rank-4
!   array

PROGRAM main
USE BaseType, ONLY: MaterialJacobian_
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: StrainMeasureSmall
USE GlobalData, ONLY: StressMeasurePK2
USE GlobalData, ONLY: StressRateJaumann
USE MaterialJacobian_Method, ONLY: Initiate
USE MaterialJacobian_Method, ONLY: IsotropicTangent
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP

! Test 1: from a 6x6 Voigt matrix, absent tags take the default
BLOCK
  TYPE(MaterialJacobian_) :: obj

  CALL Initiate(obj, IsotropicTangent(2.0_DFP, 3.0_DFP), &
                stressType=StressMeasurePK2, rateType=StressRateJaumann)
  CALL OK(MAXVAL(ABS(obj%C - IsotropicTangent(2.0_DFP, 3.0_DFP))) &
          .LT. tol, "matrix: C")
  CALL OK(obj%stressType .EQ. StressMeasurePK2, "matrix: stressType")
  CALL OK(obj%strainType .EQ. StrainMeasureSmall, "matrix: strainType")
  CALL OK(obj%rateType .EQ. StressRateJaumann, "matrix: rateType")
END BLOCK

! Test 2: from a rank-4 array, only the minor-symmetric part is kept
BLOCK
  TYPE(MaterialJacobian_) :: obj
  REAL(DFP) :: C4(3, 3, 3, 3)

  C4 = 0.0_DFP
  C4(1, 2, 1, 1) = 1.0_DFP
  CALL Initiate(obj, C4)
  CALL OK(ABS(obj%C(4, 1) - 0.5_DFP) .LT. tol, "rank-4: average C(4,1)")
  CALL OK(ABS(SUM(ABS(obj%C)) - 0.5_DFP) .LT. tol, &
          "rank-4: other entries zero")
END BLOCK

END PROGRAM main
