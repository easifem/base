!> author: Vikas Sharma, Ph. D.
! date: 2026-11-20
! summary: Initiate again resets absent tags and keeps the symmetric part
!
! Initiate does not keep the old tags of obj: absent tags take their
! default again. A rank-4 array with minor symmetry is stored exactly.

PROGRAM main
USE BaseType, ONLY: MaterialJacobian_
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: StrainMeasureGreen
USE GlobalData, ONLY: StrainMeasureSmall
USE GlobalData, ONLY: StressMeasureCauchy
USE GlobalData, ONLY: StressMeasurePK2
USE GlobalData, ONLY: StressRateJaumann
USE GlobalData, ONLY: StressRateNone
USE MaterialJacobian_Method, ONLY: GetRank4
USE MaterialJacobian_Method, ONLY: Initiate
USE MaterialJacobian_Method, ONLY: IsotropicTangent
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP

! Test 1: a second Initiate resets the absent tags to the defaults
BLOCK
  TYPE(MaterialJacobian_) :: obj

  CALL Initiate(obj, IsotropicTangent(2.0_DFP, 3.0_DFP), &
                stressType=StressMeasurePK2, &
                strainType=StrainMeasureGreen, &
                rateType=StressRateJaumann)
  CALL Initiate(obj, IsotropicTangent(1.0_DFP, 1.0_DFP))
  CALL OK(MAXVAL(ABS(obj%C - IsotropicTangent(1.0_DFP, 1.0_DFP))) &
          .LT. tol, "re-initiate: C replaced")
  CALL OK(obj%stressType .EQ. StressMeasureCauchy, &
          "re-initiate: stressType reset")
  CALL OK(obj%strainType .EQ. StrainMeasureSmall, &
          "re-initiate: strainType reset")
  CALL OK(obj%rateType .EQ. StressRateNone, &
          "re-initiate: rateType reset")
END BLOCK

! Test 2: a minor-symmetric rank-4 array is stored exactly
BLOCK
  TYPE(MaterialJacobian_) :: obj, obj2

  CALL Initiate(obj, IsotropicTangent(2.0_DFP, 3.0_DFP))
  obj%C(1, 4) = 0.7_DFP
  obj%C(5, 2) = -0.4_DFP
  CALL Initiate(obj2, GetRank4(obj), stressType=StressMeasurePK2)
  CALL OK(MAXVAL(ABS(obj2%C - obj%C)) .LT. tol, "rank-4: round trip")
  CALL OK(obj2%stressType .EQ. StressMeasurePK2, "rank-4: stressType")
END BLOCK

END PROGRAM main
