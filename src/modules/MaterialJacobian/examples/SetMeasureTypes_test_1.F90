!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: SetMeasureTypes and the getters of the measure tags

PROGRAM main
USE BaseType, ONLY: MaterialJacobian_
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: StrainMeasureGreen
USE GlobalData, ONLY: StrainMeasureSmall
USE GlobalData, ONLY: StressMeasureCauchy
USE GlobalData, ONLY: StressMeasurePK2
USE GlobalData, ONLY: StressRateNone
USE MaterialJacobian_Method, ONLY: GetRateType
USE MaterialJacobian_Method, ONLY: GetStrainType
USE MaterialJacobian_Method, ONLY: GetStressType
USE MaterialJacobian_Method, ONLY: IsotropicTangent
USE MaterialJacobian_Method, ONLY: MaterialJacobian
USE MaterialJacobian_Method, ONLY: SetMeasureTypes
USE Test_Method, ONLY: OK
IMPLICIT NONE

! Test 1: the getters return the default tags
BLOCK
  TYPE(MaterialJacobian_) :: obj

  obj = MaterialJacobian(IsotropicTangent(2.0_DFP, 3.0_DFP))
  CALL OK(GetStressType(obj) .EQ. StressMeasureCauchy, "GetStressType")
  CALL OK(GetStrainType(obj) .EQ. StrainMeasureSmall, "GetStrainType")
  CALL OK(GetRateType(obj) .EQ. StressRateNone, "GetRateType")
END BLOCK

! Test 2: only the present tags are changed
BLOCK
  TYPE(MaterialJacobian_) :: obj

  obj = MaterialJacobian(IsotropicTangent(2.0_DFP, 3.0_DFP))
  CALL SetMeasureTypes(obj, stressType=StressMeasurePK2, &
                       strainType=StrainMeasureGreen)
  CALL OK(GetStressType(obj) .EQ. StressMeasurePK2, "set stressType")
  CALL OK(GetStrainType(obj) .EQ. StrainMeasureGreen, "set strainType")
  CALL OK(GetRateType(obj) .EQ. StressRateNone, "rateType unchanged")
END BLOCK

END PROGRAM main
