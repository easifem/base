!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Get and set the stress measure of a Stress_
!
! SetStressType changes only the label, the components are not
! converted.

PROGRAM main
USE BaseType, ONLY: Stress_
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: StressMeasureCauchy
USE GlobalData, ONLY: StressMeasureKirchhoff
USE GlobalData, ONLY: StressMeasureMandel
USE GlobalData, ONLY: StressMeasurePK1
USE Rank2Tensor_Method, ONLY: GetStressType
USE Rank2Tensor_Method, ONLY: SetStressType
USE Rank2Tensor_Method, ONLY: Stress
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
REAL(DFP), PARAMETER :: s0(3, 3) = RESHAPE( &
                        [10.0_DFP, 2.0_DFP, 3.0_DFP, &
                         2.0_DFP, 20.0_DFP, 4.0_DFP, &
                         3.0_DFP, 4.0_DFP, 30.0_DFP], [3, 3])

! Test 1: GetStressType returns the measure given to the constructor
BLOCK
  TYPE(Stress_) :: sig

  sig = Stress(s0, StressMeasureKirchhoff)
  CALL OK(GetStressType(sig) .EQ. StressMeasureKirchhoff, &
          "GetStressType: Kirchhoff")
  sig = Stress(s0, StressMeasurePK1)
  CALL OK(GetStressType(sig) .EQ. StressMeasurePK1, &
          "GetStressType: PK1")
END BLOCK

! Test 2: the default measure of a Stress_ is Cauchy
BLOCK
  TYPE(Stress_) :: sig

  CALL OK(GetStressType(sig) .EQ. StressMeasureCauchy, &
          "GetStressType: default is Cauchy")
END BLOCK

! Test 3: SetStressType changes the label but not the components
BLOCK
  TYPE(Stress_) :: sig

  sig = Stress(s0, StressMeasureCauchy)
  CALL SetStressType(sig, StressMeasureMandel)
  CALL OK(GetStressType(sig) .EQ. StressMeasureMandel, &
          "SetStressType: new measure")
  CALL OK(MAXVAL(ABS(sig%T - s0)) .LT. tol, &
          "SetStressType: components unchanged")
  CALL OK(sig%isSym, "SetStressType: isSym unchanged")
END BLOCK

END PROGRAM main
