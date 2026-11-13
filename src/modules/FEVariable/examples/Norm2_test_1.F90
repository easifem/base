!> author: Vikas Sharma, Ph. D.
! date: 2026-11-14
! summary: Euclidean norm of a vector FEVariable by NORM2 and Norm2_

PROGRAM main
USE BaseType, ONLY: FEVariable_
USE BaseType, ONLY: fevaropt => TypeFEVariableOpt
USE BaseType, ONLY: Constant => TypeFEVariableConstant
USE BaseType, ONLY: Scalar => TypeFEVariableScalar
USE BaseType, ONLY: Space => TypeFEVariableSpace
USE BaseType, ONLY: SpaceTime => TypeFEVariableSpaceTime
USE BaseType, ONLY: Time => TypeFEVariableTime
USE BaseType, ONLY: Vector => TypeFEVariableVector
USE FEVariable_Method, ONLY: Get
USE FEVariable_Method, ONLY: NodalVariable
USE FEVariable_Method, ONLY: NORM2
USE FEVariable_Method, ONLY: Norm2_
USE FEVariable_Method, ONLY: QuadratureVariable
USE GlobalData, ONLY: DFP
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
REAL(DFP) :: vs(2, 2), vst(2, 2, 2)

vs = RESHAPE([3.0_DFP, 4.0_DFP, 6.0_DFP, 8.0_DFP], [2, 2])
vst = RESHAPE([3.0_DFP, 4.0_DFP, 6.0_DFP, 8.0_DFP, 0.0_DFP, 1.0_DFP, &
               5.0_DFP, 12.0_DFP], [2, 2, 2])

! Test 1: norm at each point gives a scalar variable
BLOCK
  TYPE(FEVariable_) :: ans

  ans = NORM2(QuadratureVariable([3.0_DFP, 4.0_DFP], Vector, Constant))
  CALL OK(ans%rank .EQ. fevaropt%scalar, "constant: scalar")
  CALL OK(ABS(Get(ans, Scalar, Constant) - 5.0_DFP) .LT. tol, "constant")

  ans = NORM2(QuadratureVariable(vs, Vector, Space))
  CALL OK(ALL(ABS(Get(ans, Scalar, Space) - [5.0_DFP, 10.0_DFP]) &
              .LT. tol), "space")

  ans = NORM2(NodalVariable(vs, Vector, Time))
  CALL OK(ans%defineOn .EQ. fevaropt%nodal, "time: nodal")
  CALL OK(ALL(ABS(Get(ans, Scalar, Time) - [5.0_DFP, 10.0_DFP]) &
              .LT. tol), "time")

  ans = NORM2(QuadratureVariable(vst, Vector, SpaceTime))
  CALL OK(ALL(ABS(Get(ans, Scalar, SpaceTime) - RESHAPE([5.0_DFP, &
              10.0_DFP, 1.0_DFP, 13.0_DFP], [2, 2])) .LT. tol), "spaceTime")
END BLOCK

! Test 2: Norm2_ writes into an existing scalar variable
BLOCK
  TYPE(FEVariable_) :: ans

  ans = QuadratureVariable(tsize=2, rank=Scalar, vartype=Space)
  CALL Norm2_(obj=QuadratureVariable(vs, Vector, Space), ans=ans)
  CALL OK(ALL(ABS(ans%val(1:2) - [5.0_DFP, 10.0_DFP]) .LT. tol), "Norm2_")
END BLOCK

END PROGRAM main
