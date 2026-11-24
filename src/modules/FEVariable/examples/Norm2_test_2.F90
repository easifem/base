!> author: Vikas Sharma, Ph. D.
! date: 2026-11-20
! summary: NORM2 of nodal constant and space-time vectors, and Norm2_

PROGRAM main
USE BaseType, ONLY: FEVariable_
USE BaseType, ONLY: fevaropt => TypeFEVariableOpt
USE BaseType, ONLY: Constant => TypeFEVariableConstant
USE BaseType, ONLY: Scalar => TypeFEVariableScalar
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
REAL(DFP) :: vt(3, 2), vst(2, 2, 2), expst(2, 2)

! columns: [1, 2, 2] -> 3, [2, 3, 6] -> 7
vt = RESHAPE([1.0_DFP, 2.0_DFP, 2.0_DFP, 2.0_DFP, 3.0_DFP, 6.0_DFP], &
             [3, 2])
vst = RESHAPE([3.0_DFP, 4.0_DFP, 6.0_DFP, 8.0_DFP, 0.0_DFP, 1.0_DFP, &
               5.0_DFP, 12.0_DFP], [2, 2, 2])
expst = RESHAPE([5.0_DFP, 10.0_DFP, 1.0_DFP, 13.0_DFP], [2, 2])

! Test 1: nodal constant vector gives a nodal constant scalar
BLOCK
  TYPE(FEVariable_) :: ans

  ans = NORM2(NodalVariable([3.0_DFP, 4.0_DFP], Vector, Constant))
  CALL OK(ans%rank .EQ. fevaropt%scalar, "nodal constant: scalar")
  CALL OK(ans%defineOn .EQ. fevaropt%nodal, "nodal constant: nodal")
  CALL OK(ans%varType .EQ. fevaropt%constant, "nodal constant: constant")
  CALL OK(ABS(Get(ans, Scalar, Constant) - 5.0_DFP) .LT. tol, &
          "nodal constant: value")
END BLOCK

! Test 2: quadrature time vector with three components
BLOCK
  TYPE(FEVariable_) :: ans

  ans = NORM2(QuadratureVariable(vt, Vector, Time))
  CALL OK(ans%defineOn .EQ. fevaropt%quadrature, "time: quadrature")
  CALL OK(ans%len .EQ. 2, "time: len")
  CALL OK(ALL(ABS(Get(ans, Scalar, Time) - [3.0_DFP, 7.0_DFP]) &
              .LT. tol), "time: value")
END BLOCK

! Test 3: nodal space-time vector
BLOCK
  TYPE(FEVariable_) :: ans

  ans = NORM2(NodalVariable(vst, Vector, SpaceTime))
  CALL OK(ans%defineOn .EQ. fevaropt%nodal, "spaceTime: nodal")
  CALL OK(ALL(ans%s(1:2) .EQ. [2, 2]), "spaceTime: shape")
  CALL OK(ALL(ABS(Get(ans, Scalar, SpaceTime) - expst) .LT. tol), &
          "spaceTime: value")
END BLOCK

! Test 4: Norm2_ for constant and space-time vectors
BLOCK
  TYPE(FEVariable_) :: ans

  ans = QuadratureVariable(0.0_DFP, Scalar, Constant)
  CALL Norm2_(obj=QuadratureVariable([3.0_DFP, 4.0_DFP], Vector, &
                                     Constant), ans=ans)
  CALL OK(ans%len .EQ. 1, "Norm2_ constant: len")
  CALL OK(ABS(ans%val(1) - 5.0_DFP) .LT. tol, "Norm2_ constant: value")

  ans = QuadratureVariable(nrow=2, ncol=2, rank=Scalar, vartype=SpaceTime)
  CALL Norm2_(obj=QuadratureVariable(vst, Vector, SpaceTime), ans=ans)
  CALL OK(ans%len .EQ. 4, "Norm2_ spaceTime: len")
  CALL OK(ALL(ABS(ans%val(1:4) - [5.0_DFP, 10.0_DFP, 1.0_DFP, 13.0_DFP]) &
              .LT. tol), "Norm2_ spaceTime: value")
END BLOCK

END PROGRAM main
