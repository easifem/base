!> author: Vikas Sharma, Ph. D.
! date: 2026-11-14
! summary: Interpolation of any FEVariable into a quadrature FEVariable

PROGRAM main
USE BaseType, ONLY: FEVariable_
USE BaseType, ONLY: Scalar => TypeFEVariableScalar
USE BaseType, ONLY: Space => TypeFEVariableSpace
USE BaseType, ONLY: SpaceTime => TypeFEVariableSpaceTime
USE BaseType, ONLY: Vector => TypeFEVariableVector
USE FEVariable_Method, ONLY: GetInterpolation_
USE FEVariable_Method, ONLY: NodalVariable
USE FEVariable_Method, ONLY: QuadratureVariable
USE GlobalData, ONLY: DFP
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
REAL(DFP), PARAMETER :: N(2, 3) = RESHAPE([1.0_DFP, 0.0_DFP, 0.5_DFP, &
                                           0.5_DFP, 0.0_DFP, 1.0_DFP], &
                                          [2, 3])
REAL(DFP), PARAMETER :: T(2) = [0.25_DFP, 0.75_DFP]

! Test 1: rank and variable type are taken from obj
BLOCK
  TYPE(FEVariable_) :: obj, ans

  obj = NodalVariable([2.0_DFP, 4.0_DFP], Scalar, Space)
  ans = QuadratureVariable(tsize=3, rank=Scalar, vartype=Space)
  CALL GetInterpolation_(obj=obj, N=N, nns=2, nips=3, scale=1.0_DFP, &
                         addContribution=.FALSE., ans=ans)
  CALL OK(ALL(ABS(ans%val(1:3) - [2.0_DFP, 3.0_DFP, 4.0_DFP]) .LT. tol), &
          "scalar nodal space")

  obj = NodalVariable(RESHAPE([1.0_DFP, 2.0_DFP, 3.0_DFP, 4.0_DFP], &
                              [2, 2]), Vector, Space)
  ans = QuadratureVariable(nrow=2, ncol=3, rank=Vector, vartype=Space)
  CALL GetInterpolation_(obj=obj, N=N, nns=2, nips=3, scale=1.0_DFP, &
                         addContribution=.FALSE., ans=ans)
  CALL OK(ALL(ABS(ans%val(1:6) - [1.0_DFP, 2.0_DFP, 2.0_DFP, 3.0_DFP, &
                                  3.0_DFP, 4.0_DFP]) .LT. tol), &
          "vector nodal space")
END BLOCK

! Test 2: space-time variable with time shape functions at timeIndx
BLOCK
  TYPE(FEVariable_) :: obj, ans

  obj = NodalVariable(RESHAPE([2.0_DFP, 4.0_DFP, 6.0_DFP, 8.0_DFP], &
                              [2, 2]), Scalar, SpaceTime)
  ans = QuadratureVariable(nrow=3, ncol=2, rank=Scalar, vartype=SpaceTime)
  CALL GetInterpolation_(obj=obj, N=N, nns=2, nips=3, T=T, nnt=2, &
                         scale=1.0_DFP, addContribution=.FALSE., &
                         timeIndx=2, ans=ans)
  CALL OK(ALL(ABS(ans%val(4:6) - [5.0_DFP, 6.0_DFP, 7.0_DFP]) .LT. tol), &
          "scalar nodal spaceTime at timeIndx 2")
  CALL OK(ans%len .EQ. 6, "len of ans is kept")
END BLOCK

END PROGRAM main
