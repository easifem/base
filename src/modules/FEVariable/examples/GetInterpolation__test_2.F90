!> author: Vikas Sharma, Ph. D.
! date: 2026-11-14
! summary: Interpolation of vector FEVariables to quadrature points

PROGRAM main
USE BaseType, ONLY: FEVariable_
USE BaseType, ONLY: Constant => TypeFEVariableConstant
USE BaseType, ONLY: Space => TypeFEVariableSpace
USE BaseType, ONLY: SpaceTime => TypeFEVariableSpaceTime
USE BaseType, ONLY: Vector => TypeFEVariableVector
USE FEVariable_Method, ONLY: GetInterpolation_
USE FEVariable_Method, ONLY: NodalVariable
USE FEVariable_Method, ONLY: QuadratureVariable
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
REAL(DFP), PARAMETER :: N(2, 3) = RESHAPE([1.0_DFP, 0.0_DFP, 0.5_DFP, &
                                           0.5_DFP, 0.0_DFP, 1.0_DFP], &
                                          [2, 3])
REAL(DFP), PARAMETER :: T(2) = [0.25_DFP, 0.75_DFP]
! nodal vectors [1, 2] and [3, 4] give [1, 2], [2, 3], [3, 4]
REAL(DFP), PARAMETER :: ref(2, 3) = RESHAPE([1.0_DFP, 2.0_DFP, 2.0_DFP, &
                                             3.0_DFP, 3.0_DFP, 4.0_DFP], &
                                            [2, 3])
TYPE(FEVariable_) :: nodalSpace, constVar, nodalST

nodalSpace = NodalVariable(RESHAPE([1.0_DFP, 2.0_DFP, 3.0_DFP, 4.0_DFP], &
                                   [2, 2]), Vector, Space)
constVar = QuadratureVariable([1.0_DFP, 2.0_DFP], Vector, Constant)
! the same nodal vectors at both time nodes
nodalST = NodalVariable(RESHAPE([1.0_DFP, 2.0_DFP, 3.0_DFP, 4.0_DFP, &
                                 1.0_DFP, 2.0_DFP, 3.0_DFP, 4.0_DFP], &
                                [2, 2, 2]), Vector, SpaceTime)

! Test 1: values at all quadrature points in an array
BLOCK
  REAL(DFP) :: ans(3, 4)
  INTEGER(I4B) :: nrow, ncol

  CALL GetInterpolation_(obj=nodalSpace, rank=Vector, vartype=Space, N=N, &
                         nns=2, nips=3, scale=1.0_DFP, &
                         addContribution=.FALSE., ans=ans, nrow=nrow, &
                         ncol=ncol)
  CALL OK(nrow .EQ. 2 .AND. ncol .EQ. 3, "nodal space: nrow, ncol")
  CALL OK(ALL(ABS(ans(1:2, 1:3) - ref) .LT. tol), "nodal space: values")

  CALL GetInterpolation_(obj=QuadratureVariable(ref, Vector, Space), &
                         rank=Vector, vartype=Space, N=N, nns=2, nips=3, &
                         scale=1.0_DFP, addContribution=.FALSE., ans=ans, &
                         nrow=nrow, ncol=ncol)
  CALL OK(ALL(ABS(ans(1:2, 1:3) - ref) .LT. tol), &
          "quadrature space: values are copied")

  CALL GetInterpolation_(obj=constVar, rank=Vector, vartype=Constant, N=N, &
                         nns=2, nips=3, scale=1.0_DFP, &
                         addContribution=.FALSE., ans=ans, nrow=nrow, &
                         ncol=ncol)
  CALL OK(ALL(ABS(ans(1:2, 2) - [1.0_DFP, 2.0_DFP]) .LT. tol), &
          "constant: same vector")

  CALL GetInterpolation_(obj=nodalST, rank=Vector, vartype=SpaceTime, &
                         N=N, nns=2, nips=3, T=T, nnt=2, scale=1.0_DFP, &
                         addContribution=.FALSE., ans=ans, nrow=nrow, &
                         ncol=ncol, timeIndx=1)
  CALL OK(ALL(ABS(ans(1:2, 1:3) - ref) .LT. tol), "nodal spaceTime")
END BLOCK

! Test 2: values written into a FEVariable
BLOCK
  TYPE(FEVariable_) :: ans

  ans = QuadratureVariable(nrow=2, ncol=3, rank=Vector, vartype=Space)
  CALL GetInterpolation_(obj=constVar, rank=Vector, vartype=Constant, N=N, &
                         nns=2, nips=3, scale=1.0_DFP, &
                         addContribution=.FALSE., timeIndx=1, ans=ans)
  CALL OK(ALL(ABS(ans%val(1:6) - [1.0_DFP, 2.0_DFP, 1.0_DFP, 2.0_DFP, &
                                  1.0_DFP, 2.0_DFP]) .LT. tol), &
          "FEVariable: constant at every point")

  ans%val(1:6) = 99.0_DFP
  CALL GetInterpolation_(obj=nodalST, rank=Vector, vartype=SpaceTime, &
                         N=N, nns=2, nips=3, T=T, nnt=2, scale=1.0_DFP, &
                         addContribution=.FALSE., timeIndx=1, ans=ans)
  CALL OK(ALL(ABS(ans%val(1:6) - RESHAPE(ref, [6])) .LT. tol), &
          "FEVariable: nodal spaceTime replaces old values")

  CALL GetInterpolation_(obj=nodalSpace, rank=Vector, vartype=Space, N=N, &
                         nns=2, nips=3, scale=1.0_DFP, &
                         addContribution=.TRUE., timeIndx=1, ans=ans)
  CALL OK(ALL(ABS(ans%val(1:6) - 2.0_DFP * RESHAPE(ref, [6])) .LT. tol), &
          "FEVariable: nodal space adds")
END BLOCK

! Test 3: vector at one quadrature point
BLOCK
  REAL(DFP) :: ans(3)
  INTEGER(I4B) :: tsize

  CALL GetInterpolation_(obj=nodalSpace, rank=Vector, vartype=Space, N=N, &
                         nns=2, spaceIndx=2, timeIndx=1, scale=1.0_DFP, &
                         addContribution=.FALSE., ans=ans, tsize=tsize)
  CALL OK(tsize .EQ. 2, "point: tsize = 2")
  CALL OK(ALL(ABS(ans(1:2) - [2.0_DFP, 3.0_DFP]) .LT. tol), &
          "point: nodal space")

  CALL GetInterpolation_(obj=nodalST, rank=Vector, N=N, nns=2, &
                         spaceIndx=3, timeIndx=1, T=T, nnt=2, &
                         scale=1.0_DFP, addContribution=.FALSE., ans=ans, &
                         tsize=tsize)
  CALL OK(ALL(ABS(ans(1:2) - [3.0_DFP, 4.0_DFP]) .LT. tol), &
          "point: any vartype, spaceTime")
END BLOCK

END PROGRAM main
