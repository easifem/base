!> author: Vikas Sharma, Ph. D.
! date: 2026-11-20
! summary: Space-time interpolation of any FEVariable at a time index

PROGRAM main
USE BaseType, ONLY: FEVariable_
USE BaseType, ONLY: Constant => TypeFEVariableConstant
USE BaseType, ONLY: Matrix => TypeFEVariableMatrix
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
! N(I, ips): 2 nodes, 3 quadrature points (node 1, midpoint, node 2)
REAL(DFP), PARAMETER :: N(2, 3) = RESHAPE([1.0_DFP, 0.0_DFP, 0.5_DFP, &
                                           0.5_DFP, 0.0_DFP, 1.0_DFP], &
                                          [2, 3])
! T(a): 2 time nodes at the time quadrature point
REAL(DFP), PARAMETER :: T(2) = [0.25_DFP, 0.75_DFP]

! Test 1: scalar space obj written in time slice 2 of ans
BLOCK
  TYPE(FEVariable_) :: obj, ans

  obj = NodalVariable([2.0_DFP, 4.0_DFP], Scalar, Space)
  ans = QuadratureVariable(nrow=3, ncol=2, rank=Scalar, vartype=SpaceTime)
  ans%val(1:6) = -1.0_DFP
  CALL GetInterpolation_(obj=obj, N=N, nns=2, nips=3, T=T, nnt=2, &
                         scale=1.0_DFP, addContribution=.FALSE., &
                         timeIndx=2, ans=ans)
  CALL OK(ALL(ABS(ans%val(4:6) - [2.0_DFP, 3.0_DFP, 4.0_DFP]) .LT. tol), &
          "scalar nodal space in time slice 2")
  CALL OK(ALL(ABS(ans%val(1:3) + 1.0_DFP) .LT. tol), &
          "time slice 1 is not changed")

  CALL GetInterpolation_(obj=obj, N=N, nns=2, nips=3, T=T, nnt=2, &
                         scale=2.0_DFP, addContribution=.TRUE., &
                         timeIndx=2, ans=ans)
  CALL OK(ALL(ABS(ans%val(4:6) - [6.0_DFP, 9.0_DFP, 12.0_DFP]) .LT. tol), &
          "scale 2 added to time slice 2")
END BLOCK

! Test 2: scalar constant in time slice 1
BLOCK
  TYPE(FEVariable_) :: obj, ans

  obj = NodalVariable(5.0_DFP, Scalar, Constant)
  ans = QuadratureVariable(nrow=3, ncol=2, rank=Scalar, vartype=SpaceTime)
  ans%val(1:6) = -1.0_DFP
  CALL GetInterpolation_(obj=obj, N=N, nns=2, nips=3, T=T, nnt=2, &
                         scale=1.0_DFP, addContribution=.FALSE., &
                         timeIndx=1, ans=ans)
  CALL OK(ALL(ABS(ans%val(1:3) - 5.0_DFP) .LT. tol), &
          "scalar constant in time slice 1")
  CALL OK(ALL(ABS(ans%val(4:6) + 1.0_DFP) .LT. tol), &
          "time slice 2 is not changed")
END BLOCK

! Test 3: scalar quadrature space-time obj, slice timeIndx is copied
BLOCK
  TYPE(FEVariable_) :: obj, ans

  obj = QuadratureVariable(RESHAPE([1.0_DFP, 2.0_DFP, 3.0_DFP, 4.0_DFP, &
                                    5.0_DFP, 6.0_DFP], [3, 2]), &
                           Scalar, SpaceTime)
  ans = QuadratureVariable(nrow=3, ncol=2, rank=Scalar, vartype=SpaceTime)
  CALL GetInterpolation_(obj=obj, N=N, nns=2, nips=3, T=T, nnt=2, &
                         scale=1.0_DFP, addContribution=.FALSE., &
                         timeIndx=2, ans=ans)
  CALL OK(ALL(ABS(ans%val(4:6) - [4.0_DFP, 5.0_DFP, 6.0_DFP]) .LT. tol), &
          "scalar quadrature spaceTime at timeIndx 2")
END BLOCK

! Test 4: vector nodal space-time obj
BLOCK
  TYPE(FEVariable_) :: obj, ans
  REAL(DFP) :: nodal(2, 2, 2)

  ! nodal(i, node, time); with T the nodes are [4, 5] and [6, 7]
  nodal = RESHAPE([1.0_DFP, 2.0_DFP, 3.0_DFP, 4.0_DFP, &
                   5.0_DFP, 6.0_DFP, 7.0_DFP, 8.0_DFP], [2, 2, 2])
  obj = NodalVariable(nodal, Vector, SpaceTime)
  ans = QuadratureVariable(dim1=2, dim2=3, dim3=2, rank=Vector, &
                           vartype=SpaceTime)
  CALL GetInterpolation_(obj=obj, N=N, nns=2, nips=3, T=T, nnt=2, &
                         scale=1.0_DFP, addContribution=.FALSE., &
                         timeIndx=2, ans=ans)
  CALL OK(ALL(ABS(ans%val(7:12) - [4.0_DFP, 5.0_DFP, 5.0_DFP, 6.0_DFP, &
                                   6.0_DFP, 7.0_DFP]) .LT. tol), &
          "vector nodal spaceTime at timeIndx 2")
END BLOCK

! Test 5: matrix (1 x 1) nodal space-time obj
BLOCK
  TYPE(FEVariable_) :: obj, ans
  REAL(DFP) :: nodal(1, 1, 2, 2)

  ! nodal(1, 1, node, time); with T the nodes are 4 and 6
  nodal = RESHAPE([1.0_DFP, 3.0_DFP, 5.0_DFP, 7.0_DFP], [1, 1, 2, 2])
  obj = NodalVariable(nodal, Matrix, SpaceTime)
  ans = QuadratureVariable(dim1=1, dim2=1, dim3=3, dim4=2, rank=Matrix, &
                           vartype=SpaceTime)
  CALL GetInterpolation_(obj=obj, N=N, nns=2, nips=3, T=T, nnt=2, &
                         scale=1.0_DFP, addContribution=.FALSE., &
                         timeIndx=2, ans=ans)
  CALL OK(ALL(ABS(ans%val(4:6) - [4.0_DFP, 5.0_DFP, 6.0_DFP]) .LT. tol), &
          "matrix nodal spaceTime at timeIndx 2")
END BLOCK

END PROGRAM main
