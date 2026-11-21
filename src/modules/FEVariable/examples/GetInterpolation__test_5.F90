!> author: Vikas Sharma, Ph. D.
! date: 2026-11-20
! summary: Space interpolation of any FEVariable (rank and type from obj)

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

! Test 1: scalar constant, scale and addContribution
BLOCK
  TYPE(FEVariable_) :: obj, ans

  obj = NodalVariable(3.0_DFP, Scalar, Constant)
  ans = QuadratureVariable(tsize=3, rank=Scalar, vartype=Space)
  ans%val(1:3) = -1.0_DFP
  CALL GetInterpolation_(obj=obj, N=N, nns=2, nips=3, scale=2.0_DFP, &
                         addContribution=.FALSE., ans=ans)
  CALL OK(ALL(ABS(ans%val(1:3) - 6.0_DFP) .LT. tol), &
          "scalar constant, scale 2, ans cleared")

  obj = NodalVariable([2.0_DFP, 4.0_DFP], Scalar, Space)
  CALL GetInterpolation_(obj=obj, N=N, nns=2, nips=3, scale=1.0_DFP, &
                         addContribution=.TRUE., ans=ans)
  CALL OK(ALL(ABS(ans%val(1:3) - [8.0_DFP, 9.0_DFP, 10.0_DFP]) .LT. tol), &
          "scalar nodal space added to ans")
END BLOCK

! Test 2: scalar quadrature variable is copied and scaled
BLOCK
  TYPE(FEVariable_) :: obj, ans

  obj = QuadratureVariable([1.0_DFP, 2.0_DFP, 3.0_DFP], Scalar, Space)
  ans = QuadratureVariable(tsize=3, rank=Scalar, vartype=Space)
  CALL GetInterpolation_(obj=obj, N=N, nns=2, nips=3, scale=0.5_DFP, &
                         addContribution=.FALSE., ans=ans)
  CALL OK(ALL(ABS(ans%val(1:3) - [0.5_DFP, 1.0_DFP, 1.5_DFP]) .LT. tol), &
          "scalar quadrature space")
END BLOCK

! Test 3: space-time obj uses only its first time slice
BLOCK
  TYPE(FEVariable_) :: obj, ans

  ! time slice 1: [2, 4], time slice 2: [6, 8]
  obj = NodalVariable(RESHAPE([2.0_DFP, 4.0_DFP, 6.0_DFP, 8.0_DFP], &
                              [2, 2]), Scalar, SpaceTime)
  ans = QuadratureVariable(tsize=3, rank=Scalar, vartype=Space)
  CALL GetInterpolation_(obj=obj, N=N, nns=2, nips=3, scale=1.0_DFP, &
                         addContribution=.FALSE., ans=ans)
  CALL OK(ALL(ABS(ans%val(1:3) - [2.0_DFP, 3.0_DFP, 4.0_DFP]) .LT. tol), &
          "scalar nodal spaceTime, first time slice")
END BLOCK

! Test 4: vector constant
BLOCK
  TYPE(FEVariable_) :: obj, ans

  obj = NodalVariable([1.0_DFP, 2.0_DFP], Vector, Constant)
  ans = QuadratureVariable(nrow=2, ncol=3, rank=Vector, vartype=Space)
  CALL GetInterpolation_(obj=obj, N=N, nns=2, nips=3, scale=1.0_DFP, &
                         addContribution=.FALSE., ans=ans)
  CALL OK(ALL(ABS(ans%val(1:6) - [1.0_DFP, 2.0_DFP, 1.0_DFP, 2.0_DFP, &
                                  1.0_DFP, 2.0_DFP]) .LT. tol), &
          "vector constant")
END BLOCK

! Test 5: matrix constant and matrix nodal space
BLOCK
  TYPE(FEVariable_) :: obj, ans
  REAL(DFP) :: nodal(2, 2, 2)

  obj = NodalVariable(RESHAPE([1.0_DFP, 2.0_DFP, 3.0_DFP, 4.0_DFP], &
                              [2, 2]), Matrix, Constant)
  ans = QuadratureVariable(dim1=2, dim2=2, dim3=3, rank=Matrix, &
                           vartype=Space)
  CALL GetInterpolation_(obj=obj, N=N, nns=2, nips=3, scale=1.0_DFP, &
                         addContribution=.FALSE., ans=ans)
  CALL OK(ALL(ABS(ans%val(1:12) - [1.0_DFP, 2.0_DFP, 3.0_DFP, 4.0_DFP, &
                                   1.0_DFP, 2.0_DFP, 3.0_DFP, 4.0_DFP, &
                                   1.0_DFP, 2.0_DFP, 3.0_DFP, 4.0_DFP]) &
              .LT. tol), "matrix constant")

  ! node 1: [1, 2, 3, 4], node 2: [5, 6, 7, 8]
  nodal = RESHAPE([1.0_DFP, 2.0_DFP, 3.0_DFP, 4.0_DFP, &
                   5.0_DFP, 6.0_DFP, 7.0_DFP, 8.0_DFP], [2, 2, 2])
  obj = NodalVariable(nodal, Matrix, Space)
  CALL GetInterpolation_(obj=obj, N=N, nns=2, nips=3, scale=1.0_DFP, &
                         addContribution=.FALSE., ans=ans)
  CALL OK(ALL(ABS(ans%val(5:8) - [3.0_DFP, 4.0_DFP, 5.0_DFP, 6.0_DFP]) &
              .LT. tol), "matrix nodal space at midpoint")
  CALL OK(ALL(ABS(ans%val(9:12) - [5.0_DFP, 6.0_DFP, 7.0_DFP, 8.0_DFP]) &
              .LT. tol), "matrix nodal space at node 2")
END BLOCK

END PROGRAM main
