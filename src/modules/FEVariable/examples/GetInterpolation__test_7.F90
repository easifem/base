!> author: Vikas Sharma, Ph. D.
! date: 2026-11-20
! summary: Matrix interpolation: quadrature, space-time and dispatch

PROGRAM main
USE BaseType, ONLY: FEVariable_
USE BaseType, ONLY: Constant => TypeFEVariableConstant
USE BaseType, ONLY: Matrix => TypeFEVariableMatrix
USE BaseType, ONLY: Space => TypeFEVariableSpace
USE BaseType, ONLY: SpaceTime => TypeFEVariableSpaceTime
USE BaseType, ONLY: Time => TypeFEVariableTime
USE FEVariable_Method, ONLY: GetInterpolation_
USE FEVariable_Method, ONLY: NodalVariable
USE FEVariable_Method, ONLY: QuadratureVariable
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! N(I, ips): 2 nodes, 3 quadrature points
REAL(DFP), PARAMETER :: N(2, 3) = RESHAPE([1.0_DFP, 0.0_DFP, 0.5_DFP, &
                                           0.5_DFP, 0.0_DFP, 1.0_DFP], &
                                          [2, 3])
! T(a): 2 time nodes; time node 2 holds twice the values of node 1,
! so the space-time result is (0.25 + 2 * 0.75) = 1.75 times the
! space result
REAL(DFP), PARAMETER :: T(2) = [0.25_DFP, 0.75_DFP]
REAL(DFP), PARAMETER :: tfac = 1.75_DFP
REAL(DFP), PARAMETER :: m1(2, 2) = RESHAPE([1.0_DFP, 2.0_DFP, 3.0_DFP, &
                                            4.0_DFP], [2, 2])
REAL(DFP), PARAMETER :: m2(2, 2) = RESHAPE([5.0_DFP, 6.0_DFP, 7.0_DFP, &
                                            8.0_DFP], [2, 2])
REAL(DFP), PARAMETER :: m3(2, 2) = RESHAPE([9.0_DFP, 10.0_DFP, &
                                            11.0_DFP, 12.0_DFP], [2, 2])
TYPE(FEVariable_) :: constVar, nodalSpace, nodalST, quadSpace, quadST
REAL(DFP) :: nodal(2, 2, 2), quad(2, 2, 3)

nodal(:, :, 1) = m1
nodal(:, :, 2) = m2
quad(:, :, 1) = m1
quad(:, :, 2) = m2
quad(:, :, 3) = m3

constVar = QuadratureVariable(m1, Matrix, Constant)
nodalSpace = NodalVariable(nodal, Matrix, Space)
nodalST = NodalVariable(RESHAPE([nodal, 2.0_DFP * nodal], [2, 2, 2, 2]), &
                        Matrix, SpaceTime)
quadSpace = QuadratureVariable(quad, Matrix, Space)
quadST = QuadratureVariable(RESHAPE([quad, 2.0_DFP * quad], &
                                    [2, 2, 3, 2]), Matrix, SpaceTime)

! Test 1: all points, quadrature and space-time inputs
BLOCK
  REAL(DFP) :: ans(2, 2, 3)
  INTEGER(I4B) :: dim1, dim2, dim3

  CALL GetInterpolation_(obj=quadSpace, rank=Matrix, vartype=Space, N=N, &
                         nns=2, nips=3, scale=3.0_DFP, &
                         addContribution=.FALSE., ans=ans, dim1=dim1, &
                         dim2=dim2, dim3=dim3)
  CALL OK(ALL([dim1, dim2, dim3] .EQ. [2, 2, 3]), "quad space: dims")
  CALL OK(ALL(ABS(ans(:, :, 2) - 3.0_DFP * m2) .LT. tol), &
          "quad space: scaled point 2")

  CALL GetInterpolation_(obj=nodalST, rank=Matrix, vartype=SpaceTime, &
                         N=N, nns=2, nips=3, T=T, nnt=2, scale=1.0_DFP, &
                         addContribution=.FALSE., ans=ans, dim1=dim1, &
                         dim2=dim2, dim3=dim3, timeIndx=1)
  CALL OK(ALL(ABS(ans(:, :, 2) - tfac * 0.5_DFP * (m1 + m2)) .LT. tol), &
          "nodal spaceTime: T weights")

  CALL GetInterpolation_(obj=quadST, rank=Matrix, vartype=SpaceTime, &
                         N=N, nns=2, nips=3, T=T, nnt=2, scale=1.0_DFP, &
                         addContribution=.FALSE., ans=ans, dim1=dim1, &
                         dim2=dim2, dim3=dim3, timeIndx=2)
  CALL OK(ALL(ABS(ans(:, :, 3) - 2.0_DFP * m3) .LT. tol), &
          "quad spaceTime: time point 2")
END BLOCK

! Test 2: addContribution adds to ans
BLOCK
  REAL(DFP) :: ans(2, 2, 3)
  INTEGER(I4B) :: dim1, dim2, dim3

  CALL GetInterpolation_(obj=constVar, rank=Matrix, vartype=Constant, &
                         N=N, nns=2, nips=3, scale=1.0_DFP, &
                         addContribution=.FALSE., ans=ans, dim1=dim1, &
                         dim2=dim2, dim3=dim3)
  CALL GetInterpolation_(obj=constVar, rank=Matrix, vartype=Constant, &
                         N=N, nns=2, nips=3, scale=1.0_DFP, &
                         addContribution=.TRUE., ans=ans, dim1=dim1, &
                         dim2=dim2, dim3=dim3)
  CALL OK(ALL(ABS(ans(:, :, 1) - 2.0_DFP * m1) .LT. tol), &
          "addContribution: constant added twice")
END BLOCK

! Test 3: write into a quadrature FEVariable
BLOCK
  TYPE(FEVariable_) :: ans
  REAL(DFP) :: zero(2, 2, 3, 2)

  ans = QuadratureVariable(dim1=2, dim2=2, dim3=3, rank=Matrix, &
                           vartype=Space)
  CALL GetInterpolation_(obj=quadSpace, rank=Matrix, vartype=Space, N=N, &
                         nns=2, nips=3, scale=1.0_DFP, &
                         addContribution=.FALSE., timeIndx=1, ans=ans)
  CALL OK(ALL(ABS(ans%val(5:8) - RESHAPE(m2, [4])) .LT. tol), &
          "FEVariable: quad space at point 2")

  ! values of time point 2 start after one time slice of 12 values
  zero = 0.0_DFP
  ans = QuadratureVariable(zero, Matrix, SpaceTime)
  CALL GetInterpolation_(obj=nodalST, rank=Matrix, vartype=SpaceTime, &
                         N=N, nns=2, nips=3, T=T, nnt=2, scale=1.0_DFP, &
                         addContribution=.FALSE., timeIndx=2, ans=ans)
  CALL OK(ALL(ABS(ans%val(17:20) &
                  - RESHAPE(tfac * 0.5_DFP * (m1 + m2), [4])) .LT. tol), &
          "FEVariable: nodal spaceTime at time 2, point 2")
  CALL OK(ALL(ABS(ans%val(1:12)) .LT. tol), &
          "FEVariable: time slice 1 not changed")

  CALL GetInterpolation_(obj=quadST, rank=Matrix, vartype=SpaceTime, &
                         N=N, nns=2, nips=3, T=T, nnt=2, scale=1.0_DFP, &
                         addContribution=.FALSE., timeIndx=2, ans=ans)
  CALL OK(ALL(ABS(ans%val(21:24) - RESHAPE(2.0_DFP * m3, [4])) &
              .LT. tol), "FEVariable: quad spaceTime at time 2, point 3")
END BLOCK

! Test 4: one point, each variable type
BLOCK
  REAL(DFP) :: ans(2, 2)
  INTEGER(I4B) :: nrow, ncol

  CALL GetInterpolation_(obj=constVar, rank=Matrix, vartype=Constant, &
                         N=N, nns=2, spaceIndx=2, timeIndx=1, &
                         scale=2.0_DFP, addContribution=.FALSE., ans=ans, &
                         nrow=nrow, ncol=ncol)
  CALL OK(nrow .EQ. 2 .AND. ncol .EQ. 2, "point constant: nrow, ncol")
  CALL OK(ALL(ABS(ans - 2.0_DFP * m1) .LT. tol), "point: constant")

  CALL GetInterpolation_(obj=quadSpace, rank=Matrix, vartype=Space, N=N, &
                         nns=2, spaceIndx=2, timeIndx=1, scale=1.0_DFP, &
                         addContribution=.FALSE., ans=ans, nrow=nrow, &
                         ncol=ncol)
  CALL OK(ALL(ABS(ans - m2) .LT. tol), "point: quad space")

  CALL GetInterpolation_(obj=nodalST, rank=Matrix, vartype=SpaceTime, &
                         N=N, nns=2, spaceIndx=2, timeIndx=1, T=T, &
                         nnt=2, scale=1.0_DFP, addContribution=.FALSE., &
                         ans=ans, nrow=nrow, ncol=ncol)
  CALL OK(ALL(ABS(ans - tfac * 0.5_DFP * (m1 + m2)) .LT. tol), &
          "point: nodal spaceTime")

  CALL GetInterpolation_(obj=quadST, rank=Matrix, vartype=SpaceTime, &
                         N=N, nns=2, spaceIndx=3, timeIndx=2, T=T, &
                         nnt=2, scale=1.0_DFP, addContribution=.FALSE., &
                         ans=ans, nrow=nrow, ncol=ncol)
  CALL OK(ALL(ABS(ans - 2.0_DFP * m3) .LT. tol), "point: quad spaceTime")
END BLOCK

! Test 5: one point, variable type taken from obj
BLOCK
  REAL(DFP) :: ans(2, 2)
  INTEGER(I4B) :: nrow, ncol
  TYPE(FEVariable_) :: timeVar

  CALL GetInterpolation_(obj=nodalSpace, rank=Matrix, N=N, nns=2, &
                         spaceIndx=3, timeIndx=1, T=T, nnt=2, &
                         scale=1.0_DFP, addContribution=.FALSE., ans=ans, &
                         nrow=nrow, ncol=ncol)
  CALL OK(ALL(ABS(ans - m2) .LT. tol), "any vartype: space")

  CALL GetInterpolation_(obj=nodalST, rank=Matrix, N=N, nns=2, &
                         spaceIndx=2, timeIndx=1, T=T, nnt=2, &
                         scale=1.0_DFP, addContribution=.FALSE., ans=ans, &
                         nrow=nrow, ncol=ncol)
  CALL OK(ALL(ABS(ans - tfac * 0.5_DFP * (m1 + m2)) .LT. tol), &
          "any vartype: spaceTime")

  CALL GetInterpolation_(obj=nodalST, rank=Matrix, N=N, nns=2, &
                         spaceIndx=2, timeIndx=1, T=T, nnt=2, &
                         scale=1.0_DFP, addContribution=.TRUE., ans=ans, &
                         nrow=nrow, ncol=ncol)
  CALL OK(ALL(ABS(ans - 2.0_DFP * tfac * 0.5_DFP * (m1 + m2)) .LT. tol), &
          "any vartype: addContribution")

  ! a time variable is not supported: ans is not changed
  timeVar = NodalVariable(nodal, Matrix, Time)
  ans = -1.0_DFP
  CALL GetInterpolation_(obj=timeVar, rank=Matrix, N=N, nns=2, &
                         spaceIndx=1, timeIndx=1, T=T, nnt=2, &
                         scale=1.0_DFP, addContribution=.FALSE., ans=ans, &
                         nrow=nrow, ncol=ncol)
  CALL OK(ALL(ABS(ans + 1.0_DFP) .LT. tol), "any vartype: time ignored")
END BLOCK

END PROGRAM main
