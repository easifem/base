!> author: Vikas Sharma, Ph. D.
! date: 2026-11-20
! summary: Vector interpolation: scale, add, quadrature and space-time

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
! N(I, ips): 2 nodes, 3 quadrature points
REAL(DFP), PARAMETER :: N(2, 3) = RESHAPE([1.0_DFP, 0.0_DFP, 0.5_DFP, &
                                           0.5_DFP, 0.0_DFP, 1.0_DFP], &
                                          [2, 3])
REAL(DFP), PARAMETER :: T(2) = [0.25_DFP, 0.75_DFP]
! nodal vectors [1, 2] and [3, 4] give [1, 2], [2, 3], [3, 4]
REAL(DFP), PARAMETER :: ref(2, 3) = RESHAPE([1.0_DFP, 2.0_DFP, 2.0_DFP, &
                                             3.0_DFP, 3.0_DFP, 4.0_DFP], &
                                            [2, 3])
TYPE(FEVariable_) :: constVar, nodalSpace, quadSpace, quadST, nodalST
REAL(DFP) :: stVal(2, 3, 2), nodalVal(2, 2, 2)

constVar = QuadratureVariable([1.0_DFP, 2.0_DFP], Vector, Constant)
nodalSpace = NodalVariable(RESHAPE([1.0_DFP, 2.0_DFP, 3.0_DFP, 4.0_DFP], &
                                   [2, 2]), Vector, Space)
quadSpace = QuadratureVariable(ref, Vector, Space)

! quadrature space-time: ref at time point 1, 10 * ref at time point 2
stVal(:, :, 1) = ref
stVal(:, :, 2) = 10.0_DFP * ref
quadST = QuadratureVariable(stVal, Vector, SpaceTime)

! nodal space-time: time node 2 is time node 1 plus 4
! with T = [0.25, 0.75] the result is ref + 3
nodalVal(:, :, 1) = RESHAPE([1.0_DFP, 2.0_DFP, 3.0_DFP, 4.0_DFP], [2, 2])
nodalVal(:, :, 2) = nodalVal(:, :, 1) + 4.0_DFP
nodalST = NodalVariable(nodalVal, Vector, SpaceTime)

! Test 1: constant at one point, scale and addContribution
BLOCK
  REAL(DFP) :: ans(3)
  INTEGER(I4B) :: tsize

  ans = 5.0_DFP
  CALL GetInterpolation_(obj=constVar, rank=Vector, vartype=Constant, N=N, &
                         nns=2, spaceIndx=1, timeIndx=1, scale=2.0_DFP, &
                         addContribution=.TRUE., ans=ans, tsize=tsize)
  CALL OK(tsize .EQ. 2, "constant point: tsize = 2")
  CALL OK(ALL(ABS(ans - [7.0_DFP, 9.0_DFP, 5.0_DFP]) .LT. tol), &
          "constant point: adds 2 * [1, 2], ans(3) not changed")

  CALL GetInterpolation_(obj=constVar, rank=Vector, vartype=Constant, N=N, &
                         nns=2, spaceIndx=1, timeIndx=1, scale=2.0_DFP, &
                         addContribution=.FALSE., ans=ans, tsize=tsize)
  CALL OK(ALL(ABS(ans(1:2) - [2.0_DFP, 4.0_DFP]) .LT. tol), &
          "constant point: replaces old values")
END BLOCK

! Test 2: constant at all points, scale and addContribution
BLOCK
  REAL(DFP) :: ans(3, 4)
  INTEGER(I4B) :: nrow, ncol, ii
  LOGICAL :: isok

  ans = 1.0_DFP
  CALL GetInterpolation_(obj=constVar, rank=Vector, vartype=Constant, N=N, &
                         nns=2, nips=3, scale=2.0_DFP, &
                         addContribution=.TRUE., ans=ans, nrow=nrow, &
                         ncol=ncol)
  CALL OK(nrow .EQ. 2 .AND. ncol .EQ. 3, "constant array: nrow, ncol")
  isok = .TRUE.
  DO ii = 1, 3
    isok = isok .AND. ALL(ABS(ans(1:2, ii) - [3.0_DFP, 5.0_DFP]) .LT. tol)
  END DO
  CALL OK(isok, "constant array: adds 2 * [1, 2] at every point")
  CALL OK(ALL(ABS(ans(3, :) - 1.0_DFP) .LT. tol) .AND. &
          ALL(ABS(ans(:, 4) - 1.0_DFP) .LT. tol), &
          "constant array: entries outside nrow, ncol not changed")
END BLOCK

! Test 3: quadrature space into a FEVariable, and at one point
BLOCK
  TYPE(FEVariable_) :: ansFE
  REAL(DFP) :: ans(2)
  INTEGER(I4B) :: tsize

  ansFE = QuadratureVariable(nrow=2, ncol=3, rank=Vector, vartype=Space)
  ansFE%val(1:6) = 99.0_DFP
  CALL GetInterpolation_(obj=quadSpace, rank=Vector, vartype=Space, N=N, &
                         nns=2, nips=3, scale=0.5_DFP, &
                         addContribution=.FALSE., timeIndx=1, ans=ansFE)
  CALL OK(ALL(ABS(ansFE%val(1:6) - 0.5_DFP * RESHAPE(ref, [6])) .LT. tol), &
          "quadrature space FEVariable: 0.5 * values")

  CALL GetInterpolation_(obj=quadSpace, rank=Vector, vartype=Space, N=N, &
                         nns=2, spaceIndx=3, timeIndx=1, scale=1.0_DFP, &
                         addContribution=.FALSE., ans=ans, tsize=tsize)
  CALL OK(tsize .EQ. 2, "quadrature space point: tsize = 2")
  CALL OK(ALL(ABS(ans - [3.0_DFP, 4.0_DFP]) .LT. tol), &
          "quadrature space point: values at point 3")
END BLOCK

! Test 4: quadrature space-time at time point 2
BLOCK
  REAL(DFP) :: ans2(2, 3), ans1(2)
  TYPE(FEVariable_) :: ansFE
  INTEGER(I4B) :: nrow, ncol, tsize

  CALL GetInterpolation_(obj=quadST, rank=Vector, vartype=SpaceTime, N=N, &
                         nns=2, nips=3, T=T, nnt=2, scale=1.0_DFP, &
                         addContribution=.FALSE., ans=ans2, nrow=nrow, &
                         ncol=ncol, timeIndx=2)
  CALL OK(nrow .EQ. 2 .AND. ncol .EQ. 3, "quadrature spaceTime: nrow, ncol")
  CALL OK(ALL(ABS(ans2 - 10.0_DFP * ref) .LT. tol), &
          "quadrature spaceTime array: values of time point 2")

  CALL GetInterpolation_(obj=quadST, rank=Vector, vartype=SpaceTime, N=N, &
                         nns=2, spaceIndx=2, timeIndx=2, T=T, nnt=2, &
                         scale=1.0_DFP, addContribution=.FALSE., ans=ans1, &
                         tsize=tsize)
  CALL OK(ALL(ABS(ans1 - [20.0_DFP, 30.0_DFP]) .LT. tol), &
          "quadrature spaceTime point: point 2, time point 2")

  ansFE = QuadratureVariable(dim1=2, dim2=3, dim3=2, rank=Vector, &
                             vartype=SpaceTime)
  CALL GetInterpolation_(obj=quadST, rank=Vector, vartype=SpaceTime, N=N, &
                         nns=2, nips=3, T=T, nnt=2, scale=1.0_DFP, &
                         addContribution=.FALSE., timeIndx=2, ans=ansFE)
  CALL OK(ALL(ABS(ansFE%val(7:12) - 10.0_DFP * RESHAPE(ref, [6])) &
              .LT. tol), "quadrature spaceTime FEVariable: time slice 2")
  CALL OK(ALL(ABS(ansFE%val(1:6)) .LT. tol), &
          "quadrature spaceTime FEVariable: time slice 1 not changed")
END BLOCK

! Test 5: nodal space-time with different values at the time nodes
BLOCK
  REAL(DFP) :: ans2(2, 3), ans1(2)
  TYPE(FEVariable_) :: ansFE
  INTEGER(I4B) :: nrow, ncol, tsize

  CALL GetInterpolation_(obj=nodalST, rank=Vector, vartype=SpaceTime, &
                         N=N, nns=2, nips=3, T=T, nnt=2, scale=1.0_DFP, &
                         addContribution=.FALSE., ans=ans2, nrow=nrow, &
                         ncol=ncol, timeIndx=1)
  CALL OK(ALL(ABS(ans2 - (ref + 3.0_DFP)) .LT. tol), &
          "nodal spaceTime array: SUM_a T(a) N obj")

  CALL GetInterpolation_(obj=nodalST, rank=Vector, vartype=SpaceTime, &
                         N=N, nns=2, spaceIndx=2, timeIndx=1, T=T, nnt=2, &
                         scale=1.0_DFP, addContribution=.FALSE., ans=ans1, &
                         tsize=tsize)
  CALL OK(tsize .EQ. 2, "nodal spaceTime point: tsize = 2")
  CALL OK(ALL(ABS(ans1 - [5.0_DFP, 6.0_DFP]) .LT. tol), &
          "nodal spaceTime point: point 2")

  ansFE = QuadratureVariable(nrow=2, ncol=3, rank=Vector, vartype=Space)
  CALL GetInterpolation_(obj=nodalST, rank=Vector, vartype=SpaceTime, &
                         N=N, nns=2, nips=3, T=T, nnt=2, scale=2.0_DFP, &
                         addContribution=.FALSE., timeIndx=1, ans=ansFE)
  CALL OK(ALL(ABS(ansFE%val(1:6) - 2.0_DFP * RESHAPE(ref + 3.0_DFP, [6])) &
              .LT. tol), "nodal spaceTime FEVariable: 2 * values")
END BLOCK

! Test 6: one point, variable type taken from obj
BLOCK
  REAL(DFP) :: ans(2)
  INTEGER(I4B) :: tsize

  CALL GetInterpolation_(obj=constVar, rank=Vector, N=N, nns=2, &
                         spaceIndx=1, timeIndx=1, T=T, nnt=2, &
                         scale=1.0_DFP, addContribution=.FALSE., ans=ans, &
                         tsize=tsize)
  CALL OK(tsize .EQ. 2 .AND. ALL(ABS(ans - [1.0_DFP, 2.0_DFP]) .LT. tol), &
          "any vartype: constant")

  CALL GetInterpolation_(obj=nodalSpace, rank=Vector, N=N, nns=2, &
                         spaceIndx=3, timeIndx=1, T=T, nnt=2, &
                         scale=1.0_DFP, addContribution=.FALSE., ans=ans, &
                         tsize=tsize)
  CALL OK(ALL(ABS(ans - [3.0_DFP, 4.0_DFP]) .LT. tol), &
          "any vartype: nodal space")

  CALL GetInterpolation_(obj=quadST, rank=Vector, N=N, nns=2, &
                         spaceIndx=1, timeIndx=2, T=T, nnt=2, &
                         scale=1.0_DFP, addContribution=.FALSE., ans=ans, &
                         tsize=tsize)
  CALL OK(ALL(ABS(ans - [10.0_DFP, 20.0_DFP]) .LT. tol), &
          "any vartype: quadrature spaceTime, time point 2")
END BLOCK

END PROGRAM main
