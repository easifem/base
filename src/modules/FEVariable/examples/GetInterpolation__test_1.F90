!> author: Vikas Sharma, Ph. D.
! date: 2026-11-14
! summary: Interpolation of scalar FEVariables to quadrature points

PROGRAM main
USE BaseType, ONLY: FEVariable_
USE BaseType, ONLY: Constant => TypeFEVariableConstant
USE BaseType, ONLY: Scalar => TypeFEVariableScalar
USE BaseType, ONLY: Space => TypeFEVariableSpace
USE BaseType, ONLY: SpaceTime => TypeFEVariableSpaceTime
USE FEVariable_Method, ONLY: GetInterpolation_
USE FEVariable_Method, ONLY: NodalVariable
USE FEVariable_Method, ONLY: QuadratureVariable
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! 2 nodes, 3 points: N(:, ip) = [1 - x, x] with x = 0, 1/2, 1
REAL(DFP), PARAMETER :: N(2, 3) = RESHAPE([1.0_DFP, 0.0_DFP, 0.5_DFP, &
                                           0.5_DFP, 0.0_DFP, 1.0_DFP], &
                                          [2, 3])
! time shape functions at one time point
REAL(DFP), PARAMETER :: T(2) = [0.25_DFP, 0.75_DFP]
TYPE(FEVariable_) :: nodalSpace, quadSpace, constVar, nodalST, quadST

nodalSpace = NodalVariable([2.0_DFP, 4.0_DFP], Scalar, Space)
quadSpace = QuadratureVariable([5.0_DFP, 6.0_DFP, 7.0_DFP], Scalar, Space)
constVar = QuadratureVariable(3.0_DFP, Scalar, Constant)
! nodal values at node i and time node a: [2 4; 6 8]
nodalST = NodalVariable(RESHAPE([2.0_DFP, 4.0_DFP, 6.0_DFP, 8.0_DFP], &
                                [2, 2]), Scalar, SpaceTime)
quadST = QuadratureVariable(RESHAPE([1.0_DFP, 2.0_DFP, 3.0_DFP, 4.0_DFP, &
                                     5.0_DFP, 6.0_DFP], [3, 2]), Scalar, &
                            SpaceTime)

! Test 1: values at all quadrature points in an array
BLOCK
  REAL(DFP) :: ans(5)
  INTEGER(I4B) :: tsize

  CALL GetInterpolation_(obj=nodalSpace, rank=Scalar, vartype=Space, N=N, &
                         nns=2, nips=3, scale=1.0_DFP, &
                         addContribution=.FALSE., ans=ans, tsize=tsize)
  CALL OK(tsize .EQ. 3, "nodal space: tsize = nips")
  CALL OK(ALL(ABS(ans(1:3) - [2.0_DFP, 3.0_DFP, 4.0_DFP]) .LT. tol), &
          "nodal space: N . val")

  CALL GetInterpolation_(obj=nodalSpace, rank=Scalar, vartype=Space, N=N, &
                         nns=2, nips=3, scale=2.0_DFP, &
                         addContribution=.TRUE., ans=ans, tsize=tsize)
  CALL OK(ALL(ABS(ans(1:3) - [6.0_DFP, 9.0_DFP, 12.0_DFP]) .LT. tol), &
          "nodal space: scale and addContribution")

  CALL GetInterpolation_(obj=quadSpace, rank=Scalar, vartype=Space, N=N, &
                         nns=2, nips=3, scale=1.0_DFP, &
                         addContribution=.FALSE., ans=ans, tsize=tsize)
  CALL OK(ALL(ABS(ans(1:3) - [5.0_DFP, 6.0_DFP, 7.0_DFP]) .LT. tol), &
          "quadrature space: values are copied")

  CALL GetInterpolation_(obj=constVar, rank=Scalar, vartype=Constant, N=N, &
                         nns=2, nips=3, scale=1.0_DFP, &
                         addContribution=.FALSE., ans=ans, tsize=tsize)
  CALL OK(ALL(ABS(ans(1:3) - 3.0_DFP) .LT. tol), "constant: same value")

  ! time interpolation gives nodal values [5, 7], then [5, 6, 7]
  CALL GetInterpolation_(obj=nodalST, rank=Scalar, vartype=SpaceTime, &
                         N=N, nns=2, nips=3, T=T, nnt=2, scale=1.0_DFP, &
                         addContribution=.FALSE., ans=ans, tsize=tsize, &
                         timeIndx=1)
  CALL OK(ALL(ABS(ans(1:3) - [5.0_DFP, 6.0_DFP, 7.0_DFP]) .LT. tol), &
          "nodal spaceTime: T and N")

  CALL GetInterpolation_(obj=quadST, rank=Scalar, vartype=SpaceTime, &
                         N=N, nns=2, nips=3, T=T, nnt=2, scale=1.0_DFP, &
                         addContribution=.FALSE., ans=ans, tsize=tsize, &
                         timeIndx=2)
  CALL OK(ALL(ABS(ans(1:3) - [4.0_DFP, 5.0_DFP, 6.0_DFP]) .LT. tol), &
          "quadrature spaceTime: values at timeIndx")
END BLOCK

! Test 2: values written into a space-time FEVariable at timeIndx
BLOCK
  TYPE(FEVariable_) :: ans

  ans = QuadratureVariable(nrow=3, ncol=2, rank=Scalar, vartype=SpaceTime)
  CALL GetInterpolation_(obj=nodalSpace, rank=Scalar, vartype=Space, N=N, &
                         nns=2, nips=3, scale=1.0_DFP, &
                         addContribution=.FALSE., timeIndx=1, ans=ans)
  CALL GetInterpolation_(obj=constVar, rank=Scalar, vartype=Constant, N=N, &
                         nns=2, nips=3, scale=1.0_DFP, &
                         addContribution=.FALSE., timeIndx=2, ans=ans)
  CALL OK(ALL(ABS(ans%val(1:3) - [2.0_DFP, 3.0_DFP, 4.0_DFP]) .LT. tol), &
          "FEVariable: nodal space at timeIndx 1")
  CALL OK(ALL(ABS(ans%val(4:6) - 3.0_DFP) .LT. tol), &
          "FEVariable: constant at timeIndx 2")
  CALL OK(ans%len .EQ. 6 .AND. ans%s(1) .EQ. 3, &
          "FEVariable: len and shape of ans are kept")

  CALL GetInterpolation_(obj=nodalST, rank=Scalar, vartype=SpaceTime, &
                         N=N, nns=2, nips=3, T=T, nnt=2, scale=1.0_DFP, &
                         addContribution=.FALSE., timeIndx=2, ans=ans)
  CALL OK(ALL(ABS(ans%val(4:6) - [5.0_DFP, 6.0_DFP, 7.0_DFP]) .LT. tol), &
          "FEVariable: nodal spaceTime at timeIndx 2")
END BLOCK

! Test 3: value at one quadrature point
BLOCK
  REAL(DFP) :: ans

  CALL GetInterpolation_(obj=nodalSpace, rank=Scalar, vartype=Space, N=N, &
                         nns=2, spaceIndx=2, timeIndx=1, scale=1.0_DFP, &
                         addContribution=.FALSE., ans=ans)
  CALL OK(ABS(ans - 3.0_DFP) .LT. tol, "point: nodal space")

  CALL GetInterpolation_(obj=constVar, rank=Scalar, vartype=Constant, &
                         N=N, nns=2, spaceIndx=2, timeIndx=1, &
                         scale=1.0_DFP, addContribution=.FALSE., ans=ans)
  CALL OK(ABS(ans - 3.0_DFP) .LT. tol, "point: constant")

  CALL GetInterpolation_(obj=nodalST, rank=Scalar, vartype=SpaceTime, &
                         N=N, nns=2, spaceIndx=3, timeIndx=1, T=T, nnt=2, &
                         scale=1.0_DFP, addContribution=.FALSE., ans=ans)
  CALL OK(ABS(ans - 7.0_DFP) .LT. tol, "point: nodal spaceTime")

  ! variable type is taken from obj
  CALL GetInterpolation_(obj=quadST, rank=Scalar, N=N, nns=2, &
                         spaceIndx=1, timeIndx=2, T=T, nnt=2, &
                         scale=1.0_DFP, addContribution=.FALSE., ans=ans)
  CALL OK(ABS(ans - 4.0_DFP) .LT. tol, "point: any vartype, spaceTime")
END BLOCK

END PROGRAM main
