!> author: Vikas Sharma, Ph. D.
! date: 2026-11-14
! summary: Interpolation of matrix FEVariables to quadrature points

PROGRAM main
USE BaseType, ONLY: FEVariable_
USE BaseType, ONLY: Constant => TypeFEVariableConstant
USE BaseType, ONLY: Matrix => TypeFEVariableMatrix
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
REAL(DFP), PARAMETER :: N(2, 3) = RESHAPE([1.0_DFP, 0.0_DFP, 0.5_DFP, &
                                           0.5_DFP, 0.0_DFP, 1.0_DFP], &
                                          [2, 3])
REAL(DFP), PARAMETER :: T(2) = [0.25_DFP, 0.75_DFP]
REAL(DFP), PARAMETER :: m1(2, 2) = RESHAPE([1.0_DFP, 2.0_DFP, 3.0_DFP, &
                                            4.0_DFP], [2, 2])
REAL(DFP), PARAMETER :: m2(2, 2) = RESHAPE([5.0_DFP, 6.0_DFP, 7.0_DFP, &
                                            8.0_DFP], [2, 2])
TYPE(FEVariable_) :: nodalSpace, constVar, nodalST
REAL(DFP) :: nodal(2, 2, 2)

nodal(:, :, 1) = m1
nodal(:, :, 2) = m2
nodalSpace = NodalVariable(nodal, Matrix, Space)
constVar = QuadratureVariable(m1, Matrix, Constant)
nodalST = NodalVariable(RESHAPE([nodal, nodal], [2, 2, 2, 2]), Matrix, &
                        SpaceTime)

! Test 1: matrices at all quadrature points
BLOCK
  REAL(DFP) :: ans(2, 2, 3)
  INTEGER(I4B) :: dim1, dim2, dim3

  CALL GetInterpolation_(obj=nodalSpace, rank=Matrix, vartype=Space, N=N, &
                         nns=2, nips=3, scale=1.0_DFP, &
                         addContribution=.FALSE., ans=ans, dim1=dim1, &
                         dim2=dim2, dim3=dim3)
  CALL OK(ALL([dim1, dim2, dim3] .EQ. [2, 2, 3]), "nodal space: dims")
  CALL OK(ALL(ABS(ans(:, :, 1) - m1) .LT. tol), "nodal space: point 1")
  CALL OK(ALL(ABS(ans(:, :, 2) - 0.5_DFP * (m1 + m2)) .LT. tol), &
          "nodal space: point 2")
  CALL OK(ALL(ABS(ans(:, :, 3) - m2) .LT. tol), "nodal space: point 3")

  CALL GetInterpolation_(obj=constVar, rank=Matrix, vartype=Constant, &
                         N=N, nns=2, nips=3, scale=2.0_DFP, &
                         addContribution=.FALSE., ans=ans, dim1=dim1, &
                         dim2=dim2, dim3=dim3)
  CALL OK(ALL(ABS(ans(:, :, 3) - 2.0_DFP * m1) .LT. tol), &
          "constant: scaled matrix")

  CALL GetInterpolation_(obj=nodalST, rank=Matrix, vartype=SpaceTime, &
                         N=N, nns=2, nips=3, T=T, nnt=2, scale=1.0_DFP, &
                         addContribution=.FALSE., ans=ans, dim1=dim1, &
                         dim2=dim2, dim3=dim3, timeIndx=1)
  CALL OK(ALL(ABS(ans(:, :, 2) - 0.5_DFP * (m1 + m2)) .LT. tol), &
          "nodal spaceTime: point 2")
END BLOCK

! Test 2: matrices written into a FEVariable
BLOCK
  TYPE(FEVariable_) :: ans

  ans = QuadratureVariable(dim1=2, dim2=2, dim3=3, rank=Matrix, &
                           vartype=Space)
  CALL GetInterpolation_(obj=constVar, rank=Matrix, vartype=Constant, &
                         N=N, nns=2, nips=3, scale=1.0_DFP, &
                         addContribution=.FALSE., timeIndx=1, ans=ans)
  CALL OK(ALL(ABS(ans%val(9:12) - RESHAPE(m1, [4])) .LT. tol), &
          "FEVariable: constant at the last point")

  CALL GetInterpolation_(obj=nodalSpace, rank=Matrix, vartype=Space, N=N, &
                         nns=2, nips=3, scale=1.0_DFP, &
                         addContribution=.FALSE., timeIndx=1, ans=ans)
  CALL OK(ALL(ABS(ans%val(5:8) - RESHAPE(0.5_DFP * (m1 + m2), [4])) &
              .LT. tol), "FEVariable: nodal space at point 2")
END BLOCK

! Test 3: matrix at one quadrature point
BLOCK
  REAL(DFP) :: ans(2, 2)
  INTEGER(I4B) :: nrow, ncol

  CALL GetInterpolation_(obj=nodalSpace, rank=Matrix, vartype=Space, N=N, &
                         nns=2, spaceIndx=3, timeIndx=1, scale=1.0_DFP, &
                         addContribution=.FALSE., ans=ans, nrow=nrow, &
                         ncol=ncol)
  CALL OK(nrow .EQ. 2 .AND. ncol .EQ. 2, "point: nrow, ncol")
  CALL OK(ALL(ABS(ans - m2) .LT. tol), "point: nodal space")

  CALL GetInterpolation_(obj=constVar, rank=Matrix, N=N, nns=2, &
                         spaceIndx=1, timeIndx=1, T=T, nnt=2, &
                         scale=1.0_DFP, addContribution=.FALSE., ans=ans, &
                         nrow=nrow, ncol=ncol)
  CALL OK(ALL(ABS(ans - m1) .LT. tol), "point: any vartype, constant")
END BLOCK

END PROGRAM main
