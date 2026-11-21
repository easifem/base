!> author: Vikas Sharma, Ph. D.
! date: 2026-11-14
! summary: Gradient with respect to x without allocation

PROGRAM main
USE BaseType, ONLY: ElemShapeData_
USE BaseType, ONLY: FEVariable_
USE BaseType, ONLY: Matrix => TypeFEVariableMatrix
USE BaseType, ONLY: qpopt => TypeQuadratureOpt
USE BaseType, ONLY: QuadraturePoint_
USE BaseType, ONLY: Scalar => TypeFEVariableScalar
USE BaseType, ONLY: Space => TypeFEVariableSpace
USE BaseType, ONLY: SpaceTime => TypeFEVariableSpaceTime
USE BaseType, ONLY: Vector => TypeFEVariableVector
USE ElemShapeData_Method, ONLY: GetSpatialGradient_
USE ElemShapeData_Method, ONLY: LagrangeElemShapeData
USE ElemShapeData_Method, ONLY: Set
USE FEVariable_Method, ONLY: NodalVariable
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: Quadrangle4
USE QuadraturePoint_Method, ONLY: QP_Initiate => Initiate
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
REAL(DFP), PARAMETER :: quadCoord(2, 4) = RESHAPE([-1.0_DFP, -1.0_DFP, &
                        1.0_DFP, -1.0_DFP, 1.0_DFP, 1.0_DFP, -1.0_DFP, &
                                                   1.0_DFP], [2, 4])
! physical cell: rectangle [0, 2] x [0, 1], so x = 1 + xi, y = (1 + eta) / 2
REAL(DFP), PARAMETER :: xcell(2, 4) = RESHAPE([0.0_DFP, 0.0_DFP, 2.0_DFP, &
                                               0.0_DFP, 2.0_DFP, 1.0_DFP, &
                                               0.0_DFP, 1.0_DFP], [2, 4])
! nodal values of u = x + 3 y and w = 2 x - y
REAL(DFP), PARAMETER :: u(4) = [0.0_DFP, 2.0_DFP, 5.0_DFP, 3.0_DFP]
REAL(DFP), PARAMETER :: w(4) = [0.0_DFP, 4.0_DFP, 3.0_DFP, -1.0_DFP]
TYPE(ElemShapeData_) :: cell
TYPE(QuadraturePoint_) :: quad

CALL QP_Initiate(obj=quad, elemType=Quadrangle4, domainName="BIUNIT", &
                 nips=[2], quadratureType=qpopt%GaussLegendre)
CALL LagrangeElemShapeData(obj=cell, quad=quad, nsd=2, xidim=2, &
                           elemType=Quadrangle4, refelemCoord=quadCoord, &
                           domainName="BIUNIT", order=1)
CALL Set(obj=cell, val=xcell, N=cell%N, dNdXi=cell%dNdXi)

! Test 1: nodal values into arrays larger than the result
BLOCK
  REAL(DFP) :: g2(3, 5), g3(3, 3, 5), v(2, 4)
  INTEGER(I4B) :: nrow, ncol, dim1, dim2, dim3

  CALL GetSpatialGradient_(obj=cell, val=u, ans=g2, nrow=nrow, ncol=ncol)
  CALL OK(nrow .EQ. 2 .AND. ncol .EQ. 4, "scalar: nrow = nsd, ncol = nips")
  CALL OK(ALL(ABS(g2(2, 1:4) - 3.0_DFP) .LT. tol), "scalar: du/dy = 3")

  v(1, :) = u
  v(2, :) = w
  CALL GetSpatialGradient_(obj=cell, val=v, ans=g3, dim1=dim1, dim2=dim2, &
                           dim3=dim3)
  CALL OK(ALL([dim1, dim2, dim3] .EQ. [2, 2, 4]), "vector: dims")
  CALL OK(ALL(ABS(g3(2, 1, 1:4) - 2.0_DFP) .LT. tol), "vector: dw/dx = 2")
END BLOCK

! Test 2: FEVariable input with work arrays larger than needed
BLOCK
  REAL(DFP) :: g2(2, 4), g3(2, 2, 4), g4(2, 2, 2, 4), v(2, 4), m(2, 2, 4)
  REAL(DFP) :: tempVec(6), tempMat2(3, 6), tempMat3(3, 3, 6)
  INTEGER(I4B) :: nrow, ncol, dim1, dim2, dim3, dim4

  CALL GetSpatialGradient_(obj=cell, val=NodalVariable(u, Scalar, Space), &
                           valRank=Scalar, ans=g2, nrow=nrow, ncol=ncol, &
                           tempVec=tempVec)
  CALL OK(ALL(ABS(g2(1, :) - 1.0_DFP) .LT. tol), "FEVariable: scalar")

  v(1, :) = u
  v(2, :) = w
  CALL GetSpatialGradient_(obj=cell, val=NodalVariable(v, Vector, Space), &
                           valRank=Vector, ans=g3, dim1=dim1, dim2=dim2, &
                           dim3=dim3, tempMat2=tempMat2)
  CALL OK(ALL(ABS(g3(2, 2, :) + 1.0_DFP) .LT. tol), "FEVariable: vector")

  m(1, 1, :) = u
  m(2, 1, :) = w
  m(1, 2, :) = 0.0_DFP
  m(2, 2, :) = 1.0_DFP
  CALL GetSpatialGradient_(obj=cell, val=NodalVariable(m, Matrix, Space), &
                           valRank=Matrix, ans=g4, dim1=dim1, dim2=dim2, &
                           dim3=dim3, dim4=dim4, tempMat3=tempMat3)
  CALL OK(ALL([dim1, dim2, dim3, dim4] .EQ. [2, 2, 2, 4]), &
          "FEVariable: matrix dims")
  CALL OK(ALL(ABS(g4(1, 1, 2, :) - 3.0_DFP) .LT. tol), &
          "FEVariable: matrix, larger work array")

  ! a space-time matrix variable is not a space variable: ans is zero
  g4 = 1.0_DFP
  CALL GetSpatialGradient_(obj=cell, &
                           val=NodalVariable(RESHAPE([m, m], [2, 2, 4, 2]), &
                                             Matrix, SpaceTime), &
                           valRank=Matrix, ans=g4, dim1=dim1, dim2=dim2, &
                           dim3=dim3, dim4=dim4, tempMat3=tempMat3)
  CALL OK(ALL(g4 .EQ. 0.0_DFP), "FEVariable: matrix spaceTime gives zero")
END BLOCK

END PROGRAM main
