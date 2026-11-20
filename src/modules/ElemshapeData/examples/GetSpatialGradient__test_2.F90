!> author: Vikas Sharma, Ph. D.
! date: 2026-11-19
! summary: Spatial gradient without allocation, matrix and space-time data

PROGRAM main
USE BaseType, ONLY: Constant => TypeFEVariableConstant
USE BaseType, ONLY: ElemShapeData_
USE BaseType, ONLY: Matrix => TypeFEVariableMatrix
USE BaseType, ONLY: qpopt => TypeQuadratureOpt
USE BaseType, ONLY: QuadraturePoint_
USE BaseType, ONLY: Scalar => TypeFEVariableScalar
USE BaseType, ONLY: Space => TypeFEVariableSpace
USE BaseType, ONLY: SpaceTime => TypeFEVariableSpaceTime
USE BaseType, ONLY: STElemShapeData_
USE BaseType, ONLY: Vector => TypeFEVariableVector
USE ElemShapeData_Method, ONLY: ALLOCATE
USE ElemShapeData_Method, ONLY: GetSpatialGradient_
USE ElemShapeData_Method, ONLY: Initiate
USE ElemShapeData_Method, ONLY: LagrangeElemShapeData
USE ElemShapeData_Method, ONLY: Set
USE ElemShapeData_Method, ONLY: SetdNTdXt
USE ElemShapeData_Method, ONLY: SetJacobian
USE FEVariable_Method, ONLY: NodalVariable
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: Line2
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
! space-time values on the line [0, 2]: c(a) x at time node a, c = [1, 3]
REAL(DFP), PARAMETER :: ust(2, 2) = RESHAPE([0.0_DFP, 2.0_DFP, &
                                             0.0_DFP, 6.0_DFP], [2, 2])
TYPE(ElemShapeData_) :: cell
TYPE(QuadraturePoint_) :: quad
TYPE(STElemShapeData_), ALLOCATABLE :: stelemsd(:)
REAL(DFP) :: vst(2, 2, 2), mst(2, 2, 2, 2), c
INTEGER(I4B) :: nrow, ncol, dim1, dim2, dim3, dim4

CALL QP_Initiate(obj=quad, elemType=Quadrangle4, domainName="BIUNIT", &
                 nips=[2], quadratureType=qpopt%GaussLegendre)
CALL LagrangeElemShapeData(obj=cell, quad=quad, nsd=2, xidim=2, &
                           elemType=Quadrangle4, refelemCoord=quadCoord, &
                           domainName="BIUNIT", order=1)
CALL Set(obj=cell, val=xcell, N=cell%N, dNdXi=cell%dNdXi)

! space-time: line [0, 2] in space with two time nodes, so dx/dxi = 1
BLOCK
  REAL(DFP), PARAMETER :: lineCoord(1, 2) = RESHAPE([-1.0_DFP, 1.0_DFP], &
                                                    [1, 2])
  REAL(DFP), PARAMETER :: xst(1, 2, 2) = RESHAPE([0.0_DFP, 2.0_DFP, &
                                                  0.0_DFP, 2.0_DFP], &
                                                 [1, 2, 2])
  TYPE(ElemShapeData_) :: line
  TYPE(QuadraturePoint_) :: lquad
  INTEGER(I4B) :: ipt

  CALL QP_Initiate(obj=lquad, elemType=Line2, domainName="BIUNIT", &
                   nips=[2], quadratureType=qpopt%GaussLegendre)
  CALL LagrangeElemShapeData(obj=line, quad=lquad, nsd=1, xidim=1, &
                             elemType=Line2, refelemCoord=lineCoord, &
                             domainName="BIUNIT", order=1)
  CALL Initiate(stelemsd, line)
  DO ipt = 1, 2
    CALL ALLOCATE (stelemsd(ipt), nsd=1, xidim=1, nns=2, nips=2, nnt=2)
    stelemsd(ipt)%T = line%N(:, ipt)
    stelemsd(ipt)%N = line%N
    stelemsd(ipt)%dNdXi = line%dNdXi
    CALL SetJacobian(obj=stelemsd(ipt), val=xst, dNdXi=line%dNdXi, &
                     T=stelemsd(ipt)%T)
    CALL SetdNTdXt(obj=stelemsd(ipt))
    ! dx/dxi = 1, so dN/dx = dN/dxi; used for space values
    stelemsd(ipt)%dNdXt(:, 1, :) = line%dNdXi(:, 1, :)
  END DO
END BLOCK

! vector: (c(a) x, 5); matrix: M11 = c(a) x, M22 = 7, other entries zero
vst(1, :, :) = ust
vst(2, :, :) = 5.0_DFP
mst = 0.0_DFP
mst(1, 1, :, :) = ust
mst(2, 2, :, :) = 7.0_DFP
! at time point 2: du/dx = T1 c1 + T2 c2
c = stelemsd(2)%T(1) * 1.0_DFP + stelemsd(2)%T(2) * 3.0_DFP

! Test 1: matrix nodal values [u, 0; w, 1] on the cell
BLOCK
  REAL(DFP) :: m(2, 2, 4), g4(3, 3, 3, 5)

  m(1, 1, :) = u
  m(2, 1, :) = w
  m(1, 2, :) = 0.0_DFP
  m(2, 2, :) = 1.0_DFP
  CALL GetSpatialGradient_(obj=cell, val=m, ans=g4, dim1=dim1, dim2=dim2, &
                           dim3=dim3, dim4=dim4)
  CALL OK(ALL([dim1, dim2, dim3, dim4] .EQ. [2, 2, 2, 4]), &
          "matrix: used size [nrow, ncol, nsd, nips]")
  CALL OK(ALL(ABS(g4(1, 1, 1, 1:4) - 1.0_DFP) .LT. tol), "matrix: du/dx = 1")
  CALL OK(ALL(ABS(g4(1, 1, 2, 1:4) - 3.0_DFP) .LT. tol), "matrix: du/dy = 3")
  CALL OK(ALL(ABS(g4(2, 1, 2, 1:4) + 1.0_DFP) .LT. tol), &
          "matrix: dw/dy = -1")
  CALL OK(ALL(ABS(g4(1:2, 2, 1:2, 1:4)) .LT. tol), "matrix: column 2 is 0")
END BLOCK

! Test 2: space-time nodal values: scalar, vector and matrix
BLOCK
  REAL(DFP) :: g2(2, 3), g3(3, 2, 3), g4(3, 3, 2, 3)

  CALL GetSpatialGradient_(obj=stelemsd(2), val=ust, ans=g2, nrow=nrow, &
                           ncol=ncol)
  CALL OK(nrow .EQ. 1 .AND. ncol .EQ. 2, "scalar space-time: nrow, ncol")
  CALL OK(ALL(ABS(g2(1, 1:2) - c) .LT. tol), &
          "scalar space-time: du/dx = T1 c1 + T2 c2")

  CALL GetSpatialGradient_(obj=stelemsd(2), val=vst, ans=g3, dim1=dim1, &
                           dim2=dim2, dim3=dim3)
  CALL OK(ALL([dim1, dim2, dim3] .EQ. [2, 1, 2]), &
          "vector space-time: dim1, dim2, dim3")
  CALL OK(ALL(ABS(g3(1, 1, 1:2) - c) .LT. tol), "vector space-time: dv1/dx")
  CALL OK(ALL(ABS(g3(2, 1, 1:2)) .LT. tol), "vector space-time: dv2/dx = 0")

  CALL GetSpatialGradient_(obj=stelemsd(2), val=mst, ans=g4, dim1=dim1, &
                           dim2=dim2, dim3=dim3, dim4=dim4)
  CALL OK(ALL([dim1, dim2, dim3, dim4] .EQ. [2, 2, 1, 2]), &
          "matrix space-time: dim1 to dim4")
  CALL OK(ALL(ABS(g4(1, 1, 1, 1:2) - c) .LT. tol), &
          "matrix space-time: dM11/dx")
  CALL OK(ALL(ABS(g4(2, 2, 1, 1:2)) .LT. tol), &
          "matrix space-time: dM22/dx = 0")
END BLOCK

! Test 3: scalar FEVariable with space-time shape data
BLOCK
  REAL(DFP) :: g2(2, 3), tempMat2(4, 4)

  CALL GetSpatialGradient_(obj=stelemsd(2), &
                           val=NodalVariable(ust, Scalar, SpaceTime), &
                           valRank=Scalar, ans=g2, nrow=nrow, ncol=ncol, &
                           tempMat2=tempMat2)
  CALL OK(nrow .EQ. 1 .AND. ncol .EQ. 2, "scalar FEVariable: nrow, ncol")
  CALL OK(ALL(ABS(g2(1, 1:2) - c) .LT. tol), &
          "scalar FEVariable space-time: du/dx")

  ! a space value u = 2 x uses dN/dx, so du/dx = 2
  CALL GetSpatialGradient_(obj=stelemsd(2), &
                           val=NodalVariable([0.0_DFP, 4.0_DFP], Scalar, &
                                             Space), &
                           valRank=Scalar, ans=g2, nrow=nrow, ncol=ncol, &
                           tempMat2=tempMat2)
  CALL OK(ALL(ABS(g2(1, 1:2) - 2.0_DFP) .LT. tol), &
          "scalar FEVariable space: du/dx = 2")

  g2 = 1.0_DFP
  CALL GetSpatialGradient_(obj=stelemsd(2), &
                           val=NodalVariable(2.0_DFP, Scalar, Constant), &
                           valRank=Scalar, ans=g2, nrow=nrow, ncol=ncol, &
                           tempMat2=tempMat2)
  CALL OK(ALL(g2(1, 1:2) .EQ. 0.0_DFP), "scalar FEVariable constant: zero")
END BLOCK

! Test 4: vector and matrix FEVariable with space-time shape data
BLOCK
  REAL(DFP) :: g3(3, 2, 3), g4(3, 3, 2, 3)
  REAL(DFP) :: tempMat3(3, 4, 4), tempMat4(3, 3, 4, 4)

  CALL GetSpatialGradient_(obj=stelemsd(2), &
                           val=NodalVariable(vst, Vector, SpaceTime), &
                           valRank=Vector, ans=g3, dim1=dim1, dim2=dim2, &
                           dim3=dim3, tempMat3=tempMat3)
  CALL OK(ALL([dim1, dim2, dim3] .EQ. [2, 1, 2]), &
          "vector FEVariable: dim1, dim2, dim3")
  CALL OK(ALL(ABS(g3(1, 1, 1:2) - c) .LT. tol), "vector FEVariable: dv1/dx")
  CALL OK(ALL(ABS(g3(2, 1, 1:2)) .LT. tol), "vector FEVariable: dv2/dx = 0")

  CALL GetSpatialGradient_(obj=stelemsd(2), &
                           val=NodalVariable(mst, Matrix, SpaceTime), &
                           valRank=Matrix, ans=g4, dim1=dim1, dim2=dim2, &
                           dim3=dim3, dim4=dim4, tempMat4=tempMat4)
  CALL OK(ALL([dim1, dim2, dim3, dim4] .EQ. [2, 2, 1, 2]), &
          "matrix FEVariable: dim1 to dim4")
  CALL OK(ALL(ABS(g4(1, 1, 1, 1:2) - c) .LT. tol), &
          "matrix FEVariable: dM11/dx")
  CALL OK(ALL(ABS(g4(2, 2, 1, 1:2)) .LT. tol), &
          "matrix FEVariable: dM22/dx = 0")
END BLOCK

END PROGRAM main
