!> author: Vikas Sharma, Ph. D.
! date: 2026-11-21
! summary: Spatial gradient without allocation: facet, constant and
! space FEVariable with space-time shape data

PROGRAM main
USE BaseType, ONLY: Constant => TypeFEVariableConstant
USE BaseType, ONLY: ElemShapeData_
USE BaseType, ONLY: Matrix => TypeFEVariableMatrix
USE BaseType, ONLY: qpopt => TypeQuadratureOpt
USE BaseType, ONLY: QuadraturePoint_
USE BaseType, ONLY: Space => TypeFEVariableSpace
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
REAL(DFP), PARAMETER :: lineCoord(1, 2) = RESHAPE([-1.0_DFP, 1.0_DFP], &
                                                  [1, 2])
REAL(DFP), PARAMETER :: quadCoord(2, 4) = RESHAPE([-1.0_DFP, -1.0_DFP, &
                        1.0_DFP, -1.0_DFP, 1.0_DFP, 1.0_DFP, -1.0_DFP, &
                                                   1.0_DFP], [2, 4])
! physical cell: rectangle [0, 2] x [0, 1]
REAL(DFP), PARAMETER :: xcell(2, 4) = RESHAPE([0.0_DFP, 0.0_DFP, 2.0_DFP, &
                                               0.0_DFP, 2.0_DFP, 1.0_DFP, &
                                               0.0_DFP, 1.0_DFP], [2, 4])
TYPE(ElemShapeData_) :: cell, facet
TYPE(QuadraturePoint_) :: quad, lquad
TYPE(STElemShapeData_), ALLOCATABLE :: stelemsd(:)
INTEGER(I4B) :: nrow, ncol, dim1, dim2, dim3, dim4

CALL QP_Initiate(obj=quad, elemType=Quadrangle4, domainName="BIUNIT", &
                 nips=[2], quadratureType=qpopt%GaussLegendre)
CALL LagrangeElemShapeData(obj=cell, quad=quad, nsd=2, xidim=2, &
                           elemType=Quadrangle4, refelemCoord=quadCoord, &
                           domainName="BIUNIT", order=1)
CALL Set(obj=cell, val=xcell, N=cell%N, dNdXi=cell%dNdXi)

! facet: a line (xidim = 1) in the plane (nsd = 2)
CALL QP_Initiate(obj=lquad, elemType=Line2, domainName="BIUNIT", &
                 nips=[2], quadratureType=qpopt%GaussLegendre)
CALL LagrangeElemShapeData(obj=facet, quad=lquad, nsd=2, xidim=1, &
                           elemType=Line2, refelemCoord=lineCoord, &
                           domainName="BIUNIT", order=1)

! space-time: line [0, 2] in space with two time nodes, so dx/dxi = 1
BLOCK
  REAL(DFP), PARAMETER :: xst(1, 2, 2) = RESHAPE([0.0_DFP, 2.0_DFP, &
                                                  0.0_DFP, 2.0_DFP], &
                                                 [1, 2, 2])
  TYPE(ElemShapeData_) :: line
  INTEGER(I4B) :: ipt

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

! Test 1: on a facet (nsd /= xidim) the gradient is zero
BLOCK
  REAL(DFP) :: g2(3, 3), g3(3, 3, 3), g4(3, 3, 3, 3)
  REAL(DFP) :: v(2, 2), m(2, 2, 2)

  g2 = 1.0_DFP
  CALL GetSpatialGradient_(obj=facet, val=[1.0_DFP, 3.0_DFP], ans=g2, &
                           nrow=nrow, ncol=ncol)
  CALL OK(nrow .EQ. 2 .AND. ncol .EQ. 2, "facet scalar: nrow, ncol")
  CALL OK(ALL(g2(1:2, 1:2) .EQ. 0.0_DFP), "facet scalar: zero")

  v = 1.0_DFP
  v(1, 2) = 5.0_DFP
  g3 = 1.0_DFP
  CALL GetSpatialGradient_(obj=facet, val=v, ans=g3, dim1=dim1, &
                           dim2=dim2, dim3=dim3)
  CALL OK(ALL([dim1, dim2, dim3] .EQ. [2, 2, 2]), "facet vector: dims")
  CALL OK(ALL(g3(1:2, 1:2, 1:2) .EQ. 0.0_DFP), "facet vector: zero")

  m = 1.0_DFP
  m(1, 1, 2) = 5.0_DFP
  g4 = 1.0_DFP
  CALL GetSpatialGradient_(obj=facet, val=m, ans=g4, dim1=dim1, &
                           dim2=dim2, dim3=dim3, dim4=dim4)
  CALL OK(ALL([dim1, dim2, dim3, dim4] .EQ. [2, 2, 2, 2]), &
          "facet matrix: dims")
  CALL OK(ALL(g4(1:2, 1:2, 1:2, 1:2) .EQ. 0.0_DFP), "facet matrix: zero")
END BLOCK

! Test 2: constant vector and matrix FEVariable give zero
BLOCK
  REAL(DFP) :: g3(2, 2, 4), g4(2, 2, 2, 4)
  REAL(DFP) :: tempMat2(2, 4), tempMat3(2, 2, 4), m(2, 2)

  g3 = 1.0_DFP
  CALL GetSpatialGradient_(obj=cell, &
                           val=NodalVariable([1.0_DFP, 2.0_DFP], Vector, &
                                             Constant), &
                           valRank=Vector, ans=g3, dim1=dim1, dim2=dim2, &
                           dim3=dim3, tempMat2=tempMat2)
  CALL OK(ALL([dim1, dim2, dim3] .EQ. [2, 2, 4]), "constant vector: dims")
  CALL OK(ALL(g3 .EQ. 0.0_DFP), "constant vector: zero")

  m = RESHAPE([1.0_DFP, 2.0_DFP, 3.0_DFP, 4.0_DFP], [2, 2])
  g4 = 1.0_DFP
  CALL GetSpatialGradient_(obj=cell, val=NodalVariable(m, Matrix, Constant), &
                           valRank=Matrix, ans=g4, dim1=dim1, dim2=dim2, &
                           dim3=dim3, dim4=dim4, tempMat3=tempMat3)
  CALL OK(ALL([dim1, dim2, dim3, dim4] .EQ. [2, 2, 2, 4]), &
          "constant matrix: dims")
  CALL OK(ALL(g4 .EQ. 0.0_DFP), "constant matrix: zero")
END BLOCK

! Test 3: space vector and matrix FEVariable, space-time shape data
! v = (2 x, 5) and M11 = 2 x, M22 = 7 on the line [0, 2]
BLOCK
  REAL(DFP) :: g3(3, 2, 3), g4(3, 3, 2, 3)
  REAL(DFP) :: tempMat3(3, 4, 4), tempMat4(3, 3, 4, 4)
  REAL(DFP) :: v(2, 2), m(2, 2, 2)

  v(1, :) = [0.0_DFP, 4.0_DFP]
  v(2, :) = 5.0_DFP
  CALL GetSpatialGradient_(obj=stelemsd(2), &
                           val=NodalVariable(v, Vector, Space), &
                           valRank=Vector, ans=g3, dim1=dim1, dim2=dim2, &
                           dim3=dim3, tempMat3=tempMat3)
  CALL OK(ALL([dim1, dim2, dim3] .EQ. [2, 1, 2]), "space vector: dims")
  CALL OK(ALL(ABS(g3(1, 1, 1:2) - 2.0_DFP) .LT. tol), &
          "space vector: dv1/dx = 2")
  CALL OK(ALL(ABS(g3(2, 1, 1:2)) .LT. tol), "space vector: dv2/dx = 0")

  m = 0.0_DFP
  m(1, 1, :) = [0.0_DFP, 4.0_DFP]
  m(2, 2, :) = 7.0_DFP
  CALL GetSpatialGradient_(obj=stelemsd(2), &
                           val=NodalVariable(m, Matrix, Space), &
                           valRank=Matrix, ans=g4, dim1=dim1, dim2=dim2, &
                           dim3=dim3, dim4=dim4, tempMat4=tempMat4)
  CALL OK(ALL([dim1, dim2, dim3, dim4] .EQ. [2, 2, 1, 2]), &
          "space matrix: dims")
  CALL OK(ALL(ABS(g4(1, 1, 1, 1:2) - 2.0_DFP) .LT. tol), &
          "space matrix: dM11/dx = 2")
  CALL OK(ALL(ABS(g4(2, 2, 1, 1:2)) .LT. tol), "space matrix: dM22/dx = 0")
END BLOCK

END PROGRAM main
