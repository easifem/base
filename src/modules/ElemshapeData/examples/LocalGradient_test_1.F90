!> author: Vikas Sharma, Ph. D.
! date: 2026-11-14
! summary: Function form of the gradient with respect to xi

PROGRAM main
USE BaseType, ONLY: ElemShapeData_
USE BaseType, ONLY: FEVariable_
USE BaseType, ONLY: fevaropt => TypeFEVariableOpt
USE BaseType, ONLY: qpopt => TypeQuadratureOpt
USE BaseType, ONLY: QuadraturePoint_
USE BaseType, ONLY: Scalar => TypeFEVariableScalar
USE BaseType, ONLY: Space => TypeFEVariableSpace
USE ElemShapeData_Method, ONLY: LagrangeElemShapeData
USE ElemShapeData_Method, ONLY: LocalGradient
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

! Test 1: LocalGradient of a nodal scalar FEVariable
BLOCK
  TYPE(FEVariable_) :: lg

  lg = LocalGradient(obj=cell, val=NodalVariable(u, Scalar, Space))
  CALL OK(lg%rank .EQ. fevaropt%vector, "rank is vector")
  CALL OK(lg%varType .EQ. fevaropt%space, "varType is space")
  CALL OK(ALL(lg%s(1:2) .EQ. [2, 4]), "shape [xidim, nips]")
  CALL OK(ABS(lg%val(1) - 1.0_DFP) .LT. tol, "du/dxi = 1")
  CALL OK(ABS(lg%val(2) - 1.5_DFP) .LT. tol, "du/deta = 3/2")
END BLOCK

END PROGRAM main
