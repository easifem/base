!> author: Vikas Sharma, Ph. D.
! date: 2026-11-21
! summary: HRQI length scale as quadrature FEVariables on a rectangle

PROGRAM main
USE BaseType, ONLY: ElemShapeData_
USE BaseType, ONLY: FEVariable_
USE BaseType, ONLY: feopt => TypeFEVariableOpt
USE BaseType, ONLY: qpopt => TypeQuadratureOpt
USE BaseType, ONLY: QuadraturePoint_
USE BaseType, ONLY: Scalar => TypeFEVariableScalar
USE BaseType, ONLY: Space => TypeFEVariableSpace
USE ElemShapeData_Method, ONLY: GetHRQIParam
USE ElemShapeData_Method, ONLY: LagrangeElemShapeData
USE ElemShapeData_Method, ONLY: Set
USE FEVariable_Method, ONLY: NodalVariable
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: Quadrangle4
USE QuadraturePoint_Method, ONLY: QP_Initiate => Initiate
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
REAL(DFP), PARAMETER :: quadCoord(2, 4) = RESHAPE([-1.0_DFP, -1.0_DFP, &
                        1.0_DFP, -1.0_DFP, 1.0_DFP, 1.0_DFP, -1.0_DFP, &
                                                   1.0_DFP], [2, 4])
! physical cell: rectangle [0, 2] x [0, 1], G = inv(J J^T) = diag(1, 4)
REAL(DFP), PARAMETER :: xcell(2, 4) = RESHAPE([0.0_DFP, 0.0_DFP, 2.0_DFP, &
                                               0.0_DFP, 2.0_DFP, 1.0_DFP, &
                                               0.0_DFP, 1.0_DFP], [2, 4])
TYPE(ElemShapeData_) :: cell
TYPE(QuadraturePoint_) :: quad
REAL(DFP) :: p(4)

CALL QP_Initiate(obj=quad, elemType=Quadrangle4, domainName="BIUNIT", &
                 nips=[2], quadratureType=qpopt%GaussLegendre)
CALL LagrangeElemShapeData(obj=cell, quad=quad, nsd=2, xidim=2, &
                           elemType=Quadrangle4, refelemCoord=quadCoord, &
                           domainName="BIUNIT", order=1)
CALL Set(obj=cell, val=xcell, N=cell%N, dNdXi=cell%dNdXi)
! level set p = y - 1/2, so r = (0, +-1) and h = 1
p = xcell(2, :) - 0.5_DFP

! Test 1: only h, a scalar quadrature variable in space
BLOCK
  TYPE(FEVariable_) :: h

  CALL GetHRQIParam(obj=cell, h=h, val=NodalVariable(p, Scalar, Space))
  CALL OK(h%rank .EQ. feopt%scalar, "h: scalar")
  CALL OK(h%defineOn .EQ. feopt%quadrature, "h: quadrature")
  CALL OK(h%varType .EQ. feopt%space, "h: space")
  CALL OK(h%len .EQ. 4, "h: one value per quadrature point")
  CALL OK(ALL(ABS(h%val(1:4) - 1.0_DFP) .LT. tol), "h = 1")
END BLOCK

! Test 2: h, hmax, hmin and r together
BLOCK
  TYPE(FEVariable_) :: h, hmax, hmin, r

  CALL GetHRQIParam(obj=cell, h=h, val=NodalVariable(p, Scalar, Space), &
                    hmax=hmax, hmin=hmin, r=r)
  CALL OK(ALL(ABS(h%val(1:4) - 1.0_DFP) .LT. tol), "all: h = 1")
  CALL OK(ALL(ABS(hmax%val(1:4) - 2.0_DFP) .LT. tol), "all: hmax = 2")
  CALL OK(ALL(ABS(hmin%val(1:4) - 1.0_DFP) .LT. tol), "all: hmin = 1")
  CALL OK(r%rank .EQ. feopt%vector, "all: r is a vector")
  CALL OK(ALL(r%s(1:2) .EQ. [2, 4]), "all: r(nsd, nips)")
  CALL OK(ALL(ABS(ABS(r%val(2:8:2)) - 1.0_DFP) .LT. tol), &
          "all: |r_y| = 1")
END BLOCK

! Test 3: only hmin, the other outputs are absent
BLOCK
  TYPE(FEVariable_) :: h, hmin

  CALL GetHRQIParam(obj=cell, h=h, val=NodalVariable(p, Scalar, Space), &
                    hmin=hmin)
  CALL OK(ALL(ABS(hmin%val(1:4) - 1.0_DFP) .LT. tol), "hmin only: hmin = 1")
END BLOCK

END PROGRAM main
