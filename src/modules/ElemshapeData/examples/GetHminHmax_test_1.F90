!> author: Vikas Sharma, Ph. D.
! date: 2026-11-21
! summary: Element size hmin and hmax from the Jacobian of a cell

PROGRAM main
USE BaseType, ONLY: ElemShapeData_
USE BaseType, ONLY: FEVariable_
USE BaseType, ONLY: qpopt => TypeQuadratureOpt
USE BaseType, ONLY: QuadraturePoint_
USE BaseType, ONLY: Scalar => TypeFEVariableScalar
USE BaseType, ONLY: Space => TypeFEVariableSpace
USE ElemShapeData_Method, ONLY: GetHminHmax
USE ElemShapeData_Method, ONLY: LagrangeElemShapeData
USE ElemShapeData_Method, ONLY: SetJacobian
USE FEVariable_Method, ONLY: Get
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: Quadrangle4
USE GlobalData, ONLY: Quadrature
USE QuadraturePoint_Method, ONLY: QP_Initiate => Initiate
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
REAL(DFP), PARAMETER :: quadCoord(2, 4) = RESHAPE([-1.0_DFP, -1.0_DFP, &
                        1.0_DFP, -1.0_DFP, 1.0_DFP, 1.0_DFP, -1.0_DFP, &
                                                   1.0_DFP], [2, 4])
! physical cell: rectangle [0, 2] x [0, 1], so J = diag(1, 1/2)
! the cell is 2 long in x and 1 long in y: hmax = 2, hmin = 1
REAL(DFP), PARAMETER :: xcell(2, 4) = RESHAPE([0.0_DFP, 0.0_DFP, 2.0_DFP, &
                                               0.0_DFP, 2.0_DFP, 1.0_DFP, &
                                               0.0_DFP, 1.0_DFP], [2, 4])
TYPE(ElemShapeData_) :: cell
TYPE(QuadraturePoint_) :: quad

CALL QP_Initiate(obj=quad, elemType=Quadrangle4, domainName="BIUNIT", &
                 nips=[2], quadratureType=qpopt%GaussLegendre)
CALL LagrangeElemShapeData(obj=cell, quad=quad, nsd=2, xidim=2, &
                           elemType=Quadrangle4, refelemCoord=quadCoord, &
                           domainName="BIUNIT", order=1)
CALL SetJacobian(obj=cell, val=xcell, dNdXi=cell%dNdXi)

! Test 1: real arrays, one value per quadrature point
BLOCK
  REAL(DFP), ALLOCATABLE :: hmax(:), hmin(:)

  CALL GetHminHmax(obj=cell, hmax=hmax, hmin=hmin)
  CALL OK(SIZE(hmax) .EQ. cell%nips, "array: size(hmax) = nips")
  CALL OK(SIZE(hmin) .EQ. cell%nips, "array: size(hmin) = nips")
  CALL OK(ALL(ABS(hmax - 2.0_DFP) .LT. tol), "array: hmax = 2")
  CALL OK(ALL(ABS(hmin - 1.0_DFP) .LT. tol), "array: hmin = 1")
END BLOCK

! Test 2: a rotated cell has the same hmin and hmax
BLOCK
  TYPE(ElemShapeData_) :: rotated
  REAL(DFP), ALLOCATABLE :: hmax(:), hmin(:)
  REAL(DFP) :: rot(2, 2), c, s
  INTEGER(I4B) :: ips

  c = COS(0.3_DFP)
  s = SIN(0.3_DFP)
  rot = RESHAPE([c, s, -s, c], [2, 2])
  rotated = cell
  DO ips = 1, rotated%nips
    rotated%jacobian(:, :, ips) = MATMUL(rot, cell%jacobian(:, :, ips))
  END DO
  CALL GetHminHmax(obj=rotated, hmax=hmax, hmin=hmin)
  CALL OK(ALL(ABS(hmax - 2.0_DFP) .LT. tol), "rotated: hmax = 2")
  CALL OK(ALL(ABS(hmin - 1.0_DFP) .LT. tol), "rotated: hmin = 1")
END BLOCK

! Test 3: FEVariable output, scalar quadrature variable in space
BLOCK
  TYPE(FEVariable_) :: hmax, hmin
  REAL(DFP), ALLOCATABLE :: r1(:)

  CALL GetHminHmax(obj=cell, hmax=hmax, hmin=hmin)
  CALL OK(hmax%defineOn .EQ. Quadrature, "FEVariable: defined on quad")
  r1 = Get(hmax, Scalar, Space)
  CALL OK(SIZE(r1) .EQ. cell%nips, "FEVariable: size(hmax) = nips")
  CALL OK(ALL(ABS(r1 - 2.0_DFP) .LT. tol), "FEVariable: hmax = 2")
  r1 = Get(hmin, Scalar, Space)
  CALL OK(ALL(ABS(r1 - 1.0_DFP) .LT. tol), "FEVariable: hmin = 1")
END BLOCK

END PROGRAM main
