!> author: Vikas Sharma, Ph. D.
! date: 2026-11-21
! summary: Element size hmin and hmax from a given metric tensor G

PROGRAM main
USE BaseType, ONLY: ElemShapeData_
USE BaseType, ONLY: FEVariable_
USE BaseType, ONLY: qpopt => TypeQuadratureOpt
USE BaseType, ONLY: QuadraturePoint_
USE BaseType, ONLY: Scalar => TypeFEVariableScalar
USE BaseType, ONLY: Space => TypeFEVariableSpace
USE ElemShapeData_Method, ONLY: GetHminHmax
USE ElemShapeData_Method, ONLY: LagrangeElemShapeData
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
TYPE(ElemShapeData_) :: cell
TYPE(QuadraturePoint_) :: quad
REAL(DFP), ALLOCATABLE :: G(:, :, :)
INTEGER(I4B) :: ips

! only the number of quadrature points of cell is used
CALL QP_Initiate(obj=quad, elemType=Quadrangle4, domainName="BIUNIT", &
                 nips=[2], quadratureType=qpopt%GaussLegendre)
CALL LagrangeElemShapeData(obj=cell, quad=quad, nsd=2, xidim=2, &
                           elemType=Quadrangle4, refelemCoord=quadCoord, &
                           domainName="BIUNIT", order=1)

! G = inv(J J^T) for J = diag(1, 1/2), that is diag(1, 4);
! its eigenvalues are 1 and 4, so hmax = 2 / 1 = 2, hmin = 2 / 2 = 1
ALLOCATE (G(2, 2, cell%nips))
DO ips = 1, cell%nips
  G(:, :, ips) = RESHAPE([1.0_DFP, 0.0_DFP, 0.0_DFP, 4.0_DFP], [2, 2])
END DO

! Test 1: real arrays
BLOCK
  REAL(DFP), ALLOCATABLE :: hmax(:), hmin(:)

  CALL GetHminHmax(obj=cell, hmax=hmax, hmin=hmin, G=G)
  CALL OK(SIZE(hmax) .EQ. cell%nips, "array: size(hmax) = nips")
  CALL OK(ALL(ABS(hmax - 2.0_DFP) .LT. tol), "array: hmax = 2")
  CALL OK(ALL(ABS(hmin - 1.0_DFP) .LT. tol), "array: hmin = 1")
END BLOCK

! Test 2: a non-diagonal G with eigenvalues 1 and 4
BLOCK
  REAL(DFP), ALLOCATABLE :: hmax(:), hmin(:), G2(:, :, :)

  ALLOCATE (G2(2, 2, cell%nips))
  DO ips = 1, cell%nips
    G2(:, :, ips) = RESHAPE([2.5_DFP, 1.5_DFP, 1.5_DFP, 2.5_DFP], [2, 2])
  END DO
  CALL GetHminHmax(obj=cell, hmax=hmax, hmin=hmin, G=G2)
  CALL OK(ALL(ABS(hmax - 2.0_DFP) .LT. tol), "non-diagonal G: hmax = 2")
  CALL OK(ALL(ABS(hmin - 1.0_DFP) .LT. tol), "non-diagonal G: hmin = 1")
END BLOCK

! Test 3: FEVariable output
BLOCK
  TYPE(FEVariable_) :: hmax, hmin
  REAL(DFP), ALLOCATABLE :: r1(:)

  CALL GetHminHmax(obj=cell, hmax=hmax, hmin=hmin, G=G)
  CALL OK(hmin%defineOn .EQ. Quadrature, "FEVariable: defined on quad")
  r1 = Get(hmax, Scalar, Space)
  CALL OK(ALL(ABS(r1 - 2.0_DFP) .LT. tol), "FEVariable: hmax = 2")
  r1 = Get(hmin, Scalar, Space)
  CALL OK(ALL(ABS(r1 - 1.0_DFP) .LT. tol), "FEVariable: hmin = 1")
END BLOCK

DEALLOCATE (G)
END PROGRAM main
