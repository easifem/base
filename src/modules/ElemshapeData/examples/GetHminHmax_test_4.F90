!> author: Vikas Sharma, Ph. D.
! date: 2026-11-21
! summary: Space-time hmin and hmax from a given metric tensor G

PROGRAM main
USE BaseType, ONLY: ElemShapeData_
USE BaseType, ONLY: FEVariable_
USE BaseType, ONLY: qpopt => TypeQuadratureOpt
USE BaseType, ONLY: QuadraturePoint_
USE BaseType, ONLY: Scalar => TypeFEVariableScalar
USE BaseType, ONLY: SpaceTime => TypeFEVariableSpaceTime
USE BaseType, ONLY: STElemShapeData_
USE ElemShapeData_Method, ONLY: ALLOCATE
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
INTEGER(I4B), PARAMETER :: nipt = 2
TYPE(ElemShapeData_) :: cell
TYPE(QuadraturePoint_) :: quad
TYPE(STElemShapeData_), ALLOCATABLE :: stelemsd(:)
REAL(DFP), ALLOCATABLE :: G(:, :, :, :)
INTEGER(I4B) :: ips, ipt

! only the number of space quadrature points is used
CALL QP_Initiate(obj=quad, elemType=Quadrangle4, domainName="BIUNIT", &
                 nips=[2], quadratureType=qpopt%GaussLegendre)
CALL LagrangeElemShapeData(obj=cell, quad=quad, nsd=2, xidim=2, &
                           elemType=Quadrangle4, refelemCoord=quadCoord, &
                           domainName="BIUNIT", order=1)
ALLOCATE (stelemsd(nipt))
DO ipt = 1, nipt
  CALL ALLOCATE (stelemsd(ipt), nsd=cell%nsd, xidim=cell%xidim, &
                 nns=cell%nns, nips=cell%nips, nnt=nipt)
  stelemsd(ipt)%N = cell%N
END DO

! G(:, :, :, 1) = diag(1, 4): hmax = 2, hmin = 1
! G(:, :, :, 2) = diag(1/4, 1): hmax = 4, hmin = 2
ALLOCATE (G(2, 2, cell%nips, nipt))
DO ips = 1, cell%nips
  G(:, :, ips, 1) = RESHAPE([1.0_DFP, 0.0_DFP, 0.0_DFP, 4.0_DFP], [2, 2])
  G(:, :, ips, 2) = RESHAPE([0.25_DFP, 0.0_DFP, 0.0_DFP, 1.0_DFP], [2, 2])
END DO

! Test 1: real arrays, shape [nips, nipt]
BLOCK
  REAL(DFP), ALLOCATABLE :: hmax(:, :), hmin(:, :)

  CALL GetHminHmax(obj=stelemsd, hmax=hmax, hmin=hmin, G=G)
  CALL OK(ALL(SHAPE(hmax) .EQ. [cell%nips, nipt]), &
          "array: shape(hmax) = [nips, nipt]")
  CALL OK(ALL(ABS(hmax(:, 1) - 2.0_DFP) .LT. tol), "array: hmax(:, 1) = 2")
  CALL OK(ALL(ABS(hmin(:, 1) - 1.0_DFP) .LT. tol), "array: hmin(:, 1) = 1")
  CALL OK(ALL(ABS(hmax(:, 2) - 4.0_DFP) .LT. tol), "array: hmax(:, 2) = 4")
  CALL OK(ALL(ABS(hmin(:, 2) - 2.0_DFP) .LT. tol), "array: hmin(:, 2) = 2")
END BLOCK

! Test 2: FEVariable output, scalar quadrature variable in space-time
BLOCK
  TYPE(FEVariable_) :: hmax, hmin
  REAL(DFP), ALLOCATABLE :: r2(:, :)

  CALL GetHminHmax(obj=stelemsd, hmax=hmax, hmin=hmin, G=G)
  CALL OK(hmin%defineOn .EQ. Quadrature, "FEVariable: defined on quad")
  r2 = Get(hmax, Scalar, SpaceTime)
  CALL OK(ALL(SHAPE(r2) .EQ. [cell%nips, nipt]), &
          "FEVariable: shape(hmax) = [nips, nipt]")
  CALL OK(ALL(ABS(r2(:, 1) - 2.0_DFP) .LT. tol), "FEVariable: hmax(:, 1)")
  CALL OK(ALL(ABS(r2(:, 2) - 4.0_DFP) .LT. tol), "FEVariable: hmax(:, 2)")
  r2 = Get(hmin, Scalar, SpaceTime)
  CALL OK(ALL(ABS(r2(:, 1) - 1.0_DFP) .LT. tol), "FEVariable: hmin(:, 1)")
  CALL OK(ALL(ABS(r2(:, 2) - 2.0_DFP) .LT. tol), "FEVariable: hmin(:, 2)")
END BLOCK

DEALLOCATE (stelemsd, G)
END PROGRAM main
