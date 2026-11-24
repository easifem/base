!> author: Vikas Sharma, Ph. D.
! date: 2026-11-21
! summary: Element size hmin and hmax for elements with xidim < nsd

PROGRAM main
USE BaseType, ONLY: ElemShapeData_
USE BaseType, ONLY: qpopt => TypeQuadratureOpt
USE BaseType, ONLY: QuadraturePoint_
USE ElemShapeData_Method, ONLY: GetHminHmax
USE ElemShapeData_Method, ONLY: LagrangeElemShapeData
USE ElemShapeData_Method, ONLY: SetJacobian
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: Line2
USE QuadraturePoint_Method, ONLY: QP_Initiate => Initiate
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
REAL(DFP), PARAMETER :: lineCoord(1, 2) = RESHAPE([-1.0_DFP, 1.0_DFP], &
                                                  [1, 2])
TYPE(QuadraturePoint_) :: quad

CALL QP_Initiate(obj=quad, elemType=Line2, domainName="BIUNIT", &
                 nips=[2], quadratureType=qpopt%GaussLegendre)

! Test 1: line in 2D from (0, 0) to (2, 0); J = [1; 0], length 2
BLOCK
  REAL(DFP), PARAMETER :: xfacet(2, 2) = RESHAPE([0.0_DFP, 0.0_DFP, &
                                                  2.0_DFP, 0.0_DFP], [2, 2])
  TYPE(ElemShapeData_) :: facet
  REAL(DFP), ALLOCATABLE :: hmax(:), hmin(:)

  CALL LagrangeElemShapeData(obj=facet, quad=quad, nsd=2, xidim=1, &
                             elemType=Line2, refelemCoord=lineCoord, &
                             domainName="BIUNIT", order=1)
  CALL SetJacobian(obj=facet, val=xfacet, dNdXi=facet%dNdXi)
  CALL GetHminHmax(obj=facet, hmax=hmax, hmin=hmin)
  CALL OK(SIZE(hmax) .EQ. facet%nips, "line 2D: size(hmax) = nips")
  CALL OK(ALL(ABS(hmax - 2.0_DFP) .LT. tol), "line 2D: hmax = 2")
  CALL OK(ALL(ABS(hmin - 2.0_DFP) .LT. tol), "line 2D: hmin = 2")
END BLOCK

! Test 2: line in 3D from (0, 0, 0) to (1, 2, 2); length 3
BLOCK
  REAL(DFP), PARAMETER :: xline(3, 2) = RESHAPE([0.0_DFP, 0.0_DFP, &
                                                 0.0_DFP, 1.0_DFP, &
                                                 2.0_DFP, 2.0_DFP], [3, 2])
  TYPE(ElemShapeData_) :: line
  REAL(DFP), ALLOCATABLE :: hmax(:), hmin(:)

  CALL LagrangeElemShapeData(obj=line, quad=quad, nsd=3, xidim=1, &
                             elemType=Line2, refelemCoord=lineCoord, &
                             domainName="BIUNIT", order=1)
  CALL SetJacobian(obj=line, val=xline, dNdXi=line%dNdXi)
  CALL GetHminHmax(obj=line, hmax=hmax, hmin=hmin)
  CALL OK(ALL(ABS(hmax - 3.0_DFP) .LT. tol), "line 3D: hmax = 3")
  CALL OK(ALL(ABS(hmin - 3.0_DFP) .LT. tol), "line 3D: hmin = 3")
END BLOCK

END PROGRAM main
