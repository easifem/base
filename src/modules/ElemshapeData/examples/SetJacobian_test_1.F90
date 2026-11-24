!> author: Vikas Sharma, Ph. D.
! date: 2026-11-14
! summary: Jacobian of the map from reference to physical element

PROGRAM main
USE BaseType, ONLY: ElemShapeData_
USE BaseType, ONLY: qpopt => TypeQuadratureOpt
USE BaseType, ONLY: QuadraturePoint_
USE BaseType, ONLY: STElemShapeData_
USE ElemShapeData_Method, ONLY: ALLOCATE
USE ElemShapeData_Method, ONLY: Initiate
USE ElemShapeData_Method, ONLY: LagrangeElemShapeData
USE ElemShapeData_Method, ONLY: SetJacobian
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
REAL(DFP), PARAMETER :: lineCoord(1, 2) = RESHAPE([-1.0_DFP, 1.0_DFP], &
                                                  [1, 2])
! physical cell: rectangle [0, 2] x [0, 1], so x = 1 + xi, y = (1 + eta) / 2
REAL(DFP), PARAMETER :: xcell(2, 4) = RESHAPE([0.0_DFP, 0.0_DFP, 2.0_DFP, &
                                               0.0_DFP, 2.0_DFP, 1.0_DFP, &
                                               0.0_DFP, 1.0_DFP], [2, 4])
! physical facet: bottom edge from (0, 0) to (2, 0)
REAL(DFP), PARAMETER :: xfacet(2, 2) = RESHAPE([0.0_DFP, 0.0_DFP, &
                                                2.0_DFP, 0.0_DFP], [2, 2])
TYPE(ElemShapeData_) :: cell, facet
TYPE(QuadraturePoint_) :: quad, facetQuad
TYPE(STElemShapeData_), ALLOCATABLE :: stelemsd(:)

CALL QP_Initiate(obj=quad, elemType=Quadrangle4, domainName="BIUNIT", &
                 nips=[2], quadratureType=qpopt%GaussLegendre)
CALL QP_Initiate(obj=facetQuad, elemType=Line2, domainName="BIUNIT", &
                 nips=[2], quadratureType=qpopt%GaussLegendre)
CALL LagrangeElemShapeData(obj=cell, quad=quad, nsd=2, xidim=2, &
                           elemType=Quadrangle4, refelemCoord=quadCoord, &
                           domainName="BIUNIT", order=1)
CALL LagrangeElemShapeData(obj=facet, quad=facetQuad, nsd=2, xidim=1, &
                           elemType=Line2, refelemCoord=lineCoord, &
                           domainName="BIUNIT", order=1)

! Test 1: cell, J = dx/dxi = diag(1, 1/2) at every point
BLOCK
  INTEGER(I4B) :: ips

  CALL SetJacobian(obj=cell, val=xcell, dNdXi=cell%dNdXi)
  DO ips = 1, cell%nips
    CALL OK(ALL(ABS(cell%jacobian(:, :, ips) - RESHAPE([1.0_DFP, 0.0_DFP, &
                0.0_DFP, 0.5_DFP], [2, 2])) .LT. tol), &
            "cell: J = diag(1, 1/2)")
  END DO
END BLOCK

! Test 2: facet, J = dx/dxi = [1, 0]
BLOCK
  CALL SetJacobian(obj=facet, val=xfacet, dNdXi=facet%dNdXi)
  CALL OK(ALL(ABS(facet%jacobian(1, 1, :) - 1.0_DFP) .LT. tol), &
          "facet: dx/dxi = 1")
  CALL OK(ALL(ABS(facet%jacobian(2, 1, :)) .LT. tol), "facet: dy/dxi = 0")
END BLOCK

! Test 3: space-time, J from nodal coordinates interpolated in time
BLOCK
  REAL(DFP) :: xst(2, 2, 2)

  CALL MakeST(facet, stelemsd)
  xst(:, :, 1) = xfacet
  xst(:, :, 2) = xfacet
  CALL SetJacobian(obj=stelemsd(1), val=xst, dNdXi=facet%dNdXi, &
                   T=stelemsd(1)%T)
  CALL OK(ALL(ABS(stelemsd(1)%jacobian(1, 1, :) - 1.0_DFP) .LT. tol), &
          "space-time: dx/dxi = 1")
END BLOCK

CONTAINS

!> Space-time shape data: facet data in space, the same at every time
SUBROUTINE MakeST(elemsd, stelemsd)
  TYPE(ElemShapeData_), INTENT(IN) :: elemsd
  !! space shape data, also used as a linear time element
  TYPE(STElemShapeData_), ALLOCATABLE, INTENT(INOUT) :: stelemsd(:)
  !! space-time shape data, one per time point
  INTEGER(I4B) :: ipt

  CALL Initiate(stelemsd, elemsd)
  DO ipt = 1, SIZE(stelemsd)
    CALL ALLOCATE (stelemsd(ipt), nsd=elemsd%nsd, xidim=elemsd%xidim, &
                   nns=elemsd%nns, nips=elemsd%nips, nnt=elemsd%nns)
    stelemsd(ipt)%T = elemsd%N(:, ipt)
    stelemsd(ipt)%N = elemsd%N
    stelemsd(ipt)%dNdXi = elemsd%dNdXi
  END DO
END SUBROUTINE MakeST

END PROGRAM main
