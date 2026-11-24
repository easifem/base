!> author: Vikas Sharma, Ph. D.
! date: 2026-11-19
! summary: Set the shape data of two cells sharing a facet, and of space-time

PROGRAM main
USE BaseType, ONLY: ElemShapeData_
USE BaseType, ONLY: qpopt => TypeQuadratureOpt
USE BaseType, ONLY: QuadraturePoint_
USE BaseType, ONLY: STElemShapeData_
USE ElemShapeData_Method, ONLY: ALLOCATE
USE ElemShapeData_Method, ONLY: ASSIGNMENT(=)
USE ElemShapeData_Method, ONLY: Initiate
USE ElemShapeData_Method, ONLY: LagrangeElemShapeData
USE ElemShapeData_Method, ONLY: Set
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
! master cell [0, 2] x [0, 1] and slave cell [0, 2] x [-1, 0]
REAL(DFP), PARAMETER :: xmaster(2, 4) = RESHAPE([0.0_DFP, 0.0_DFP, &
                        2.0_DFP, 0.0_DFP, 2.0_DFP, 1.0_DFP, 0.0_DFP, &
                                                 1.0_DFP], [2, 4])
REAL(DFP), PARAMETER :: xslave(2, 4) = RESHAPE([0.0_DFP, -1.0_DFP, &
                        2.0_DFP, -1.0_DFP, 2.0_DFP, 0.0_DFP, 0.0_DFP, &
                                                0.0_DFP], [2, 4])
! the shared facet y = 0: from (0, 0) to (2, 0) for the master cell, and
! from (2, 0) to (0, 0) for the slave cell
REAL(DFP), PARAMETER :: xmf(2, 2) = RESHAPE([0.0_DFP, 0.0_DFP, &
                                             2.0_DFP, 0.0_DFP], [2, 2])
REAL(DFP), PARAMETER :: xsf(2, 2) = RESHAPE([2.0_DFP, 0.0_DFP, &
                                             0.0_DFP, 0.0_DFP], [2, 2])
! space element [0, 2], time element [0, 1]: x = 1 + xi, jt = 1/2
REAL(DFP), PARAMETER :: xst(1, 2, 2) = RESHAPE([0.0_DFP, 2.0_DFP, 0.0_DFP, &
                                                2.0_DFP], [1, 2, 2])
TYPE(ElemShapeData_) :: cell, facet, line
TYPE(QuadraturePoint_) :: quad, facetQuad

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
CALL LagrangeElemShapeData(obj=line, quad=facetQuad, nsd=1, xidim=1, &
                           elemType=Line2, refelemCoord=lineCoord, &
                           domainName="BIUNIT", order=1)

! Test 1: master and slave cells that share the facet y = 0
BLOCK
  TYPE(ElemShapeData_) :: mf, mc, sf, sc

  ! cell data at the facet points: only the transfer of normal, js and ws
  ! from the facet to the cell is checked
  mf = facet
  sf = facet
  mc = cell
  sc = cell
  mc%nips = facet%nips
  sc%nips = facet%nips
  CALL Set(masterFacetObj=mf, masterCellObj=mc, masterCellVal=xmaster, &
           masterCellN=cell%N(:, 1:2), &
           masterCelldNdXi=cell%dNdXi(:, :, 1:2), &
           masterFacetN=facet%N, masterFacetdNdXi=facet%dNdXi, &
           masterFacetVal=xmf, &
           slaveFacetObj=sf, slaveCellObj=sc, slaveCellVal=xslave, &
           slaveCellN=cell%N(:, 1:2), slaveCelldNdXi=cell%dNdXi(:, :, 1:2), &
           slaveFacetN=facet%N, slaveFacetdNdXi=facet%dNdXi, &
           slaveFacetVal=xsf)
  CALL OK(ALL(ABS(mf%js - 1.0_DFP) .LT. tol), "master facet: js = 1")
  CALL OK(ALL(ABS(mf%normal(2, :) + 1.0_DFP) .LT. tol), &
          "master facet: normal = (0, -1)")
  CALL OK(ALL(ABS(sf%normal(2, :) - 1.0_DFP) .LT. tol), &
          "slave facet: normal = (0, 1)")
  CALL OK(ALL(ABS(mc%normal(2, 1:2) + 1.0_DFP) .LT. tol), &
          "master cell: normal of its facet")
  CALL OK(ALL(ABS(sc%normal(2, 1:2) - 1.0_DFP) .LT. tol), &
          "slave cell: normal of its facet")
  CALL OK(ALL(ABS(sc%js(1:2) - 1.0_DFP) .LT. tol), &
          "slave cell: js of its facet")
  CALL OK(ALL(ABS(sc%ws(1:2) - facet%ws) .LT. tol), &
          "slave cell: ws of its facet")
END BLOCK

! Test 2: space-time element, fixed line [0, 2] in time [0, 1]
BLOCK
  TYPE(STElemShapeData_), ALLOCATABLE :: st(:)
  INTEGER(I4B) :: ips, aa

  CALL MakeST(line, st)
  CALL Set(obj=st(1), val=xst, N=st(1)%N, T=st(1)%T, dNdXi=st(1)%dNdXi)
  CALL OK(ALL(ABS(st(1)%jacobian(1, 1, :) - 1.0_DFP) .LT. tol), &
          "space-time: jacobian dx/dxi = 1")
  CALL OK(ALL(ABS(st(1)%js - 1.0_DFP) .LT. tol), "space-time: js = 1")
  CALL OK(ALL(ABS(st(1)%dNdXt - st(1)%dNdXi) .LT. tol), &
          "space-time: dNdXt = dNdXi")
  CALL OK(ALL(ABS(st(1)%coord(1, :) - (1.0_DFP + facetQuad%points(1, 1:2))) &
              .LT. tol), "space-time: coord x = 1 + xi")
  DO ips = 1, 2
    DO aa = 1, 2
      CALL OK(ALL(ABS(st(1)%dNTdXt(:, aa, 1, ips) - st(1)%dNdXi(:, 1, ips) &
                      * st(1)%T(aa)) .LT. tol), &
              "space-time: dNTdXt = dN/dx T_a")
      CALL OK(ALL(ABS(st(1)%dNTdt(:, aa, ips) - st(1)%N(:, ips) * &
                      st(1)%dTdTheta(aa) / 0.5_DFP) .LT. tol), &
              "space-time: fixed mesh, dNTdt = N dT/dtheta / jt")
    END DO
  END DO
END BLOCK

CONTAINS

!> Space-time shape data of a linear line in space and in time; the
! space part is the same at every time point and jt = 1/2
SUBROUTINE MakeST(elemsd, stelemsd)
  TYPE(ElemShapeData_), INTENT(IN) :: elemsd
  !! shape data of a linear line, used in space and in time
  TYPE(STElemShapeData_), ALLOCATABLE, INTENT(INOUT) :: stelemsd(:)
  !! space-time shape data, one per time point
  INTEGER(I4B) :: ipt

  CALL Initiate(stelemsd, elemsd)
  DO ipt = 1, SIZE(stelemsd)
    CALL ALLOCATE (stelemsd(ipt), nsd=1, xidim=1, nns=2, nips=2, nnt=2)
    stelemsd(ipt)%T = elemsd%N(:, ipt)
    stelemsd(ipt)%dTdTheta = elemsd%dNdXi(:, 1, ipt)
    stelemsd(ipt)%jt = 0.5_DFP
    stelemsd(ipt)%wt = elemsd%ws(ipt)
    stelemsd(ipt)%N = elemsd%N
    stelemsd(ipt)%dNdXi = elemsd%dNdXi
    stelemsd(ipt)%ws = elemsd%ws
  END DO
END SUBROUTINE MakeST

END PROGRAM main
