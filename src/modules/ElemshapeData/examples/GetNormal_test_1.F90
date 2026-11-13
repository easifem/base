!> author: Vikas Sharma, Ph. D.
! date: 2026-11-14
! summary: Get the unit normal stored in ElemShapeData_

PROGRAM main
USE BaseType, ONLY: ElemShapeData_
USE BaseType, ONLY: FEVariable_
USE BaseType, ONLY: fevaropt => TypeFEVariableOpt
USE BaseType, ONLY: qpopt => TypeQuadratureOpt
USE BaseType, ONLY: QuadraturePoint_
USE BaseType, ONLY: STElemShapeData_
USE ElemShapeData_Method, ONLY: ASSIGNMENT(=)
USE ElemShapeData_Method, ONLY: GetNormal
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
! physical cell: rectangle [0, 2] x [0, 1], so x = 1 + xi, y = (1 + eta) / 2
REAL(DFP), PARAMETER :: xcell(2, 4) = RESHAPE([0.0_DFP, 0.0_DFP, 2.0_DFP, &
                                               0.0_DFP, 2.0_DFP, 1.0_DFP, &
                                               0.0_DFP, 1.0_DFP], [2, 4])
! physical facet: bottom edge from (0, 0) to (2, 0)
REAL(DFP), PARAMETER :: xfacet(2, 2) = RESHAPE([0.0_DFP, 0.0_DFP, &
                                                2.0_DFP, 0.0_DFP], [2, 2])
TYPE(ElemShapeData_) :: cell, facet, cellAtFacet
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

! cell data at 2 points, only to compute the facet normal
cellAtFacet = cell
cellAtFacet%nips = facet%nips
CALL Set(facetobj=facet, cellobj=cellAtFacet, cellval=xcell, &
         facetval=xfacet, cellN=cell%N(:, 1:2), facetN=facet%N, &
         celldNdXi=cell%dNdXi(:, :, 1:2), facetdNdXi=facet%dNdXi)

! Test 1: normal as an array, with and without nsd
BLOCK
  REAL(DFP), ALLOCATABLE :: normal(:, :)

  CALL GetNormal(obj=facet, normal=normal)
  CALL OK(ALL(SHAPE(normal) .EQ. [3, 2]), "array: normal(3, nips)")
  CALL GetNormal(obj=facet, normal=normal, nsd=2)
  CALL OK(ALL(SHAPE(normal) .EQ. [2, 2]), "array: normal(nsd, nips)")
  CALL OK(ALL(ABS(normal(2, :) + 1.0_DFP) .LT. tol), &
          "array: normal = (0, -1)")
END BLOCK

! Test 2: normal as a quadrature vector space variable
BLOCK
  TYPE(FEVariable_) :: normal

  CALL GetNormal(obj=facet, normal=normal, nsd=2)
  CALL OK(normal%rank .EQ. fevaropt%vector, "FEVariable: vector")
  CALL OK(normal%varType .EQ. fevaropt%space, "FEVariable: space")
  CALL OK(normal%defineOn .EQ. fevaropt%quadrature, &
          "FEVariable: quadrature")
  CALL OK(ALL(normal%s(1:2) .EQ. [2, 2]), "FEVariable: shape [nsd, nips]")
  CALL OK(ABS(normal%val(2) + 1.0_DFP) .LT. tol, "FEVariable: n_y = -1")
END BLOCK

! Test 3: space-time shape data gives a vector space-time variable
BLOCK
  TYPE(STElemShapeData_) :: stelemsd(2)
  TYPE(FEVariable_) :: normal
  INTEGER(I4B) :: ipt

  DO ipt = 1, 2
    stelemsd(ipt)%nips = facet%nips
    stelemsd(ipt)%normal = facet%normal
  END DO
  CALL GetNormal(obj=stelemsd, normal=normal, nsd=2)
  CALL OK(normal%varType .EQ. fevaropt%spacetime, &
          "space-time: varType")
  CALL OK(ALL(normal%s(1:3) .EQ. [2, 2, 2]), &
          "space-time: shape [nsd, nips, nipt]")
  CALL OK(ABS(normal%val(8) + 1.0_DFP) .LT. tol, &
          "space-time: n_y at the last point")
END BLOCK

END PROGRAM main
