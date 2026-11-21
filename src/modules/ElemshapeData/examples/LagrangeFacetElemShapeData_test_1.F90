!> author: Vikas Sharma, Ph. D.
! date: 2026-11-14
! summary: Lagrange shape functions of a cell and one of its facets

PROGRAM main
USE BaseType, ONLY: ElemShapeData_
USE BaseType, ONLY: QuadraturePoint_
USE BaseType, ONLY: qpopt => TypeQuadratureOpt
USE ElemShapeData_Method, ONLY: LagrangeFacetElemShapeData
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: Line2
USE GlobalData, ONLY: Quadrangle4
USE QuadraturePoint_Method, ONLY: QP_Initiate => Initiate
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
REAL(DFP), PARAMETER :: quadCoord(2, 4) = RESHAPE([-1.0_DFP, -1.0_DFP, &
                        1.0_DFP, -1.0_DFP, 1.0_DFP, 1.0_DFP, -1.0_DFP, &
                                                   1.0_DFP], [2, 4])

! Test 1: bilinear quadrangle and its first face, a linear line
BLOCK
  TYPE(ElemShapeData_) :: elemsd, facetElemsd
  TYPE(QuadraturePoint_) :: quad, facetQuad
  REAL(DFP) :: xi(3)

  CALL QP_Initiate(obj=quad, elemType=Quadrangle4, domainName="BIUNIT", &
                   nips=[2], quadratureType=qpopt%GaussLegendre)
  CALL QP_Initiate(obj=facetQuad, elemType=Line2, domainName="BIUNIT", &
                   nips=[3], quadratureType=qpopt%GaussLegendre)
  CALL LagrangeFacetElemShapeData(obj=elemsd, facetElemsd=facetElemsd, &
                                  quad=quad, facetQuad=facetQuad, &
                                  localFaceNumber=1, nsd=2, xidim=2, &
                                  elemType=Quadrangle4, &
                                  refelemCoord=quadCoord, &
                                  domainName="BIUNIT", order=1)
  xi = facetQuad%points(1, 1:3)

  CALL OK(elemsd%nns .EQ. 4 .AND. elemsd%nips .EQ. 4, "cell: nns, nips")
  CALL OK(facetElemsd%nns .EQ. 2, "facet: nns = 2")
  CALL OK(facetElemsd%nips .EQ. 3, "facet: nips = 3")
  CALL OK(facetElemsd%xidim .EQ. 1, "facet: xidim = 1")
  CALL OK(facetElemsd%nsd .EQ. 2, "facet: nsd = 2")
  CALL OK(ALL(ABS(SUM(facetElemsd%N, dim=1) - 1.0_DFP) .LT. tol), &
          "facet: sum of N = 1")
  CALL OK(ALL(ABS(facetElemsd%N(1, :) - 0.5_DFP * (1.0_DFP - xi)) &
              .LT. tol), "facet: N1 = (1 - xi) / 2")
END BLOCK

END PROGRAM main
