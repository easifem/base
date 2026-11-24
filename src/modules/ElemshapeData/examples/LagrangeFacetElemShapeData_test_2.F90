!> author: Vikas Sharma, Ph. D.
! date: 2026-11-21
! summary: Lagrange shape data of a hexahedron and one quadrangle facet

PROGRAM main
USE BaseType, ONLY: ElemShapeData_
USE BaseType, ONLY: QuadraturePoint_
USE BaseType, ONLY: qpopt => TypeQuadratureOpt
USE Display_Method, ONLY: Display
USE ElemShapeData_Method, ONLY: LagrangeFacetElemShapeData
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: Hexahedron8
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: Quadrangle4
USE QuadraturePoint_Method, ONLY: QP_Initiate => Initiate
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-5_DFP
REAL(DFP), PARAMETER :: hexCoord(3, 8) = RESHAPE([ &
                                                 -1.0_DFP, -1.0_DFP, &
                                                 -1.0_DFP, &
                                                 1.0_DFP, -1.0_DFP, &
                                                 -1.0_DFP, &
                                                 1.0_DFP, 1.0_DFP, -1.0_DFP, &
                                                 -1.0_DFP, 1.0_DFP, &
                                                 -1.0_DFP, &
                                                 -1.0_DFP, &
                                                 -1.0_DFP, 1.0_DFP, &
                                                 1.0_DFP, -1.0_DFP, 1.0_DFP, &
                                                 1.0_DFP, 1.0_DFP, 1.0_DFP, &
                                                 -1.0_DFP, 1.0_DFP, &
                                                 1.0_DFP], [3, 8])

! Test 1: trilinear hexahedron and its first face, a bilinear quadrangle
BLOCK
  TYPE(ElemShapeData_) :: elemsd, facetElemsd
  TYPE(QuadraturePoint_) :: quad, facetQuad
  INTEGER(I4B) :: ii

  CALL QP_Initiate(obj=quad, elemType=Hexahedron8, domainName="BIUNIT", &
                   nips=[2], quadratureType=qpopt%GaussLegendre)
  CALL QP_Initiate(obj=facetQuad, elemType=Quadrangle4, &
                   domainName="BIUNIT", nips=[2], &
                   quadratureType=qpopt%GaussLegendre)
  CALL LagrangeFacetElemShapeData(obj=elemsd, facetElemsd=facetElemsd, &
                                  quad=quad, facetQuad=facetQuad, &
                                  localFaceNumber=1, nsd=3, xidim=3, &
                                  elemType=Hexahedron8, &
                                  refelemCoord=hexCoord, &
                                  domainName="BIUNIT", order=1)

  CALL OK(elemsd%nns .EQ. 8 .AND. elemsd%nips .EQ. 8, &
          "cell: nns = 8, nips = 8")
  CALL OK(ALL(ABS(SUM(elemsd%N, dim=1) - 1.0_DFP) .LT. tol), &
          "cell: sum of N = 1")
  CALL Display(SUM(elemsd%N, dim=1), "SUM elemsd%N: ")
  CALL OK(facetElemsd%nns .EQ. 4 .AND. facetElemsd%nips .EQ. 4, &
          "facet: nns = 4, nips = 4")
  CALL OK(facetElemsd%xidim .EQ. 2, "facet: xidim = 2")
  CALL OK(facetElemsd%nsd .EQ. 3, "facet: nsd = 3")
  CALL OK(ALL(ABS(SUM(facetElemsd%N, dim=1) - 1.0_DFP) .LT. tol), &
          "facet: sum of N = 1")
  DO ii = 1, 2
    CALL OK(ALL(ABS(SUM(facetElemsd%dNdXi(:, ii, :), dim=1)) .LT. tol), &
            "facet: sum of dN/dxi = 0")
  END DO
  CALL OK(ABS(SUM(facetElemsd%ws) - 4.0_DFP) .LT. tol, &
          "facet: sum of ws = 4")
END BLOCK

END PROGRAM main
