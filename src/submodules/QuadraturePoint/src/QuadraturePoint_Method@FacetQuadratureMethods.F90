! This program is a part of EASIFEM library
! Expandable And Scalable Infrastructure for Finite Element Methods
! htttps://www.easifem.com
!
! This program is free software: you can redistribute it and/or modify
! it under the terms of the GNU General Public License as published by
! the Free Software Foundation, either version 3 of the License, or
! (at your option) any later version.
!
! This program is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.
!
! You should have received a copy of the GNU General Public License
! along with this program.  If not, see <https: //www.gnu.org/licenses/>
!

SUBMODULE(QuadraturePoint_Method) FacetQuadratureMethods
USE GlobalData, ONLY: stderr
USE ErrorHandling, ONLY: ErrorMsg
USE BaseInterpolation_Method, ONLY: InterpolationPoint_ToChar
USE BaseInterpolation_Method, ONLY: InterpolationPoint_ToInteger
USE BaseInterpolation_Method, ONLY: InterpolationPoint_ToString
USE ReallocateUtility, ONLY: Reallocate
USE ReferenceElement_Method, ONLY: ElementTopology
USE ReferenceElement_Method, ONLY: XiDimension
USE BaseType, ONLY: ReferenceElementInfo => TypeRefelemOpt
USE LineInterpolationUtility, ONLY: QuadratureNumber_Line
USE LineInterpolationUtility, ONLY: QuadraturePoint_Line_
USE TriangleInterpolationUtility, ONLY: QuadraturePoint_Triangle_
USE TriangleInterpolationUtility, ONLY: QuadratureNumber_Triangle
USE TriangleInterpolationUtility, ONLY: FacetConnectivity_Triangle
USE QuadrangleInterpolationUtility, ONLY: QuadraturePoint_Quadrangle_
USE QuadrangleInterpolationUtility, ONLY: QuadratureNumber_Quadrangle
USE QuadrangleInterpolationUtility, ONLY: FacetConnectivity_Quadrangle
USE TetrahedronInterpolationUtility, ONLY: QuadraturePoint_Tetrahedron_
USE TetrahedronInterpolationUtility, ONLY: QuadratureNumber_Tetrahedron
USE HexahedronInterpolationUtility, ONLY: QuadraturePoint_Hexahedron_
USE HexahedronInterpolationUtility, ONLY: QuadratureNumber_Hexahedron
USE BaseType, ONLY: elem => TypeElemNameOpt
USE MappingUtility, ONLY: FromBiUnitLine2Segment_

IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                     InitiateFacetQuadrature
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_InitiateFacetQuadrature1
CALL InitiateFacetQuadrature( &
  obj=obj, facetQuad=facetQuad, localFaceNumber=localFaceNumber, &
  elemType=elemtype, domainName=domainname, p=order, q=order, r=order, &
  quadratureType1=quadratureType, quadratureType2=quadratureType, &
  quadratureType3=quadratureType, alpha1=alpha, beta1=beta, lambda1=lambda, &
  alpha2=alpha, beta2=beta, lambda2=lambda, alpha3=alpha, beta3=beta, &
  lambda3=lambda, xij=xij)
END PROCEDURE obj_InitiateFacetQuadrature1

!----------------------------------------------------------------------------
!                                                     InitiateFacetQuadrature
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_InitiateFacetQuadrature2
CALL InitiateFacetQuadrature(obj=obj, facetQuad=facetQuad, &
                             localFaceNumber=localFaceNumber, &
                             elemType=elemtype, domainName=domainName, &
                             nipsx=nips, nipsy=nips, nipsz=nips, &
                             quadratureType1=quadratureType, &
                             quadratureType2=quadratureType, &
                             quadratureType3=quadratureType, &
                             alpha1=alpha, beta1=beta, lambda1=lambda, &
                             alpha2=alpha, beta2=beta, lambda2=lambda, &
                             alpha3=alpha, beta3=beta, lambda3=lambda, &
                             xij=xij)
END PROCEDURE obj_InitiateFacetQuadrature2

!----------------------------------------------------------------------------
!                                                     InitiateFacetQuadrature
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_InitiateFacetQuadrature3
INTEGER(I4B) :: topo, nrow, ncol, nipsx(1), nsd
INTEGER(I4B) :: facecon(ReferenceElementInfo%maxPoints, &
                        ReferenceElementInfo%maxEdges)
REAL(DFP) :: x1(3), x2(3)

topo = ElementTopology(elemType)

SELECT CASE (topo)

CASE (elem%triangle)

  nsd = SIZE(xij, 1)
  nrow = nsd + 1
  nipsx(1) = QuadratureNumber_Line(order=p, quadtype=quadratureType1)
  ncol = nipsx(1)

  CALL Reallocate(obj%points, nrow, ncol)
  CALL Reallocate(facetQuad%points, 2, ncol)

  ! Get quadrature points on [-1, 1]
  CALL QuadraturePoint_Line_(nips=nipsx, &
                             quadType=quadratureType1, &
                             layout="INCREASING", &
                             alpha=alpha1, &
                             beta=beta1, &
                             lambda=lambda1, &
                             ans=facetQuad%points, &
                             nrow=nrow, ncol=ncol)

  facecon(1:2, 1:3) = FacetConnectivity_Triangle()
  x1(1:nsd) = xij(1:nsd, facecon(1, localFaceNumber))
  x2(1:nsd) = xij(1:nsd, facecon(2, localFaceNumber))

  ! Map quadrature points from[-1, 1] to the face of quadrangle
  CALL FromBiUnitLine2Segment_(xin=facetQuad%points(1, :), &
                               x1=x1(1:nsd), &
                               x2=x2(1:nsd), &
                               ans=obj%points, &
                               nrow=nrow, ncol=ncol)

  obj%txi = SIZE(obj%points, 1) - 1
  facetQuad%txi = SIZE(facetQuad%points, 1) - 1

  CALL GetQuadratureWeights_(obj=facetQuad, &
                             weights=obj%points(obj%txi + 1, :), &
                             tsize=ncol)

CASE (elem%quadrangle)

  nsd = SIZE(xij, 1)
  nrow = nsd + 1
  nipsx(1) = QuadratureNumber_Line(order=p, quadtype=quadratureType1)
  ncol = nipsx(1)

  CALL Reallocate(obj%points, nrow, ncol)
  CALL Reallocate(facetQuad%points, 2, ncol)

  ! Get quadrature points on [-1, 1]
  CALL QuadraturePoint_Line_(nips=nipsx, &
                             quadType=quadratureType1, &
                             layout="INCREASING", &
                             alpha=alpha1, &
                             beta=beta1, &
                             lambda=lambda1, &
                             ans=facetQuad%points, &
                             nrow=nrow, ncol=ncol)

  facecon(1:2, 1:4) = FacetConnectivity_Quadrangle()
  x1(1:nsd) = xij(1:nsd, facecon(1, localFaceNumber))
  x2(1:nsd) = xij(1:nsd, facecon(2, localFaceNumber))

  ! Map quadrature points from[-1, 1] to the face of quadrangle
  CALL FromBiUnitLine2Segment_(xin=facetQuad%points(1, :), &
                               x1=x1(1:nsd), &
                               x2=x2(1:nsd), &
                               ans=obj%points, &
                               nrow=nrow, ncol=ncol)

  obj%txi = SIZE(obj%points, 1) - 1
  facetQuad%txi = SIZE(facetQuad%points, 1) - 1

  CALL GetQuadratureWeights_(obj=facetQuad, &
                             weights=obj%points(obj%txi + 1, :), &
                             tsize=ncol)

CASE DEFAULT
#ifdef DEBUG_VER
  CALL Errormsg(msg="No case found for give topo", &
                file=__FILE__, routine="obj_Initiate11()", &
                line=__LINE__, unitno=stderr)
  STOP
#endif

END SELECT
END PROCEDURE obj_InitiateFacetQuadrature3

!----------------------------------------------------------------------------
!                                                     InitiateFacetQuadrature
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_InitiateFacetQuadrature4

END PROCEDURE obj_InitiateFacetQuadrature4

!----------------------------------------------------------------------------
!                                                            Include Error
!----------------------------------------------------------------------------

END SUBMODULE FacetQuadratureMethods
