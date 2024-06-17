! This program is a part of EASIFEM library
! Copyright (C) 2020-2021  Vikas Sharma, Ph.D
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

!> author: Vikas Sharma, Ph. D.
! date: 1 March 2021
! summary: This sumodule contains method for geometry

SUBMODULE(ReferenceElement_Method) FacetElementMethods
USE ReferenceLine_Method, ONLY: DEFAULT_REF_LINE_COORD,  &
  & FacetElements_Line, &
  & FacetTopology_Line

USE ReferenceTriangle_Method, ONLY: GetEdgeConnectivity_Triangle,  &
  & FacetElements_Triangle, &
  & FacetTopology_Triangle

USE ReferenceQuadrangle_Method, ONLY: GetEdgeConnectivity_Quadrangle,  &
  & FacetElements_Quadrangle, &
  & FacetTopology_Quadrangle

USE ReferenceTetrahedron_Method, ONLY: FacetElements_Tetrahedron, &
  & FacetTopology_Tetrahedron

USE ReferenceTetrahedron_Method, ONLY: FacetElements_Tetrahedron, &
  & FacetTopology_Tetrahedron

USE ReferenceHexahedron_Method, ONLY: FacetElements_Hexahedron, &
  & FacetTopology_Hexahedron

USE ReferencePrism_Method, ONLY: FacetElements_Prism, &
  & FacetTopology_Prism

USE ReferencePyramid_Method, ONLY: FacetElements_Pyramid, &
  & FacetTopology_Pyramid

USE LineInterpolationUtility, ONLY: InterpolationPoint_Line
USE TriangleInterpolationUtility, ONLY: InterpolationPoint_Triangle
USE QuadrangleInterpolationUtility, ONLY: InterpolationPoint_Quadrangle
! USE TetrahedronInterpolationUtility
! USE HexahedronInterpolationUtility
! USE PrismInterpolationUtility
! USE PyramidInterpolationUtility

USE ErrorHandling

USE ReallocateUtility

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                               FacetMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE Facet_Matrix_refelem
INTEGER(I4B) :: xicell, T(4), i, istart, iend, max_nns, nns, tFacet
T(1) = 0
DO i = 2, 4
  T(i) = SUM(refelem%entityCounts(1:i - 1))
END DO

xicell = refelem%xiDimension

SELECT CASE (xicell)
CASE (1)
  tFacet = 2
  istart = 1
  iend = 2
  max_nns = 2
  ALLOCATE (FM(tFacet, max_nns + 3))
  FM = 0
  DO i = 0, tFacet - 1
    FM(i + 1, 1) = refelem%topology(istart + i)%name
    FM(i + 1, 2) = refelem%topology(istart + i)%xiDimension
    nns = SIZE(refelem%topology(istart + i)%nptrs)
    FM(i + 1, 3) = nns
    FM(i + 1, 4:(3 + nns)) = refelem%topology(istart + i)%nptrs
  END DO

CASE (2, 3)
  tFacet = refelem%entityCounts(xicell)
  istart = T(xicell) + 1
  iend = T(xicell) + tFacet
  max_nns = 0
  DO i = istart, iend
    nns = SIZE(refelem%topology(i)%nptrs)
    IF (max_nns .LT. nns) max_nns = nns
  END DO
  ALLOCATE (FM(tFacet, max_nns + 3))
  FM = 0
  DO i = 0, tFacet - 1
    FM(i + 1, 1) = refelem%topology(istart + i)%name
    FM(i + 1, 2) = refelem%topology(istart + i)%xiDimension
    nns = SIZE(refelem%topology(istart + i)%nptrs)
    FM(i + 1, 3) = nns
    FM(i + 1, 4:(3 + nns)) = refelem%topology(istart + i)%nptrs
  END DO
END SELECT

END PROCEDURE Facet_Matrix_refelem

!----------------------------------------------------------------------------
!                                                             FacetElements
!----------------------------------------------------------------------------

MODULE PROCEDURE refelem_GetFacetElements1
INTEGER(I4B) :: topo

topo = ElementTopology(refelem)

SELECT CASE (topo)
CASE (Line)
  CALL FacetElements_Line(refelem=refelem, ans=ans)

CASE (Triangle)
  CALL FacetElements_Triangle(refelem=refelem, ans=ans)

CASE (Quadrangle)
  CALL FacetElements_Quadrangle(refelem=refelem, ans=ans)

CASE (Tetrahedron)
  CALL FacetElements_Tetrahedron(refelem=refelem, ans=ans)

CASE (Hexahedron)
  CALL FacetElements_Hexahedron(refelem=refelem, ans=ans)

CASE (Prism)
  CALL FacetElements_Prism(refelem=refelem, ans=ans)

CASE (Pyramid)
  CALL FacetElements_Pyramid(refelem=refelem, ans=ans)

END SELECT

END PROCEDURE refelem_GetFacetElements1

!----------------------------------------------------------------------------
!                                                             FacetElements
!----------------------------------------------------------------------------

MODULE PROCEDURE refelem_GetFacetElements2
INTEGER(I4B) :: topo

topo = ElementTopology(elemType)

SELECT CASE (topo)
CASE (Line)
  CALL FacetElements_Line(elemType=elemType, nsd=nsd, ans=ans)

CASE (Triangle)
  CALL FacetElements_Triangle(elemType=elemType, nsd=nsd, ans=ans)

CASE (Quadrangle)
  CALL FacetElements_Quadrangle(elemType=elemType, nsd=nsd, ans=ans)

CASE (Tetrahedron)
  CALL FacetElements_Tetrahedron(elemType=elemType, nsd=nsd, ans=ans)

CASE (Hexahedron)
  CALL FacetElements_Hexahedron(elemType=elemType, nsd=nsd, ans=ans)

CASE (Prism)
  CALL FacetElements_Prism(elemType=elemType, nsd=nsd, ans=ans)

CASE (Pyramid)
  CALL FacetElements_Pyramid(elemType=elemType, nsd=nsd, ans=ans)

END SELECT

END PROCEDURE refelem_GetFacetElements2

!----------------------------------------------------------------------------
!                                                         GetFacetTopology
!----------------------------------------------------------------------------

MODULE PROCEDURE refelem_GetFacettopology
INTEGER(I4B) :: topo
topo = ElementTopology(elemType)

SELECT CASE (topo)
CASE (Line)
  CALL FacetTopology_Line(elemType=elemType, nptrs=nptrs, ans=ans)

CASE (Triangle)
  CALL FacetTopology_Triangle(elemType=elemType, nptrs=nptrs, ans=ans)

CASE (Quadrangle)
  CALL FacetTopology_Quadrangle(elemType=elemType, nptrs=nptrs, ans=ans)

CASE (Tetrahedron)
  CALL FacetTopology_Tetrahedron(elemType=elemType, nptrs=nptrs, ans=ans)

CASE (Prism)
  CALL FacetTopology_Prism(elemType=elemType, nptrs=nptrs, ans=ans)

CASE (Pyramid)
  CALL FacetTopology_Pyramid(elemType=elemType, nptrs=nptrs, ans=ans)

CASE (Hexahedron)
  CALL FacetTopology_Hexahedron(elemType=elemType, nptrs=nptrs, ans=ans)

END SELECT
END PROCEDURE refelem_GetFacettopology

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE FacetElementMethods
