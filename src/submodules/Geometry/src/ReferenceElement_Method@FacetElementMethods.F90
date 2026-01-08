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
USE BaseType, ONLY: TypeElemNameOpt
USE ErrorHandling, ONLY: Errormsg
USE LineInterpolationUtility, ONLY: InterpolationPoint_Line
USE QuadrangleInterpolationUtility, ONLY: InterpolationPoint_Quadrangle
USE ReferenceHexahedron_Method, ONLY: FacetElements_Hexahedron
USE ReferenceHexahedron_Method, ONLY: FacetTopology_Hexahedron
USE ReferenceLine_Method, ONLY: DEFAULT_REF_LINE_COORD
USE ReferenceLine_Method, ONLY: FacetElements_Line
USE ReferenceLine_Method, ONLY: FacetTopology_Line
USE ReferencePrism_Method, ONLY: FacetElements_Prism
USE ReferencePrism_Method, ONLY: FacetTopology_Prism
USE ReferencePyramid_Method, ONLY: FacetElements_Pyramid
USE ReferencePyramid_Method, ONLY: FacetTopology_Pyramid
USE ReferenceQuadrangle_Method, ONLY: FacetElements_Quadrangle
USE ReferenceQuadrangle_Method, ONLY: FacetTopology_Quadrangle
USE ReferenceQuadrangle_Method, ONLY: GetEdgeConnectivity_Quadrangle
USE ReferenceTetrahedron_Method, ONLY: FacetElements_Tetrahedron
USE ReferenceTetrahedron_Method, ONLY: FacetTopology_Tetrahedron
USE ReferenceTriangle_Method, ONLY: FacetElements_Triangle
USE ReferenceTriangle_Method, ONLY: FacetTopology_Triangle
USE ReferenceTriangle_Method, ONLY: GetEdgeConnectivity_Triangle
USE TriangleInterpolationUtility, ONLY: InterpolationPoint_Triangle

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

! CASE (2, 3)
CASE DEFAULT
  tFacet = refelem%entityCounts(xicell)
  istart = T(xicell) + 1
  iend = T(xicell) + tFacet
  max_nns = 0
  DO i = istart, iend
    nns = SIZE(refelem%topology(i)%nptrs)
    IF (max_nns < nns) max_nns = nns
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
CASE (TypeElemNameOpt%Line)
  CALL FacetElements_Line(refelem=refelem, ans=ans)

CASE (TypeElemNameOpt%Triangle)
  CALL FacetElements_Triangle(refelem=refelem, ans=ans)

CASE (TypeElemNameOpt%Quadrangle)
  CALL FacetElements_Quadrangle(refelem=refelem, ans=ans)

CASE (TypeElemNameOpt%Tetrahedron)
  CALL FacetElements_Tetrahedron(refelem=refelem, ans=ans)

CASE (TypeElemNameOpt%Hexahedron)
  CALL FacetElements_Hexahedron(refelem=refelem, ans=ans)

CASE (TypeElemNameOpt%Prism)
  CALL FacetElements_Prism(refelem=refelem, ans=ans)

CASE (TypeElemNameOpt%Pyramid)
  CALL FacetElements_Pyramid(refelem=refelem, ans=ans)

CASE DEFAULT

END SELECT

END PROCEDURE refelem_GetFacetElements1

!----------------------------------------------------------------------------
!                                                             FacetElements
!----------------------------------------------------------------------------

MODULE PROCEDURE refelem_GetFacetElements2
INTEGER(I4B) :: topo

topo = ElementTopology(elemType)

SELECT CASE (topo)
CASE (TypeElemNameOpt%Line)
  CALL FacetElements_Line(elemType=elemType, nsd=nsd, ans=ans)

CASE (TypeElemNameOpt%Triangle)
  CALL FacetElements_Triangle(elemType=elemType, nsd=nsd, ans=ans)

CASE (TypeElemNameOpt%Quadrangle)
  CALL FacetElements_Quadrangle(elemType=elemType, nsd=nsd, ans=ans)

CASE (TypeElemNameOpt%Tetrahedron)
  CALL FacetElements_Tetrahedron(elemType=elemType, nsd=nsd, ans=ans)

CASE (TypeElemNameOpt%Hexahedron)
  CALL FacetElements_Hexahedron(elemType=elemType, nsd=nsd, ans=ans)

CASE (TypeElemNameOpt%Prism)
  CALL FacetElements_Prism(elemType=elemType, nsd=nsd, ans=ans)

CASE (TypeElemNameOpt%Pyramid)
  CALL FacetElements_Pyramid(elemType=elemType, nsd=nsd, ans=ans)

CASE DEFAULT

END SELECT

END PROCEDURE refelem_GetFacetElements2

!----------------------------------------------------------------------------
!                                                         GetFacetTopology
!----------------------------------------------------------------------------

MODULE PROCEDURE refelem_GetFacettopology
INTEGER(I4B) :: topo
topo = ElementTopology(elemType)

SELECT CASE (topo)
CASE (TypeElemNameOpt%Line)
  CALL FacetTopology_Line(elemType=elemType, nptrs=nptrs, ans=ans)

CASE (TypeElemNameOpt%Triangle)
  CALL FacetTopology_Triangle(elemType=elemType, nptrs=nptrs, ans=ans)

CASE (TypeElemNameOpt%Quadrangle)
  CALL FacetTopology_Quadrangle(elemType=elemType, nptrs=nptrs, ans=ans)

CASE (TypeElemNameOpt%Tetrahedron)
  CALL FacetTopology_Tetrahedron(elemType=elemType, nptrs=nptrs, ans=ans)

CASE (TypeElemNameOpt%Prism)
  CALL FacetTopology_Prism(elemType=elemType, nptrs=nptrs, ans=ans)

CASE (TypeElemNameOpt%Pyramid)
  CALL FacetTopology_Pyramid(elemType=elemType, nptrs=nptrs, ans=ans)

CASE (TypeElemNameOpt%Hexahedron)
  CALL FacetTopology_Hexahedron(elemType=elemType, nptrs=nptrs, ans=ans)

CASE DEFAULT

END SELECT
END PROCEDURE refelem_GetFacettopology

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE FacetElementMethods
