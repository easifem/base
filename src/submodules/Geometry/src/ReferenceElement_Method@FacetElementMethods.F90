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
  & FacetElements_Line

USE ReferenceTriangle_Method, ONLY: GetEdgeConnectivity_Triangle,  &
  & FacetElements_Triangle

USE ReferenceQuadrangle_Method, ONLY: GetEdgeConnectivity_Quadrangle,  &
  & FacetElements_Quadrangle

USE ReferenceTetrahedron_Method, ONLY: FacetElements_Tetrahedron

USE ReferenceTetrahedron_Method, ONLY: FacetElements_Tetrahedron

USE ReferenceHexahedron_Method, ONLY: FacetElements_Hexahedron

USE ReferencePrism_Method, ONLY: FacetElements_Prism

USE ReferencePyramid_Method, ONLY: FacetElements_Pyramid

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
!                                                             Facettopology
!----------------------------------------------------------------------------

MODULE PROCEDURE refelem_Facettopology
SELECT CASE (ElemType)
CASE (Line2)
  ALLOCATE (ans(2))
  ans(1)%nptrs = nptrs(1:1)
  ans(1)%name = point
  ans(1)%xiDimension = 0
  ans(2)%nptrs = nptrs(2:2)
  ans(2)%name = point
  ans(2)%xiDimension = 0

CASE (Line3)
  ALLOCATE (ans(2))
  ans(1)%nptrs = nptrs([1])
  ans(1)%name = point
  ans(1)%xiDimension = 0
  ans(2)%nptrs = nptrs([2])
  ans(2)%name = point
  ans(2)%xiDimension = 0

CASE (Line4)
  ALLOCATE (ans(2))
  ans(1)%nptrs = nptrs([1])
  ans(1)%name = point
  ans(1)%xiDimension = 0
  ans(2)%nptrs = nptrs([2])
  ans(2)%name = point
  ans(2)%xiDimension = 0

CASE (Line5)
  ALLOCATE (ans(2))
  ans(1)%nptrs = nptrs([1])
  ans(1)%name = point
  ans(1)%xiDimension = 0
  ans(2)%nptrs = nptrs([2])
  ans(2)%name = point
  ans(2)%xiDimension = 0

CASE (Line6)
  ALLOCATE (ans(2))
  ans(1)%nptrs = nptrs([1])
  ans(1)%name = point
  ans(1)%xiDimension = 0
  ans(2)%nptrs = nptrs([2])
  ans(2)%name = point
  ans(2)%xiDimension = 0

CASE (Triangle3)
  ALLOCATE (ans(3))
  ans(1)%nptrs = nptrs([1, 2])
  ans(2)%nptrs = nptrs([2, 3])
  ans(3)%nptrs = nptrs([3, 1])
  ans(1:3)%xiDimension = 1
  ans(1:3)%name = line2

CASE (Triangle6)
  ALLOCATE (ans(3))
  ans(1)%nptrs = nptrs([1, 2, 4])
  ans(2)%nptrs = nptrs([2, 3, 5])
  ans(3)%nptrs = nptrs([3, 1, 6])
  ans(1:3)%xiDimension = 1
  ans(1:3)%name = line3

CASE (Triangle9)
  ALLOCATE (ans(3))
  ans(1)%nptrs = nptrs([1, 2, 4, 5])
  ans(2)%nptrs = nptrs([2, 3, 6, 7])
  ans(3)%nptrs = nptrs([3, 1, 8, 9])
  ans(1:3)%xiDimension = 1
  ans(1:3)%name = line4

CASE (Triangle10)
  ALLOCATE (ans(3))
  ans(1)%nptrs = nptrs([1, 2, 4, 5])
  ans(2)%nptrs = nptrs([2, 3, 6, 7])
  ans(3)%nptrs = nptrs([3, 1, 8, 9])
  ans(1:3)%xiDimension = 1
  ans(1:3)%name = line4

CASE (Triangle12)
  ALLOCATE (ans(3))
  ans(1)%nptrs = nptrs([1, 2, 4, 5, 6])
  ans(2)%nptrs = nptrs([2, 3, 7, 8, 9])
  ans(3)%nptrs = nptrs([3, 1, 10, 11, 12])
  ans(1:3)%xiDimension = 1
  ans(1:3)%name = line5

CASE (Triangle15a)
  ALLOCATE (ans(3))
  ans(1)%nptrs = nptrs([1, 2, 4, 5, 6])
  ans(2)%nptrs = nptrs([2, 3, 7, 8, 9])
  ans(3)%nptrs = nptrs([3, 1, 10, 11, 12])
  ans(1:3)%xiDimension = 1
  ans(1:3)%name = line5

CASE (Quadrangle4)
  ALLOCATE (ans(4))
  ans(1)%nptrs = nptrs([1, 2])
  ans(2)%nptrs = nptrs([2, 3])
  ans(3)%nptrs = nptrs([3, 4])
  ans(4)%nptrs = nptrs([4, 1])
  ans(1:)%xiDimension = 1
  ans(1:)%name = line2

CASE (Quadrangle8)
  ALLOCATE (ans(4))
  ans(1)%nptrs = nptrs([1, 2, 5])
  ans(2)%nptrs = nptrs([2, 3, 6])
  ans(3)%nptrs = nptrs([3, 4, 7])
  ans(4)%nptrs = nptrs([4, 1, 8])
  ans(1:)%xiDimension = 1
  ans(1:)%name = line3

CASE (Quadrangle9)
  ALLOCATE (ans(4))
  ans(1)%nptrs = nptrs([1, 2, 5])
  ans(2)%nptrs = nptrs([2, 3, 6])
  ans(3)%nptrs = nptrs([3, 4, 7])
  ans(4)%nptrs = nptrs([4, 1, 8])
  ans(1:)%xiDimension = 1
  ans(1:)%name = line3

CASE (Tetrahedron4)
  ALLOCATE (ans(4))
  ans(1)%nptrs = nptrs([1, 2, 3])
  ans(2)%nptrs = nptrs([3, 1, 4])
  ans(3)%nptrs = nptrs([4, 2, 3])
  ans(4)%nptrs = nptrs([1, 2, 4])
  ans(:)%xiDimension = 2
  ans(:)%name = Triangle3

CASE (Tetrahedron10)
  ALLOCATE (ans(4))
  ans(1)%nptrs = nptrs([1, 2, 3, 5, 6, 7])
  ans(2)%nptrs = nptrs([3, 1, 4, 7, 8, 10])
  ans(3)%nptrs = nptrs([4, 2, 3, 9, 6, 10])
  ans(4)%nptrs = nptrs([1, 2, 4, 5, 9, 8])
  ans(:)%xiDimension = 2
  ans(:)%name = Triangle6

CASE (Prism6)
  ALLOCATE (ans(5))
  ans(1)%nptrs = nptrs([5, 4, 1, 2])
  ans(2)%nptrs = nptrs([4, 6, 3, 1])
  ans(3)%nptrs = nptrs([2, 3, 6, 5])
  ans(4)%nptrs = nptrs([1, 3, 2])
  ans(5)%nptrs = nptrs([4, 5, 6])
  ans(:)%xiDimension = 2
  ans(1:3)%name = Quadrangle4
  ans(4:5)%name = Triangle3

CASE (Pyramid5)
  ALLOCATE (ans(5))
  ans(1)%nptrs = nptrs([1, 2, 5])
  ans(2)%nptrs = nptrs([2, 3, 5])
  ans(3)%nptrs = nptrs([3, 4, 5])
  ans(4)%nptrs = nptrs([1, 5, 4])
  ans(5)%nptrs = nptrs([4, 3, 2, 1])
  ans(:)%xiDimension = 2
  ans(1:4)%name = Triangle3
  ans(5)%name = Quadrangle4
  ! Order=2 elements

CASE (Hexahedron8)
  ALLOCATE (ans(6))
  ans(1)%nptrs = nptrs([1, 4, 3, 2])
  ans(2)%nptrs = nptrs([1, 5, 8, 4])
  ans(3)%nptrs = nptrs([5, 6, 7, 8])
  ans(4)%nptrs = nptrs([2, 3, 7, 6])
  ans(5)%nptrs = nptrs([3, 4, 8, 7])
  ans(6)%nptrs = nptrs([1, 2, 6, 5])
  ans(:)%xiDimension = 2
  ans(:)%name = Quadrangle4

CASE (Hexahedron20)
  ALLOCATE (ans(6))
  ans(1)%nptrs = nptrs([1, 4, 3, 2, 10, 14, 12, 9])
  ans(2)%nptrs = nptrs([1, 5, 8, 4, 11, 18, 16, 10])
  ans(3)%nptrs = nptrs([5, 6, 7, 8, 17, 19, 20, 18])
  ans(4)%nptrs = nptrs([2, 3, 7, 6, 12, 15, 19, 13])
  ans(5)%nptrs = nptrs([3, 4, 8, 7, 14, 16, 20, 15])
  ans(6)%nptrs = nptrs([1, 2, 6, 5, 9, 13, 17, 11])
  ans(:)%xiDimension = 2
  ans(:)%name = Quadrangle8

CASE (Hexahedron27)
  ALLOCATE (ans(6))
  ans(1)%nptrs = nptrs([1, 4, 3, 2, 10, 14, 12, 9, 21])
  ans(2)%nptrs = nptrs([1, 5, 8, 4, 11, 18, 16, 10, 23])
  ans(3)%nptrs = nptrs([5, 6, 7, 8, 17, 19, 20, 18, 26])
  ans(4)%nptrs = nptrs([2, 3, 7, 6, 12, 15, 19, 13, 24])
  ans(5)%nptrs = nptrs([3, 4, 8, 7, 14, 16, 20, 15, 25])
  ans(6)%nptrs = nptrs([1, 2, 6, 5, 9, 13, 17, 11, 22])
  ans(:)%xiDimension = 2
  ans(:)%name = Quadrangle9

CASE (Prism15)
  ALLOCATE (ans(5))
  ans(1)%nptrs = nptrs([5, 4, 1, 2, 13, 9, 7, 11])
  ans(2)%nptrs = nptrs([4, 6, 3, 1, 14, 12, 8, 9])
  ans(3)%nptrs = nptrs([2, 3, 6, 5, 10, 12, 15, 11])
  ans(4)%nptrs = nptrs([1, 3, 2, 8, 10, 7])
  ans(5)%nptrs = nptrs([4, 5, 6, 13, 15, 14])
  ans(:)%xiDimension = 2
  ans(1:3)%name = Quadrangle8
  ans(4:5)%name = Triangle6

CASE (Prism18)
  ALLOCATE (ans(5))
  ans(1)%nptrs = nptrs([5, 4, 1, 2, 13, 9, 7, 11, 16])
  ans(2)%nptrs = nptrs([4, 6, 3, 1, 14, 12, 8, 9, 17])
  ans(3)%nptrs = nptrs([2, 3, 6, 5, 10, 12, 15, 11, 18])
  ans(4)%nptrs = nptrs([1, 3, 2, 8, 10, 7])
  ans(5)%nptrs = nptrs([4, 5, 6, 13, 15, 14])
  ans(:)%xiDimension = 2
  ans(1:3)%name = Quadrangle9
  ans(4:5)%name = Triangle6

CASE (Pyramid13)
  ALLOCATE (ans(5))
  ans(1)%nptrs = nptrs([1, 2, 5, 6, 10, 8])
  ans(2)%nptrs = nptrs([2, 3, 5, 9, 12, 10])
  ans(3)%nptrs = nptrs([3, 4, 5, 11, 13, 12])
  ans(4)%nptrs = nptrs([1, 5, 4, 8, 13, 7])
  ans(5)%nptrs = nptrs([4, 3, 2, 1, 11, 9, 6, 7])
  ans(:)%xiDimension = 2
  ans(1:4)%name = Triangle6
  ans(5)%name = Quadrangle8

CASE (Pyramid14)
  ALLOCATE (ans(5))
  ans(1)%nptrs = nptrs([1, 2, 5, 6, 10, 8])
  ans(2)%nptrs = nptrs([2, 3, 5, 9, 12, 10])
  ans(3)%nptrs = nptrs([3, 4, 5, 11, 13, 12])
  ans(4)%nptrs = nptrs([1, 5, 4, 8, 13, 7])
  ans(5)%nptrs = nptrs([4, 3, 2, 1, 11, 9, 6, 7, 13])
  ans(:)%xiDimension = 2
  ans(1:4)%name = Triangle6
  ans(5)%name = Quadrangle9

CASE (Triangle15b, Triangle21, Tetrahedron20, Tetrahedron35, &
  & Tetrahedron56, Hexahedron64, Hexahedron125)

END SELECT
END PROCEDURE refelem_Facettopology

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE FacetElementMethods
