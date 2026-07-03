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
! date: 2 March 2021
! summary: VTK format element

SUBMODULE(ReferenceElement_Method) VTKMethods
USE ArangeUtility, ONLY: arange
USE BaseType, ONLY: TypeElemNameOpt
USE BaseType, ONLY: TypePointNameOpt
USE BaseType, ONLY: TypeLineNameOpt
USE BaseType, ONLY: TypeTriangleNameOpt
USE BaseType, ONLY: TypeQuadrangleNameOpt
USE BaseType, ONLY: TypeTetrahedronNameOpt
USE BaseType, ONLY: TypeHexahedronNameOpt
USE BaseType, ONLY: TypePrismNameOpt
USE BaseType, ONLY: TypePyramidNameOpt
USE ReferenceElement_Method, ONLY: TotalNodesInElement
USE ReferenceElement_Method, ONLY: ElementTopology

IMPLICIT NONE

INTEGER(I4B), PARAMETER :: vtk_point = 1
INTEGER(I4B), PARAMETER :: vtk_line2 = 3
INTEGER(I4B), PARAMETER :: vtk_triangle3 = 5
INTEGER(I4B), PARAMETER :: vtk_quadrangle4 = 9
INTEGER(I4B), PARAMETER :: vtk_tetrahedron4 = 10
INTEGER(I4B), PARAMETER :: vtk_hexahedron8 = 12
INTEGER(I4B), PARAMETER :: vtk_prism6 = 13
INTEGER(I4B), PARAMETER :: vtk_pyramid5 = 14
INTEGER(I4B), PARAMETER :: vtk_line3 = 21
INTEGER(I4B), PARAMETER :: vtk_triangle6 = 22
INTEGER(I4B), PARAMETER :: vtk_quadrangle8 = 23
INTEGER(I4B), PARAMETER :: vtk_quadrangle9 = 28
INTEGER(I4B), PARAMETER :: vtk_tetrahedron10 = 24
INTEGER(I4B), PARAMETER :: vtk_hexahedron20 = 25
INTEGER(I4B), PARAMETER :: vtk_hexahedron27 = 29
INTEGER(I4B), PARAMETER :: vtk_prism15 = 26
INTEGER(I4B), PARAMETER :: vtk_prism18 = 32
INTEGER(I4B), PARAMETER :: vtk_line4 = 35
INTEGER(I4B), PARAMETER :: vtk_pyramid13 = 27
INTEGER(I4B), PARAMETER :: vtk_LagrangeCurve = 68
! any order line
INTEGER(I4B), PARAMETER :: vtk_LagrangeTriangle = 69
! any order triangle
INTEGER(I4B), PARAMETER :: vtk_LagrangeQuadrilateral = 70
! any order quadrilateral
INTEGER(I4B), PARAMETER :: vtk_LagrangeTetrahedron = 71
! any order tetrahedron
INTEGER(I4B), PARAMETER :: vtk_LagrangeHexahedron = 72
! any order hexahedron
! TODO: generic higher order elements
! INTEGER(I4B), PARAMETER :: vtk_HigherOrderCurve = 60
! INTEGER(I4B), PARAMETER :: vtk_HigherOrderTriangle = 61
! INTEGER(I4B), PARAMETER :: vtk_HigherOrderQuadrilateral = 62
! INTEGER(I4B), PARAMETER :: vtk_HigherOrderTetrahedron = 64
! INTEGER(I4B), PARAMETER :: vtk_HigherOrderWedge = 65
! INTEGER(I4B), PARAMETER :: vtk_HigherOrderPyramid = 66
 !!not implemented in paraview
! INTEGER(I4B), PARAMETER :: vtk_HigherOrderHexahedron = 67

CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE GetVTKElementType1
INTEGER(I4B) :: tsize

tsize = TotalNodesInElement(elemType)

IF (.NOT. ALLOCATED(nptrs)) ALLOCATE (nptrs(tsize))

CALL GetVTKElementType1_(elemType, vtk_type, nptrs, tsize)

END PROCEDURE GetVTKElementType1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE GetVTKElementType1_
INTEGER(I4B) :: topo

topo = ElementTopology(elemType)

SELECT CASE (topo)

CASE (TypeElemNameOpt%point)

  CALL GetVTKElementType_Point_(elemType, nptrs, vtk_type, tsize)

CASE (TypeElemNameOpt%line)

  CALL GetVTKElementType_Line_(elemType, nptrs, vtk_type, tsize)

CASE (TypeElemNameOpt%triangle)

  CALL GetVTKElementType_Triangle_(elemType, nptrs, vtk_type, tsize)

CASE (TypeElemNameOpt%quadrangle)

  CALL GetVTKElementType_Quadrangle_(elemType, nptrs, vtk_type, tsize)

CASE (TypeElemNameOpt%tetrahedron)

  CALL GetVTKElementType_Tetrahedron_(elemType, nptrs, vtk_type, tsize)

CASE (TypeElemNameOpt%hexahedron)

  CALL GetVTKElementType_Hexahedron_(elemType, nptrs, vtk_type, tsize)

CASE (TypeElemNameOpt%prism)

  CALL GetVTKElementType_Prism_(elemType, nptrs, vtk_type, tsize)

CASE (TypeElemNameOpt%pyramid)

  CALL GetVTKElementType_Pyramid_(elemType, nptrs, vtk_type, tsize)

CASE DEFAULT
  vtk_type = -1
  tsize = 0
END SELECT

END PROCEDURE GetVTKElementType1_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

PURE SUBROUTINE GetVTKElementType_Point_(elemType, nptrs, vtk_type, tsize)
  INTEGER(I4B), INTENT(IN) :: elemType
  INTEGER(I4B), INTENT(INOUT) :: nptrs(:)
  INTEGER(INT8), INTENT(OUT) :: vtk_type
  INTEGER(I4B), INTENT(OUT) :: tsize

  vtk_type = vtk_point
  tsize = 1
  nptrs(1:tsize) = [1]

END SUBROUTINE GetVTKElementType_Point_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

PURE SUBROUTINE GetVTKElementType_Line_(elemType, nptrs, vtk_type, tsize)
  INTEGER(I4B), INTENT(IN) :: elemType
  INTEGER(I4B), INTENT(INOUT) :: nptrs(:)
  INTEGER(INT8), INTENT(OUT) :: vtk_type
  INTEGER(I4B), INTENT(OUT) :: tsize

  tsize = TotalNodesInElement(elemType)

  SELECT CASE (elemType)
  CASE (TypeLineNameOpt%line2)
    vtk_type = vtk_line2
    nptrs(1:tsize) = [1, 2]
  CASE (TypeLineNameOpt%line3)
    vtk_type = vtk_line3
    nptrs(1:tsize) = [1, 2, 3]
  CASE (TypeLineNameOpt%line4)
    vtk_type = vtk_line4
    nptrs(1:tsize) = [1, 2, 3, 4]
  CASE default
    vtk_type = -1
    tsize = 0
  END SELECT

END SUBROUTINE GetVTKElementType_Line_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

PURE SUBROUTINE GetVTKElementType_Triangle_(elemType, nptrs, vtk_type, tsize)
  INTEGER(I4B), INTENT(IN) :: elemType
  INTEGER(I4B), INTENT(INOUT) :: nptrs(:)
  INTEGER(INT8), INTENT(OUT) :: vtk_type
  INTEGER(I4B), INTENT(OUT) :: tsize

  tsize = TotalNodesInElement(elemType)

  SELECT CASE (elemType)
  CASE (TypeTriangleNameOpt%triangle)
    vtk_type = vtk_triangle3
    nptrs(1:tsize) = [1, 2, 3]
  CASE (TypeTriangleNameOpt%triangle6)
    vtk_type = vtk_triangle6
    nptrs(1:tsize) = [1, 2, 3, 4, 5, 6]
  CASE default
    vtk_type = -1
    tsize = 0
  END SELECT

END SUBROUTINE GetVTKElementType_Triangle_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

PURE SUBROUTINE GetVTKElementType_Quadrangle_(elemType, nptrs, &
                                              vtk_type, tsize)
  INTEGER(I4B), INTENT(IN) :: elemType
  INTEGER(I4B), INTENT(INOUT) :: nptrs(:)
  INTEGER(INT8), INTENT(OUT) :: vtk_type
  INTEGER(I4B), INTENT(OUT) :: tsize

  tsize = TotalNodesInElement(elemType)

  SELECT CASE (elemType)
  CASE (TypeQuadrangleNameOpt%quadrangle4)
    vtk_type = vtk_quadrangle4
    nptrs(1:tsize) = [1, 2, 3, 4]

  CASE (TypeQuadrangleNameOpt%quadrangle8)
    vtk_type = vtk_Quadrangle8
    nptrs(1:tsize) = [1, 2, 3, 4, 5, 6, 7, 8]

  CASE (TypeQuadrangleNameOpt%quadrangle9)
    vtk_type = vtk_Quadrangle9
    nptrs(1:tsize) = [1, 2, 3, 4, 5, 6, 7, 8, 9]

  CASE (TypeQuadrangleNameOpt%quadrangle16)
    vtk_type = vtk_LagrangeQuadrilateral
    nptrs(1:tsize) = [1, 2, 3, 4, &
                      5, 6, & ! face 1
                      7, 8, & ! face 2
                      10, 9, & ! face 3
                      12, 11, & ! face 4
                      13, 14, & ! cell 1st row
                      16, 15] ! cell 2nd row

  CASE (TypeQuadrangleNameOpt%quadrangle25)
    vtk_type = vtk_LagrangeQuadrilateral
    nptrs(1:tsize) = [1, 2, 3, 4, &
                      5, 6, 7, & ! face 1
                      8, 9, 10, & ! face 2
                      13, 12, 11, & ! face 3
                      16, 15, 14, & ! face 4
                      17, 21, 18, & ! cell 1st row
                      24, 25, 22, & ! cell 2nd row
                      20, 23, 19] ! cell 3rd row

  CASE (TypeQuadrangleNameOpt%quadrangle36)
    vtk_type = vtk_LagrangeQuadrilateral
    nptrs(1:tsize) = [1, 2, 3, 4, & ! vertex
                      5, 6, 7, 8, & ! face 1
                      9, 10, 11, 12, & ! face 2
                      16, 15, 14, 13, & ! face 3
                      20, 19, 18, 17, & ! face 4
                      21, 25, 26, 22, & ! cell 1st row
                      32, 33, 34, 27, & ! cell 2nd row
                      31, 36, 35, 28, & ! cell 3rd row
                      24, 30, 29, 23] ! cell 4th row

  CASE (TypeQuadrangleNameOpt%quadrangle49)
    vtk_type = vtk_LagrangeQuadrilateral
    nptrs(1:tsize) = [1, 2, 3, 4, &
                      5, 6, 7, 8, 9, & ! face 1
                      10, 11, 12, 13, 14, & ! face 2
                      19, 18, 17, 16, 15, & ! face 3
                      24, 23, 22, 21, 20, & ! face 4
                      25, 29, 30, 31, 26, & ! cell 1st row
                      40, 41, 45, 42, 32, & ! cell 2nd row
                      39, 48, 49, 46, 33, & ! cell 3rd row
                      38, 44, 47, 43, 34, & ! cell 4th row
                      28, 37, 36, 35, 27] ! cell 5th row

  CASE (TypeQuadrangleNameOpt%quadrangle64)
    vtk_type = vtk_LagrangeQuadrilateral
    nptrs(1:tsize) = [1, 2, 3, 4, & ! vertex
                      5, 6, 7, 8, 9, 10, & ! face 1
                      11, 12, 13, 14, 15, 16, & ! face 2
                      22, 21, 20, 19, 18, 17, & ! face 3
                      28, 27, 26, 25, 24, 23, & ! face 4
                      29, 33, 34, 35, 36, 30, & ! cell 1st row
                      48, 49, 53, 54, 50, 37, & ! cell 2nd row
                      47, 60, 61, 62, 55, 38, & ! cell 3rd row
                      46, 59, 64, 63, 56, 39, & ! cell 4th row
                      45, 52, 58, 57, 51, 40, & ! cell 5th row
                      32, 44, 43, 42, 41, 31] ! cell 6th row

  CASE (TypeQuadrangleNameOpt%quadrangle81)
    vtk_type = vtk_LagrangeQuadrilateral
    nptrs(1:tsize) = [1, 2, 3, 4, & ! vertex
                      5, 6, 7, 8, 9, 10, 11, & ! face 1
                      12, 13, 14, 15, 16, 17, 18, & ! face 2
                      25, 24, 23, 22, 21, 20, 19, & ! face 3
                      32, 31, 30, 29, 28, 27, 26, & ! face 4
                      33, 37, 38, 39, 40, 41, 34, & ! cell 1st row
                      56, 57, 61, 62, 63, 58, 42, & ! cell 2nd row
                      55, 72, 73, 77, 74, 64, 43, & ! cell 3rd row
                      54, 71, 80, 81, 78, 65, 44, & ! cell 4th row
                      53, 70, 76, 79, 75, 66, 45, & ! cell 5th row
                      52, 60, 69, 68, 67, 59, 46, & ! cell 6th row
                      36, 51, 50, 49, 48, 47, 35] ! cell 7th row

  CASE (TypeQuadrangleNameOpt%quadrangle100)
    vtk_type = vtk_LagrangeQuadrilateral
    nptrs(1:tsize) = [1, 2, 3, 4, & ! vertex
                      5, 6, 7, 8, 9, 10, 11, 12, & ! face 1
                      13, 14, 15, 16, 17, 18, 19, 20, & ! face 2
                      28, 27, 26, 25, 24, 23, 22, 21, & ! face 3
                      36, 35, 34, 33, 32, 31, 30, 29, & ! face 4
                      37, 41, 42, 43, 44, 45, 46, 38, & ! cell 1st row
                      64, 65, 69, 70, 71, 72, 66, 47, & ! cell 2nd row
                      63, 84, 85, 89, 90, 86, 73, 48, & ! cell 3rd row
                      62, 83, 96, 97, 98, 91, 74, 49, & ! cell 4th row
                      61, 82, 95, 100, 99, 92, 75, 50, & ! cell 5th row
                      60, 81, 88, 94, 93, 87, 76, 51, & ! cell 6th row
                      59, 68, 80, 79, 78, 77, 67, 52, & ! cell 7th row
                      40, 58, 57, 56, 55, 54, 53, 39] ! cell 8th row

  CASE (TypeQuadrangleNameOpt%quadrangle121)
    vtk_type = vtk_LagrangeQuadrilateral
    nptrs(1:tsize) = [1, 2, 3, 4, & ! vertex
                      5, 6, 7, 8, 9, 10, 11, 12, 13, & ! face 1
                      14, 15, 16, 17, 18, 19, 20, 21, 22, & ! face 2
                      31, 30, 29, 28, 27, 26, 25, 24, 23, & ! face 3
                      40, 39, 38, 37, 36, 35, 34, 33, 32, & ! face 4
                      41, 45, 46, 47, 48, 49, 50, 51, 42, & ! cell 1st row
                      72, 73, 77, 78, 79, 80, 81, 74, 52, & ! cell 2nd row
                      71, 96, 97, 101, 102, 103, 98, 82, 53, & ! cell 3rd row
                      70, 95, 112, 113, 117, 114, 104, 83, 54, & ! cell 4th row
                      69, 94, 111, 120, 121, 118, 105, 84, 55, & ! cell 5th row
                      68, 93, 110, 116, 119, 115, 106, 85, 56, & ! cell 6th row
                      67, 92, 100, 109, 108, 107, 99, 86, 57, & ! cell 7th row
                      66, 76, 91, 90, 89, 88, 87, 75, 58, & ! cell 8th row
                      44, 65, 64, 63, 62, 61, 60, 59, 43] ! cell 9th row

  CASE DEFAULT
    vtk_type = -1
    tsize = 0
  END SELECT

END SUBROUTINE GetVTKElementType_Quadrangle_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

PURE SUBROUTINE GetVTKElementType_Tetrahedron_(elemType, nptrs, &
                                               vtk_type, tsize)
  INTEGER(I4B), INTENT(IN) :: elemType
  INTEGER(I4B), INTENT(INOUT) :: nptrs(:)
  INTEGER(INT8), INTENT(OUT) :: vtk_type
  INTEGER(I4B), INTENT(OUT) :: tsize

  tsize = TotalNodesInElement(elemType)

  SELECT CASE (elemType)
  CASE (TypeTetrahedronNameOpt%tetrahedron4)
    vtk_type = vtk_Tetrahedron4
    nptrs(1:tsize) = [1, 2, 3, 4]

  CASE (TypeTetrahedronNameOpt%tetrahedron10)
    vtk_type = vtk_Tetrahedron10
    tsize = 10
    nptrs(1:tsize) = 1 + [0, 1, 2, 3, 4, 5, 6, 7, 9, 8]

  CASE default
    vtk_type = -1
    tsize = 0
  END SELECT

END SUBROUTINE GetVTKElementType_Tetrahedron_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

PURE SUBROUTINE GetVTKElementType_Hexahedron_(elemType, nptrs, &
                                              vtk_type, tsize)
  INTEGER(I4B), INTENT(IN) :: elemType
  INTEGER(I4B), INTENT(INOUT) :: nptrs(:)
  INTEGER(INT8), INTENT(OUT) :: vtk_type
  INTEGER(I4B), INTENT(OUT) :: tsize

  tsize = TotalNodesInElement(elemType)

  SELECT CASE (elemType)
  CASE (TypeHexahedronNameOpt%hexahedron8)
    vtk_type = vtk_Hexahedron8
    nptrs(1:tsize) = [1, 2, 3, 4, 5, 6, 7, 8]

  CASE (TypeHexahedronNameOpt%hexahedron27)
    vtk_type = vtk_Hexahedron27
    nptrs(1:tsize) = 1 + [0, 1, 2, 3, 4, 5, 6, 7, &
                          8, 11, 16, 9, 17, 10, 18, 19, 12, 15, 13, 14, &
                          24, 22, 20, 21, 23, 25, 26]

  CASE (TypeHexahedronNameOpt%hexahedron20)
    vtk_type = vtk_Hexahedron20
    nptrs(1:tsize) = 1 + [0, 1, 2, 3, 4, 5, 6, 7, &
                          8, 11, 16, 9, 17, 10, 18, 19, 12, 15, 13, 14]

  CASE default
    vtk_type = -1
    tsize = 0
  END SELECT

END SUBROUTINE GetVTKElementType_Hexahedron_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

PURE SUBROUTINE GetVTKElementType_Prism_(elemType, nptrs, &
                                         vtk_type, tsize)
  INTEGER(I4B), INTENT(IN) :: elemType
  INTEGER(I4B), INTENT(INOUT) :: nptrs(:)
  INTEGER(INT8), INTENT(OUT) :: vtk_type
  INTEGER(I4B), INTENT(OUT) :: tsize

  tsize = TotalNodesInElement(elemType)

  SELECT CASE (elemType)
  CASE (TypePrismNameOpt%prism6)
    vtk_type = vtk_Prism6
    nptrs(1:tsize) = [1, 2, 3, 4, 5, 6]

  CASE (TypePrismNameOpt%prism15)
    vtk_type = vtk_Prism15
    nptrs(1:tsize) = 1 + [0, 1, 2, 3, 4, 5, &
                          6, 8, 12, 7, 13, 14, 9, 11, 10]

  CASE (TypePrismNameOpt%prism18)
    vtk_type = vtk_Prism18
    nptrs(1:tsize) = 1 + [0, 1, 2, 3, 4, 5, &
                          6, 8, 12, 7, 13, 14, 9, 11, 10, &
                          15, 17, 16]
  CASE default
    vtk_type = -1
    tsize = 0
  END SELECT

END SUBROUTINE GetVTKElementType_Prism_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

PURE SUBROUTINE GetVTKElementType_Pyramid_(elemType, nptrs, &
                                           vtk_type, tsize)
  INTEGER(I4B), INTENT(IN) :: elemType
  INTEGER(I4B), INTENT(INOUT) :: nptrs(:)
  INTEGER(INT8), INTENT(OUT) :: vtk_type
  INTEGER(I4B), INTENT(OUT) :: tsize

  tsize = TotalNodesInElement(elemType)

  SELECT CASE (elemType)
  CASE (TypePyramidNameOpt%pyramid5)
    vtk_type = vtk_Pyramid5
    nptrs(1:tsize) = [1, 2, 3, 4, 5]

  CASE (TypePyramidNameOpt%pyramid13)
    vtk_type = vtk_Pyramid13
    tsize = 13
    nptrs(1:tsize) = 1 + [0, 1, 2, 3, 4, 5, &
                          5, 8, 9, 6, 10, 7, 11, 12]

  CASE (TypePyramidNameOpt%pyramid14)
    vtk_type = vtk_Pyramid13
    tsize = 14
    nptrs(1:tsize) = 1 + [0, 1, 2, 3, 4, 5, &
                          5, 8, 9, 6, 10, 7, 11, 12]
  CASE default
    vtk_type = -1
    tsize = 0
  END SELECT

END SUBROUTINE GetVTKElementType_Pyramid_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE VTKMethods
