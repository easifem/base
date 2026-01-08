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
USE BaseType, ONLY: TypePointNameOpt
USE BaseType, ONLY: TypeLineNameOpt
USE BaseType, ONLY: TypeTriangleNameOpt
USE BaseType, ONLY: TypeQuadrangleNameOpt
USE BaseType, ONLY: TypeTetrahedronNameOpt
USE BaseType, ONLY: TypeHexahedronNameOpt
USE BaseType, ONLY: TypePrismNameOpt
USE BaseType, ONLY: TypePyramidNameOpt

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
INTEGER(I4B), PARAMETER :: vtk_quadrangle16 = 70

! VTK_LAGRANGE_QUADRILATERAL
CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE GetVTKElementType1

SELECT CASE (elemType)
CASE (TypePointNameOpt%point)
  vtk_type = vtk_point
  nptrs = [1]

CASE (TypeLineNameOpt%line)
  vtk_type = vtk_line2
  nptrs = [1, 2]

CASE (TypeTriangleNameOpt%triangle)
  vtk_type = vtk_triangle3
  nptrs = [1, 2, 3]

CASE (TypeQuadrangleNameOpt%quadrangle)
  vtk_type = vtk_quadrangle4
  nptrs = [1, 2, 3, 4]

CASE (TypeTetrahedronNameOpt%tetrahedron)
  vtk_type = vtk_tetrahedron4
  nptrs = [1, 2, 3, 4]

CASE (TypeHexahedronNameOpt%hexahedron)
  vtk_type = vtk_hexahedron8
  nptrs = [1, 2, 3, 4, 5, 6, 7, 8]

CASE (TypePrismNameOpt%prism)
  vtk_type = vtk_Prism6
  nptrs = [1, 2, 3, 4, 5, 6]

CASE (TypePyramidNameOpt%pyramid)
  vtk_type = vtk_Pyramid5
  nptrs = [1, 2, 3, 4, 5]

  !! Order=2 elements
CASE (TypeLineNameOpt%line3)
  vtk_type = vtk_line3
  nptrs = [1, 2, 3]

CASE (TypeTriangleNameOpt%triangle6)
  vtk_type = vtk_Triangle6
  nptrs = [1, 2, 3, 4, 5, 6]

CASE (TypeQuadrangleNameOpt%quadrangle9)
  vtk_type = vtk_Quadrangle9
  nptrs = [1, 2, 3, 4, 5, 6, 7, 8, 9]

CASE (TypeQuadrangleNameOpt%quadrangle8)
  vtk_type = vtk_Quadrangle8
  nptrs = [1, 2, 3, 4, 5, 6, 7, 8]

CASE (TypeTetrahedronNameOpt%tetrahedron10)
  vtk_type = vtk_Tetrahedron10
  nptrs = 1 + [0, 1, 2, 3, 4, 5, 6, 7, 9, 8]

CASE (TypeHexahedronNameOpt%hexahedron20)
  vtk_type = vtk_Hexahedron20
  nptrs = 1 + [0, 1, 2, 3, 4, 5, 6, 7, &
               8, 11, 16, 9, 17, 10, 18, 19, 12, 15, 13, 14]

CASE (TypeHexahedronNameOpt%hexahedron27)
  vtk_type = vtk_Hexahedron27
  nptrs = 1 + [0, 1, 2, 3, 4, 5, 6, 7, &
               8, 11, 16, 9, 17, 10, 18, 19, 12, 15, 13, 14, &
               24, 22, 20, 21, 23, 25, 26]

CASE (TypePrismNameOpt%prism15)
  vtk_type = vtk_Prism15
  nptrs = 1 + [0, 1, 2, 3, 4, 5, &
               6, 8, 12, 7, 13, 14, 9, 11, 10]

CASE (TypePrismNameOpt%prism18)
  vtk_type = vtk_Prism18
  nptrs = 1 + [0, 1, 2, 3, 4, 5, &
               6, 8, 12, 7, 13, 14, 9, 11, 10, &
               15, 17, 16]

CASE (TypePyramidNameOpt%pyramid13)
  vtk_type = vtk_Pyramid13
  nptrs = 1 + [0, 1, 2, 3, 4, 5, &
               5, 8, 9, 6, 10, 7, 11, 12]

CASE (TypePyramidNameOpt%pyramid14)
  vtk_type = vtk_Pyramid13
  nptrs = 1 + [0, 1, 2, 3, 4, 5, &
               5, 8, 9, 6, 10, 7, 11, 12]

  !! order=3 element
CASE (TypeLineNameOpt%line4)
  vtk_type = vtk_line4
  nptrs = [1, 2, 3, 4]

CASE (TypeQuadrangleNameOpt%quadrangle16)
  vtk_type = vtk_Quadrangle16
  nptrs = [1, 2, 3, 4, 5, 6, 7, 8, 10, 9, &
           12, 11, 13, 14, 16, 15]
CASE DEFAULT
  vtk_type = -1
  ALLOCATE (nptrs(0))
END SELECT
END PROCEDURE GetVTKElementType1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE GetVTKElementType1_

SELECT CASE (elemType)
CASE (TypePointNameOpt%point)
  vtk_type = vtk_point
  tsize = 1
  nptrs(1:tsize) = [1]

CASE (TypeLineNameOpt%line)
  vtk_type = vtk_line2
  tsize = 2
  nptrs(1:tsize) = [1, 2]

CASE (TypeTriangleNameOpt%triangle)
  vtk_type = vtk_triangle3
  tsize = 3
  nptrs(1:tsize) = [1, 2, 3]

CASE (TypeQuadrangleNameOpt%quadrangle)
  vtk_type = vtk_quadrangle4
  tsize = 4
  nptrs(1:tsize) = [1, 2, 3, 4]

CASE (TypeTetrahedronNameOpt%tetrahedron)
  vtk_type = vtk_Tetrahedron4
  tsize = 4
  nptrs(1:tsize) = [1, 2, 3, 4]

CASE (TypeHexahedronNameOpt%hexahedron)
  vtk_type = vtk_Hexahedron8
  tsize = 8
  nptrs(1:tsize) = [1, 2, 3, 4, 5, 6, 7, 8]

CASE (TypePrismNameOpt%prism)
  vtk_type = vtk_Prism6
  tsize = 6
  nptrs(1:tsize) = [1, 2, 3, 4, 5, 6]

CASE (TypePyramidNameOpt%pyramid)
  vtk_type = vtk_Pyramid5
  tsize = 5
  nptrs(1:tsize) = [1, 2, 3, 4, 5]

  !! Order=2 elements
CASE (TypeLineNameOpt%line3)
  vtk_type = vtk_line3
  tsize = 3
  nptrs(1:tsize) = [1, 2, 3]

CASE (TypeTriangleNameOpt%triangle6)
  vtk_type = vtk_Triangle6
  tsize = 6
  nptrs(1:tsize) = [1, 2, 3, 4, 5, 6]

CASE (TypeQuadrangleNameOpt%quadrangle9)
  vtk_type = vtk_Quadrangle9
  tsize = 9
  nptrs(1:tsize) = [1, 2, 3, 4, 5, 6, 7, 8, 9]

CASE (TypeQuadrangleNameOpt%quadrangle8)
  vtk_type = vtk_Quadrangle8
  tsize = 8
  nptrs(1:tsize) = [1, 2, 3, 4, 5, 6, 7, 8]

CASE (TypeTetrahedronNameOpt%tetrahedron10)
  vtk_type = vtk_Tetrahedron10
  tsize = 10
  nptrs(1:tsize) = 1 + [0, 1, 2, 3, 4, 5, 6, 7, 9, 8]

CASE (TypeHexahedronNameOpt%hexahedron20)
  vtk_type = vtk_Hexahedron20
  tsize = 20
  nptrs(1:tsize) = 1 + [0, 1, 2, 3, 4, 5, 6, 7, &
                        8, 11, 16, 9, 17, 10, 18, 19, 12, 15, 13, 14]

CASE (TypeHexahedronNameOpt%hexahedron27)
  vtk_type = vtk_Hexahedron27
  tsize = 27
  nptrs(1:tsize) = 1 + [0, 1, 2, 3, 4, 5, 6, 7, &
                        8, 11, 16, 9, 17, 10, 18, 19, 12, 15, 13, 14, &
                        24, 22, 20, 21, 23, 25, 26]

CASE (TypePrismNameOpt%prism15)
  vtk_type = vtk_Prism15
  tsize = 15
  nptrs(1:tsize) = 1 + [0, 1, 2, 3, 4, 5, &
                        6, 8, 12, 7, 13, 14, 9, 11, 10]

CASE (TypePrismNameOpt%prism18)
  vtk_type = vtk_Prism18
  tsize = 18
  nptrs(1:tsize) = 1 + [0, 1, 2, 3, 4, 5, &
                        6, 8, 12, 7, 13, 14, 9, 11, 10, &
                        15, 17, 16]

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

  !! order=3 element
CASE (TypeLineNameOpt%line4)
  vtk_type = vtk_line4
  tsize = 4
  nptrs(1:tsize) = [1, 2, 3, 4]

CASE (TypeQuadrangleNameOpt%quadrangle16)
  vtk_type = vtk_Quadrangle16
  tsize = 16
  nptrs(1:tsize) = [1, 2, 3, 4, 5, 6, 7, 8, 10, 9, &
                    12, 11, 13, 14, 16, 15]

CASE DEFAULT
  vtk_type = -1
  tsize = 0
END SELECT

END PROCEDURE GetVTKElementType1_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE VTKMethods
