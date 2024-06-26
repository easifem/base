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
USE Utility, ONLY: arange
IMPLICIT NONE

INTEGER(I4B), PARAMETER :: &
  & vtk_point = 1, &
  & vtk_line2 = 3, &
  & vtk_triangle3 = 5, &
  & vtk_quadrangle4 = 9, &
  & vtk_tetrahedron4 = 10, &
  & vtk_hexahedron8 = 12, &
  & vtk_prism6 = 13, &
  & vtk_pyramid5 = 14, &
  & vtk_line3 = 21, &
  & vtk_triangle6 = 22, &
  & vtk_quadrangle8 = 23, &
  & vtk_quadrangle9 = 28, &
  & vtk_tetrahedron10 = 24, &
  & vtk_hexahedron20 = 25, &
  & vtk_hexahedron27 = 29, &
  & vtk_prism15 = 26, &
  & vtk_prism18 = 32, &
  & vtk_line4 = 35, &
  & vtk_pyramid13 = 27,  &
  & vtk_quadrangle16 = 70
! VTK_LAGRANGE_QUADRILATERAL
CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE get_vtk_elemType

SELECT CASE (ElemType)
CASE (Point1)
  vtk_type = vtk_point
  nptrs = [1]

CASE (Line2)
  vtk_type = vtk_line2
  nptrs = [1, 2]

CASE (Triangle3)
  vtk_type = vtk_triangle3
  nptrs = [1, 2, 3]

CASE (Quadrangle4)
  vtk_type = vtk_quadrangle4
  nptrs = [1, 2, 3, 4]

CASE (Tetrahedron4)
  vtk_type = vtk_Tetrahedron4
  nptrs = [1, 2, 3, 4]

CASE (Hexahedron8)
  vtk_type = vtk_Hexahedron8
  nptrs = [1, 2, 3, 4, 5, 6, 7, 8]

CASE (Prism6)
  vtk_type = vtk_Prism6
  nptrs = [1, 2, 3, 4, 5, 6]

CASE (Pyramid5)
  vtk_type = vtk_Pyramid5
  nptrs = [1, 2, 3, 4, 5]

  !! Order=2 elements
CASE (Line3)
  vtk_type = vtk_line3
  nptrs = [1, 2, 3]

CASE (Triangle6)
  vtk_type = vtk_Triangle6
  nptrs = [1, 2, 3, 4, 5, 6]

CASE (Quadrangle9)
  vtk_type = vtk_Quadrangle9
  nptrs = [1, 2, 3, 4, 5, 6, 7, 8, 9]

CASE (Quadrangle8)
  vtk_type = vtk_Quadrangle8
  nptrs = [1, 2, 3, 4, 5, 6, 7, 8]

CASE (Tetrahedron10)
  vtk_type = vtk_Tetrahedron10
  nptrs = 1 + [0, 1, 2, 3, 4, 5, 6, 7, 9, 8]

CASE (Hexahedron20)
  vtk_type = vtk_Hexahedron20
  nptrs = 1 + [0, 1, 2, 3, 4, 5, 6, 7, &
    & 8, 11, 16, 9, 17, 10, 18, 19, 12, 15, 13, 14]

CASE (Hexahedron27)
  vtk_type = vtk_Hexahedron27
  nptrs = 1 + [0, 1, 2, 3, 4, 5, 6, 7, &
    & 8, 11, 16, 9, 17, 10, 18, 19, 12, 15, 13, 14, &
    & 24, 22, 20, 21, 23, 25, 26]

CASE (Prism15)
  vtk_type = vtk_Prism15
  nptrs = 1 + [0, 1, 2, 3, 4, 5, &
    & 6, 8, 12, 7, 13, 14, 9, 11, 10]

CASE (Prism18)
  vtk_type = vtk_Prism18
  nptrs = 1 + [0, 1, 2, 3, 4, 5, &
    & 6, 8, 12, 7, 13, 14, 9, 11, 10, &
    & 15, 17, 16]

CASE (Pyramid13)
  vtk_type = vtk_Pyramid13
  nptrs = 1 + [0, 1, 2, 3, 4, 5, &
    & 5, 8, 9, 6, 10, 7, 11, 12]

CASE (Pyramid14)
  vtk_type = vtk_Pyramid13
  nptrs = 1 + [0, 1, 2, 3, 4, 5, &
    & 5, 8, 9, 6, 10, 7, 11, 12]

  !! order=3 element
CASE (Line4)
  vtk_type = vtk_line4
  nptrs = [1, 2, 3, 4]

CASE (Quadrangle16)
  vtk_type = vtk_Quadrangle16
  nptrs = [1, 2, 3, 4, 5, 6, 7, 8, 10, 9, &
           12, 11, 13, 14, 16, 15]
END SELECT
END PROCEDURE get_vtk_elemType

END SUBMODULE VTKMethods
