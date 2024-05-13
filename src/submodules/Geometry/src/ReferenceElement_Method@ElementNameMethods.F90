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

SUBMODULE(ReferenceElement_Method) ElementNameMethods
USE ReferenceLine_Method, ONLY: ElementType_Line,  &
  & TotalNodesInElement_Line,  &
  & ElementName_Line,  &
  & ElementOrder_Line

USE ReferenceTriangle_Method, ONLY: ElementType_Triangle,  &
  & TotalNodesInElement_Triangle,  &
  & ElementName_Triangle,  &
  & ElementOrder_Triangle

USE ReferenceQuadrangle_Method, ONLY: ElementType_Quadrangle,  &
  & TotalNodesInElement_Quadrangle, &
  & ElementName_Quadrangle, &
  & ElementOrder_Quadrangle

USE ReferenceTetrahedron_Method, ONLY: ElementType_Tetrahedron,  &
  & TotalNodesInElement_Tetrahedron,  &
  & ElementName_Tetrahedron, &
  & ElementOrder_Tetrahedron

USE ReferenceHexahedron_Method, ONLY: ElementName_Hexahedron,  &
  & ElementType_Hexahedron, &
  & ElementOrder_Hexahedron, &
  & TotalNodesInElement_Hexahedron

USE ReferencePrism_Method, ONLY: ElementName_Prism,  &
  & ElementType_Prism, &
  & ElementOrder_Prism, &
  & TotalNodesInElement_Prism

USE ReferencePyramid_Method, ONLY: ElementName_Pyramid,  &
  & ElementType_Pyramid, &
  & ElementOrder_Pyramid, &
  & TotalNodesInElement_Pyramid

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                ElementName
!----------------------------------------------------------------------------

MODULE PROCEDURE Element_Name
SELECT CASE (elemType)

CASE (Line2, Line3, Line4, Line5, Line6, Point)

  ans = ElementName_Line(elemType)

CASE (Triangle3, Triangle6, Triangle9, Triangle10,  &
  & Triangle12, Triangle15a, Triangle15b, Triangle21)

  ans = ElementName_Triangle(elemType)

CASE (Quadrangle4, Quadrangle8, Quadrangle9, Quadrangle16)

  ans = ElementName_Quadrangle(elemType)

CASE (Tetrahedron4, Tetrahedron10, Tetrahedron20, Tetrahedron35,  &
  & Tetrahedron56)

  ans = ElementName_Tetrahedron(elemType)

CASE (Hexahedron8, Hexahedron27, Hexahedron20, Hexahedron64,  &
  & Hexahedron125)

  ans = ElementName_Hexahedron(elemType)

CASE (Prism6, Prism18, Prism15)

  ans = ElementName_Prism(elemType)

CASE (Pyramid5, Pyramid13, Pyramid14)

  ans = ElementName_Pyramid(elemType)

CASE DEFAULT
  ans = "NONE"

END SELECT

END PROCEDURE Element_Name

!----------------------------------------------------------------------------
!                                                         Element_Name_obj
!----------------------------------------------------------------------------

MODULE PROCEDURE Element_Name_obj
ans = Element_Name(obj%name)
END PROCEDURE Element_Name_obj

!----------------------------------------------------------------------------
!                                                               ElementType
!----------------------------------------------------------------------------
MODULE PROCEDURE Element_Type
CHARACTER(4) :: name
name = elemName(1:4)

SELECT CASE (name)
CASE ("Line", "Point")
  ans = ElementType_Line(elemName)

CASE ("Tria")
  ans = ElementType_Triangle(elemName)

CASE ("Quad")
  ans = ElementType_Quadrangle(elemName)

CASE ("Tetr")
  ans = ElementType_Tetrahedron(elemName)

CASE ("Hexa")
  ans = ElementType_Hexahedron(elemName)

CASE ("Pris")
  ans = ElementType_Prism(elemName)

CASE ("Pyra")
  ans = ElementType_Pyramid(elemName)

CASE DEFAULT
  ans = 0
END SELECT
END PROCEDURE Element_Type

!----------------------------------------------------------------------------
!                                                                ElementType
!----------------------------------------------------------------------------

MODULE PROCEDURE Element_Type_obj
ans = obj%name
END PROCEDURE Element_Type_obj

!----------------------------------------------------------------------------
!                                                              ElementOrder
!----------------------------------------------------------------------------

MODULE PROCEDURE Element_Order
SELECT CASE (elemType)

CASE (Line2, Line3, Line4, Line5, Line6, Point)

  ans = ElementOrder_Line(elemType)

CASE (Triangle3, Triangle6, Triangle9, Triangle10,  &
  & Triangle12, Triangle15a, Triangle15b, Triangle21)

  ans = ElementOrder_Triangle(elemType)

CASE (Quadrangle4, Quadrangle8, Quadrangle9, Quadrangle16)

  ans = ElementOrder_Quadrangle(elemType)

CASE (Tetrahedron4, Tetrahedron10, Tetrahedron20, Tetrahedron35,  &
  & Tetrahedron56)

  ans = ElementOrder_Tetrahedron(elemType)

CASE (Hexahedron8, Hexahedron27, Hexahedron20, Hexahedron64,  &
  & Hexahedron125)

  ans = ElementOrder_Hexahedron(elemType)

CASE (Prism6, Prism18, Prism15)

  ans = ElementOrder_Prism(elemType)

CASE (Pyramid5, Pyramid13, Pyramid14)

  ans = ElementOrder_Pyramid(elemType)

CASE DEFAULT

  ans = 0

END SELECT
END PROCEDURE Element_Order

!----------------------------------------------------------------------------
!                                                              ElementOrder
!----------------------------------------------------------------------------

MODULE PROCEDURE Element_Order_refelem
ans = refelem%Order
END PROCEDURE Element_Order_refelem

!----------------------------------------------------------------------------
!                                                               XiDimension
!----------------------------------------------------------------------------

MODULE PROCEDURE Elem_XiDimension1
SELECT CASE (elemType)
CASE (Tetrahedron4, &
      Hexahedron8, &
      Prism6, &
      Pyramid5, &
      Tetrahedron10, &
      Hexahedron27, &
      Prism18, &
      Pyramid14, &
      Hexahedron20, &
      Prism15, &
      Pyramid13, &
      Tetrahedron20, &
      Tetrahedron35, &
      Tetrahedron56, &
      Hexahedron64, &
      Hexahedron125)
  ans = 3
CASE (Triangle3, &
      Triangle6, &
      Triangle9, &
      Triangle10, &
      Triangle12, &
      Triangle15a, &
      Triangle15b, &
      Triangle21, &
      Quadrangle4, &
      Quadrangle8, &
      Quadrangle9, &
      Quadrangle16)
  ans = 2
CASE (Line2, &
      Line3, &
      Line4, &
      Line5, &
      Line6)
  ans = 1
CASE DEFAULT
  ans = 0
END SELECT
END PROCEDURE Elem_XiDimension1

!----------------------------------------------------------------------------
!                                                                Xidimension
!----------------------------------------------------------------------------

MODULE PROCEDURE Elem_Xidimension2
ans = obj%xidimension
END PROCEDURE Elem_Xidimension2

!----------------------------------------------------------------------------
!                                                            ElementTopology
!----------------------------------------------------------------------------

MODULE PROCEDURE refelem_ElementTopology1
SELECT CASE (elemType)
CASE (Line2, &
  & Line3, &
  & Line4, &
  & Line5, &
  & Line6)
  ans = Line
CASE (Triangle3, Triangle6, &
  & Triangle9, Triangle10, Triangle12, Triangle15a, &
  & Triangle15b, Triangle21)
  ans = Triangle
CASE (Quadrangle4, Quadrangle8, &
  & Quadrangle9, Quadrangle16)
  ans = Quadrangle
CASE (Tetrahedron4, Tetrahedron10, &
  & Tetrahedron20, Tetrahedron35, Tetrahedron56)
  ans = Tetrahedron
CASE (Hexahedron8, Hexahedron27, &
  & Hexahedron20, Hexahedron64, Hexahedron125)
  ans = Hexahedron
CASE (Prism6, Prism18, Prism15)
  ans = Prism
CASE (Pyramid5, Pyramid13, Pyramid14)
  ans = Pyramid
CASE DEFAULT
  ans = 0
END SELECT
END PROCEDURE refelem_ElementTopology1

!----------------------------------------------------------------------------
!                                                            ElementTopology
!----------------------------------------------------------------------------

MODULE PROCEDURE refelem_ElementTopology2
ans = refelem_ElementTopology1(obj%name)
END PROCEDURE refelem_ElementTopology2

!----------------------------------------------------------------------------
!                                                        TotalNodesInElement
!----------------------------------------------------------------------------

MODULE PROCEDURE Total_Nodes_In_Element
SELECT CASE (elemType)

CASE (Line2, Line3, Line4, Line5, Line6, Point)

  ans = TotalNodesInElement_Line(elemType)

CASE (Triangle3, Triangle6, Triangle9, Triangle10,  &
  & Triangle12, Triangle15a, Triangle15b, Triangle21)

  ans = TotalNodesInElement_Triangle(elemType)

CASE (Quadrangle4, Quadrangle8, Quadrangle9, Quadrangle16)

  ans = TotalNodesInElement_Quadrangle(elemType)

CASE (Tetrahedron4, Tetrahedron10, Tetrahedron20, Tetrahedron35,  &
  & Tetrahedron56)

  ans = TotalNodesInElement_Tetrahedron(elemType)

CASE (Hexahedron8, Hexahedron27, Hexahedron20, Hexahedron64,  &
  & Hexahedron125)

  ans = TotalNodesInElement_Hexahedron(elemType)

CASE (Prism6, Prism18, Prism15)

  ans = TotalNodesInElement_Prism(elemType)

CASE (Pyramid5, Pyramid13, Pyramid14)

  ans = TotalNodesInElement_Pyramid(elemType)

CASE DEFAULT
  ans = 0

END SELECT
END PROCEDURE Total_Nodes_In_Element

END SUBMODULE ElementNameMethods
