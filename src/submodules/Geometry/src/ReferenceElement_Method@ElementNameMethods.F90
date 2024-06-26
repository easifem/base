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
!                                                            ElementTopology
!----------------------------------------------------------------------------

MODULE PROCEDURE refelem_ElementTopology1
SELECT CASE (elemType)
CASE (Point)
  ans = Point

CASE (Line2, Line3, Line4, Line5, Line6, Line7, Line8, Line9, Line10, Line11)

  ans = Line

CASE (Triangle3, Triangle6, Triangle9, Triangle10, Triangle12, &
      Triangle15a, Triangle15b, Triangle18, Triangle21a, Triangle21b, &
      Triangle24, Triangle27, Triangle28, Triangle30, Triangle36, &
      Triangle45, Triangle55, Triangle66)

  ans = Triangle

CASE (Quadrangle4, Quadrangle8, Quadrangle9, Quadrangle16a, Quadrangle16b, &
      Quadrangle20, Quadrangle24, Quadrangle25, Quadrangle28, Quadrangle32, &
     Quadrangle36a, Quadrangle36b, Quadrangle40, Quadrangle49, Quadrangle64, &
      Quadrangle81, Quadrangle100, Quadrangle121)
  ans = Quadrangle

CASE (Tetrahedron4, Tetrahedron10, Tetrahedron20, Tetrahedron35, &
      Tetrahedron56)
  ans = Tetrahedron

CASE (Hexahedron8, Hexahedron27, Hexahedron20, Hexahedron64, Hexahedron125)
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
!                                                                ElementName
!----------------------------------------------------------------------------

MODULE PROCEDURE Element_Name
INTEGER(I4B) :: topo

topo = refelem_elementtopology1(elemType)

SELECT CASE (topo)

CASE (Point)
  ans = "Point"

CASE (Line)

  ans = ElementName_Line(elemType)

CASE (Triangle)

  ans = ElementName_Triangle(elemType)

CASE (Quadrangle)

  ans = ElementName_Quadrangle(elemType)

CASE (Tetrahedron)

  ans = ElementName_Tetrahedron(elemType)

CASE (Hexahedron)

  ans = ElementName_Hexahedron(elemType)

CASE (Prism)

  ans = ElementName_Prism(elemType)

CASE (Pyramid)

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
CHARACTER(2) :: name
name = elemName(1:2)

SELECT CASE (name)
CASE ("Li", "Po")
  ans = ElementType_Line(elemName)

CASE ("Tr")
  ans = ElementType_Triangle(elemName)

CASE ("Qu")
  ans = ElementType_Quadrangle(elemName)

CASE ("Te")
  ans = ElementType_Tetrahedron(elemName)

CASE ("He")
  ans = ElementType_Hexahedron(elemName)

CASE ("Pr")
  ans = ElementType_Prism(elemName)

CASE ("Py")
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
INTEGER(I4B) :: topo

topo = refelem_elementtopology1(elemType)

SELECT CASE (topo)

CASE (Line)

  ans = ElementOrder_Line(elemType)

CASE (Triangle)

  ans = ElementOrder_Triangle(elemType)

CASE (Quadrangle)

  ans = ElementOrder_Quadrangle(elemType)

CASE (Tetrahedron)

  ans = ElementOrder_Tetrahedron(elemType)

CASE (Hexahedron)

  ans = ElementOrder_Hexahedron(elemType)

CASE (Prism)

  ans = ElementOrder_Prism(elemType)

CASE (Pyramid)

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
INTEGER(I4B) :: topo
topo = refelem_elementtopology1(elemType)

SELECT CASE (topo)
CASE (Tetrahedron, Hexahedron, Prism, Pyramid)
  ans = 3

CASE (Triangle, Quadrangle)
  ans = 2

CASE (Line)
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
!                                                        TotalNodesInElement
!----------------------------------------------------------------------------

MODULE PROCEDURE Total_Nodes_In_Element
INTEGER(I4B) :: topo

topo = refelem_elementtopology1(elemType)

SELECT CASE (topo)

CASE (Line)

  ans = TotalNodesInElement_Line(elemType)

CASE (Triangle)

  ans = TotalNodesInElement_Triangle(elemType)

CASE (Quadrangle)

  ans = TotalNodesInElement_Quadrangle(elemType)

CASE (Tetrahedron)

  ans = TotalNodesInElement_Tetrahedron(elemType)

CASE (Hexahedron)

  ans = TotalNodesInElement_Hexahedron(elemType)

CASE (Prism)

  ans = TotalNodesInElement_Prism(elemType)

CASE (Pyramid)

  ans = TotalNodesInElement_Pyramid(elemType)

CASE DEFAULT
  ans = 0

END SELECT

END PROCEDURE Total_Nodes_In_Element

END SUBMODULE ElementNameMethods
