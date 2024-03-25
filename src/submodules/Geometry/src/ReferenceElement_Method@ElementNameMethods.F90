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
  & ElementName_Line

USE ReferenceTriangle_Method, ONLY: ElementType_Triangle,  &
  & TotalNodesInElement_Triangle,  &
  & ElementName_Triangle

USE ReferenceQuadrangle_Method, ONLY: ElementType_Quadrangle,  &
  & TotalNodesInElement_Quadrangle,  &
  & ElementName_Quadrangle

USE ReferenceTetrahedron_Method, ONLY: ElementType_Tetrahedron,  &
  & TotalNodesInElement_Tetrahedron,  &
  & ElementName_Tetrahedron

USE ReferenceHexahedron_Method, ONLY: ElementName_Hexahedron
USE ReferencePrism_Method, ONLY: ElementName_Prism
USE ReferencePyramid_Method, ONLY: ElementName_Pyramid

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
SELECT CASE (TRIM(ElemName))
CASE ("Line0")
  ans = 0
CASE ("Line1", "Point")
  ans = Point
CASE ("Line2", "Line")
  ans = Line2
CASE ("Triangle3", "Triangle")
  ans = Triangle3
CASE ("Quadrangle4", "Quadrangle")
  ans = Quadrangle4
CASE ("Tetrahedron4", "Tetrahedron")
  ans = Tetrahedron4
CASE ("Hexahedron8", "Hexahedron")
  ans = Hexahedron8
CASE ("Prism6", "Prism")
  ans = Prism6
CASE ("Pyramid5", "Pyramid")
  ans = Pyramid5
CASE ("Line3")
  ans = Line3
CASE ("Triangle6")
  ans = Triangle6
CASE ("Quadrangle9")
  ans = Quadrangle9
CASE ("Tetrahedron10")
  ans = Tetrahedron10
CASE ("Hexahedron27")
  ans = Hexahedron27
CASE ("Prism18")
  ans = Prism18
CASE ("Pyramid14")
  ans = Pyramid14
CASE ("Point1")
  ans = Point1
CASE ("Quadrangle8")
  ans = Quadrangle8
CASE ("Hexahedron20")
  ans = Hexahedron20
CASE ("Prism15")
  ans = Prism15
CASE ("Pyramid13")
  ans = Pyramid13
CASE ("Triangle9")
  ans = Triangle9
CASE ("Triangle10")
  ans = Triangle10
CASE ("Triangle12")
  ans = Triangle12
CASE ("Triangle15a")
  ans = Triangle15a
CASE ("Triangle15b")
  ans = Triangle15b
CASE ("Triangle21")
  ans = Triangle21
CASE ("Line4")
  ans = Line4
CASE ("Line5")
  ans = Line5
CASE ("Line6")
  ans = Line6
CASE ("Tetrahedron20")
  ans = Tetrahedron20
CASE ("Tetrahedron35")
  ans = Tetrahedron35
CASE ("Tetrahedron56")
  ans = Tetrahedron56
CASE ("Hexahedron64")
  ans = Hexahedron64
CASE ("Hexahedron125")
  ans = Hexahedron125
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
SELECT CASE (ElemType)
CASE (Line2)
  ans = 1
CASE (Triangle3)
  ans = 1
CASE (Quadrangle4)
  ans = 1
CASE (Tetrahedron4)
  ans = 1
CASE (Hexahedron8)
  ans = 1
CASE (Prism6)
  ans = 1
CASE (Pyramid5)
  ans = 1
CASE (Line3)
  ans = 2
CASE (Triangle6)
  ans = 2
CASE (Quadrangle9)
  ans = 2
CASE (Tetrahedron10)
  ans = 2
CASE (Hexahedron27)
  ans = 2
CASE (Prism18)
  ans = 2
CASE (Pyramid14)
  ans = 2
CASE (Point1)
  ans = 0
CASE (Quadrangle8)
  ans = 2
CASE (Hexahedron20)
  ans = 2
CASE (Prism15)
  ans = 2
CASE (Pyramid13)
  ans = 2
CASE (Triangle9)
  ans = 3
CASE (Triangle10)
  ans = 3
CASE (Triangle12)
  ans = 4
CASE (Triangle15a)
  ans = 4
CASE (Triangle15b)
  ans = 5
CASE (Triangle21)
  ans = 5
CASE (Line4)
  ans = 3
CASE (Line5)
  ans = 4
CASE (Line6)
  ans = 5
CASE (Tetrahedron20)
  ans = 3
CASE (Tetrahedron35)
  ans = 4
CASE (Tetrahedron56)
  ans = 5
CASE (Hexahedron64)
  ans = 3
CASE (Hexahedron125)
  ans = 4
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
SELECT CASE (ElemType)
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
      Quadrangle9)
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
SELECT CASE (ElemType)
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
  & Quadrangle9)
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
SELECT CASE (ElemType)
CASE (Line2)
  ans = 2
CASE (Triangle3)
  ans = 3
CASE (Quadrangle4)
  ans = 4
CASE (Tetrahedron4)
  ans = 4
CASE (Hexahedron8)
  ans = 8
CASE (Prism6)
  ans = 6
CASE (Pyramid5)
  ans = 5
CASE (Line3)
  ans = 3
CASE (Triangle6)
  ans = 6
CASE (Quadrangle9)
  ans = 9
CASE (Tetrahedron10)
  ans = 10
CASE (Hexahedron27)
  ans = 27
CASE (Prism18)
  ans = 18
CASE (Pyramid14)
  ans = 14
CASE (Point1)
  ans = 1
CASE (Quadrangle8)
  ans = 8
CASE (Hexahedron20)
  ans = 20
CASE (Prism15)
  ans = 15
CASE (Pyramid13)
  ans = 13
CASE (Triangle9)
  ans = 9
CASE (Triangle10)
  ans = 10
CASE (Triangle12)
  ans = 12
CASE (Triangle15a)
  ans = 15
CASE (Triangle15b)
  ans = 15
CASE (Triangle21)
  ans = 21
CASE (Line4)
  ans = 4
CASE (Line5)
  ans = 5
CASE (Line6)
  ans = 6
CASE (Tetrahedron20)
  ans = 20
CASE (Tetrahedron35)
  ans = 35
CASE (Tetrahedron56)
  ans = 56
CASE (Hexahedron64)
  ans = 64
CASE (Hexahedron125)
  ans = 125
END SELECT
!
END PROCEDURE Total_Nodes_In_Element

END SUBMODULE ElementNameMethods
