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
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                ElementName
!----------------------------------------------------------------------------

MODULE PROCEDURE Element_Name
! Define internal variables
CHARACTER(50) :: Str1
SELECT CASE (ElemType)
CASE (Point1)
  Str1 = "Point1"
CASE (Line2)
  Str1 = "Line2"
CASE (Triangle3)
  Str1 = "Triangle3"
CASE (Quadrangle4)
  Str1 = "Quadrangle4"
CASE (Tetrahedron4)
  Str1 = "Tetrahedron4"
CASE (Hexahedron8)
  Str1 = "Hexahedron8"
CASE (Prism6)
  Str1 = "Prism6"
CASE (Pyramid5)
  Str1 = "Pyramid5"
CASE (Line3)
  Str1 = "Line3"
CASE (Triangle6)
  Str1 = "Triangle6"
CASE (Quadrangle9)
  Str1 = "Quadrangle9"
CASE (Quadrangle8)
  Str1 = "Quadrangle8"
CASE (Tetrahedron10)
  Str1 = "Tetrahedron10"
CASE (Hexahedron20)
  Str1 = "Hexahedron20"
CASE (Hexahedron27)
  Str1 = "Hexahedron27"
CASE (Prism15)
  Str1 = "Prism15"
CASE (Prism18)
  Str1 = "Prism18"
CASE (Pyramid13)
  Str1 = "Pyramid13"
CASE (Pyramid14)
  Str1 = "Pyramid14"
CASE (Triangle9)
  Str1 = "Triangle9"
CASE (Triangle10)
  Str1 = "Triangle10"
CASE (Triangle12)
  Str1 = "Triangle12"
CASE (Triangle15a)
  Str1 = "Triangle15a"
CASE (Triangle15b)
  Str1 = "Triangle15b"
CASE (Triangle21)
  Str1 = "Triangle21"
CASE (Line4)
  Str1 = "Line4"
CASE (Line5)
  Str1 = "Line5"
CASE (Line6)
  Str1 = "Line6"
CASE (Tetrahedron20)
  Str1 = "Tetrahedron20"
CASE (Tetrahedron35)
  Str1 = "Tetrahedron35"
CASE (Tetrahedron56)
  Str1 = "Tetrahedron56"
CASE (Hexahedron64)
  Str1 = "Hexahedron64"
CASE (Hexahedron125)
  Str1 = "Hexahedron125"
END SELECT
Ans = TRIM(Str1)
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
  Ans = 0
CASE ("Line1", "Point")
  Ans = Point
CASE ("Line2", "Line")
  Ans = Line2
CASE ("Triangle3", "Triangle")
  Ans = Triangle3
CASE ("Quadrangle4", "Quadrangle")
  Ans = Quadrangle4
CASE ("Tetrahedron4", "Tetrahedron")
  Ans = Tetrahedron4
CASE ("Hexahedron8", "Hexahedron")
  Ans = Hexahedron8
CASE ("Prism6", "Prism")
  Ans = Prism6
CASE ("Pyramid5", "Pyramid")
  Ans = Pyramid5
CASE ("Line3")
  Ans = Line3
CASE ("Triangle6")
  Ans = Triangle6
CASE ("Quadrangle9")
  Ans = Quadrangle9
CASE ("Tetrahedron10")
  Ans = Tetrahedron10
CASE ("Hexahedron27")
  Ans = Hexahedron27
CASE ("Prism18")
  Ans = Prism18
CASE ("Pyramid14")
  Ans = Pyramid14
CASE ("Point1")
  Ans = Point1
CASE ("Quadrangle8")
  Ans = Quadrangle8
CASE ("Hexahedron20")
  Ans = Hexahedron20
CASE ("Prism15")
  Ans = Prism15
CASE ("Pyramid13")
  Ans = Pyramid13
CASE ("Triangle9")
  Ans = Triangle9
CASE ("Triangle10")
  Ans = Triangle10
CASE ("Triangle12")
  Ans = Triangle12
CASE ("Triangle15a")
  Ans = Triangle15a
CASE ("Triangle15b")
  Ans = Triangle15b
CASE ("Triangle21")
  Ans = Triangle21
CASE ("Line4")
  Ans = Line4
CASE ("Line5")
  Ans = Line5
CASE ("Line6")
  Ans = Line6
CASE ("Tetrahedron20")
  Ans = Tetrahedron20
CASE ("Tetrahedron35")
  Ans = Tetrahedron35
CASE ("Tetrahedron56")
  Ans = Tetrahedron56
CASE ("Hexahedron64")
  Ans = Hexahedron64
CASE ("Hexahedron125")
  Ans = Hexahedron125
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
  Ans = 1
CASE (Triangle3)
  Ans = 1
CASE (Quadrangle4)
  Ans = 1
CASE (Tetrahedron4)
  Ans = 1
CASE (Hexahedron8)
  Ans = 1
CASE (Prism6)
  Ans = 1
CASE (Pyramid5)
  Ans = 1
CASE (Line3)
  Ans = 2
CASE (Triangle6)
  Ans = 2
CASE (Quadrangle9)
  Ans = 2
CASE (Tetrahedron10)
  Ans = 2
CASE (Hexahedron27)
  Ans = 2
CASE (Prism18)
  Ans = 2
CASE (Pyramid14)
  Ans = 2
CASE (Point1)
  Ans = 0
CASE (Quadrangle8)
  Ans = 2
CASE (Hexahedron20)
  Ans = 2
CASE (Prism15)
  Ans = 2
CASE (Pyramid13)
  Ans = 2
CASE (Triangle9)
  Ans = 3
CASE (Triangle10)
  Ans = 3
CASE (Triangle12)
  Ans = 4
CASE (Triangle15a)
  Ans = 4
CASE (Triangle15b)
  Ans = 5
CASE (Triangle21)
  Ans = 5
CASE (Line4)
  Ans = 3
CASE (Line5)
  Ans = 4
CASE (Line6)
  Ans = 5
CASE (Tetrahedron20)
  Ans = 3
CASE (Tetrahedron35)
  Ans = 4
CASE (Tetrahedron56)
  Ans = 5
CASE (Hexahedron64)
  Ans = 3
CASE (Hexahedron125)
  Ans = 4
END SELECT
END PROCEDURE Element_Order

!----------------------------------------------------------------------------
!                                                              ElementOrder
!----------------------------------------------------------------------------

MODULE PROCEDURE Element_Order_refelem
Ans = refelem%Order
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
  Ans = 3
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
  Ans = 2
CASE (Line2, &
      Line3, &
      Line4, &
      Line5, &
      Line6)
  Ans = 1
CASE DEFAULT
  Ans = 0
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
  Ans = Line
CASE (Triangle3, Triangle6, &
  & Triangle9, Triangle10, Triangle12, Triangle15a, &
  & Triangle15b, Triangle21)
  Ans = Triangle
CASE (Quadrangle4, Quadrangle8, &
  & Quadrangle9)
  Ans = Quadrangle
CASE (Tetrahedron4, Tetrahedron10, &
  & Tetrahedron20, Tetrahedron35, Tetrahedron56)
  Ans = Tetrahedron
CASE (Hexahedron8, Hexahedron27, &
  & Hexahedron20, Hexahedron64, Hexahedron125)
  Ans = Hexahedron
CASE (Prism6, Prism18, Prism15)
  Ans = Prism
CASE (Pyramid5, Pyramid13, Pyramid14)
  Ans = Pyramid
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
  Ans = 2
CASE (Triangle3)
  Ans = 3
CASE (Quadrangle4)
  Ans = 4
CASE (Tetrahedron4)
  Ans = 4
CASE (Hexahedron8)
  Ans = 8
CASE (Prism6)
  Ans = 6
CASE (Pyramid5)
  Ans = 5
CASE (Line3)
  Ans = 3
CASE (Triangle6)
  Ans = 6
CASE (Quadrangle9)
  Ans = 9
CASE (Tetrahedron10)
  Ans = 10
CASE (Hexahedron27)
  Ans = 27
CASE (Prism18)
  Ans = 18
CASE (Pyramid14)
  Ans = 14
CASE (Point1)
  Ans = 1
CASE (Quadrangle8)
  Ans = 8
CASE (Hexahedron20)
  Ans = 20
CASE (Prism15)
  Ans = 15
CASE (Pyramid13)
  Ans = 13
CASE (Triangle9)
  Ans = 9
CASE (Triangle10)
  Ans = 10
CASE (Triangle12)
  Ans = 12
CASE (Triangle15a)
  Ans = 15
CASE (Triangle15b)
  Ans = 15
CASE (Triangle21)
  Ans = 21
CASE (Line4)
  Ans = 4
CASE (Line5)
  Ans = 5
CASE (Line6)
  Ans = 6
CASE (Tetrahedron20)
  Ans = 20
CASE (Tetrahedron35)
  Ans = 35
CASE (Tetrahedron56)
  Ans = 56
CASE (Hexahedron64)
  Ans = 64
CASE (Hexahedron125)
  Ans = 125
END SELECT
!
END PROCEDURE Total_Nodes_In_Element

END SUBMODULE ElementNameMethods
