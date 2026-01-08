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
USE BaseType, ONLY: TypePointNameOpt
USE BaseType, ONLY: TypeLineNameOpt
USE BaseType, ONLY: TypeTriangleNameOpt
USE BaseType, ONLY: TypeQuadrangleNameOpt
USE BaseType, ONLY: TypeTetrahedronNameOpt
USE BaseType, ONLY: TypeHexahedronNameOpt
USE BaseType, ONLY: TypePrismNameOpt
USE BaseType, ONLY: TypePyramidNameOpt
USE BaseType, ONLY: TypeElemNameOpt
USE ReferencePoint_Method, ONLY: TotalNodesInElement_Point
USE ReferenceLine_Method, ONLY: ElementType_Line
USE ReferenceLine_Method, ONLY: TotalNodesInElement_Line
USE ReferenceLine_Method, ONLY: ElementName_Line
USE ReferenceLine_Method, ONLY: ElementOrder_Line
USE ReferenceTriangle_Method, ONLY: ElementType_Triangle
USE ReferenceTriangle_Method, ONLY: TotalNodesInElement_Triangle
USE ReferenceTriangle_Method, ONLY: ElementName_Triangle
USE ReferenceTriangle_Method, ONLY: ElementOrder_Triangle
USE ReferenceQuadrangle_Method, ONLY: ElementType_Quadrangle
USE ReferenceQuadrangle_Method, ONLY: TotalNodesInElement_Quadrangle
USE ReferenceQuadrangle_Method, ONLY: ElementName_Quadrangle
USE ReferenceQuadrangle_Method, ONLY: ElementOrder_Quadrangle
USE ReferenceTetrahedron_Method, ONLY: ElementType_Tetrahedron
USE ReferenceTetrahedron_Method, ONLY: TotalNodesInElement_Tetrahedron
USE ReferenceTetrahedron_Method, ONLY: ElementName_Tetrahedron
USE ReferenceTetrahedron_Method, ONLY: ElementOrder_Tetrahedron
USE ReferenceHexahedron_Method, ONLY: ElementName_Hexahedron
USE ReferenceHexahedron_Method, ONLY: ElementType_Hexahedron
USE ReferenceHexahedron_Method, ONLY: ElementOrder_Hexahedron
USE ReferenceHexahedron_Method, ONLY: TotalNodesInElement_Hexahedron
USE ReferencePrism_Method, ONLY: ElementName_Prism
USE ReferencePrism_Method, ONLY: ElementType_Prism
USE ReferencePrism_Method, ONLY: ElementOrder_Prism
USE ReferencePrism_Method, ONLY: TotalNodesInElement_Prism
USE ReferencePyramid_Method, ONLY: ElementName_Pyramid
USE ReferencePyramid_Method, ONLY: ElementType_Pyramid
USE ReferencePyramid_Method, ONLY: ElementOrder_Pyramid
USE ReferencePyramid_Method, ONLY: TotalNodesInElement_Pyramid

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                            ElementTopology
!----------------------------------------------------------------------------

MODULE PROCEDURE refelem_ElementTopology1
SELECT CASE (elemType)
CASE (TypePointNameOpt%point)
  ans = TypeElemNameOpt%point

CASE (TypeLineNameOpt%line, &
      TypeLineNameOpt%line3, &
      TypeLineNameOpt%line4, &
      TypeLineNameOpt%line5, &
      TypeLineNameOpt%line6, &
      TypeLineNameOpt%line7, &
      TypeLineNameOpt%line8, &
      TypeLineNameOpt%line9, &
      TypeLineNameOpt%line10, &
      TypeLineNameOpt%line11)

  ans = TypeElemNameOpt%line

CASE (TypeTriangleNameOpt%triangle, &
      TypeTriangleNameOpt%triangle6, &
      TypeTriangleNameOpt%triangle9, &
      TypeTriangleNameOpt%triangle10, &
      TypeTriangleNameOpt%triangle12, &
      TypeTriangleNameOpt%triangle15a, &
      TypeTriangleNameOpt%triangle15b, &
      TypeTriangleNameOpt%triangle18, &
      TypeTriangleNameOpt%triangle21a, &
      TypeTriangleNameOpt%triangle21b, &
      TypeTriangleNameOpt%triangle24, &
      TypeTriangleNameOpt%triangle27, &
      TypeTriangleNameOpt%triangle28, &
      TypeTriangleNameOpt%triangle30, &
      TypeTriangleNameOpt%triangle36, &
      TypeTriangleNameOpt%triangle45, &
      TypeTriangleNameOpt%triangle55, &
      TypeTriangleNameOpt%triangle66)

  ans = TypeElemNameOpt%triangle

CASE (TypeQuadrangleNameOpt%quadrangle, &
      TypeQuadrangleNameOpt%quadrangle8, &
      TypeQuadrangleNameOpt%quadrangle9, &
      TypeQuadrangleNameOpt%quadrangle16a, &
      TypeQuadrangleNameOpt%quadrangle16b, &
      TypeQuadrangleNameOpt%quadrangle20, &
      TypeQuadrangleNameOpt%quadrangle24, &
      TypeQuadrangleNameOpt%quadrangle25, &
      TypeQuadrangleNameOpt%quadrangle28, &
      TypeQuadrangleNameOpt%quadrangle32, &
      TypeQuadrangleNameOpt%quadrangle36a, &
      TypeQuadrangleNameOpt%quadrangle36b, &
      TypeQuadrangleNameOpt%quadrangle40, &
      TypeQuadrangleNameOpt%quadrangle49, &
      TypeQuadrangleNameOpt%quadrangle64, &
      TypeQuadrangleNameOpt%quadrangle81, &
      TypeQuadrangleNameOpt%quadrangle100, &
      TypeQuadrangleNameOpt%quadrangle121)

  ans = TypeElemNameOpt%quadrangle

CASE (TypeTetrahedronNameOpt%tetrahedron, &
      TypeTetrahedronNameOpt%tetrahedron10, &
      TypeTetrahedronNameOpt%tetrahedron20, &
      TypeTetrahedronNameOpt%tetrahedron35, &
      TypeTetrahedronNameOpt%tetrahedron56)

  ans = TypeElemNameOpt%tetrahedron

CASE (TypeHexahedronNameOpt%hexahedron8, &
      TypeHexahedronNameOpt%hexahedron27, &
      TypeHexahedronNameOpt%hexahedron20, &
      TypeHexahedronNameOpt%hexahedron64, &
      TypeHexahedronNameOpt%hexahedron125)

  ans = TypeElemNameOpt%hexahedron

CASE (TypePrismNameOpt%prism6, &
      TypePrismNameOpt%prism18, &
      TypePrismNameOpt%prism15)

  ans = TypeElemNameOpt%prism

CASE (TypePyramidNameOpt%pyramid5, &
      TypePyramidNameOpt%pyramid13, &
      TypePyramidNameOpt%pyramid14)

  ans = TypeElemNameOpt%pyramid

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

CASE (TypeElemNameOpt%point)
  ans = "Point"

CASE (TypeElemNameOpt%line)

  ans = ElementName_Line(elemType)

CASE (TypeElemNameOpt%triangle)

  ans = ElementName_Triangle(elemType)

CASE (TypeElemNameOpt%quadrangle)

  ans = ElementName_Quadrangle(elemType)

CASE (TypeElemNameOpt%tetrahedron)

  ans = ElementName_Tetrahedron(elemType)

CASE (TypeElemNameOpt%hexahedron)

  ans = ElementName_Hexahedron(elemType)

CASE (TypeElemNameOpt%prism)

  ans = ElementName_Prism(elemType)

CASE (TypeElemNameOpt%pyramid)

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

CASE (TypeElemNameOpt%line)

  ans = ElementOrder_Line(elemType)

CASE (TypeElemNameOpt%triangle)

  ans = ElementOrder_Triangle(elemType)

CASE (TypeElemNameOpt%quadrangle)

  ans = ElementOrder_Quadrangle(elemType)

CASE (TypeElemNameOpt%tetrahedron)

  ans = ElementOrder_Tetrahedron(elemType)

CASE (TypeElemNameOpt%hexahedron)

  ans = ElementOrder_Hexahedron(elemType)

CASE (TypeElemNameOpt%prism)

  ans = ElementOrder_Prism(elemType)

CASE (TypeElemNameOpt%pyramid)

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
CASE (TypeElemNameOpt%tetrahedron, &
      TypeElemNameOpt%hexahedron, &
      TypeElemNameOpt%prism, &
      TypeElemNameOpt%pyramid)

  ans = 3

CASE (TypeElemNameOpt%triangle, &
      TypeElemNameOpt%quadrangle)

  ans = 2

CASE (TypeElemNameOpt%line)

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

CASE (TypeElemNameOpt%point)

  ans = TotalNodesInElement_Point(elemType)

CASE (TypeElemNameOpt%line)

  ans = TotalNodesInElement_Line(elemType)

CASE (TypeElemNameOpt%triangle)

  ans = TotalNodesInElement_Triangle(elemType)

CASE (TypeElemNameOpt%quadrangle)

  ans = TotalNodesInElement_Quadrangle(elemType)

CASE (TypeElemNameOpt%tetrahedron)

  ans = TotalNodesInElement_Tetrahedron(elemType)

CASE (TypeElemNameOpt%hexahedron)

  ans = TotalNodesInElement_Hexahedron(elemType)

CASE (TypeElemNameOpt%prism)

  ans = TotalNodesInElement_Prism(elemType)

CASE (TypeElemNameOpt%pyramid)

  ans = TotalNodesInElement_Pyramid(elemType)

CASE DEFAULT
  ans = 0

END SELECT

END PROCEDURE Total_Nodes_In_Element

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE ElementNameMethods
