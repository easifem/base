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

!> authors: Vikas Sharma, Ph. D.
! date: 1 March 2021
! summary: This sumodule contains method for geometry

SUBMODULE(ReferenceElement_Method) GeometryMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!-----------------------------------------------------------------------------
!                                                                ElementName
!-----------------------------------------------------------------------------

MODULE PROCEDURE Element_Name
! Define internal variables
CHARACTER(LEN=50) :: Str1

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

  !! Order=2 elements
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
!                                                               ElementType
!----------------------------------------------------------------------------
MODULE PROCEDURE Element_Type

SELECT CASE (TRIM(ElemName))
CASE ("Line0")
  Ans = 0
CASE ("Line1")
  Ans = Point
CASE ("Line2")
  Ans = Line2
CASE ("Triangle3")
  Ans = Triangle3
CASE ("Quadrangle4")
  Ans = Quadrangle4
CASE ("Tetrahedron4")
  Ans = Tetrahedron4
CASE ("Hexahedron8")
  Ans = Hexahedron8
CASE ("Prism6")
  Ans = Prism6
CASE ("Pyramid5")
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

END PROCEDURE Total_Nodes_In_Element

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

MODULE PROCEDURE Element_Order_RefElem
Ans = RefElem%Order
END PROCEDURE Element_Order_RefElem

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
!                                                                 isVolume
!----------------------------------------------------------------------------

MODULE PROCEDURE isVolume

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

  Ans = .TRUE.

CASE DEFAULT

  Ans = .FALSE.

END SELECT

END PROCEDURE isVolume

!----------------------------------------------------------------------------
!                                                                 isSurface
!----------------------------------------------------------------------------

MODULE PROCEDURE isSurface
SELECT CASE (ElemType)
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
  Ans = .TRUE.
CASE DEFAULT
  Ans = .FALSE.
END SELECT
END PROCEDURE isSurface

!----------------------------------------------------------------------------
!                                                                 isLine
!----------------------------------------------------------------------------

MODULE PROCEDURE isLine
SELECT CASE (ElemType)
CASE (Line2, &
  & Line3, &
  & Line4, &
  & Line5, &
  & Line6)
  Ans = .TRUE.
CASE DEFAULT
  Ans = .FALSE.
END SELECT
END PROCEDURE isLine

!----------------------------------------------------------------------------
!                                                                 isPoint
!----------------------------------------------------------------------------

MODULE PROCEDURE isPoint
SELECT CASE (ElemType)
CASE (Point1)
  Ans = .TRUE.
CASE DEFAULT
  Ans = .FALSE.
END SELECT
END PROCEDURE isPoint

!----------------------------------------------------------------------------
!                                                                 isTriangle
!----------------------------------------------------------------------------

MODULE PROCEDURE isTriangle
SELECT CASE (ElemType)
CASE (Triangle3, Triangle6, &
& Triangle9, Triangle10, Triangle12, Triangle15a, &
& Triangle15b, Triangle21)
  Ans = .TRUE.
CASE DEFAULT
  Ans = .FALSE.
END SELECT
END PROCEDURE isTriangle

!----------------------------------------------------------------------------
!                                                              isQuadrangle
!----------------------------------------------------------------------------

MODULE PROCEDURE isQuadrangle
SELECT CASE (ElemType)
CASE (Quadrangle4, Quadrangle8, &
& Quadrangle9)
  Ans = .TRUE.
CASE DEFAULT
  Ans = .FALSE.
END SELECT
END PROCEDURE isQuadrangle

!----------------------------------------------------------------------------
!                                                            isTetrahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE isTetrahedron
SELECT CASE (ElemType)
CASE (Tetrahedron4, Tetrahedron10, &
& Tetrahedron20, Tetrahedron35, Tetrahedron56)
  Ans = .TRUE.
CASE DEFAULT
  Ans = .FALSE.
END SELECT
END PROCEDURE isTetrahedron

!----------------------------------------------------------------------------
!                                                               isHexahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE isHexahedron
SELECT CASE (ElemType)
CASE (Hexahedron8, Hexahedron27, &
  & Hexahedron20, Hexahedron64, Hexahedron125)
  Ans = .TRUE.
CASE DEFAULT
  Ans = .FALSE.
END SELECT
END PROCEDURE isHexahedron

!----------------------------------------------------------------------------
!                                                                    isPrism
!----------------------------------------------------------------------------

MODULE PROCEDURE isPrism
SELECT CASE (ElemType)
CASE (Prism6, Prism18, Prism15)
  Ans = .TRUE.
CASE DEFAULT
  Ans = .FALSE.
END SELECT
END PROCEDURE isPrism

!----------------------------------------------------------------------------
!                                                                  isPyramid
!----------------------------------------------------------------------------

MODULE PROCEDURE isPyramid
SELECT CASE (ElemType)
CASE (Pyramid5, Pyramid13, Pyramid14)
  Ans = .TRUE.
CASE DEFAULT
  Ans = .FALSE.
END SELECT
END PROCEDURE isPyramid

!----------------------------------------------------------------------------
!                                                        isSerendipityElement
!----------------------------------------------------------------------------

MODULE PROCEDURE isSerendipityElement
SELECT CASE (ElemType)
CASE (Triangle9, Triangle12, Triangle15b, Quadrangle8)
  Ans = .TRUE.
CASE DEFAULT
  Ans = .FALSE.
END SELECT
END PROCEDURE isSerendipityElement

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
!                                                               FacetMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE Facet_Matrix_RefElem
INTEGER(I4B) :: XiCell, T(4), i, istart, iend, max_nns, nns, tFacet
!> main
T(1) = 0
DO i = 2, 4
  T(i) = SUM(RefElem%EntityCounts(1:i - 1))
END DO
!>
XiCell = RefElem%XiDimension
SELECT CASE (XiCell)
CASE (1)
  tFacet = 2
  istart = 1
  iend = 2
  max_nns = 2
  ALLOCATE (FM(tFacet, max_nns + 3))
  FM = 0
  DO i = 0, tFacet - 1
    FM(i + 1, 1) = RefElem%Topology(iStart + i)%Name
    FM(i + 1, 2) = RefElem%Topology(iStart + i)%XiDimension
    nns = SIZE(RefElem%Topology(iStart + i)%Nptrs)
    FM(i + 1, 3) = nns
    FM(i + 1, 4:(3 + nns)) = RefElem%Topology(iStart + i)%Nptrs
  END DO
CASE (2, 3)
  tFacet = RefElem%EntityCounts(XiCell)
  istart = T(XiCell) + 1
  iend = T(XiCell) + tFacet
  max_nns = 0
  DO i = istart, iend
    nns = SIZE(RefElem%Topology(i)%Nptrs)
    IF (max_nns .LT. nns) max_nns = nns
  END DO
  ALLOCATE (FM(tFacet, max_nns + 3))
  FM = 0
  DO i = 0, tFacet - 1
    FM(i + 1, 1) = RefElem%Topology(iStart + i)%Name
    FM(i + 1, 2) = RefElem%Topology(iStart + i)%XiDimension
    nns = SIZE(RefElem%Topology(iStart + i)%Nptrs)
    FM(i + 1, 3) = nns
    FM(i + 1, 4:(3 + nns)) = RefElem%Topology(iStart + i)%Nptrs
  END DO
END SELECT
END PROCEDURE Facet_Matrix_RefElem

!----------------------------------------------------------------------------
!                                                             FacetElements
!----------------------------------------------------------------------------

MODULE PROCEDURE RefElem_FacetElements
INTEGER(I4B) :: tFacet, ii, xiCell, T(4), istart, iend, tsize, jj
INTEGER(I4B), ALLOCATABLE :: nptrs(:)
TYPE(ReferenceTopology_) :: topo
!> main
xiCell = refelem%xidimension
SELECT CASE (xiCell)
  !!
  !! Reference cell is a curve
  !!
CASE (1)
  !!
  tFacet = 2; ALLOCATE (ans(tFacet))
  !!
  DO ii = 1, tFacet
    nptrs = refelem%Topology(ii)%nptrs
    ans(ii)%xij = refelem%xij(:, nptrs)
    ans(ii)%EntityCounts = [1, 0, 0, 0]
    ans(ii)%xidimension = 0
    ans(ii)%Name = Point
    ans(ii)%Order = 0
    ans(ii)%NSD = refelem%nsd
    ALLOCATE (ans(ii)%Topology(1))
    ans(ii)%Topology(1) = ReferenceTopology(nptrs=nptrs, name=Point)
    ans(ii)%LagrangeElement => NULL()
  END DO
  !!
  !! Reference cell is a surface
  !!
CASE (2)
  !!
  tFacet = refelem%EntityCounts(xicell)
  ALLOCATE (ans(tFacet))
  T(1) = 0
  !!
  DO ii = 2, 4
    T(ii) = SUM(refelem%EntityCounts(1:ii - 1))
  END DO
  !!
  istart = T(XiCell) + 1
  iend = T(XiCell) + tFacet
  !!
  DO ii = 1, tFacet
    !!
    topo = refelem%Topology(iStart + ii - 1)
    nptrs = topo%nptrs
    ans(ii)%xidimension = topo%Xidimension
    ans(ii)%Name = topo%Name
    ans(ii)%xij = refelem%xij(:, nptrs)
    ans(ii)%Order = ElementOrder(ElemType=topo%Name)
    ans(ii)%NSD = refelem%nsd
    ans(ii)%EntityCounts = [SIZE(nptrs), 1, 0, 0]
    tsize = SIZE(nptrs) + 1
    !!
    ALLOCATE (ans(ii)%Topology(tsize))
    !!
    DO jj = 1, SIZE(nptrs)
      ans(ii)%Topology(jj) = ReferenceTopology(nptrs=nptrs(jj:jj), name=Point)
    END DO
    !!
    ans(ii)%Topology(tsize) = &
      & ReferenceTopology(nptrs=nptrs, name=ans(ii)%Name)
    !!
  END DO
  !!
  !! Reference cell is a volume
  !!
CASE (3)
  !!
  tFacet = refelem%EntityCounts(xicell)
  ALLOCATE (ans(tFacet))
  T(1) = 0
  !!
  DO ii = 2, 4
    T(ii) = SUM(refelem%EntityCounts(1:ii - 1))
  END DO
  !!
  istart = T(XiCell) + 1
  iend = T(XiCell) + tFacet
  !!
  DO ii = 1, tFacet
    topo = refelem%Topology(iStart + ii - 1)
    nptrs = topo%nptrs
    ans(ii)%xidimension = topo%Xidimension
    ans(ii)%Name = topo%Name
    ans(ii)%xij = refelem%xij(:, nptrs)
    ans(ii)%Order = ElementOrder(ElemType=topo%Name)
    ans(ii)%NSD = refelem%nsd
    ans(ii)%EntityCounts = TotalEntities(topo%Name)
    tsize = SUM(ans(ii)%EntityCounts)
    ALLOCATE (ans(ii)%Topology(tsize))
    ! points
    DO jj = 1, ans(ii)%EntityCounts(1)
      ans(ii)%Topology(jj) = ReferenceTopology(nptrs=nptrs(jj:jj), name=Point)
    END DO
    ! lines
    jj = ans(ii)%EntityCounts(1)
    tsize = jj + ans(ii)%EntityCounts(2)
      ans( ii )%Topology( jj+1 : tsize ) = &
        & FacetTopology( ElemType=ans(ii)%name, Nptrs=nptrs )
    ! surface
    ans(ii)%Topology(tsize) = &
      & ReferenceTopology(nptrs=nptrs, name=ans(ii)%Name)
  END DO
  !!
END SELECT
!!
IF (ALLOCATED(nptrs)) DEALLOCATE (nptrs)
!!
END PROCEDURE RefElem_FacetElements

!----------------------------------------------------------------------------
!                                                            LocalNodeCoord
!----------------------------------------------------------------------------

MODULE PROCEDURE Local_NodeCoord
IF (ALLOCATED(NodeCoord)) DEALLOCATE (NodeCoord)

SELECT CASE (ElemType)
CASE (Point1)
  ALLOCATE (NodeCoord(3, 1))
  NodeCoord = 0.0_DFP

CASE (Line2)
  ALLOCATE (NodeCoord(3, 2))
  NodeCoord = 0.0_DFP
  NodeCoord(1, :) = [-1.0_DFP, 1.0_DFP]

CASE (Line3)
  ALLOCATE (NodeCoord(3, 3))
  NodeCoord = 0.0_DFP
  NodeCoord(1, :) = [-1.0_DFP, 1.0_DFP, 0.0_DFP]

CASE (Line4)
  ALLOCATE (NodeCoord(3, 4))
  NodeCoord = 0.0_DFP
  NodeCoord(1, :) = [ &
    & -1.0_DFP, 1.0_DFP, &
    & -0.333333333333333_DFP, &
    &  0.333333333333333_DFP]

CASE (Line5)
  ALLOCATE (NodeCoord(3, 5))
  NodeCoord = 0.0_DFP
  NodeCoord(1, :) = [&
    & -1.0_DFP, 1.0_DFP, &
    & -0.5_DFP, 0.0_DFP, &
    & 0.5_DFP]

CASE (Line6)
  ALLOCATE (NodeCoord(3, 6))
  NodeCoord = 0.0_DFP
  NodeCoord(1, :) = [ &
    & -1.0_DFP, 1.0_DFP, &
    & -0.666666666666666_DFP, &
    & -0.333333333333333_DFP, &
    & 0.666666666666666_DFP, &
    & 0.333333333333333_DFP]

CASE (Triangle3)
  ALLOCATE (NodeCoord(3, 3))
  NodeCoord = 0.0_DFP
  NodeCoord(1, :) = [0.0_DFP, 1.0_DFP, 0.0_DFP]
  NodeCoord(2, :) = [0.0_DFP, 0.0_DFP, 1.0_DFP]

CASE (Triangle6)
  ALLOCATE (NodeCoord(3, 6))
  NodeCoord = 0.0_DFP
  NodeCoord(1, :) = [0.0_DFP, 1.0_DFP, 0.0_DFP, &
    & 0.5_DFP, 0.5_DFP, 0.0_DFP]
  NodeCoord(2, :) = [0.0_DFP, 0.0_DFP, 1.0_DFP, &
    & 0.0_DFP, 0.5_DFP, 0.5_DFP]

CASE (Triangle9)
  ALLOCATE (NodeCoord(3, 9))
  NodeCoord = 0.0_DFP
  NodeCoord(1, :) = [ &
    & 0.0_DFP, &
    & 1.0_DFP, &
    & 0.0_DFP, &
    & 0.33333333333333333333_DFP, &
    & 0.66666666666666666667_DFP, &
    & 0.66666666666666666667_DFP, &
    & 0.33333333333333333333_DFP, &
    & 0.0_DFP, &
    & 0.0_DFP]

  NodeCoord(2, :) = [ &
    & 0.0_DFP, &
    & 0.0_DFP, &
    & 1.0_DFP, &
    & 0.0_DFP, &
    & 0.0_DFP, &
    & 0.33333333333333333333_DFP, &
    & 0.66666666666666666667_DFP, &
    & 0.66666666666666666667_DFP, &
    & 0.33333333333333333333_DFP]

CASE (Triangle10)
  ALLOCATE (NodeCoord(3, 10))
  NodeCoord = 0.0_DFP
  NodeCoord(1, :) = [ &
    & 0.0_DFP, &
    & 1.0_DFP, &
    & 0.0_DFP, &
    & 0.33333333333333333333_DFP, &
    & 0.66666666666666666667_DFP, &
    & 0.66666666666666666667_DFP, &
    & 0.33333333333333333333_DFP, &
    & 0.0_DFP, &
    & 0.0_DFP, &
    & 0.33333333333333333333_DFP]

  NodeCoord(2, :) = [ &
    & 0.0_DFP, &
    & 0.0_DFP, &
    & 1.0_DFP, &
    & 0.0_DFP, &
    & 0.0_DFP, &
    & 0.33333333333333333333_DFP, &
    & 0.66666666666666666667_DFP, &
    & 0.66666666666666666667_DFP, &
    & 0.33333333333333333333_DFP, &
    & 0.33333333333333333333_DFP]

CASE (Triangle12)
  ! incomplete triangle; all nodes on boundary
  ALLOCATE (NodeCoord(3, 12))
  NodeCoord = 0.0_DFP
  NodeCoord(1, :) = [ &
    & 0.0_DFP, &
    & 1.0_DFP, &
    & 0.0_DFP, &
    & 0.25_DFP, &
    & 0.5_DFP, &
    & 0.75_DFP, &
    & 0.75_DFP, &
    & 0.5_DFP, &
    & 0.25_DFP, &
    & 0.0_DFP, &
    & 0.0_DFP, &
    & 0.0_DFP]

  NodeCoord(2, :) = [ &
    & 0.0_DFP, &
    & 0.0_DFP, &
    & 1.0_DFP, &
    & 0.0_DFP, &
    & 0.0_DFP, &
    & 0.0_DFP, &
    & 0.25_DFP, &
    & 0.5_DFP, &
    & 0.75_DFP, &
    & 0.75_DFP, &
    & 0.5_DFP, &
    & 0.25_DFP]

CASE (Triangle15a)
  ! complete triangle; 12 nodes on boundary and
  ! 3 nodes are inside
  ALLOCATE (NodeCoord(3, 15))
  NodeCoord = 0.0_DFP
  NodeCoord(1, :) = [ &
    & 0.0_DFP, &
    & 1.0_DFP, &
    & 0.0_DFP, &
    & 0.25_DFP, &
    & 0.5_DFP, &
    & 0.75_DFP, &
    & 0.75_DFP, &
    & 0.5_DFP, &
    & 0.25_DFP, &
    & 0.0_DFP, &
    & 0.0_DFP, &
    & 0.0_DFP, &
    & 0.25_DFP, &
    & 0.5_DFP, &
    & 0.25_DFP]

  NodeCoord(2, :) = [ &
    & 0.0_DFP, &
    & 0.0_DFP, &
    & 1.0_DFP, &
    & 0.0_DFP, &
    & 0.0_DFP, &
    & 0.0_DFP, &
    & 0.25_DFP, &
    & 0.5_DFP, &
    & 0.75_DFP, &
    & 0.75_DFP, &
    & 0.5_DFP, &
    & 0.25_DFP, &
    & 0.25_DFP, &
    & 0.25_DFP, &
    & 0.5_DFP]

CASE (Triangle15b)
  ! Incomplete triangle
  ALLOCATE (NodeCoord(3, 15))
  NodeCoord = 0.0_DFP

  NodeCoord(1, :) = [ &
    & 0.0_DFP, &
    & 1.0_DFP, &
    & 0.0_DFP, &
    & 0.2_DFP, &
    & 0.4_DFP, &
    & 0.6_DFP, &
    & 0.8_DFP, &
    & 0.8_DFP, &
    & 0.6_DFP, &
    & 0.4_DFP, &
    & 0.2_DFP, &
    & 0.0_DFP, &
    & 0.0_DFP, &
    & 0.0_DFP, &
    & 0.0_DFP]

  NodeCoord(2, :) = [ &
    & 0.0_DFP, &
    & 0.0_DFP, &
    & 1.0_DFP, &
    & 0.0_DFP, &
    & 0.0_DFP, &
    & 0.0_DFP, &
    & 0.0_DFP, &
    & 0.2_DFP, &
    & 0.4_DFP, &
    & 0.6_DFP, &
    & 0.8_DFP, &
    & 0.8_DFP, &
    & 0.6_DFP, &
    & 0.4_DFP, &
    & 0.2_DFP]

CASE (Triangle21)
  ALLOCATE (NodeCoord(3, 21))
  NodeCoord = 0.0_DFP

  NodeCoord(1, :) = [ &
    & 0.0_DFP, &
    & 1.0_DFP, &
    & 0.0_DFP, &
    & 0.2_DFP, &
    & 0.4_DFP, &
    & 0.6_DFP, &
    & 0.8_DFP, &
    & 0.8_DFP, &
    & 0.6_DFP, &
    & 0.4_DFP, &
    & 0.2_DFP, &
    & 0.0_DFP, &
    & 0.0_DFP, &
    & 0.0_DFP, &
    & 0.0_DFP, &
    & 0.2_DFP, &
    & 0.6_DFP, &
    & 0.2_DFP, &
    & 0.4_DFP, &
    & 0.4_DFP, &
    & 0.2_DFP]

  NodeCoord(2, :) = [ &
    & 0.0_DFP, &
    & 0.0_DFP, &
    & 1.0_DFP, &
    & 0.0_DFP, &
    & 0.0_DFP, &
    & 0.0_DFP, &
    & 0.0_DFP, &
    & 0.2_DFP, &
    & 0.4_DFP, &
    & 0.6_DFP, &
    & 0.8_DFP, &
    & 0.8_DFP, &
    & 0.6_DFP, &
    & 0.4_DFP, &
    & 0.2_DFP, &
    & 0.2_DFP, &
    & 0.2_DFP, &
    & 0.6_DFP, &
    & 0.2_DFP, &
    & 0.4_DFP, &
    & 0.4_DFP]

CASE (Quadrangle4)
  ALLOCATE (NodeCoord(3, 4))
  NodeCoord = 0.0_DFP
  NodeCoord(1, :) = [-1.0_DFP, 1.0_DFP, 1.0_DFP, 1.0_DFP]
  NodeCoord(2, :) = [-1.0_DFP, -1.0_DFP, 1.0_DFP, 1.0_DFP]

CASE (Quadrangle8)
  NodeCoord = RESHAPE([ &
    & -1.0_DFP, -1.0_DFP, 0.0_DFP, &
    & 1.0_DFP, -1.0_DFP, 0.0_DFP, &
    & 1.0_DFP, 1.0_DFP, 0.0_DFP, &
    & -1.0_DFP, 1.0_DFP, 0.0_DFP, &
    & 0.0_DFP, -1.0_DFP, 0.0_DFP, &
    & 1.0_DFP, 0.0_DFP, 0.0_DFP, &
    & 0.0_DFP, 1.0_DFP, 0.0_DFP, &
    & -1.0_DFP, 0.0_DFP, 0.0_DFP], [3, 8])

CASE (Quadrangle9)
  NodeCoord = RESHAPE([ &
    & -1.0_DFP, -1.0_DFP, 0.0_DFP, &
    & 1.0_DFP, -1.0_DFP, 0.0_DFP, &
    & 1.0_DFP, 1.0_DFP, 0.0_DFP, &
    & -1.0_DFP, 1.0_DFP, 0.0_DFP, &
    & 0.0_DFP, -1.0_DFP, 0.0_DFP, &
    & 1.0_DFP, 0.0_DFP, 0.0_DFP, &
    & 0.0_DFP, 1.0_DFP, 0.0_DFP, &
    & -1.0_DFP, 0.0_DFP, 0.0_DFP, &
    & 0.0_DFP, 0.0_DFP, 0.0_DFP], [3, 9])

CASE (Hexahedron8)
  NodeCoord = RESHAPE([ &
  & -1.0_DFP, -1.0_DFP, -1.0_DFP, &
  & 1.0_DFP, -1.0_DFP, -1.0_DFP, &
  & 1.0_DFP, 1.0_DFP, -1.0_DFP, &
  & -1.0_DFP, 1.0_DFP, -1.0_DFP, &
  & -1.0_DFP, -1.0_DFP, 1.0_DFP, &
  & 1.0_DFP, -1.0_DFP, 1.0_DFP, &
  & 1.0_DFP, 1.0_DFP, 1.0_DFP, &
  & -1.0_DFP, 1.0_DFP, 1.0_DFP], [3, 8])

CASE (Hexahedron20)
  NodeCoord = RESHAPE([ &
  & -1.0_DFP, -1.0_DFP, -1.0_DFP, &
  & 1.0_DFP, -1.0_DFP, -1.0_DFP, &
  & 1.0_DFP, 1.0_DFP, -1.0_DFP, &
  & -1.0_DFP, 1.0_DFP, -1.0_DFP, &
  & -1.0_DFP, -1.0_DFP, 1.0_DFP, &
  & 1.0_DFP, -1.0_DFP, 1.0_DFP, &
  & 1.0_DFP, 1.0_DFP, 1.0_DFP, &
  & -1.0_DFP, 1.0_DFP, 1.0_DFP, &
  & 0.0_DFP, -1.0_DFP, -1.0_DFP, &
  & -1.0_DFP, 0.0_DFP, -1.0_DFP, &
  & -1.0_DFP, -1.0_DFP, 0.0_DFP, &
  & 1.0_DFP, 0.0_DFP, -1.0_DFP, &
  & 1.0_DFP, -1.0_DFP, 0.0_DFP, &
  & 0.0_DFP, 1.0_DFP, -1.0_DFP, &
  & 1.0_DFP, 1.0_DFP, 0.0_DFP, &
  & -1.0_DFP, 1.0_DFP, 0.0_DFP, &
  & 0.0_DFP, -1.0_DFP, 1.0_DFP, &
  & -1.0_DFP, 0.0_DFP, 1.0_DFP, &
  & 1.0_DFP, 0.0_DFP, 1.0_DFP, &
  & 0.0_DFP, 1.0_DFP, 1.0_DFP], [3, 20])

CASE (Hexahedron27)
  NodeCoord = RESHAPE([ &
  & -1.0_DFP, -1.0_DFP, -1.0_DFP, &
  & 1.0_DFP, -1.0_DFP, -1.0_DFP, &
  & 1.0_DFP, 1.0_DFP, -1.0_DFP, &
  & -1.0_DFP, 1.0_DFP, -1.0_DFP, &
  & -1.0_DFP, -1.0_DFP, 1.0_DFP, &
  & 1.0_DFP, -1.0_DFP, 1.0_DFP, &
  & 1.0_DFP, 1.0_DFP, 1.0_DFP, &
  & -1.0_DFP, 1.0_DFP, 1.0_DFP, &
  & 0.0_DFP, -1.0_DFP, -1.0_DFP, &
  & -1.0_DFP, 0.0_DFP, -1.0_DFP, &
  & -1.0_DFP, -1.0_DFP, 0.0_DFP, &
  & 1.0_DFP, 0.0_DFP, -1.0_DFP, &
  & 1.0_DFP, -1.0_DFP, 0.0_DFP, &
  & 0.0_DFP, 1.0_DFP, -1.0_DFP, &
  & 1.0_DFP, 1.0_DFP, 0.0_DFP, &
  & -1.0_DFP, 1.0_DFP, 0.0_DFP, &
  & 0.0_DFP, -1.0_DFP, 1.0_DFP, &
  & -1.0_DFP, 0.0_DFP, 1.0_DFP, &
  & 1.0_DFP, 0.0_DFP, 1.0_DFP, &
  & 0.0_DFP, 1.0_DFP, 1.0_DFP, &
  & 0.0_DFP, 0.0_DFP, -1.0_DFP, &
  & 0.0_DFP, -1.0_DFP, 0.0_DFP, &
  & -1.0_DFP, 0.0_DFP, 0.0_DFP, &
  & 1.0_DFP, 0.0_DFP, 0.0_DFP, &
  & 0.0_DFP, 1.0_DFP, 0.0_DFP, &
  & 0.0_DFP, 0.0_DFP, 1.0_DFP, &
  & 0.0_DFP, 0.0_DFP, 0.0_DFP], [3, 27])
END SELECT

END PROCEDURE Local_NodeCoord

!-----------------------------------------------------------------------------
!                                                                FacetMatrix
!-----------------------------------------------------------------------------

MODULE PROCEDURE Local_NodeCoord_RefElem
IF (ALLOCATED(RefElem%XiJ)) NodeCoord = RefElem%XiJ
END PROCEDURE Local_NodeCoord_RefElem

!----------------------------------------------------------------------------
!                                                            MeasureSimplex
!----------------------------------------------------------------------------

MODULE PROCEDURE Measure_Simplex
Ans = 0.0_DFP
SELECT TYPE (RefElem)
TYPE IS (ReferencePoint_)
  Ans = Measure_Simplex_Point(RefElem, XiJ)
TYPE IS (ReferenceLine_)
  Ans = Measure_Simplex_Line(RefElem, XiJ)
TYPE IS (ReferenceTriangle_)
  Ans = Measure_Simplex_Triangle(RefElem, XiJ)
TYPE IS (ReferenceQuadrangle_)
  Ans = Measure_Simplex_Quadrangle(RefElem, XiJ)
TYPE IS (ReferenceTetrahedron_)
  Ans = Measure_Simplex_Tetrahedron(RefElem, XiJ)
TYPE IS (ReferenceHexahedron_)
  Ans = Measure_Simplex_Hexahedron(RefElem, XiJ)
TYPE IS (ReferencePrism_)
  Ans = Measure_Simplex_Prism(RefElem, XiJ)
TYPE IS (ReferencePyramid_)
  Ans = Measure_Simplex_Pyramid(RefElem, XiJ)
END SELECT
END PROCEDURE Measure_Simplex

!----------------------------------------------------------------------------
!                                                             ElementQuality
!----------------------------------------------------------------------------

MODULE PROCEDURE Element_Quality
SELECT TYPE (refelem)
CLASS IS (ReferencePoint_)
  Ans = Point_quality(refelem, xij, measure)
CLASS IS (ReferenceLine_)
  Ans = Line_quality(refelem, xij, measure)
CLASS IS (ReferenceTriangle_)
  Ans = Triangle_quality(refelem, xij, measure)
CLASS IS (ReferenceQuadrangle_)
  Ans = Quadrangle_quality(refelem, xij, measure)
CLASS IS (ReferenceTetrahedron_)
  Ans = Tetrahedron_quality(refelem, xij, measure)
CLASS IS (ReferencePrism_)
  Ans = Prism_quality(refelem, xij, measure)
CLASS IS (ReferenceHexahedron_)
  Ans = Hexahedron_quality(refelem, xij, measure)
CLASS IS (ReferencePyramid_)
  Ans = Pyramid_quality(refelem, xij, measure)
END SELECT
END PROCEDURE Element_Quality

!----------------------------------------------------------------------------
!                                                              ContainsPoint
!----------------------------------------------------------------------------

MODULE PROCEDURE contains_point
SELECT TYPE (refelem)
CLASS IS (ReferenceLine_)
  CALL Display("ERROR:: ReferenceElement_Method@Geometry.F90")
  CALL Display("          Contains_point()")
  CALL Display("            No case found for ReferenceLine_")
  CALL Display("            Program stopped!!!")
  STOP
CLASS IS (ReferenceTriangle_)
  Ans = triangle_contains_point(refelem, xij, x)
CLASS IS (ReferenceQuadrangle_)
  CALL Display("ERROR:: ReferenceElement_Method@Geometry.F90")
  CALL Display("          Contains_point()")
  CALL Display("            No case found for Quadrangle_")
  CALL Display("            Program stopped!!!")
  STOP
END SELECT
END PROCEDURE contains_point

!----------------------------------------------------------------------------
!                                                       TotalEntities
!----------------------------------------------------------------------------

MODULE PROCEDURE RefElem_TotalEntities
SELECT CASE (ElemType)
CASE (Point1)
  ans = 0
  ans(1) = 1
CASE (Line2)
  ans = [2, 1, 0, 0]
CASE (Triangle3)
  ans = [3, 3, 1, 0]
CASE (Quadrangle4)
  ans = [4, 4, 1, 0]
CASE (Tetrahedron4)
  ans = [4, 6, 4, 1]
CASE (Hexahedron8)
  ans = [8, 12, 6, 1]
CASE (Prism6)
  ans = [6, 9, 5, 1]
CASE (Pyramid5)
  ans = [5, 8, 5, 1]
  !! Order=2 elements
CASE (Line3)
  ans = [3, 1, 0, 0]
CASE (Triangle6)
  ans = [6, 3, 1, 0]
CASE (Quadrangle9)
  ans = [9, 4, 1, 0]
CASE (Quadrangle8)
  ans = [8, 4, 1, 0]
CASE (Tetrahedron10)
  ans = [10, 6, 4, 1]
CASE (Hexahedron20)
  ans = [20, 12, 6, 1]
CASE (Hexahedron27)
  ans = [27, 12, 6, 1]
CASE (Prism15)
  ans = [15, 9, 5, 1]
CASE (Prism18)
  ans = [18, 9, 5, 1]
CASE (Pyramid13)
  ans = [13, 8, 5, 1]
CASE (Pyramid14)
  ans = [14, 8, 5, 1]
CASE (Triangle9)
  ans = [9, 3, 1, 0]
CASE (Triangle10)
  ans = [10, 3, 1, 0]
CASE (Triangle12)
  ans = [12, 3, 1, 0]
CASE (Triangle15a)
  ans = [15, 3, 1, 0]
CASE (Triangle15b)
  ans = [15, 3, 1, 0]
CASE (Triangle21)
  ans = [21, 3, 1, 0]
CASE (Line4)
  ans = [4, 1, 0, 0]
CASE (Line5)
  ans = [5, 1, 0, 0]
CASE (Line6)
  ans = [6, 1, 0, 0]
CASE (Tetrahedron20)
  ans = [20, 6, 4, 1]
CASE (Tetrahedron35)
  ans = [35, 6, 4, 1]
CASE (Tetrahedron56)
  ans = [56, 6, 4, 1]
CASE (Hexahedron64)
  ans = [64, 6, 4, 1]
CASE (Hexahedron125)
  ans = [125, 6, 4, 1]
END SELECT
END PROCEDURE RefElem_TotalEntities

!----------------------------------------------------------------------------
!                                                      FacetTopology
!----------------------------------------------------------------------------

MODULE PROCEDURE RefElem_FacetTopology
SELECT CASE (ElemType)
CASE (Line2)
  ALLOCATE (ans(2))
  ans(1)%nptrs = Nptrs(1:1)
  ans(1)%name = point
  ans(1)%xidimension = 0

  ans(2)%nptrs = Nptrs(2:2)
  ans(2)%name = point
  ans(2)%xidimension = 0

CASE (Triangle3)
  ALLOCATE (ans(3))
  ans(1)%nptrs = Nptrs([1, 2])
  ans(2)%nptrs = Nptrs([2, 3])
  ans(3)%nptrs = Nptrs([3, 1])

  ans(1:3)%Xidimension = 1
  ans(1:3)%name = line2

CASE (Quadrangle4)
  ALLOCATE (ans(4))
  ans(1)%nptrs = Nptrs([1, 2])
  ans(2)%nptrs = Nptrs([2, 3])
  ans(3)%nptrs = Nptrs([3, 4])
  ans(4)%nptrs = Nptrs([4, 1])
  ans(1:)%Xidimension = 1
  ans(1:)%name = line2
CASE (Tetrahedron4)
  ALLOCATE (ans(4))
  ans(1)%nptrs = Nptrs([1, 2, 3])
  ans(2)%nptrs = Nptrs([3, 1, 4])
  ans(3)%nptrs = Nptrs([4, 2, 3])
  ans(4)%nptrs = Nptrs([1, 2, 4])
  ans(:)%Xidimension = 2
  ans(:)%name = Triangle3

CASE (Hexahedron8)
  ALLOCATE (ans(6))
  ans(1)%nptrs = Nptrs([1, 4, 3, 2])
  ans(2)%nptrs = Nptrs([1, 5, 8, 4])
  ans(3)%nptrs = Nptrs([5, 6, 7, 8])
  ans(4)%nptrs = Nptrs([2, 3, 7, 6])
  ans(5)%nptrs = Nptrs([3, 4, 8, 7])
  ans(6)%nptrs = Nptrs([1, 2, 6, 5])
  ans(:)%Xidimension = 2
  ans(:)%name = Quadrangle4
CASE (Prism6)
  ALLOCATE (ans(5))
  ans(1)%nptrs = Nptrs([5, 4, 1, 2])
  ans(2)%nptrs = Nptrs([4, 6, 3, 1])
  ans(3)%nptrs = Nptrs([2, 3, 6, 5])
  ans(4)%nptrs = Nptrs([1, 3, 2])
  ans(5)%nptrs = Nptrs([4, 5, 6])
  ans(:)%Xidimension = 2
  ans(1:3)%name = Quadrangle4
  ans(4:5)%name = Triangle3
CASE (Pyramid5)
  ALLOCATE (ans(5))
  ans(1)%nptrs = Nptrs([1, 2, 5])
  ans(2)%nptrs = Nptrs([2, 3, 5])
  ans(3)%nptrs = Nptrs([3, 4, 5])
  ans(4)%nptrs = Nptrs([1, 5, 4])
  ans(5)%nptrs = Nptrs([4, 3, 2, 1])
  ans(:)%Xidimension = 2
  ans(1:4)%name = Triangle3
  ans(5)%name = Quadrangle4
  !! Order=2 elements
CASE (Line3)
  ALLOCATE (ans(2))
  ans(1)%nptrs = Nptrs([1])
  ans(1)%name = point
  ans(1)%xidimension = 0

  ans(2)%nptrs = Nptrs([2])
  ans(2)%name = point
  ans(2)%xidimension = 0
CASE (Triangle6)
  ALLOCATE (ans(3))
  ans(1)%nptrs = Nptrs([1, 2, 4])
  ans(2)%nptrs = Nptrs([2, 3, 5])
  ans(3)%nptrs = Nptrs([3, 1, 6])

  ans(1:3)%Xidimension = 1
  ans(1:3)%name = line3

CASE (Quadrangle9)
  ALLOCATE (ans(4))
  ans(1)%nptrs = Nptrs([1, 2, 5])
  ans(2)%nptrs = Nptrs([2, 3, 6])
  ans(3)%nptrs = Nptrs([3, 4, 7])
  ans(4)%nptrs = Nptrs([4, 1, 8])
  ans(1:)%Xidimension = 1
  ans(1:)%name = line3

CASE (Quadrangle8)
  ALLOCATE (ans(4))
  ans(1)%nptrs = Nptrs([1, 2, 5])
  ans(2)%nptrs = Nptrs([2, 3, 6])
  ans(3)%nptrs = Nptrs([3, 4, 7])
  ans(4)%nptrs = Nptrs([4, 1, 8])
  ans(1:)%Xidimension = 1
  ans(1:)%name = line3

CASE (Tetrahedron10)
  ALLOCATE (ans(4))
  ans(1)%nptrs = Nptrs([1, 2, 3, 5, 6, 7])
  ans(2)%nptrs = Nptrs([3, 1, 4, 7, 8, 10])
  ans(3)%nptrs = Nptrs([4, 2, 3, 9, 6, 10])
  ans(4)%nptrs = Nptrs([1, 2, 4, 5, 9, 8])
  ans(:)%Xidimension = 2
  ans(:)%name = Triangle6

CASE (Hexahedron20)
  ALLOCATE (ans(6))
  ans(1)%nptrs = Nptrs([1, 4, 3, 2, 10, 14, 12, 9])
  ans(2)%nptrs = Nptrs([1, 5, 8, 4, 11, 18, 16, 10])
  ans(3)%nptrs = Nptrs([5, 6, 7, 8, 17, 19, 20, 18])
  ans(4)%nptrs = Nptrs([2, 3, 7, 6, 12, 15, 19, 13])
  ans(5)%nptrs = Nptrs([3, 4, 8, 7, 14, 16, 20, 15])
  ans(6)%nptrs = Nptrs([1, 2, 6, 5, 9, 13, 17, 11])
  ans(:)%Xidimension = 2
  ans(:)%name = Quadrangle8
CASE (Hexahedron27)
  ALLOCATE (ans(6))
  ans(1)%nptrs = Nptrs([1, 4, 3, 2, 10, 14, 12, 9, 21])
  ans(2)%nptrs = Nptrs([1, 5, 8, 4, 11, 18, 16, 10, 23])
  ans(3)%nptrs = Nptrs([5, 6, 7, 8, 17, 19, 20, 18, 26])
  ans(4)%nptrs = Nptrs([2, 3, 7, 6, 12, 15, 19, 13, 24])
  ans(5)%nptrs = Nptrs([3, 4, 8, 7, 14, 16, 20, 15, 25])
  ans(6)%nptrs = Nptrs([1, 2, 6, 5, 9, 13, 17, 11, 22])
  ans(:)%Xidimension = 2
  ans(:)%name = Quadrangle9
CASE (Prism15)
  ALLOCATE (ans(5))
  ans(1)%nptrs = Nptrs([5, 4, 1, 2, 13, 9, 7, 11])
  ans(2)%nptrs = Nptrs([4, 6, 3, 1, 14, 12, 8, 9])
  ans(3)%nptrs = Nptrs([2, 3, 6, 5, 10, 12, 15, 11])
  ans(4)%nptrs = Nptrs([1, 3, 2, 8, 10, 7])
  ans(5)%nptrs = Nptrs([4, 5, 6, 13, 15, 14])
  ans(:)%Xidimension = 2
  ans(1:3)%name = Quadrangle8
  ans(4:5)%name = Triangle6

CASE (Prism18)
  ALLOCATE (ans(5))
  ans(1)%nptrs = Nptrs([5, 4, 1, 2, 13, 9, 7, 11, 16])
  ans(2)%nptrs = Nptrs([4, 6, 3, 1, 14, 12, 8, 9, 17])
  ans(3)%nptrs = Nptrs([2, 3, 6, 5, 10, 12, 15, 11, 18])
  ans(4)%nptrs = Nptrs([1, 3, 2, 8, 10, 7])
  ans(5)%nptrs = Nptrs([4, 5, 6, 13, 15, 14])
  ans(:)%Xidimension = 2
  ans(1:3)%name = Quadrangle9
  ans(4:5)%name = Triangle6

CASE (Pyramid13)
  ALLOCATE (ans(5))
  ans(1)%nptrs = Nptrs([1, 2, 5, 6, 10, 8])
  ans(2)%nptrs = Nptrs([2, 3, 5, 9, 12, 10])
  ans(3)%nptrs = Nptrs([3, 4, 5, 11, 13, 12])
  ans(4)%nptrs = Nptrs([1, 5, 4, 8, 13, 7])
  ans(5)%nptrs = Nptrs([4, 3, 2, 1, 11, 9, 6, 7])
  ans(:)%Xidimension = 2
  ans(1:4)%name = Triangle6
  ans(5)%name = Quadrangle8

CASE (Pyramid14)
  ALLOCATE (ans(5))
  ans(1)%nptrs = Nptrs([1, 2, 5, 6, 10, 8])
  ans(2)%nptrs = Nptrs([2, 3, 5, 9, 12, 10])
  ans(3)%nptrs = Nptrs([3, 4, 5, 11, 13, 12])
  ans(4)%nptrs = Nptrs([1, 5, 4, 8, 13, 7])
  ans(5)%nptrs = Nptrs([4, 3, 2, 1, 11, 9, 6, 7, 13])
  ans(:)%Xidimension = 2
  ans(1:4)%name = Triangle6
  ans(5)%name = Quadrangle9

CASE (Triangle9)
  ALLOCATE (ans(3))
  ans(1)%nptrs = Nptrs([1, 2, 4, 5])
  ans(2)%nptrs = Nptrs([2, 3, 6, 7])
  ans(3)%nptrs = Nptrs([3, 1, 8, 9])

  ans(1:3)%Xidimension = 1
  ans(1:3)%name = line4

CASE (Triangle10)
  ALLOCATE (ans(3))
  ans(1)%nptrs = Nptrs([1, 2, 4, 5])
  ans(2)%nptrs = Nptrs([2, 3, 6, 7])
  ans(3)%nptrs = Nptrs([3, 1, 8, 9])

  ans(1:3)%Xidimension = 1
  ans(1:3)%name = line4

CASE (Triangle12)
  ALLOCATE (ans(3))
  ans(1)%nptrs = Nptrs([1, 2, 4, 5, 6])
  ans(2)%nptrs = Nptrs([2, 3, 7, 8, 9])
  ans(3)%nptrs = Nptrs([3, 1, 10, 11, 12])

  ans(1:3)%Xidimension = 1
  ans(1:3)%name = line5

CASE (Triangle15a)
  ALLOCATE (ans(3))
  ans(1)%nptrs = Nptrs([1, 2, 4, 5, 6])
  ans(2)%nptrs = Nptrs([2, 3, 7, 8, 9])
  ans(3)%nptrs = Nptrs([3, 1, 10, 11, 12])

  ans(1:3)%Xidimension = 1
  ans(1:3)%name = line5

CASE (Line4)
  ALLOCATE (ans(2))
  ans(1)%nptrs = Nptrs([1])
  ans(1)%name = point
  ans(1)%xidimension = 0

  ans(2)%nptrs = Nptrs([2])
  ans(2)%name = point
  ans(2)%xidimension = 0

CASE (Line5)
  ALLOCATE (ans(2))
  ans(1)%nptrs = Nptrs([1])
  ans(1)%name = point
  ans(1)%xidimension = 0

  ans(2)%nptrs = Nptrs([2])
  ans(2)%name = point
  ans(2)%xidimension = 0

CASE (Line6)
  ALLOCATE (ans(2))
  ans(1)%nptrs = Nptrs([1])
  ans(1)%name = point
  ans(1)%xidimension = 0

  ans(2)%nptrs = Nptrs([2])
  ans(2)%name = point
  ans(2)%xidimension = 0

CASE (Triangle15b, Triangle21, Tetrahedron20, Tetrahedron35, &
  & Tetrahedron56, Hexahedron64, Hexahedron125)

END SELECT
END PROCEDURE RefElem_FacetTopology

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE GeometryMethods
