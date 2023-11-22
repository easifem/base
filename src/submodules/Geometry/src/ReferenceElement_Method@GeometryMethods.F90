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

SUBMODULE(ReferenceElement_Method) GeometryMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                          GetTotalEdges
!----------------------------------------------------------------------------

MODULE PROCEDURE GetTotalEdges1
SELECT CASE (elemType)
CASE (Point)
  ans = 0_I4B
CASE (Line)
  ans = 1_I4B
CASE (Triangle)
  ans = 3_I4B
CASE (Quadrangle)
  ans = 4_I4B
CASE (Tetrahedron)
  ans = 6_I4B
CASE (Hexahedron)
  ans = 12_I4B
CASE (Prism)
  ans = 9_I4B
CASE (Pyramid)
  ans = 8_I4B
END SELECT
END PROCEDURE GetTotalEdges1

!----------------------------------------------------------------------------
!                                                          GetTotalFaces
!----------------------------------------------------------------------------

MODULE PROCEDURE GetTotalFaces1
SELECT CASE (elemType)
CASE (Point)
  ans = 0_I4B
CASE (Line)
  ans = 0_I4B
CASE (Triangle)
  ans = 1_I4B
CASE (Quadrangle)
  ans = 1_I4B
CASE (Tetrahedron)
  ans = 4_I4B
CASE (Hexahedron)
  ans = 6_I4B
CASE (Prism)
  ans = 5_I4B
CASE (Pyramid)
  ans = 5_I4B
END SELECT
END PROCEDURE GetTotalFaces1

!----------------------------------------------------------------------------
!                                                          GetTotalCells
!----------------------------------------------------------------------------

MODULE PROCEDURE GetTotalCells1
SELECT CASE (elemType)
CASE (Point, Line, Triangle, Quadrangle)
  ans = 0_I4B
CASE (Tetrahedron, Hexahedron, Prism, Pyramid)
  ans = 1_I4B
END SELECT
END PROCEDURE GetTotalCells1

!----------------------------------------------------------------------------
!                                                               FacetMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE Facet_Matrix_refelem
INTEGER(I4B) :: XiCell, T(4), i, istart, iend, max_nns, nns, tFacet
T(1) = 0
DO i = 2, 4
  T(i) = SUM(refelem%EntityCounts(1:i - 1))
END DO

XiCell = refelem%XiDimension
SELECT CASE (XiCell)
CASE (1)
  tFacet = 2
  istart = 1
  iend = 2
  max_nns = 2
  ALLOCATE (FM(tFacet, max_nns + 3))
  FM = 0
  DO i = 0, tFacet - 1
    FM(i + 1, 1) = refelem%Topology(iStart + i)%Name
    FM(i + 1, 2) = refelem%Topology(iStart + i)%XiDimension
    nns = SIZE(refelem%Topology(iStart + i)%Nptrs)
    FM(i + 1, 3) = nns
    FM(i + 1, 4:(3 + nns)) = refelem%Topology(iStart + i)%Nptrs
  END DO
CASE (2, 3)
  tFacet = refelem%EntityCounts(XiCell)
  istart = T(XiCell) + 1
  iend = T(XiCell) + tFacet
  max_nns = 0
  DO i = istart, iend
    nns = SIZE(refelem%Topology(i)%Nptrs)
    IF (max_nns .LT. nns) max_nns = nns
  END DO
  ALLOCATE (FM(tFacet, max_nns + 3))
  FM = 0
  DO i = 0, tFacet - 1
    FM(i + 1, 1) = refelem%Topology(iStart + i)%Name
    FM(i + 1, 2) = refelem%Topology(iStart + i)%XiDimension
    nns = SIZE(refelem%Topology(iStart + i)%Nptrs)
    FM(i + 1, 3) = nns
    FM(i + 1, 4:(3 + nns)) = refelem%Topology(iStart + i)%Nptrs
  END DO
END SELECT
END PROCEDURE Facet_Matrix_refelem

!----------------------------------------------------------------------------
!                                                             FacetElements
!----------------------------------------------------------------------------

MODULE PROCEDURE refelem_FacetElements
INTEGER(I4B) :: tFacet, ii, xiCell, T(4), istart, iend, tsize, jj
INTEGER(I4B), ALLOCATABLE :: nptrs(:)
TYPE(ReferenceTopology_) :: topo
!> main
xiCell = refelem%xidimension
SELECT CASE (xiCell)
  !
  ! Reference cell is a curve
  !
CASE (1)
  !
  tFacet = 2; ALLOCATE (ans(tFacet))
  !
  DO ii = 1, tFacet
    nptrs = refelem%Topology(ii)%nptrs
    ans(ii)%xij = refelem%xij(:, nptrs)
    ans(ii)%EntityCounts = [1, 0, 0, 0]
    ans(ii)%xidimension = 0
    ans(ii)%Name = Point
    ans(ii)%interpolationPointType = refelem%interpolationPointType
    ans(ii)%Order = 0
    ans(ii)%NSD = refelem%nsd
    ALLOCATE (ans(ii)%Topology(1))
    ans(ii)%Topology(1) = ReferenceTopology(nptrs=nptrs, name=Point)
    ans(ii)%highOrderElement => NULL()
  END DO
  !
  ! Reference cell is a surface
  !
CASE (2)
  !
  tFacet = refelem%EntityCounts(xicell)
  ALLOCATE (ans(tFacet))
  T(1) = 0
  !
  DO ii = 2, 4
    T(ii) = SUM(refelem%EntityCounts(1:ii - 1))
  END DO
  !
  istart = T(XiCell) + 1
  iend = T(XiCell) + tFacet
  !
  DO ii = 1, tFacet
    !
    topo = refelem%Topology(iStart + ii - 1)
    nptrs = topo%nptrs
    ans(ii)%xidimension = topo%Xidimension
    ans(ii)%Name = topo%Name
    ans(ii)%interpolationPointType = refelem%interpolationPointType
    ! ans(ii)%xij = refelem%xij(:, nptrs)
    !
    ans(ii)%xij = InterpolationPoint_Line(  &
      & order=refelem%order, &
      & ipType=refelem%interpolationPointType, &
      & layout="VEFC")
    !
    ans(ii)%Order = ElementOrder(ElemType=topo%Name)
    ans(ii)%NSD = refelem%nsd
    ans(ii)%EntityCounts = [SIZE(nptrs), 1, 0, 0]
    tsize = SIZE(nptrs) + 1
    !
    ALLOCATE (ans(ii)%Topology(tsize))
    !
    DO jj = 1, SIZE(nptrs)
      ans(ii)%Topology(jj) = ReferenceTopology(nptrs=nptrs(jj:jj), &
        & name=Point)
    END DO
    !
    ans(ii)%Topology(tsize) = &
      & ReferenceTopology(nptrs=nptrs, name=ans(ii)%Name)
    !
  END DO
  !
  ! Reference cell is a volume
  !
CASE (3)
  !
  tFacet = refelem%EntityCounts(xicell)
  ALLOCATE (ans(tFacet))
  T(1) = 0
  !
  DO ii = 2, 4
    T(ii) = SUM(refelem%EntityCounts(1:ii - 1))
  END DO
  !
  istart = T(XiCell) + 1
  iend = T(XiCell) + tFacet
  !
  DO ii = 1, tFacet
    topo = refelem%Topology(iStart + ii - 1)
    nptrs = topo%nptrs
    ans(ii)%xidimension = topo%Xidimension
    ans(ii)%Name = topo%Name
    ans(ii)%interpolationPointType = refelem%interpolationPointType
    ! ans(ii)%xij = refelem%xij(:, nptrs)
    ans(ii)%Order = ElementOrder(ElemType=topo%Name)
    ans(ii)%NSD = refelem%nsd
    ans(ii)%EntityCounts = TotalEntities(topo%Name)
    tsize = SUM(ans(ii)%EntityCounts)
    ALLOCATE (ans(ii)%Topology(tsize))
    ! points
    DO jj = 1, ans(ii)%EntityCounts(1)
      ans(ii)%Topology(jj) = ReferenceTopology(nptrs=nptrs(jj:jj), &
        & name=Point)
    END DO
    ! lines
    jj = ans(ii)%EntityCounts(1)
    tsize = jj + ans(ii)%EntityCounts(2)
    ans(ii)%Topology(jj + 1:tsize) = &
      & FacetTopology(ElemType=ans(ii)%name, Nptrs=nptrs)
    ! surface
    ans(ii)%Topology(tsize) = &
      & ReferenceTopology(nptrs=nptrs, name=ans(ii)%Name)
    !
    !
    !
    IF (isTriangle(topo%Name)) THEN
      !
      ans(ii)%xij = InterpolationPoint_Triangle( &
        & order=refelem%order, &
        & ipType=refelem%interpolationPointType, &
        & layout="VEFC")
      !
    ELSE IF (isQuadrangle(topo%Name)) THEN
      !
      ans(ii)%xij = InterpolationPoint_Quadrangle( &
        & order=refelem%order, &
        & ipType=refelem%interpolationPointType, &
        & layout="VEFC")
      !
    END IF
  END DO
  !
END SELECT
!
IF (ALLOCATED(nptrs)) DEALLOCATE (nptrs)
!
END PROCEDURE refelem_FacetElements

!----------------------------------------------------------------------------
!                                                            MeasureSimplex
!----------------------------------------------------------------------------

MODULE PROCEDURE Measure_Simplex
INTEGER(I4B) :: elemType
Ans = 0.0_DFP
SELECT TYPE (refelem)
TYPE IS (ReferencePoint_)
  Ans = Measure_Simplex_Point(refelem, xij)
TYPE IS (ReferenceLine_)
  Ans = Measure_Simplex_Line(refelem, xij)
TYPE IS (ReferenceTriangle_)
  Ans = Measure_Simplex_Triangle(refelem, xij)
TYPE IS (ReferenceQuadrangle_)
  Ans = Measure_Simplex_Quadrangle(refelem, xij)
TYPE IS (ReferenceTetrahedron_)
  Ans = Measure_Simplex_Tetrahedron(refelem, xij)
TYPE IS (ReferenceHexahedron_)
  Ans = Measure_Simplex_Hexahedron(refelem, xij)
TYPE IS (ReferencePrism_)
  Ans = Measure_Simplex_Prism(refelem, xij)
TYPE IS (ReferencePyramid_)
  Ans = Measure_Simplex_Pyramid(refelem, xij)
CLASS DEFAULT
  elemType = refelem%name
  IF (isPoint(elemType)) THEN
    Ans = Measure_Simplex_Point(refelem, xij)
  ELSEIF (isLine(elemType)) THEN
    Ans = Measure_Simplex_Line(refelem, xij)
  ELSEIF (isTriangle(elemType)) THEN
    Ans = Measure_Simplex_Triangle(refelem, xij)
  ELSEIF (isQuadrangle(elemType)) THEN
    Ans = Measure_Simplex_Quadrangle(refelem, xij)
  ELSEIF (isTetrahedron(elemType)) THEN
    Ans = Measure_Simplex_Tetrahedron(refelem, xij)
  ELSEIF (isHexahedron(elemType)) THEN
    Ans = Measure_Simplex_Hexahedron(refelem, xij)
  ELSEIF (isPrism(elemType)) THEN
    Ans = Measure_Simplex_Prism(refelem, xij)
  ELSEIF (isPyramid(elemType)) THEN
    Ans = Measure_Simplex_Pyramid(refelem, xij)
  END IF
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
  CALL Display("            Program stopped!!")
  STOP
CLASS IS (ReferenceTriangle_)
  Ans = triangle_contains_point(refelem, xij, x)
CLASS IS (ReferenceQuadrangle_)
  CALL Display("ERROR:: ReferenceElement_Method@Geometry.F90")
  CALL Display("          Contains_point()")
  CALL Display("            No case found for Quadrangle_")
  CALL Display("            Program stopped!!")
  STOP
CLASS DEFAULT
  CALL Display("ERROR:: ReferenceElement_Method@Geometry.F90")
  CALL Display("          Contains_point()")
  CALL Display("            No case found")
  CALL Display("            Program stopped!!")
  STOP
END SELECT
END PROCEDURE contains_point

!----------------------------------------------------------------------------
!                                                       TotalEntities
!----------------------------------------------------------------------------

MODULE PROCEDURE refelem_TotalEntities
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
  ! Order=2 elements
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
END PROCEDURE refelem_TotalEntities

!----------------------------------------------------------------------------
!                                                             FacetTopology
!----------------------------------------------------------------------------

MODULE PROCEDURE refelem_FacetTopology
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
  ! Order=2 elements
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
END PROCEDURE refelem_FacetTopology

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE GeometryMethods
