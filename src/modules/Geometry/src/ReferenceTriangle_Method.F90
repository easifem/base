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
! date: 5 March 2021
! summary: This module contains method for [[ReferenceTriangle_]] data type.

MODULE ReferenceTriangle_Method
USE GlobalData
USE BaseType
IMPLICIT NONE
PRIVATE
PUBLIC :: Initiate
PUBLIC :: ReferenceTriangle
PUBLIC :: ReferenceTriangle_Pointer
PUBLIC :: HighorderElement_Triangle
PUBLIC :: Measure_Simplex_Triangle
PUBLIC :: Triangle_Contains_Point
PUBLIC :: Contains_Point_Triangle
PUBLIC :: Angles
PUBLIC :: Area
PUBLIC :: ArealVector
PUBLIC :: Barycentric
PUBLIC :: Centroid
PUBLIC :: CircumCenter
PUBLIC :: CircumCircle
PUBLIC :: CircumRadius
PUBLIC :: ContainsLine
PUBLIC :: Diameter
PUBLIC :: EdgeLength
PUBLIC :: Incenter
PUBLIC :: Incircle
PUBLIC :: Inradius
PUBLIC :: Orthocenter
PUBLIC :: DistanceFromPoint
PUBLIC :: NearestPoint
PUBLIC :: RandomPoint
PUBLIC :: Triangle_Quality
PUBLIC :: Quality_Triangle
PUBLIC :: TriangleArea3D
PUBLIC :: TriangleArea2D
PUBLIC :: GetEdgeConnectivity_Triangle
PUBLIC :: RefTriangleCoord
PUBLIC :: RefCoord_Triangle
PUBLIC :: FacetElements_Triangle
PUBLIC :: DEFAULT_OPT_TRIANGLE_EDGE_CON
PUBLIC :: ElementOrder_Triangle
PUBLIC :: ElementType_Triangle
PUBLIC :: TotalNodesInElement_Triangle
PUBLIC :: TotalEntities_Triangle
PUBLIC :: FacetTopology_Triangle
PUBLIC :: ElementName_Triangle
PUBLIC :: MaxOrder_Triangle
PUBLIC :: FaceShapeMetaData_Triangle

#ifdef MAX_TRIANGLE_ORDER
INTEGER(I4B), PARAMETER :: MaxOrder_Triangle = MAX_TRIANGLE_ORDER
#else
INTEGER(I4B), PARAMETER :: MaxOrder_Triangle = 2_I4B
#endif

#ifdef _TRIANGLE_EDGE_CON_DEFAULT_OPT_2
INTEGER(I4B), PARAMETER :: DEFAULT_OPT_TRIANGLE_EDGE_CON = 1_I4B
!! This means edges are [1,2], [1,3], [2,3]
#else
INTEGER(I4B), PARAMETER :: DEFAULT_OPT_TRIANGLE_EDGE_CON = 2_I4B
!! This means edges are [1,2], [2,3], [3,1]
!! This is default option
#endif

!----------------------------------------------------------------------------
!                                                               ElementName
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-03-25
! summary: Returns element name in character from element number/type

INTERFACE
  MODULE PURE FUNCTION ElementName_Triangle(elemType) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: elemType
    CHARACTER(:), ALLOCATABLE :: ans
  END FUNCTION ElementName_Triangle
END INTERFACE

!----------------------------------------------------------------------------
!                                             FacetTopology@GeometryMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-03-22
! summary: Returns the facet topology of the given element type

INTERFACE
  MODULE PURE SUBROUTINE FacetTopology_Triangle(elemType, nptrs, ans)
    INTEGER(I4B), INTENT(IN) :: elemType
    INTEGER(I4B), INTENT(IN) :: nptrs(:)
    TYPE(ReferenceTopology_), INTENT(INOUT) :: ans(:)
  END SUBROUTINE FacetTopology_Triangle
END INTERFACE

!----------------------------------------------------------------------------
!                                                    TotalEntities_Triangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-03-22
! summary:  Returns total entities

INTERFACE
  MODULE PURE FUNCTION TotalEntities_Triangle(elemType) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: elemType
    INTEGER(I4B) :: ans(4)
  END FUNCTION TotalEntities_Triangle
END INTERFACE

!----------------------------------------------------------------------------
!                                               TotalNodesInElement_Triangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-03-22
! summary:  Returns total nodes in element

INTERFACE
  MODULE PURE FUNCTION TotalNodesInElement_Triangle(elemType) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: elemType
    INTEGER(I4B) :: ans
  END FUNCTION TotalNodesInElement_Triangle
END INTERFACE

!----------------------------------------------------------------------------
!                                                     ElementType_Triangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-03-22
! summary:  Returns the type of element from char name

INTERFACE
  MODULE PURE FUNCTION ElementType_Triangle(elemName) RESULT(ans)
    CHARACTER(*), INTENT(IN) :: elemName
    INTEGER(I4B) :: ans
  END FUNCTION ElementType_Triangle
END INTERFACE

!----------------------------------------------------------------------------
!                                                      ElementOrder_Triangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-03-22
! summary:  Returns the order of element

INTERFACE
  MODULE PURE FUNCTION ElementOrder_Triangle(elemType) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: elemType
    INTEGER(I4B) :: ans
  END FUNCTION ElementOrder_Triangle
END INTERFACE

!----------------------------------------------------------------------------
!                                                     FacetElements_Triangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-03-21
! summary:  Get FacetElements

INTERFACE FacetElements_Triangle
  MODULE SUBROUTINE FacetElements_Triangle1(refelem, ans)
    CLASS(ReferenceElement_), INTENT(IN) :: refelem
    TYPE(ReferenceElement_), INTENT(INOUT) :: ans(:)
  END SUBROUTINE FacetElements_Triangle1
END INTERFACE FacetElements_Triangle

!----------------------------------------------------------------------------
!                                                    FacetElements_Triangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-03-21
! summary:  Get FacetElements

INTERFACE FacetElements_Triangle
  MODULE SUBROUTINE FacetElements_Triangle2(elemType, nsd, ans)
    INTEGER(I4B), INTENT(IN) :: elemType
    INTEGER(I4B), INTENT(IN) :: nsd
    TYPE(ReferenceElement_), INTENT(INOUT) :: ans(:)
  END SUBROUTINE FacetElements_Triangle2
END INTERFACE FacetElements_Triangle

!----------------------------------------------------------------------------
!                                                         Initiate@Triangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 3 March 2021
! summary: This routine constructs an instance of [[ReferenceTriangle_]]
!
!# Introduction
!
! - This routine contructs an instance of [[ReferenceTriangle_]]
! - User can specify the coordinates of the trinagle
!
!@note
! This routine will contruct a three node triangle.
! Also, SHAPE(xij) = [3,3]
!@endnote
!
!### Usage
!
!```fortran
! subroutine test1
!   type( ReferenceTriangle_ ) :: obj
!   real( dfp ) :: xij( 3, 3 )
!   xij( 1, 1:3 ) = [1.0, 2.0, 1.0]
!   xij( 2, 1:3 ) = [0.0, 0.0, 1.0]
!   xij( 3, : ) = 0.0
!   call initiate( obj, nsd = 2, xij = xij )
!   call display( obj, "obj : " )
! end
!```

INTERFACE Initiate
  MODULE PURE SUBROUTINE Initiate_Ref_Triangle(obj, nsd, xij, domainName)
    CLASS(ReferenceTriangle_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: nsd
    REAL(DFP), INTENT(IN), OPTIONAL :: xij(:, :)
    CHARACTER(*), OPTIONAL, INTENT(IN) :: domainName
  END SUBROUTINE Initiate_Ref_Triangle
END INTERFACE Initiate

!----------------------------------------------------------------------------
!                                                 ReferenceTriangle@Triangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         3 March 2021
! summary: This function returns an instance of [[ReferenceTriangle_]]
!
!# Introduction
! * This routine contructs an instance of [[ReferenceTriangle_]]
! * User can specify the coordinates of the trinagle
!@note
! This routine will contruct a three node triangle. Also, SHAPE(xij) = [3,3]
!@endnote
!
!### Usage
!
!```fortran
! subroutine test2
!   type( ReferenceTriangle_ ) :: obj
!   obj = referenceTriangle( nsd = 2 )
!   call display( obj, "obj : " )
! end
!```

INTERFACE ReferenceTriangle
  MODULE PURE FUNCTION Reference_Triangle(nsd, xij, domainName) RESULT(obj)
    INTEGER(I4B), INTENT(IN) :: nsd
    REAL(DFP), INTENT(IN), OPTIONAL :: xij(:, :)
    CHARACTER(*), OPTIONAL, INTENT(IN) :: domainName
    TYPE(ReferenceTriangle_) :: obj
  END FUNCTION Reference_Triangle
END INTERFACE ReferenceTriangle

!----------------------------------------------------------------------------
!                                         ReferenceTriangle_Pointer@Triangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         3 March 2021
! summary: This function returns an instance of [[ReferenceTriangle_]]
!
!# Introduction
! * This routine contructs an instance of [[ReferenceTriangle_]]
! * User can specify the coordinates of the trinagle
!@note
!This routine will contruct a three node triangle. Also, SHAPE(xij) = [3,3]
!@endnote
!
!### Usage
!
!```fortran
! subroutine test3
!   class( ReferenceElement_ ), pointer :: obj => null()
!   obj => referenceTriangle_pointer( nsd = 2 )
!   call display( obj, "obj : " )
! end
!```

INTERFACE ReferenceTriangle_Pointer
  MODULE FUNCTION Reference_Triangle_Pointer(nsd, xij, domainName) &
    & RESULT(obj)
    INTEGER(I4B), INTENT(IN) :: nsd
    REAL(DFP), INTENT(IN), OPTIONAL :: xij(:, :)
    CHARACTER(*), OPTIONAL, INTENT(IN) :: domainName
    CLASS(ReferenceTriangle_), POINTER :: obj
  END FUNCTION Reference_Triangle_Pointer
END INTERFACE ReferenceTriangle_Pointer

!----------------------------------------------------------------------------
!                                                   LagrangeElement@Triangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 1 March 2021
! summary: Returns lagrange Triangle element of higher order
!
!# Introduction
!         This routine retuns the lagrance element of higher order
! This routine will be called by [[ReferenceTriangle_:LagrangeElement]]
! Currently upto 3rd order triangle elements are supported.
!
!### Usage
!
!```fortran
! subroutine test4
!   class( ReferenceElement_ ), pointer :: obj_ptr => null()
!   type( ReferenceTriangle_ ) :: obj
!   obj_ptr => referenceTriangle_pointer( nsd = 2 )
!   call obj_ptr%highorderElement( order = 2, Highorderobj = obj )
!   call display( obj, "higher order obj : ")
!   call obj_ptr%highorderElement( order = 3, Highorderobj = obj )
!   call display( obj, "3rd order obj : ")
! end
!```

INTERFACE
  MODULE SUBROUTINE HighorderElement_Triangle(refelem, order, obj, ipType)
    CLASS(ReferenceElement_), INTENT(IN) :: refelem
    INTEGER(I4B), INTENT(IN) :: order
    CLASS(ReferenceElement_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: ipType
  END SUBROUTINE HighorderElement_Triangle
END INTERFACE

!----------------------------------------------------------------------------
!                                                 MeasureSimplex@Triangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 5 March 2021
! summary: Returns the measure of linear triangle
!
!# Introduction
!
! This function returns the measure of linear triangle. This function belongs
! to the generic function [[ReferenceElement_Method:MeasureSimplex]].

INTERFACE
  MODULE PURE FUNCTION Measure_Simplex_Triangle(refelem, xij) RESULT(Ans)
    CLASS(ReferenceElement_), INTENT(IN) :: refelem
    REAL(DFP), INTENT(IN) :: xij(:, :)
    REAL(DFP) :: Ans
  END FUNCTION Measure_Simplex_Triangle
END INTERFACE

!----------------------------------------------------------------------------
!                                                            Angles@Triangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         5 March 2021
! summary: Returns three angles of a triangle

INTERFACE Angles
  MODULE PURE FUNCTION Triangle_angles(refelem, xij) RESULT(Ans)
    CLASS(ReferenceElement_), INTENT(IN) :: refelem
    REAL(DFP), INTENT(IN) :: xij(:, :)
    REAL(DFP) :: Ans(3)
  END FUNCTION Triangle_angles
END INTERFACE Angles

!----------------------------------------------------------------------------
!                                                             Area@Triangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         5 March 2021
! summary:         Returns the area of triangle

INTERFACE Area
  MODULE PURE FUNCTION Triangle_area(refelem, xij) RESULT(Ans)
    CLASS(ReferenceElement_), INTENT(IN) :: refelem
    REAL(DFP), INTENT(IN) :: xij(:, :)
    REAL(DFP) :: Ans
  END FUNCTION Triangle_area
END INTERFACE Area

!----------------------------------------------------------------------------
!                                                        ArealVector@Triangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 5 March 2021
! summary: Returns the area vector

INTERFACE ArealVector
  MODULE PURE FUNCTION Triangle_arealVector(refelem, xij) RESULT(Ans)
    CLASS(ReferenceElement_), INTENT(IN) :: refelem
    REAL(DFP), INTENT(IN) :: xij(:, :)
    REAL(DFP) :: Ans(3)
  END FUNCTION Triangle_arealVector
END INTERFACE ArealVector

!----------------------------------------------------------------------------
!                                                      Barycentric@Triangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         5 March 2021
! summary: Returns the barycentric coordinates of triangle

INTERFACE Barycentric
  MODULE PURE FUNCTION Triangle_barycentric(refelem, xij, x) RESULT(Ans)
    CLASS(ReferenceElement_), INTENT(IN) :: refelem
    REAL(DFP), INTENT(IN) :: xij(:, :)
    REAL(DFP), INTENT(IN) :: x(:)
    REAL(DFP) :: Ans(3)
  END FUNCTION Triangle_barycentric
END INTERFACE Barycentric

!----------------------------------------------------------------------------
!                                                          Centroid@Triangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         5 March 2021
! summary: Returns the centroid of a triangle

INTERFACE Centroid
  MODULE PURE FUNCTION Triangle_centroid(refelem, xij) RESULT(Ans)
    CLASS(ReferenceElement_), INTENT(IN) :: refelem
    REAL(DFP), INTENT(IN) :: xij(:, :)
    REAL(DFP) :: Ans(3)
  END FUNCTION Triangle_centroid
END INTERFACE Centroid

!----------------------------------------------------------------------------
!                                                      CircumCenter@Triangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         5 March 2021
! summary:         Returns the circum center of the triangle

INTERFACE CircumCenter
  MODULE PURE FUNCTION Triangle_circumcentre(refelem, xij) RESULT(Ans)
    CLASS(ReferenceElement_), INTENT(IN) :: refelem
    REAL(DFP), INTENT(IN) :: xij(:, :)
    REAL(DFP) :: Ans(3)
  END FUNCTION Triangle_circumcentre
END INTERFACE CircumCenter

!----------------------------------------------------------------------------
!                                                      CircumCircle@Triangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         5 March 2021
! summary: Returns circum circle of triangle

INTERFACE CircumCircle
  MODULE PURE FUNCTION Triangle_circumcircle(refelem, xij) RESULT(Ans)
    CLASS(ReferenceElement_), INTENT(IN) :: refelem
    REAL(DFP), INTENT(IN) :: xij(:, :)
    REAL(DFP) :: Ans(4)
    !! Ans(1) = radius and Ans(2:4) center
  END FUNCTION Triangle_circumcircle
END INTERFACE CircumCircle

!----------------------------------------------------------------------------
!                                                      CircumRadius@Triangle
!----------------------------------------------------------------------------

INTERFACE CircumRadius
  MODULE PURE FUNCTION Triangle_circumradius(refelem, xij) RESULT(Ans)
    CLASS(ReferenceElement_), INTENT(IN) :: refelem
    REAL(DFP), INTENT(IN) :: xij(:, :)
    REAL(DFP) :: Ans
  END FUNCTION Triangle_circumradius
END INTERFACE CircumRadius

!----------------------------------------------------------------------------
!                                                     ContainsLine@Triangle
!----------------------------------------------------------------------------

INTERFACE ContainsLine
  MODULE PURE SUBROUTINE Triangle_contains_line(refelem, xij, x1, x2, &
    & parametricLine, inside, xint)
    CLASS(ReferenceElement_), INTENT(IN) :: refelem
    REAL(DFP), INTENT(IN) :: xij(:, :), x1(3), x2(3)
    LOGICAL(LGT), INTENT(IN) :: parametricLine
    LOGICAL(LGT), INTENT(OUT) :: inside
    REAL(DFP), INTENT(OUT) :: xint(3)
  END SUBROUTINE Triangle_contains_line
END INTERFACE ContainsLine

!----------------------------------------------------------------------------
!                                                    ContainsPoint@Triangle
!----------------------------------------------------------------------------

INTERFACE Contains_Point_Triangle
  MODULE PURE FUNCTION Triangle_Contains_Point(refelem, xij, x) RESULT(Ans)
    CLASS(ReferenceElement_), INTENT(IN) :: refelem
    REAL(DFP), INTENT(IN) :: xij(:, :), x(:)
    LOGICAL(LGT) :: Ans
  END FUNCTION Triangle_Contains_Point
END INTERFACE Contains_Point_Triangle

!----------------------------------------------------------------------------
!                                                         Diameter@Triangle
!----------------------------------------------------------------------------

INTERFACE Diameter
  MODULE PURE FUNCTION triangle_diameter(refelem, xij) RESULT(Ans)
    CLASS(ReferenceElement_), INTENT(IN) :: refelem
    REAL(DFP), INTENT(IN) :: xij(:, :)
    REAL(DFP) :: Ans
  END FUNCTION triangle_diameter
END INTERFACE Diameter

!----------------------------------------------------------------------------
!                                                       EdgeLength@Triangle
!----------------------------------------------------------------------------

INTERFACE EdgeLength
  MODULE PURE FUNCTION triangle_edge_length(refelem, xij) RESULT(Ans)
    CLASS(ReferenceElement_), INTENT(IN) :: refelem
    REAL(DFP), INTENT(IN) :: xij(:, :)
    REAL(DFP) :: Ans(3)
  END FUNCTION triangle_edge_length
END INTERFACE EdgeLength

!----------------------------------------------------------------------------
!                                                         Incenter@Triangle
!----------------------------------------------------------------------------

INTERFACE Incenter
  MODULE PURE FUNCTION triangle_incenter(refelem, xij) RESULT(Ans)
    CLASS(ReferenceElement_), INTENT(IN) :: refelem
    REAL(DFP), INTENT(IN) :: xij(:, :)
    REAL(DFP) :: Ans(3)
  END FUNCTION triangle_incenter
END INTERFACE Incenter

!----------------------------------------------------------------------------
!                                                         Incircle@Triangle
!----------------------------------------------------------------------------

INTERFACE Incircle
  MODULE PURE FUNCTION triangle_incircle(refelem, xij) RESULT(Ans)
    CLASS(ReferenceElement_), INTENT(IN) :: refelem
    REAL(DFP), INTENT(IN) :: xij(:, :)
    REAL(DFP) :: Ans(4)
  END FUNCTION triangle_incircle
END INTERFACE Incircle

!----------------------------------------------------------------------------
!                                                         Inradius@Triangle
!----------------------------------------------------------------------------

INTERFACE Inradius
  MODULE PURE FUNCTION triangle_inradius(refelem, xij) RESULT(Ans)
    CLASS(ReferenceElement_), INTENT(IN) :: refelem
    REAL(DFP), INTENT(IN) :: xij(:, :)
    REAL(DFP) :: Ans
  END FUNCTION triangle_inradius
END INTERFACE Inradius

!----------------------------------------------------------------------------
!                                                      Orthocenter@Triangle
!----------------------------------------------------------------------------

INTERFACE Orthocenter
  MODULE PURE FUNCTION triangle_orthocenter(refelem, xij) RESULT(Ans)
    CLASS(ReferenceElement_), INTENT(IN) :: refelem
    REAL(DFP), INTENT(IN) :: xij(:, :)
    REAL(DFP) :: Ans(3)
  END FUNCTION triangle_orthocenter
END INTERFACE Orthocenter

!----------------------------------------------------------------------------
!                                                DistanceFromPoint@Triangle
!----------------------------------------------------------------------------

INTERFACE DistanceFromPoint
  MODULE PURE FUNCTION triangle_point_dist(refelem, xij, x) &
    & RESULT(Ans)
    CLASS(ReferenceElement_), INTENT(IN) :: refelem
    REAL(DFP), INTENT(IN) :: xij(:, :), x(:)
    REAL(DFP) :: Ans
  END FUNCTION triangle_point_dist
END INTERFACE DistanceFromPoint

!----------------------------------------------------------------------------
!                                                      NearestPoint@Triangle
!----------------------------------------------------------------------------

INTERFACE NearestPoint
  MODULE PURE SUBROUTINE triangle_get_nearest_point(refelem, xij, x, xn,  &
    & dist)
    CLASS(ReferenceElement_), INTENT(IN) :: refelem
    REAL(DFP), INTENT(IN) :: xij(:, :), x(:)
    REAL(DFP), INTENT(INOUT) :: xn(:)
    REAL(DFP), INTENT(OUT) :: dist
  END SUBROUTINE triangle_get_nearest_point
END INTERFACE NearestPoint

!----------------------------------------------------------------------------
!                                                       RandomPoint@Triangle
!----------------------------------------------------------------------------

INTERFACE RandomPoint
  MODULE PURE FUNCTION triangle_random_point(refelem, xij, n, seed) &
    & RESULT(Ans)
    CLASS(ReferenceElement_), INTENT(IN) :: refelem
    REAL(DFP), INTENT(IN) :: xij(:, :)
    INTEGER(I4B), INTENT(IN) :: n
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: seed
    REAL(DFP) :: Ans(3, n)
  END FUNCTION triangle_random_point
END INTERFACE RandomPoint

!----------------------------------------------------------------------------
!                                                          Quality@Triangle
!----------------------------------------------------------------------------

INTERFACE Quality_Triangle
  MODULE PURE FUNCTION Triangle_Quality(refelem, xij, measure) RESULT(Ans)
    CLASS(ReferenceElement_), INTENT(IN) :: refelem
    REAL(DFP), INTENT(IN) :: xij(:, :)
    INTEGER(I4B), INTENT(IN) :: measure
    REAL(DFP) :: Ans
  END FUNCTION Triangle_Quality
END INTERFACE Quality_Triangle

!----------------------------------------------------------------------------
!                                                            TriangleArea3D
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 10 Aug 2022
! summary: Area of triangle in 3D
!
!# Introduction
!
!- TRIANGLEAREA3D computes the area of a triangle in 3D.
!- This routine uses the fact that the norm of the cross product
! of two vectors is the area of the parallelogram they form.
! Therefore, the area of the triangle is half of that value.

INTERFACE
  MODULE PURE SUBROUTINE TriangleArea3D(t, ans)
    REAL(DFP), INTENT(IN) :: t(3, 3)
    REAL(DFP), INTENT(OUT) :: ans
  END SUBROUTINE TriangleArea3D
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 10 Aug 2022
! summary:         Return are of triangle in 2D
!
!# Introduction
!
!- TRIANGLEAREA2D computes the area of a triangle in 2D.
!- If the triangle's vertices are given in counter clockwise order,
! the area will be positive.  If the triangle's vertices are given
! in clockwise order, the area will be negative!

INTERFACE
  MODULE PURE SUBROUTINE TriangleArea2D(t, ans)
    REAL(DFP), INTENT(IN) :: t(2, 3)
    REAL(DFP), INTENT(OUT) :: ans
  END SUBROUTINE TriangleArea2D
END INTERFACE

!----------------------------------------------------------------------------
!                                                        GetEdgeConnectivity
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-03-08
! summary:  Returns number of edges in the element

INTERFACE
  MODULE PURE SUBROUTINE GetEdgeConnectivity_Triangle(con, opt, order)
    INTEGER(I4B), INTENT(INOUT) :: con(:, :)
    !! Connectivity
    !! The columns represents the edge number
    !! The row represents a edge
    !! con should be allocated by the user
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: opt
    !! If opt = 1, then edge connectivity for hierarchial approximation
    !! [1,2], [1,3], [2,3]. This is DEFAULT
    !! If opt =2, then edge connectivity for Lagrangian approximation
    !! [1,2], [2,3], [3,1]
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: order
    !! order of element
    !! Currently order is used only when opt=2
    !! Currently any order is valid
  END SUBROUTINE GetEdgeConnectivity_Triangle
END INTERFACE

!----------------------------------------------------------------------------
!                                                           RefTriangleCoord
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-07-03
! summary:  Returns the coordinate of reference triangle

INTERFACE RefCoord_Triangle
  MODULE PURE FUNCTION RefTriangleCoord(refTriangle) RESULT(ans)
    CHARACTER(*), INTENT(IN) :: refTriangle
    REAL(DFP) :: ans(2, 3)
  END FUNCTION RefTriangleCoord
END INTERFACE RefCoord_Triangle

!----------------------------------------------------------------------------
!                                           FaceShapeMetaData_Triangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-03-13
! summary:  Returns meta data for global orientation of face

INTERFACE
  MODULE SUBROUTINE FaceShapeMetaData_Triangle(face, sorted_face,  &
    & faceOrient, localFaces)
    INTEGER(I4B), INTENT(INOUT) :: face(:)
    INTEGER(I4B), INTENT(INOUT) :: sorted_face(:)
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: faceOrient(:)
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: localFaces(:)
  END SUBROUTINE FaceShapeMetaData_Triangle
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE ReferenceTriangle_Method
