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

!----------------------------------------------------------------------------
!                                                         Initiate@Triangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         3 March 2021
! summary: This routine constructs an instance of [[ReferenceTriangle_]]
!
!# Introduction
!
! - This routine contructs an instance of [[ReferenceTriangle_]]
! - User can specify the coordinates of the trinagle
!
!@note
!         This routine will contruct a three node triangle. Also, SHAPE(xij) = [3,3]
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

INTERFACE
  MODULE PURE SUBROUTINE initiate_ref_Triangle(obj, nsd, xij)
    CLASS(ReferenceTriangle_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: nsd
    REAL(DFP), INTENT(IN), OPTIONAL :: xij(:, :)
  END SUBROUTINE initiate_ref_Triangle
END INTERFACE

INTERFACE Initiate
  MODULE PROCEDURE initiate_ref_Triangle
END INTERFACE Initiate

PUBLIC :: Initiate

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

INTERFACE
  MODULE PURE FUNCTION reference_Triangle(nsd, xij) RESULT(obj)
    INTEGER(I4B), INTENT(IN) :: nsd
    REAL(DFP), INTENT(IN), OPTIONAL :: xij(:, :)
    TYPE(ReferenceTriangle_) :: obj
  END FUNCTION reference_Triangle
END INTERFACE

INTERFACE ReferenceTriangle
  MODULE PROCEDURE reference_Triangle
END INTERFACE ReferenceTriangle

PUBLIC :: ReferenceTriangle

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

INTERFACE
  MODULE PURE FUNCTION reference_Triangle_pointer(nsd, xij) RESULT(obj)
    INTEGER(I4B), INTENT(IN) :: nsd
    REAL(DFP), INTENT(IN), OPTIONAL :: xij(:, :)
    CLASS(ReferenceTriangle_), POINTER :: obj
  END FUNCTION reference_Triangle_pointer
END INTERFACE

INTERFACE ReferenceTriangle_Pointer
  MODULE PROCEDURE reference_Triangle_Pointer
END INTERFACE ReferenceTriangle_Pointer

PUBLIC :: ReferenceTriangle_Pointer

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
  MODULE PURE SUBROUTINE highorderElement_Triangle(refelem, order, obj,  &
    & ipType)
    CLASS(ReferenceElement_), INTENT(IN) :: refelem
    INTEGER(I4B), INTENT(IN) :: order
    CLASS(ReferenceElement_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: ipType
  END SUBROUTINE highorderElement_Triangle
END INTERFACE

PUBLIC :: highorderElement_Triangle

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
!
!### Usage
!
!```fortran
!
!```

INTERFACE
  MODULE PURE FUNCTION Measure_Simplex_Triangle(refelem, xij) RESULT(Ans)
    CLASS(ReferenceTriangle_), INTENT(IN) :: refelem
    REAL(DFP), INTENT(IN) :: xij(:, :)
    REAL(DFP) :: Ans
  END FUNCTION Measure_Simplex_Triangle
END INTERFACE

PUBLIC :: Measure_Simplex_Triangle

!----------------------------------------------------------------------------
!                                                            Angles@Triangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         5 March 2021
! summary: Returns three angles of a triangle

INTERFACE
  MODULE PURE FUNCTION triangle_angles(refelem, xij) RESULT(Ans)
    CLASS(ReferenceTriangle_), INTENT(IN) :: refelem
    REAL(DFP), INTENT(IN) :: xij(:, :)
    REAL(DFP) :: Ans(3)
  END FUNCTION triangle_angles
END INTERFACE

INTERFACE Angles
  MODULE PROCEDURE triangle_angles
END INTERFACE Angles

PUBLIC :: Angles

!----------------------------------------------------------------------------
!                                                             Area@Triangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         5 March 2021
! summary:         Returns the area of triangle

INTERFACE
  MODULE PURE FUNCTION triangle_area(refelem, xij) RESULT(Ans)
    CLASS(ReferenceTriangle_), INTENT(IN) :: refelem
    REAL(DFP), INTENT(IN) :: xij(:, :)
    REAL(DFP) :: Ans
  END FUNCTION triangle_area
END INTERFACE

INTERFACE Area
  MODULE PROCEDURE triangle_area
END INTERFACE Area

PUBLIC :: Area

!----------------------------------------------------------------------------
!                                                        ArealVector@Triangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 5 March 2021
! summary: Returns the area vector

INTERFACE
  MODULE PURE FUNCTION triangle_arealVector(refelem, xij) RESULT(Ans)
    CLASS(ReferenceTriangle_), INTENT(IN) :: refelem
    REAL(DFP), INTENT(IN) :: xij(:, :)
    REAL(DFP) :: Ans(3)
  END FUNCTION triangle_arealVector
END INTERFACE

INTERFACE ArealVector
  MODULE PROCEDURE triangle_arealVector
END INTERFACE ArealVector

PUBLIC :: ArealVector

!----------------------------------------------------------------------------
!                                                      Barycentric@Triangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         5 March 2021
! summary: Returns the barycentric coordinates of triangle

INTERFACE
  MODULE PURE FUNCTION triangle_barycentric(refelem, xij, x) RESULT(Ans)
    CLASS(ReferenceTriangle_), INTENT(IN) :: refelem
    REAL(DFP), INTENT(IN) :: xij(:, :)
    REAL(DFP), INTENT(IN) :: x(:)
    REAL(DFP) :: Ans(3)
  END FUNCTION triangle_barycentric
END INTERFACE

INTERFACE Barycentric
  MODULE PROCEDURE triangle_barycentric
END INTERFACE Barycentric

PUBLIC :: Barycentric

!----------------------------------------------------------------------------
!                                                          Centroid@Triangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         5 March 2021
! summary: Returns the centroid of a triangle

INTERFACE
  MODULE PURE FUNCTION triangle_centroid(refelem, xij) RESULT(Ans)
    CLASS(ReferenceTriangle_), INTENT(IN) :: refelem
    REAL(DFP), INTENT(IN) :: xij(:, :)
    REAL(DFP) :: Ans(3)
  END FUNCTION triangle_centroid
END INTERFACE

INTERFACE Centroid
  MODULE PROCEDURE triangle_centroid
END INTERFACE Centroid

PUBLIC :: Centroid

!----------------------------------------------------------------------------
!                                                      CircumCenter@Triangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         5 March 2021
! summary:         Returns the circum center of the triangle

INTERFACE
  MODULE PURE FUNCTION triangle_circumcentre(refelem, xij) RESULT(Ans)
    CLASS(ReferenceTriangle_), INTENT(IN) :: refelem
    REAL(DFP), INTENT(IN) :: xij(:, :)
    REAL(DFP) :: Ans(3)
  END FUNCTION triangle_circumcentre
END INTERFACE

INTERFACE CircumCenter
  MODULE PROCEDURE triangle_circumcentre
END INTERFACE CircumCenter

PUBLIC :: CircumCenter

!----------------------------------------------------------------------------
!                                                      CircumCircle@Triangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         5 March 2021
! summary: Returns circum circle of triangle

INTERFACE
  MODULE PURE FUNCTION triangle_circumcircle(refelem, xij) RESULT(Ans)
    CLASS(ReferenceTriangle_), INTENT(IN) :: refelem
    REAL(DFP), INTENT(IN) :: xij(:, :)
    REAL(DFP) :: Ans(4)
    !! Ans(1) = radius and Ans(2:4) center
  END FUNCTION triangle_circumcircle
END INTERFACE

INTERFACE CircumCircle
  MODULE PROCEDURE triangle_circumcircle
END INTERFACE CircumCircle

PUBLIC :: CircumCircle

!----------------------------------------------------------------------------
!                                                      CircumRadius@Triangle
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION triangle_circumradius(refelem, xij) RESULT(Ans)
    CLASS(ReferenceTriangle_), INTENT(IN) :: refelem
    REAL(DFP), INTENT(IN) :: xij(:, :)
    REAL(DFP) :: Ans
  END FUNCTION triangle_circumradius
END INTERFACE

INTERFACE CircumRadius
  MODULE PROCEDURE triangle_circumradius
END INTERFACE CircumRadius

PUBLIC :: CircumRadius

!----------------------------------------------------------------------------
!                                                     ContainsLine@Triangle
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE SUBROUTINE triangle_contains_line(refelem, xij, x1, x2, &
    & parametricLine, inside, xint)
    CLASS(ReferenceTriangle_), INTENT(IN) :: refelem
    REAL(DFP), INTENT(IN) :: xij(:, :), x1(3), x2(3)
    LOGICAL(LGT), INTENT(IN) :: parametricLine
    LOGICAL(LGT), INTENT(OUT) :: inside
    REAL(DFP), INTENT(OUT) :: xint(3)
  END SUBROUTINE triangle_contains_line
END INTERFACE

INTERFACE ContainsLine
  MODULE PROCEDURE triangle_contains_line
END INTERFACE ContainsLine

PUBLIC :: ContainsLine

!----------------------------------------------------------------------------
!                                                    ContainsPoint@Triangle
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION triangle_contains_point(refelem, xij, x) RESULT(Ans)
    CLASS(ReferenceTriangle_), INTENT(IN) :: refelem
    REAL(DFP), INTENT(IN) :: xij(:, :), x(:)
    LOGICAL(LGT) :: Ans
  END FUNCTION triangle_contains_point
END INTERFACE

PUBLIC :: triangle_contains_point

!----------------------------------------------------------------------------
!                                                         Diameter@Triangle
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION triangle_diameter(refelem, xij) RESULT(Ans)
    CLASS(ReferenceTriangle_), INTENT(IN) :: refelem
    REAL(DFP), INTENT(IN) :: xij(:, :)
    REAL(DFP) :: Ans
  END FUNCTION triangle_diameter
END INTERFACE

INTERFACE diameter
  MODULE PROCEDURE triangle_diameter
END INTERFACE diameter

PUBLIC :: diameter

!----------------------------------------------------------------------------
!                                                       EdgeLength@Triangle
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION triangle_edge_length(refelem, xij) RESULT(Ans)
    CLASS(ReferenceTriangle_), INTENT(IN) :: refelem
    REAL(DFP), INTENT(IN) :: xij(:, :)
    REAL(DFP) :: Ans(3)
  END FUNCTION triangle_edge_length
END INTERFACE

INTERFACE EdgeLength
  MODULE PROCEDURE triangle_edge_length
END INTERFACE EdgeLength

PUBLIC :: EdgeLength

!----------------------------------------------------------------------------
!                                                         Incenter@Triangle
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION triangle_incenter(refelem, xij) RESULT(Ans)
    CLASS(ReferenceTriangle_), INTENT(IN) :: refelem
    REAL(DFP), INTENT(IN) :: xij(:, :)
    REAL(DFP) :: Ans(3)
  END FUNCTION triangle_incenter
END INTERFACE

INTERFACE Incenter
  MODULE PROCEDURE triangle_incenter
END INTERFACE Incenter

PUBLIC :: Incenter

!----------------------------------------------------------------------------
!                                                         Incircle@Triangle
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION triangle_incircle(refelem, xij) RESULT(Ans)
    CLASS(ReferenceTriangle_), INTENT(IN) :: refelem
    REAL(DFP), INTENT(IN) :: xij(:, :)
    REAL(DFP) :: Ans(4)
  END FUNCTION triangle_incircle
END INTERFACE

INTERFACE Incircle
  MODULE PROCEDURE triangle_incircle
END INTERFACE Incircle

PUBLIC :: Incircle

!----------------------------------------------------------------------------
!                                                         Inradius@Triangle
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION triangle_inradius(refelem, xij) RESULT(Ans)
    CLASS(ReferenceTriangle_), INTENT(IN) :: refelem
    REAL(DFP), INTENT(IN) :: xij(:, :)
    REAL(DFP) :: Ans
  END FUNCTION triangle_inradius
END INTERFACE

INTERFACE Inradius
  MODULE PROCEDURE triangle_inradius
END INTERFACE Inradius

PUBLIC :: Inradius

!----------------------------------------------------------------------------
!                                                      Orthocenter@Triangle
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION triangle_orthocenter(refelem, xij) RESULT(Ans)
    CLASS(ReferenceTriangle_), INTENT(IN) :: refelem
    REAL(DFP), INTENT(IN) :: xij(:, :)
    REAL(DFP) :: Ans(3)
  END FUNCTION triangle_orthocenter
END INTERFACE

INTERFACE Orthocenter
  MODULE PROCEDURE triangle_orthocenter
END INTERFACE Orthocenter

PUBLIC :: Orthocenter

!----------------------------------------------------------------------------
!                                                DistanceFromPoint@Triangle
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION triangle_point_dist(refelem, xij, x) &
    & RESULT(Ans)
    CLASS(ReferenceTriangle_), INTENT(IN) :: refelem
    REAL(DFP), INTENT(IN) :: xij(:, :), x(:)
    REAL(DFP) :: Ans
  END FUNCTION triangle_point_dist
END INTERFACE

INTERFACE DistanceFromPoint
  MODULE PROCEDURE triangle_point_dist
END INTERFACE DistanceFromPoint

PUBLIC :: DistanceFromPoint

!----------------------------------------------------------------------------
!                                                      NearestPoint@Triangle
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE SUBROUTINE triangle_get_nearest_point(refelem, xij, x, xn, dist)
    CLASS(ReferenceTriangle_), INTENT(IN) :: refelem
    REAL(DFP), INTENT(IN) :: xij(:, :), x(:)
    REAL(DFP), INTENT(INOUT) :: xn(:)
    REAL(DFP), INTENT(OUT) :: dist
  END SUBROUTINE triangle_get_nearest_point
END INTERFACE

INTERFACE NearestPoint
  MODULE PROCEDURE triangle_get_nearest_point
END INTERFACE NearestPoint

PUBLIC :: NearestPoint

!----------------------------------------------------------------------------
!                                                       RandomPoint@Triangle
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION triangle_random_point(refelem, xij, n, seed) &
    & RESULT(Ans)
    CLASS(ReferenceTriangle_), INTENT(IN) :: refelem
    REAL(DFP), INTENT(IN) :: xij(:, :)
    INTEGER(I4B), INTENT(IN) :: n
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: seed
    REAL(DFP) :: Ans(3, n)
  END FUNCTION triangle_random_point
END INTERFACE

INTERFACE RandomPoint
  MODULE PROCEDURE triangle_random_point
END INTERFACE RandomPoint

PUBLIC :: RandomPoint

!----------------------------------------------------------------------------
!                                                          Quality@Triangle
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION triangle_quality(refelem, xij, measure) RESULT(Ans)
    CLASS(ReferenceTriangle_), INTENT(IN) :: refelem
    REAL(DFP), INTENT(IN) :: xij(:, :)
    INTEGER(I4B), INTENT(IN) :: measure
    REAL(DFP) :: Ans
  END FUNCTION triangle_quality
END INTERFACE

PUBLIC :: triangle_quality

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
  MODULE PURE SUBROUTINE TriangleArea3D(t, area)
    REAL(DFP), INTENT(IN) :: t(3, 3)
    REAL(DFP), INTENT(OUT) :: area
  END SUBROUTINE TriangleArea3D
END INTERFACE

PUBLIC :: TriangleArea3D

!-----------------------------------------------------------------------------
!
!-----------------------------------------------------------------------------

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
  MODULE PURE SUBROUTINE TriangleArea2D(t, area)
    REAL(DFP), INTENT(IN) :: t(2, 3)
    REAL(DFP), INTENT(OUT) :: area
  END SUBROUTINE TriangleArea2D
END INTERFACE

PUBLIC :: TriangleArea2D

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE ReferenceTriangle_Method
