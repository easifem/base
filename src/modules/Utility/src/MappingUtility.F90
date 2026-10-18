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
! date: 2022-10-19
! summary: Maps between reference elements and physical elements.
!
! Description:
!   This module contains routines that map points between reference
!   elements, and from a reference element to a physical element. It
!   also contains routines for barycentric coordinates and for the
!   Jacobian of these maps.
!
!   Points are stored in xij format: `xin(:, j)` holds the coordinates
!   of point `j`. So `SIZE(xin, 1)` is the number of spatial dimensions
!   and `SIZE(xin, 2)` is the number of points.
!
!   The reference elements and their vertices are:
!
!   - unit line: [0, 1],
!   - biunit line: [-1, 1],
!   - unit triangle: (0,0), (1,0), (0,1),
!   - biunit triangle: (-1,-1), (1,-1), (-1,1),
!   - unit quadrangle: (0,0), (1,0), (1,1), (0,1),
!   - biunit quadrangle: (-1,-1), (1,-1), (1,1), (-1,1),
!   - unit tetrahedron: (0,0,0), (1,0,0), (0,1,0), (0,0,1),
!   - biunit tetrahedron: (-1,-1,-1), (1,-1,-1), (-1,1,-1), (-1,-1,1),
!   - unit hexahedron [0,1]^3 and biunit hexahedron [-1,1]^3. Vertices
!     1 to 4 are the bottom face in the order of the quadrangle, and
!     vertices 5 to 8 are the top face in the same order.
!
!   Routines whose name ends with an underscore do not allocate. They
!   write into `ans` and return the number of entries (`tsize`) or of
!   rows and columns (`nrow`, `ncol`) that were written. `ans` must be
!   large enough; this is not checked.

MODULE MappingUtility
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
IMPLICIT NONE
PRIVATE

PUBLIC :: FromBiunitLine2Segment
PUBLIC :: FromBiunitLine2Segment_
PUBLIC :: FromBiUnitLine2UnitLine

PUBLIC :: FromSegment2BiunitLine
PUBLIC :: FromSegment2BiunitLine_

PUBLIC :: FromUnitLine2BiUnitLine
PUBLIC :: FromUnitLine2BiUnitLine_

PUBLIC :: FromLine2Line_

PUBLIC :: FromBiUnitQuadrangle2Quadrangle
PUBLIC :: FromBiUnitQuadrangle2Quadrangle_

PUBLIC :: FromBiUnitQuadrangle2UnitQuadrangle
PUBLIC :: FromBiUnitQuadrangle2UnitQuadrangle_
PUBLIC :: FromUnitQuadrangle2BiUnitQuadrangle
PUBLIC :: FromBiUnitHexahedron2Hexahedron_
PUBLIC :: FromBiUnitHexahedron2UnitHexahedron_
PUBLIC :: FromUnitHexahedron2BiUnitHexahedron_

PUBLIC :: FromBiUnitHexahedron2Hexahedron
PUBLIC :: FromBiUnitHexahedron2UnitHexahedron
PUBLIC :: FromUnitHexahedron2BiUnitHexahedron

PUBLIC :: FromBiUnitTriangle2BiUnitSqr
PUBLIC :: FromBiUnitTriangle2BiUnitQuadrangle

PUBLIC :: FromBiUnitSqr2BiUnitTriangle
PUBLIC :: FromBiUnitQuadrangle2BiUnitTriangle

PUBLIC :: FromUnitTriangle2BiUnitSqr
PUBLIC :: FromUnitTriangle2BiUnitQuadrangle

PUBLIC :: FromBiUnitSqr2UnitTriangle
PUBLIC :: FromBiUnitQuadrangle2UnitTriangle

PUBLIC :: FromTriangle2Square_
PUBLIC :: FromSquare2Triangle_

PUBLIC :: FromUnitTriangle2Triangle
PUBLIC :: FromUnitTriangle2Triangle_

PUBLIC :: BarycentricCoordUnitTriangle
!! This is function
PUBLIC :: BarycentricCoordBiUnitTriangle
!! This is function
PUBLIC :: BarycentricCoordTriangle
!! This is function
PUBLIC :: BarycentricCoordTriangle_
!! This is a subroutine without allocation

PUBLIC :: FromBiUnitTriangle2UnitTriangle
PUBLIC :: FromUnitTriangle2BiUnitTriangle

PUBLIC :: FromTriangle2Triangle_

PUBLIC :: FromUnitTetrahedron2BiUnitTetrahedron
PUBLIC :: FromUnitTetrahedron2BiUnitTetrahedron_

PUBLIC :: FromBiUnitTetrahedron2UnitTetrahedron
PUBLIC :: FromUnitTetrahedron2Tetrahedron
PUBLIC :: FromUnitTetrahedron2Tetrahedron_
PUBLIC :: FromBiUnitTetrahedron2Tetrahedron
PUBLIC :: BarycentricCoordUnitTetrahedron
PUBLIC :: BarycentricCoordUnitTetrahedron_
PUBLIC :: BarycentricCoordBiUnitTetrahedron
PUBLIC :: BarycentricCoordBiUnitTetrahedron_
PUBLIC :: BarycentricCoordTetrahedron
PUBLIC :: BarycentricCoordTetrahedron_
PUBLIC :: FromBiUnitTetrahedron2BiUnitHexahedron

PUBLIC :: FromBiUnitHexahedron2BiUnitTetrahedron
PUBLIC :: FromBiUnitHexahedron2BiUnitTetrahedron_

PUBLIC :: FromUnitTetrahedron2BiUnitHexahedron

PUBLIC :: FromBiUnitHexahedron2UnitTetrahedron
PUBLIC :: FromBiUnitHexahedron2UnitTetrahedron_

PUBLIC :: JacobianLine
PUBLIC :: JacobianTriangle
PUBLIC :: JacobianQuadrangle
PUBLIC :: JacobianHexahedron
PUBLIC :: JacobianTetrahedron
! PUBLIC :: JacobianPrism
! PUBLIC :: JacobianPyramid

!----------------------------------------------------------------------------
!                                                     FromBiunitLine2Segment
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-10-19
! summary: Maps points from the biunit line to the segment [x1, x2].
!
! Description:
!   Maps each point `xin(i)` of the biunit line [-1, 1] to the segment
!   [x1, x2] of the real line. The map is linear, so -1 goes to `x1`
!   and 1 goes to `x2`. `x1 > x2` is allowed and gives a reversed
!   segment.
!
! Arguments:
!   xin : REAL(DFP), rank(1) - coordinates in [-1, 1]
!   x1 : REAL(DFP) - first end point of the segment
!   x2 : REAL(DFP) - second end point of the segment
!
! Returns:
!   ans : REAL(DFP), rank(1) - mapped coordinates,
!     `SIZE(ans) = SIZE(xin)`
!
! Formula:
!   $$\mathrm{ans}_i = \frac{x_1 + x_2}{2} + \frac{x_2 - x_1}{2}\,\xi_i$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE MappingUtility, ONLY: FromBiunitLine2Segment
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
! REAL(DFP) :: xin(3), ans(3), ans1(1)
!
! xin = [-1.0_DFP, 0.0_DFP, 1.0_DFP]
!
! ! map [-1, 1] to [2, 6]
! ans = FromBiunitLine2Segment(xin, 2.0_DFP, 6.0_DFP)
! CALL OK(ALL(ABS(ans - [2.0_DFP, 4.0_DFP, 6.0_DFP]) .LT. tol), &
!         "end points and mid point")
!
! ! an interior point
! ans1 = FromBiunitLine2Segment([0.5_DFP], 2.0_DFP, 6.0_DFP)
! CALL OK(ABS(ans1(1) - 5.0_DFP) .LT. tol, "xi = 0.5")
!
! ! a reversed segment, x1 > x2
! ans = FromBiunitLine2Segment(xin, 6.0_DFP, 2.0_DFP)
! CALL OK(ALL(ABS(ans - [6.0_DFP, 4.0_DFP, 2.0_DFP]) .LT. tol), &
!         "reversed segment")
!
! END PROGRAM main
!```

INTERFACE FromBiunitLine2Segment
  MODULE PURE FUNCTION FromBiunitLine2Segment1(xin, x1, x2) RESULT(ans)
    REAL(DFP), INTENT(IN) :: xin(:)
    !! coordinates in [-1,1]
    REAL(DFP), INTENT(IN) :: x1
    !! x1 of physical domain
    REAL(DFP), INTENT(IN) :: x2
    !! x2 of physical  domain
    REAL(DFP) :: ans(SIZE(xin))
    !! mapped coordinates of xin in physical domain
  END FUNCTION FromBiunitLine2Segment1
END INTERFACE FromBiunitLine2Segment

!----------------------------------------------------------------------------
!                                                     FromSegment2BiunitLine
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-10-19
! summary: Maps points from the segment [x1, x2] to the biunit line.
!
! Description:
!   Maps each point `xin(i)` of the segment [x1, x2] to the biunit
!   line [-1, 1]. This is the inverse of `FromBiunitLine2Segment`.
!   `x1` and `x2` must be different; this is not checked.
!
! Arguments:
!   xin : REAL(DFP), rank(1) - coordinates in [x1, x2]
!   x1 : REAL(DFP) - first end point of the segment
!   x2 : REAL(DFP) - second end point of the segment
!
! Returns:
!   ans : REAL(DFP), rank(1) - coordinates in [-1, 1],
!     `SIZE(ans) = SIZE(xin)`
!
! Formula:
!   $$\mathrm{ans}_i = \frac{2 x_i - (x_1 + x_2)}{x_2 - x_1}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE MappingUtility, ONLY: FromSegment2BiunitLine
! USE MappingUtility, ONLY: FromBiunitLine2Segment
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
! REAL(DFP) :: xin(3), ans(3)
!
! ! end points and mid point of [2, 6]
! xin = [2.0_DFP, 4.0_DFP, 6.0_DFP]
! ans = FromSegment2BiunitLine(xin, 2.0_DFP, 6.0_DFP)
! CALL OK(ALL(ABS(ans - [-1.0_DFP, 0.0_DFP, 1.0_DFP]) .LT. tol), &
!         "end points and mid point")
!
! ! inverse of FromBiunitLine2Segment
! xin = [2.5_DFP, 3.0_DFP, 5.5_DFP]
! ans = FromBiunitLine2Segment( &
!       FromSegment2BiunitLine(xin, 2.0_DFP, 6.0_DFP), 2.0_DFP, 6.0_DFP)
! CALL OK(ALL(ABS(ans - xin) .LT. tol), "round trip")
!
! END PROGRAM main
!```

INTERFACE FromSegment2BiunitLine
  MODULE PURE FUNCTION FromSegment2BiunitLine1(xin, x1, x2) RESULT(ans)
    REAL(DFP), INTENT(IN) :: xin(:)
    !! coordinates in [x1,x2]
    REAL(DFP), INTENT(IN) :: x1
    !! x1 of physical domain
    REAL(DFP), INTENT(IN) :: x2
    !! x2 of physical  domain
    REAL(DFP) :: ans(SIZE(xin))
    !! mapped coordinates of xin in physical domain
  END FUNCTION FromSegment2BiunitLine1
END INTERFACE FromSegment2BiunitLine

!----------------------------------------------------------------------------
!                                                    FromBiunitLine2Segment_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-06-26
! summary: Maps the biunit line to [x1, x2] without allocation.
!
! Description:
!   Same as `FromBiunitLine2Segment(xin, x1, x2)`, but the result is
!   written to `ans(1:tsize)`, where `tsize = SIZE(xin)`. Entries of
!   `ans` after `tsize` are not changed.
!
! Arguments:
!   xin : REAL(DFP), rank(1) - coordinates in [-1, 1]
!   x1 : REAL(DFP) - first end point of the segment
!   x2 : REAL(DFP) - second end point of the segment
!   ans : REAL(DFP), rank(1) - output, `SIZE(ans) >= SIZE(xin)`
!   tsize : INTEGER(I4B) - number of entries written, `SIZE(xin)`
!
! Formula:
!   $$\mathrm{ans}_i = \frac{x_1 + x_2}{2} + \frac{x_2 - x_1}{2}\,\xi_i$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE MappingUtility, ONLY: FromBiunitLine2Segment
! USE MappingUtility, ONLY: FromBiunitLine2Segment_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
! REAL(DFP) :: xin(3), ans(5)
! INTEGER(I4B) :: tsize
!
! xin = [-1.0_DFP, 0.0_DFP, 1.0_DFP]
!
! ! ans can be larger than xin, only ans(1:tsize) is written
! ans = -100.0_DFP
! CALL FromBiunitLine2Segment_(xin, 2.0_DFP, 6.0_DFP, ans, tsize)
! CALL OK(tsize .EQ. 3_I4B, "tsize is SIZE(xin)")
! CALL OK(ALL(ABS(ans(1:tsize) - [2.0_DFP, 4.0_DFP, 6.0_DFP]) .LT. tol), &
!         "mapped values")
! CALL OK(ALL(ABS(ans(4:5) + 100.0_DFP) .LT. tol), &
!         "extra entries are not changed")
!
! ! same as the function
! CALL OK(ALL(ABS(ans(1:tsize) - &
!                 FromBiunitLine2Segment(xin, 2.0_DFP, 6.0_DFP)) .LT. tol), &
!         "same as FromBiunitLine2Segment")
!
! END PROGRAM main
!```

INTERFACE FromBiunitLine2Segment_
  MODULE PURE SUBROUTINE FromBiunitLine2Segment1_(xin, x1, x2, ans, tsize)
    REAL(DFP), INTENT(IN) :: xin(:)
    !! coordinates in [-1,1]
    REAL(DFP), INTENT(IN) :: x1
    !! x1 of physical domain
    REAL(DFP), INTENT(IN) :: x2
    !! x2 of physical  domain
    REAL(DFP), INTENT(INOUT) :: ans(:)
    !! mapped coordinates of xin in physical domain
    INTEGER(I4B), INTENT(OUT) :: tsize
  END SUBROUTINE FromBiunitLine2Segment1_
END INTERFACE FromBiunitLine2Segment_

!----------------------------------------------------------------------------
!                                                    FromSegment2BiunitLine_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-08-27
! summary: Maps [x1, x2] to the biunit line without allocation.
!
! Description:
!   Same as `FromSegment2BiunitLine(xin, x1, x2)`, but the result is
!   written to `ans(1:tsize)`, where `tsize = SIZE(xin)`. Entries of
!   `ans` after `tsize` are not changed.
!
! Arguments:
!   xin : REAL(DFP), rank(1) - coordinates in [x1, x2]
!   x1 : REAL(DFP) - first end point of the segment
!   x2 : REAL(DFP) - second end point of the segment
!   ans : REAL(DFP), rank(1) - output, `SIZE(ans) >= SIZE(xin)`
!   tsize : INTEGER(I4B) - number of entries written, `SIZE(xin)`
!
! Formula:
!   $$\mathrm{ans}_i = \frac{2 x_i - (x_1 + x_2)}{x_2 - x_1}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE MappingUtility, ONLY: FromSegment2BiunitLine_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
! REAL(DFP) :: xin(3), ans(5)
! INTEGER(I4B) :: tsize
!
! xin = [2.0_DFP, 4.0_DFP, 6.0_DFP]
!
! ans = -100.0_DFP
! CALL FromSegment2BiunitLine_(xin, 2.0_DFP, 6.0_DFP, ans, tsize)
! CALL OK(tsize .EQ. 3_I4B, "tsize is SIZE(xin)")
! CALL OK(ALL(ABS(ans(1:tsize) - [-1.0_DFP, 0.0_DFP, 1.0_DFP]) .LT. tol), &
!         "mapped values")
! CALL OK(ALL(ABS(ans(4:5) + 100.0_DFP) .LT. tol), &
!         "extra entries are not changed")
!
! END PROGRAM main
!```

INTERFACE FromSegment2BiunitLine_
  MODULE PURE SUBROUTINE FromSegment2BiunitLine1_(xin, x1, x2, ans, tsize)
    REAL(DFP), INTENT(IN) :: xin(:)
    !! coordinates between [x1,x2]
    REAL(DFP), INTENT(IN) :: x1
    !! x1 of physical domain
    REAL(DFP), INTENT(IN) :: x2
    !! x2 of physical  domain
    REAL(DFP), INTENT(INOUT) :: ans(:)
    !! mapped coordinates of xin in biunit domain
    INTEGER(I4B), INTENT(OUT) :: tsize
  END SUBROUTINE FromSegment2BiunitLine1_
END INTERFACE FromSegment2BiunitLine_

!----------------------------------------------------------------------------
!                                                     FromBiunitLine2Segment
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-10-19
! summary: Maps points from the biunit line to a segment in space.
!
! Description:
!   Maps each point `xin(i)` of the biunit line [-1, 1] to the segment
!   from `x1` to `x2` in `nsd` dimensional space, where
!   `nsd = SIZE(x1)`. The result is in xij format: `ans(:, i)` is the
!   image of `xin(i)`.
!
! Arguments:
!   xin : REAL(DFP), rank(1) - coordinates in [-1, 1], `SIZE(xin) = n`
!   x1 : REAL(DFP), rank(1) - first end point, `SIZE(x1) = nsd`
!   x2 : REAL(DFP), rank(1) - second end point, `SIZE(x2) = nsd`
!
! Returns:
!   ans : REAL(DFP), rank(2) - mapped points,
!     `SHAPE(ans) = [SIZE(x1), SIZE(xin)]`
!
! Formula:
!   $$\mathbf{ans}_{:,i} = \frac{\mathbf{x}_1 + \mathbf{x}_2}{2}
!     + \frac{\mathbf{x}_2 - \mathbf{x}_1}{2}\,\xi_i$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE MappingUtility, ONLY: FromBiunitLine2Segment
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
! REAL(DFP) :: xin(3), x1(2), x2(2), ans(2, 3)
!
! xin = [-1.0_DFP, 0.0_DFP, 1.0_DFP]
! x1 = [0.0_DFP, 0.0_DFP]
! x2 = [2.0_DFP, 4.0_DFP]
!
! ans = FromBiunitLine2Segment(xin, x1, x2)
! CALL OK(ALL(ABS(ans(:, 1) - x1) .LT. tol), "xi = -1 gives x1")
! CALL OK(ALL(ABS(ans(:, 3) - x2) .LT. tol), "xi = 1 gives x2")
! CALL OK(ALL(ABS(ans(:, 2) - [1.0_DFP, 2.0_DFP]) .LT. tol), &
!         "xi = 0 gives the mid point")
!
! ! the shape of the result is (SIZE(x1), SIZE(xin))
! CALL OK(ALL(SHAPE(FromBiunitLine2Segment(xin, x1, x2)) .EQ. &
!             [2_I4B, 3_I4B]), "shape of the result")
!
! END PROGRAM main
!```

INTERFACE FromBiunitLine2Segment
  MODULE PURE FUNCTION FromBiunitLine2Segment2(xin, x1, x2) RESULT(ans)
    REAL(DFP), INTENT(IN) :: xin(:)
    !! coordinates in [-1,1], SIZE(xin) = n
    REAL(DFP), INTENT(IN) :: x1(:)
    !! x1 of physical domain, SIZE(x1) = nsd
    REAL(DFP), INTENT(IN) :: x2(:)
    !! x2 of physical  domain, SIZE(x2) = nsd
    REAL(DFP) :: ans(SIZE(x1), SIZE(xin))
    !! returned coordinates in physical space
    !! ans is in xij format
  END FUNCTION FromBiunitLine2Segment2
END INTERFACE FromBiunitLine2Segment

!----------------------------------------------------------------------------
!                                                     FromBiunitLine2Segment
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-10-19
! summary: Maps the biunit line to a segment in space, no allocation.
!
! Description:
!   Same as `FromBiunitLine2Segment(xin, x1, x2)` with vector end
!   points, but the result is written to `ans(1:nrow, 1:ncol)`, where
!   `nrow = SIZE(x1)` and `ncol = SIZE(xin)`.
!
! Arguments:
!   xin : REAL(DFP), rank(1) - coordinates in [-1, 1]
!   x1 : REAL(DFP), rank(1) - first end point, `SIZE(x1) = nsd`
!   x2 : REAL(DFP), rank(1) - second end point, `SIZE(x2) = nsd`
!   ans : REAL(DFP), rank(2) - output,
!     `SIZE(ans, 1) >= nsd`, `SIZE(ans, 2) >= SIZE(xin)`
!   nrow : INTEGER(I4B) - number of rows written, `SIZE(x1)`
!   ncol : INTEGER(I4B) - number of columns written, `SIZE(xin)`
!
! Formula:
!   $$\mathbf{ans}_{:,i} = \frac{\mathbf{x}_1 + \mathbf{x}_2}{2}
!     + \frac{\mathbf{x}_2 - \mathbf{x}_1}{2}\,\xi_i$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE MappingUtility, ONLY: FromBiunitLine2Segment
! USE MappingUtility, ONLY: FromBiunitLine2Segment_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
! REAL(DFP) :: xin(3), x1(2), x2(2), ans(3, 4)
! INTEGER(I4B) :: nrow, ncol
!
! xin = [-1.0_DFP, 0.0_DFP, 1.0_DFP]
! x1 = [0.0_DFP, 0.0_DFP]
! x2 = [2.0_DFP, 4.0_DFP]
!
! ! ans can be larger, only ans(1:nrow, 1:ncol) is written
! CALL FromBiunitLine2Segment_(xin, x1, x2, ans, nrow, ncol)
! CALL OK(nrow .EQ. 2_I4B, "nrow is SIZE(x1)")
! CALL OK(ncol .EQ. 3_I4B, "ncol is SIZE(xin)")
! CALL OK(ALL(ABS(ans(1:2, 2) - [1.0_DFP, 2.0_DFP]) .LT. tol), &
!         "mid point")
! CALL OK(ALL(ABS(ans(1:nrow, 1:ncol) - &
!                 FromBiunitLine2Segment(xin, x1, x2)) .LT. tol), &
!         "same as FromBiunitLine2Segment")
!
! END PROGRAM main
!```

INTERFACE FromBiunitLine2Segment_
  MODULE PURE SUBROUTINE FromBiunitLine2Segment2_(xin, x1, x2, ans, nrow, &
                                                  ncol)
    REAL(DFP), INTENT(IN) :: xin(:)
    !! coordinates in [-1,1], SIZE(xin) = n
    REAL(DFP), INTENT(IN) :: x1(:)
    !! x1 of physical domain, SIZE(x1) = nsd
    REAL(DFP), INTENT(IN) :: x2(:)
    !! x2 of physical  domain, SIZE(x2) = nsd
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! returned coordinates in physical space
    !! ans is in xij format
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  END SUBROUTINE FromBiunitLine2Segment2_
END INTERFACE FromBiunitLine2Segment_

!----------------------------------------------------------------------------
!                                                 FromUnitTriangle2Triangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-10-19
! summary: Maps points from the unit triangle to a triangle.
!
! Description:
!   Maps the points `xin` of the unit triangle (0,0), (1,0), (0,1) to
!   the triangle with vertices `x1`, `x2`, `x3`. The map is affine, so
!   vertex k of the unit triangle goes to `xk`. The triangle can be in
!   2D or 3D, `nsd = SIZE(x1)`.
!
! Arguments:
!   xin : REAL(DFP), rank(2) - points in the unit triangle,
!     `SHAPE(xin) = [2, N]`
!   x1 : REAL(DFP), rank(1) - vertex 1, `SIZE(x1) = nsd`
!   x2 : REAL(DFP), rank(1) - vertex 2, `SIZE(x2) = nsd`
!   x3 : REAL(DFP), rank(1) - vertex 3, `SIZE(x3) = nsd`
!
! Returns:
!   ans : REAL(DFP), rank(2) - mapped points,
!     `SHAPE(ans) = [nsd, N]`
!
! Formula:
!   $$\mathbf{x} = \mathbf{x}_1 + (\mathbf{x}_2 - \mathbf{x}_1)\,\xi
!     + (\mathbf{x}_3 - \mathbf{x}_1)\,\eta$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE MappingUtility, ONLY: FromUnitTriangle2Triangle
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
! REAL(DFP), PARAMETER :: third = 1.0_DFP / 3.0_DFP
! REAL(DFP) :: xin(2, 4), x1(2), x2(2), x3(2), ans(2, 4), ans3(3, 4)
!
! ! vertices and centroid of the unit triangle
! xin = RESHAPE([0.0_DFP, 0.0_DFP, 1.0_DFP, 0.0_DFP, 0.0_DFP, 1.0_DFP, &
!                third, third], [2_I4B, 4_I4B])
!
! ! a triangle in 2D
! x1 = [1.0_DFP, 1.0_DFP]
! x2 = [4.0_DFP, 1.0_DFP]
! x3 = [1.0_DFP, 3.0_DFP]
!
! ans = FromUnitTriangle2Triangle(xin, x1, x2, x3)
! CALL OK(ALL(ABS(ans(:, 1) - x1) .LT. tol), "(0,0) goes to x1")
! CALL OK(ALL(ABS(ans(:, 2) - x2) .LT. tol), "(1,0) goes to x2")
! CALL OK(ALL(ABS(ans(:, 3) - x3) .LT. tol), "(0,1) goes to x3")
! CALL OK(ALL(ABS(ans(:, 4) - (x1 + x2 + x3) / 3.0_DFP) .LT. tol), &
!         "centroid goes to centroid")
!
! ! a triangle in 3D, the number of rows of ans is SIZE(x1)
! ans3 = FromUnitTriangle2Triangle(xin, [0.0_DFP, 0.0_DFP, 1.0_DFP], &
!                                  [1.0_DFP, 0.0_DFP, 1.0_DFP], &
!                                  [0.0_DFP, 1.0_DFP, 1.0_DFP])
! CALL OK(ALL(ABS(ans3(:, 4) - [third, third, 1.0_DFP]) .LT. tol), &
!         "centroid in 3D")
!
! END PROGRAM main
!```

INTERFACE FromUnitTriangle2Triangle
  MODULE PURE FUNCTION FromUnitTriangle2Triangle1(xin, x1, x2, x3) RESULT(ans)
    REAL(DFP), INTENT(IN) :: xin(:, :)
    !! vertex coordinate of unit triangle
    !! (0,0), (1,0), (0,1)
    !! shape(xin) = (2,N)
    REAL(DFP), INTENT(IN) :: x1(:)
    !! x1 of physical domain, size(x1) = nsd
    REAL(DFP), INTENT(IN) :: x2(:)
    !! x2 of physical domain, size(x2) = nsd
    REAL(DFP), INTENT(IN) :: x3(:)
    !! x3 of physical domain, size(x3) = nsd
    REAL(DFP) :: ans(SIZE(x1), SIZE(xin, 2))
    !! mapped coordinates of xin in physical domain
    !! shape(ans) = nsd, N
  END FUNCTION FromUnitTriangle2Triangle1
END INTERFACE FromUnitTriangle2Triangle

!----------------------------------------------------------------------------
!                                                FromUnitTriangle2Triangle_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-06-26
! summary: Maps the unit triangle to a triangle without allocation.
!
! Description:
!   Same as `FromUnitTriangle2Triangle`, but the result is written to
!   `ans(1:nrow, 1:ncol)`, where `nrow = SIZE(x1)` and
!   `ncol = SIZE(xin, 2)`.
!
! Arguments:
!   xin : REAL(DFP), rank(2) - points in the unit triangle,
!     `SHAPE(xin) = [2, N]`
!   x1, x2, x3 : REAL(DFP), rank(1) - vertices, `SIZE(x1) = nsd`
!   ans : REAL(DFP), rank(2) - output,
!     `SIZE(ans, 1) >= nsd`, `SIZE(ans, 2) >= N`
!   nrow : INTEGER(I4B) - number of rows written, `SIZE(x1)`
!   ncol : INTEGER(I4B) - number of columns written, `SIZE(xin, 2)`
!
! Formula:
!   $$\mathbf{x} = \mathbf{x}_1 + (\mathbf{x}_2 - \mathbf{x}_1)\,\xi
!     + (\mathbf{x}_3 - \mathbf{x}_1)\,\eta$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE MappingUtility, ONLY: FromUnitTriangle2Triangle
! USE MappingUtility, ONLY: FromUnitTriangle2Triangle_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
! REAL(DFP), PARAMETER :: third = 1.0_DFP / 3.0_DFP
! REAL(DFP) :: xin(2, 4), x1(2), x2(2), x3(2), ans(3, 10)
! INTEGER(I4B) :: nrow, ncol
!
! xin = RESHAPE([0.0_DFP, 0.0_DFP, 1.0_DFP, 0.0_DFP, 0.0_DFP, 1.0_DFP, &
!                third, third], [2_I4B, 4_I4B])
! x1 = [1.0_DFP, 1.0_DFP]
! x2 = [4.0_DFP, 1.0_DFP]
! x3 = [1.0_DFP, 3.0_DFP]
!
! ! ans can be larger, only ans(1:nrow, 1:ncol) is written
! CALL FromUnitTriangle2Triangle_(xin, x1, x2, x3, ans, nrow, ncol)
! CALL OK(nrow .EQ. 2_I4B, "nrow is SIZE(x1)")
! CALL OK(ncol .EQ. 4_I4B, "ncol is SIZE(xin, 2)")
! CALL OK(ALL(ABS(ans(1:nrow, 1:ncol) - &
!                 FromUnitTriangle2Triangle(xin, x1, x2, x3)) .LT. tol), &
!         "same as FromUnitTriangle2Triangle")
!
! END PROGRAM main
!```

INTERFACE FromUnitTriangle2Triangle_
  MODULE PURE SUBROUTINE FromUnitTriangle2Triangle1_(xin, x1, x2, x3, ans, &
                                                     nrow, ncol)
    REAL(DFP), INTENT(IN) :: xin(:, :)
    !! vertex coordinate of unit triangle
    !! (0,0), (1,0), (0,1)
    !! shape(xin) = (2,N)
    REAL(DFP), INTENT(IN) :: x1(:)
    !! x1 of physical domain, size(x1) = nsd
    REAL(DFP), INTENT(IN) :: x2(:)
    !! x2 of physical domain, size(x2) = nsd
    REAL(DFP), INTENT(IN) :: x3(:)
    !! x3 of physical domain, size(x3) = nsd
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    ! REAL(DFP) :: ans(SIZE(x1), SIZE(xin, 2))
    !! mapped coordinates of xin in physical domain
    !! shape(ans) = nsd, N
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  END SUBROUTINE FromUnitTriangle2Triangle1_
END INTERFACE FromUnitTriangle2Triangle_

!----------------------------------------------------------------------------
!                                       FromBiUnitQuadrangle2UnitQuadrangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-10-19
! summary: Maps points from the biunit to the unit quadrangle.
!
! Description:
!   Maps the points `xin` of the biunit quadrangle [-1,1]^2 to the unit
!   quadrangle [0,1]^2. Each coordinate is mapped by (1 + x) / 2.
!
! Arguments:
!   xin : REAL(DFP), rank(2) - points in the biunit quadrangle,
!     `SHAPE(xin) = [2, N]`
!
! Returns:
!   ans : REAL(DFP), rank(2) - points in the unit quadrangle,
!     `SHAPE(ans) = SHAPE(xin)`
!
! Formula:
!   $$\mathrm{ans}_{ij} = \frac{1 + x_{ij}}{2}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE MappingUtility, ONLY: FromBiUnitQuadrangle2UnitQuadrangle
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
! REAL(DFP) :: xin(2, 5), ans(2, 5), expected(2, 5)
!
! ! vertices and centre of the biunit quadrangle
! xin = RESHAPE([-1.0_DFP, -1.0_DFP, 1.0_DFP, -1.0_DFP, 1.0_DFP, 1.0_DFP, &
!                -1.0_DFP, 1.0_DFP, 0.0_DFP, 0.0_DFP], [2_I4B, 5_I4B])
!
! ! vertices and centre of the unit quadrangle
! expected = RESHAPE([0.0_DFP, 0.0_DFP, 1.0_DFP, 0.0_DFP, 1.0_DFP, 1.0_DFP, &
!                     0.0_DFP, 1.0_DFP, 0.5_DFP, 0.5_DFP], [2_I4B, 5_I4B])
!
! ans = FromBiUnitQuadrangle2UnitQuadrangle(xin)
! CALL OK(ALL(ABS(ans - expected) .LT. tol), "vertices and centre")
!
! ! the map is (1 + x) / 2 in each direction
! CALL OK(ALL(ABS(ans - 0.5_DFP * (1.0_DFP + xin)) .LT. tol), &
!         "ans = (1 + xin) / 2")
!
! END PROGRAM main
!```

INTERFACE FromBiUnitQuadrangle2UnitQuadrangle
  MODULE PURE FUNCTION FromBiUnitQuadrangle2UnitQuadrangle1(xin) &
    RESULT(ans)
    REAL(DFP), INTENT(IN) :: xin(:, :)
    !! vertex coordinate of biunit Quadrangle in xij format
    !! SIZE(xin,1) = 2
    REAL(DFP) :: ans(SIZE(xin, 1), SIZE(xin, 2))
    !! mapped coordinates of xin in physical domain
    !! shape(ans) = nsd, N
  END FUNCTION FromBiUnitQuadrangle2UnitQuadrangle1
END INTERFACE FromBiUnitQuadrangle2UnitQuadrangle

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-07-08
! summary: Biunit to unit quadrangle without allocation.
!
! Description:
!   Same as `FromBiUnitQuadrangle2UnitQuadrangle`, but the result is
!   written to `ans(1:nrow, 1:ncol)`, where `nrow = 2` and
!   `ncol = SIZE(xin, 2)`.
!
! Arguments:
!   xin : REAL(DFP), rank(2) - points in the biunit quadrangle,
!     `SHAPE(xin) = [2, N]`
!   ans : REAL(DFP), rank(2) - output,
!     `SIZE(ans, 1) >= 2`, `SIZE(ans, 2) >= N`
!   nrow : INTEGER(I4B) - number of rows written, 2
!   ncol : INTEGER(I4B) - number of columns written, `SIZE(xin, 2)`
!
! Formula:
!   $$\mathrm{ans}_{ij} = \frac{1 + x_{ij}}{2}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE MappingUtility, ONLY: FromBiUnitQuadrangle2UnitQuadrangle_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
! REAL(DFP) :: xin(2, 2), ans(3, 4)
! INTEGER(I4B) :: nrow, ncol
!
! xin = RESHAPE([0.0_DFP, 0.0_DFP, 0.5_DFP, -0.5_DFP], [2_I4B, 2_I4B])
!
! CALL FromBiUnitQuadrangle2UnitQuadrangle_(xin, ans, nrow, ncol)
! CALL OK(nrow .EQ. 2_I4B, "nrow is 2")
! CALL OK(ncol .EQ. 2_I4B, "ncol is SIZE(xin, 2)")
! CALL OK(ALL(ABS(ans(1:2, 1) - [0.5_DFP, 0.5_DFP]) .LT. tol), "centre")
! CALL OK(ALL(ABS(ans(1:2, 2) - [0.75_DFP, 0.25_DFP]) .LT. tol), &
!         "interior point")
!
! END PROGRAM main
!```

INTERFACE FromBiUnitQuadrangle2UnitQuadrangle_
  MODULE PURE SUBROUTINE FromBiUnitQuadrangle2UnitQuadrangle1_(xin, ans, &
                                                               nrow, ncol)
    REAL(DFP), INTENT(IN) :: xin(:, :)
    !! vertex coordinate of biunit Quadrangle in xij format
    !! SIZE(xin,1) = 2
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! mapped coordinates of xin in physical domain
    !! shape(ans) = nsd, N
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! nrow = SIZE(xin, 1)
    !! ncol = SIZE(xin, 2)
  END SUBROUTINE FromBiUnitQuadrangle2UnitQuadrangle1_
END INTERFACE FromBiUnitQuadrangle2UnitQuadrangle_

!----------------------------------------------------------------------------
!                                        FromUnitQuadrangle2BiUnitQuadrangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-10-19
! summary: Maps points from the unit to the biunit quadrangle.
!
! Description:
!   Maps the points `xin` of the unit quadrangle [0,1]^2 to the biunit
!   quadrangle [-1,1]^2. It is the inverse of
!   `FromBiUnitQuadrangle2UnitQuadrangle`.
!
! Arguments:
!   xin : REAL(DFP), rank(2) - points in the unit quadrangle,
!     `SHAPE(xin) = [2, N]`
!
! Returns:
!   ans : REAL(DFP), rank(2) - points in the biunit quadrangle,
!     `SHAPE(ans) = SHAPE(xin)`
!
! Formula:
!   $$\mathrm{ans}_{ij} = 2 x_{ij} - 1$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE MappingUtility, ONLY: FromUnitQuadrangle2BiUnitQuadrangle
! USE MappingUtility, ONLY: FromBiUnitQuadrangle2UnitQuadrangle
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
! REAL(DFP) :: xin(2, 5), ans(2, 5)
!
! ! vertices of the unit quadrangle and an interior point
! xin = RESHAPE([0.0_DFP, 0.0_DFP, 1.0_DFP, 0.0_DFP, 1.0_DFP, 1.0_DFP, &
!                0.0_DFP, 1.0_DFP, 0.25_DFP, 0.75_DFP], [2_I4B, 5_I4B])
!
! ans = FromUnitQuadrangle2BiUnitQuadrangle(xin)
! CALL OK(ALL(ABS(ans - (2.0_DFP * xin - 1.0_DFP)) .LT. tol), &
!         "ans = 2 xin - 1")
! CALL OK(ALL(ABS(ans(:, 5) - [-0.5_DFP, 0.5_DFP]) .LT. tol), &
!         "interior point")
!
! ! inverse of FromBiUnitQuadrangle2UnitQuadrangle
! ans = FromBiUnitQuadrangle2UnitQuadrangle( &
!       FromUnitQuadrangle2BiUnitQuadrangle(xin))
! CALL OK(ALL(ABS(ans - xin) .LT. tol), "round trip")
!
! END PROGRAM main
!```

INTERFACE FromUnitQuadrangle2BiUnitQuadrangle
  MODULE PURE FUNCTION FromUnitQuadrangle2BiUnitQuadrangle1(xin) &
    RESULT(ans)
    REAL(DFP), INTENT(IN) :: xin(:, :)
    !! vertex coordinate of biunit Quadrangle in xij format
    !! SIZE(xin,1) = 2
    REAL(DFP) :: ans(SIZE(xin, 1), SIZE(xin, 2))
    !! mapped coordinates of xin in physical domain
    !! shape(ans) = nsd, N
  END FUNCTION FromUnitQuadrangle2BiUnitQuadrangle1
END INTERFACE FromUnitQuadrangle2BiUnitQuadrangle

!----------------------------------------------------------------------------
!                                            FromBiUnitQuadrangle2Quadrangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-10-19
! summary: Maps points from the biunit quadrangle to a quadrangle.
!
! Description:
!   Maps the points `xin` of the biunit quadrangle [-1,1]^2 to the
!   quadrangle with vertices `x1`, `x2`, `x3`, `x4` by bilinear
!   interpolation. Vertex k of the biunit quadrangle goes to `xk`, and
!   the centre goes to the average of the four vertices. The
!   quadrangle can be in 2D or 3D, `nsd = SIZE(x1)`.
!
! Arguments:
!   xin : REAL(DFP), rank(2) - points in the biunit quadrangle,
!     `SHAPE(xin) = [2, N]`
!   x1, x2, x3, x4 : REAL(DFP), rank(1) - vertices in counter
!     clockwise order, `SIZE(xk) = nsd`
!
! Returns:
!   ans : REAL(DFP), rank(2) - mapped points, `SHAPE(ans) = [nsd, N]`
!
! Formula:
!   $$\mathbf{x} = \sum_{k=1}^{4} N_k(\xi, \eta)\,\mathbf{x}_k, \quad
!     N_k = \frac{1}{4}(1 + \xi_k \xi)(1 + \eta_k \eta)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE MappingUtility, ONLY: FromBiUnitQuadrangle2Quadrangle
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
! REAL(DFP) :: xin(2, 6), x1(2), x2(2), x3(2), x4(2), ans(2, 6)
!
! ! vertices, centre and mid point of edge 2-3 of the biunit quadrangle
! xin = RESHAPE([-1.0_DFP, -1.0_DFP, 1.0_DFP, -1.0_DFP, 1.0_DFP, 1.0_DFP, &
!                -1.0_DFP, 1.0_DFP, 0.0_DFP, 0.0_DFP, 1.0_DFP, 0.0_DFP], &
!               [2_I4B, 6_I4B])
!
! ! a general quadrangle, vertices in counter clockwise order
! x1 = [0.0_DFP, 0.0_DFP]
! x2 = [4.0_DFP, 0.0_DFP]
! x3 = [4.0_DFP, 2.0_DFP]
! x4 = [0.0_DFP, 3.0_DFP]
!
! ans = FromBiUnitQuadrangle2Quadrangle(xin, x1, x2, x3, x4)
! CALL OK(ALL(ABS(ans(:, 1) - x1) .LT. tol), "(-1,-1) goes to x1")
! CALL OK(ALL(ABS(ans(:, 2) - x2) .LT. tol), "(1,-1) goes to x2")
! CALL OK(ALL(ABS(ans(:, 3) - x3) .LT. tol), "(1,1) goes to x3")
! CALL OK(ALL(ABS(ans(:, 4) - x4) .LT. tol), "(-1,1) goes to x4")
! CALL OK(ALL(ABS(ans(:, 5) - [2.0_DFP, 1.25_DFP]) .LT. tol), &
!         "centre goes to the average of the vertices")
! CALL OK(ALL(ABS(ans(:, 6) - [4.0_DFP, 1.0_DFP]) .LT. tol), &
!         "edge mid point goes to edge mid point")
!
! END PROGRAM main
!```

INTERFACE FromBiUnitQuadrangle2Quadrangle
  MODULE PURE FUNCTION FromBiUnitQuadrangle2Quadrangle1(xin, x1, x2, x3, x4) &
    RESULT(ans)
    REAL(DFP), INTENT(IN) :: xin(:, :)
    !! vertex coordinate of biunit Quadrangle in xij format
    !! SIZE(xin,1) = 2
    REAL(DFP), INTENT(IN) :: x1(:)
    !! vertex x1 of physical domain, size(x1) = nsd
    REAL(DFP), INTENT(IN) :: x2(:)
    !! vertex x2 of physical domain, size(x2) = nsd
    REAL(DFP), INTENT(IN) :: x3(:)
    !! vertex x3 of physical domain, size(x3) = nsd
    REAL(DFP), INTENT(IN) :: x4(:)
    !! vertex x4 of physical domain, size(x4) = nsd
    REAL(DFP) :: ans(SIZE(x1), SIZE(xin, 2))
    !! mapped coordinates of xin in physical domain
    !! shape(ans) = nsd, N
  END FUNCTION FromBiUnitQuadrangle2Quadrangle1
END INTERFACE FromBiUnitQuadrangle2Quadrangle

!----------------------------------------------------------------------------
!                                            FromBiUnitQuadrangle2Quadrangle_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-10-19
! summary: Biunit quadrangle to a quadrangle without allocation.
!
! Description:
!   Same as `FromBiUnitQuadrangle2Quadrangle`, but the result is
!   written to `ans(1:nrow, 1:ncol)`, where `nrow = SIZE(x1)` and
!   `ncol = SIZE(xin, 2)`.
!
! Arguments:
!   xin : REAL(DFP), rank(2) - points in the biunit quadrangle,
!     `SHAPE(xin) = [2, N]`
!   x1, x2, x3, x4 : REAL(DFP), rank(1) - vertices, `SIZE(xk) = nsd`
!   ans : REAL(DFP), rank(2) - output,
!     `SIZE(ans, 1) >= nsd`, `SIZE(ans, 2) >= N`
!   nrow : INTEGER(I4B) - number of rows written, `SIZE(x1)`
!   ncol : INTEGER(I4B) - number of columns written, `SIZE(xin, 2)`
!
! Formula:
!   $$\mathbf{x} = \sum_{k=1}^{4} N_k(\xi, \eta)\,\mathbf{x}_k, \quad
!     N_k = \frac{1}{4}(1 + \xi_k \xi)(1 + \eta_k \eta)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE MappingUtility, ONLY: FromBiUnitQuadrangle2Quadrangle_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
! REAL(DFP) :: xin(2, 2), x1(3), x2(3), x3(3), x4(3), ans(4, 5)
! INTEGER(I4B) :: nrow, ncol
!
! ! centre and first vertex of the biunit quadrangle
! xin = RESHAPE([0.0_DFP, 0.0_DFP, -1.0_DFP, -1.0_DFP], [2_I4B, 2_I4B])
!
! ! the unit square lifted to z = 2
! x1 = [0.0_DFP, 0.0_DFP, 2.0_DFP]
! x2 = [1.0_DFP, 0.0_DFP, 2.0_DFP]
! x3 = [1.0_DFP, 1.0_DFP, 2.0_DFP]
! x4 = [0.0_DFP, 1.0_DFP, 2.0_DFP]
!
! CALL FromBiUnitQuadrangle2Quadrangle_(xin, x1, x2, x3, x4, ans, nrow, ncol)
! CALL OK(nrow .EQ. 3_I4B, "nrow is SIZE(x1)")
! CALL OK(ncol .EQ. 2_I4B, "ncol is SIZE(xin, 2)")
! CALL OK(ALL(ABS(ans(1:3, 1) - [0.5_DFP, 0.5_DFP, 2.0_DFP]) .LT. tol), &
!         "centre")
! CALL OK(ALL(ABS(ans(1:3, 2) - x1) .LT. tol), "first vertex")
!
! END PROGRAM main
!```

INTERFACE FromBiUnitQuadrangle2Quadrangle_
  MODULE PURE SUBROUTINE FromBiUnitQuadrangle2Quadrangle1_(xin, x1, x2, x3, &
                                                          x4, ans, nrow, ncol)
    REAL(DFP), INTENT(IN) :: xin(:, :)
    !! vertex coordinate of biunit Quadrangle in xij format
    !! SIZE(xin,1) = 2
    REAL(DFP), INTENT(IN) :: x1(:)
    !! vertex x1 of physical domain, size(x1) = nsd
    REAL(DFP), INTENT(IN) :: x2(:)
    !! vertex x2 of physical domain, size(x2) = nsd
    REAL(DFP), INTENT(IN) :: x3(:)
    !! vertex x3 of physical domain, size(x3) = nsd
    REAL(DFP), INTENT(IN) :: x4(:)
    !! vertex x4 of physical domain, size(x4) = nsd
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    ! ans(SIZE(x1), SIZE(xin, 2))
    !! mapped coordinates of xin in physical domain
    !! shape(ans) = nsd, N
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  END SUBROUTINE FromBiUnitQuadrangle2Quadrangle1_
END INTERFACE FromBiUnitQuadrangle2Quadrangle_

!----------------------------------------------------------------------------
!                                            FromBiUnitHexahedron2Hexahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-10-19
! summary: Maps points from the biunit hexahedron to a hexahedron.
!
! Description:
!   Maps the points `xin` of the biunit hexahedron [-1,1]^3 to the
!   hexahedron with vertices `x1` to `x8` by trilinear interpolation.
!   Vertex k of the biunit hexahedron goes to `xk`. Vertices 1 to 4
!   are the bottom face and vertices 5 to 8 are the top face, in the
!   same order.
!
! Arguments:
!   xin : REAL(DFP), rank(2) - points in the biunit hexahedron,
!     `SHAPE(xin) = [3, N]`
!   x1, ..., x8 : REAL(DFP), rank(1) - vertices, `SIZE(xk) = nsd`
!
! Returns:
!   ans : REAL(DFP), rank(2) - mapped points, `SHAPE(ans) = [nsd, N]`
!
! Formula:
!   $$\mathbf{x} = \sum_{k=1}^{8} N_k\,\mathbf{x}_k, \quad
!     N_k = \frac{1}{8}(1 + \xi_k \xi)(1 + \eta_k \eta)
!     (1 + \zeta_k \zeta)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE MappingUtility, ONLY: FromBiUnitHexahedron2Hexahedron
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
! REAL(DFP) :: xin(3, 9), xv(3, 8), ans(3, 9)
!
! ! vertices of the biunit hexahedron (bottom face, then top face)
! xin(:, 1) = [-1.0_DFP, -1.0_DFP, -1.0_DFP]
! xin(:, 2) = [1.0_DFP, -1.0_DFP, -1.0_DFP]
! xin(:, 3) = [1.0_DFP, 1.0_DFP, -1.0_DFP]
! xin(:, 4) = [-1.0_DFP, 1.0_DFP, -1.0_DFP]
! xin(:, 5:8) = xin(:, 1:4)
! xin(3, 5:8) = 1.0_DFP
! ! centre
! xin(:, 9) = 0.0_DFP
!
! ! vertices of the box [0,2] x [0,3] x [0,4]
! xv(:, 1) = [0.0_DFP, 0.0_DFP, 0.0_DFP]
! xv(:, 2) = [2.0_DFP, 0.0_DFP, 0.0_DFP]
! xv(:, 3) = [2.0_DFP, 3.0_DFP, 0.0_DFP]
! xv(:, 4) = [0.0_DFP, 3.0_DFP, 0.0_DFP]
! xv(:, 5:8) = xv(:, 1:4)
! xv(3, 5:8) = 4.0_DFP
!
! ans = FromBiUnitHexahedron2Hexahedron(xin, xv(:, 1), xv(:, 2), xv(:, 3), &
!                                       xv(:, 4), xv(:, 5), xv(:, 6), &
!                                       xv(:, 7), xv(:, 8))
! CALL OK(ALL(ABS(ans(:, 1:8) - xv) .LT. tol), &
!         "vertices go to vertices")
! CALL OK(ALL(ABS(ans(:, 9) - [1.0_DFP, 1.5_DFP, 2.0_DFP]) .LT. tol), &
!         "centre goes to centre")
!
! END PROGRAM main
!```

INTERFACE FromBiUnitHexahedron2Hexahedron
  MODULE PURE FUNCTION FromBiUnitHexahedron2Hexahedron1(xin, &
    & x1, x2, x3, x4, x5, x6, x7, x8) &
    & RESULT(ans)
    REAL(DFP), INTENT(IN) :: xin(:, :)
    !! vertex coordinate of biunit Hexahedron in xij format
    !! SIZE(xin,1) = 3
    REAL(DFP), INTENT(IN) :: x1(:)
    !! vertex x1 of physical domain, size(x1) = nsd
    REAL(DFP), INTENT(IN) :: x2(:)
    !! vertex x2 of physical domain, size(x2) = nsd
    REAL(DFP), INTENT(IN) :: x3(:)
    !! vertex x3 of physical domain, size(x3) = nsd
    REAL(DFP), INTENT(IN) :: x4(:)
    !! vertex x4 of physical domain, size(x4) = nsd
    REAL(DFP), INTENT(IN) :: x5(:)
    !! vertex x5 of physical domain, size(x5) = nsd
    REAL(DFP), INTENT(IN) :: x6(:)
    !! vertex x6 of physical domain, size(x6) = nsd
    REAL(DFP), INTENT(IN) :: x7(:)
    !! vertex x7 of physical domain, size(x7) = nsd
    REAL(DFP), INTENT(IN) :: x8(:)
    !! vertex x8 of physical domain, size(x8) = nsd
    REAL(DFP) :: ans(SIZE(x1), SIZE(xin, 2))
    !! mapped coordinates of xin in physical domain
    !! shape(ans) = nsd, N
  END FUNCTION FromBiUnitHexahedron2Hexahedron1
END INTERFACE FromBiUnitHexahedron2Hexahedron

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-07-09
! summary: Biunit hexahedron to a hexahedron without allocation.
!
! Description:
!   Same as `FromBiUnitHexahedron2Hexahedron`, but the result is
!   written to `ans(1:nrow, 1:ncol)`, where `nrow = SIZE(x1)` and
!   `ncol = SIZE(xin, 2)`.
!
! Arguments:
!   xin : REAL(DFP), rank(2) - points in the biunit hexahedron,
!     `SHAPE(xin) = [3, N]`
!   x1, ..., x8 : REAL(DFP), rank(1) - vertices, `SIZE(xk) = nsd`
!   ans : REAL(DFP), rank(2) - output,
!     `SIZE(ans, 1) >= nsd`, `SIZE(ans, 2) >= N`
!   nrow : INTEGER(I4B) - number of rows written, `SIZE(x1)`
!   ncol : INTEGER(I4B) - number of columns written, `SIZE(xin, 2)`
!
! Formula:
!   $$\mathbf{x} = \sum_{k=1}^{8} N_k\,\mathbf{x}_k, \quad
!     N_k = \frac{1}{8}(1 + \xi_k \xi)(1 + \eta_k \eta)
!     (1 + \zeta_k \zeta)$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE MappingUtility, ONLY: FromBiUnitHexahedron2Hexahedron_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
! REAL(DFP) :: xin(3, 2), xv(3, 8), ans(4, 5)
! INTEGER(I4B) :: nrow, ncol
!
! ! centre and vertex 7 of the biunit hexahedron
! xin(:, 1) = 0.0_DFP
! xin(:, 2) = 1.0_DFP
!
! ! vertices of the box [0,2] x [0,3] x [0,4]
! xv(:, 1) = [0.0_DFP, 0.0_DFP, 0.0_DFP]
! xv(:, 2) = [2.0_DFP, 0.0_DFP, 0.0_DFP]
! xv(:, 3) = [2.0_DFP, 3.0_DFP, 0.0_DFP]
! xv(:, 4) = [0.0_DFP, 3.0_DFP, 0.0_DFP]
! xv(:, 5:8) = xv(:, 1:4)
! xv(3, 5:8) = 4.0_DFP
!
! CALL FromBiUnitHexahedron2Hexahedron_(xin, xv(:, 1), xv(:, 2), xv(:, 3), &
!                                       xv(:, 4), xv(:, 5), xv(:, 6), &
!                                       xv(:, 7), xv(:, 8), ans, nrow, ncol)
! CALL OK(nrow .EQ. 3_I4B, "nrow is SIZE(x1)")
! CALL OK(ncol .EQ. 2_I4B, "ncol is SIZE(xin, 2)")
! CALL OK(ALL(ABS(ans(1:3, 1) - [1.0_DFP, 1.5_DFP, 2.0_DFP]) .LT. tol), &
!         "centre")
! CALL OK(ALL(ABS(ans(1:3, 2) - xv(:, 7)) .LT. tol), "vertex 7")
!
! END PROGRAM main
!```

INTERFACE FromBiUnitHexahedron2Hexahedron_
  MODULE PURE SUBROUTINE FromBiUnitHexahedron2Hexahedron1_(xin, x1, x2, x3, &
                                          x4, x5, x6, x7, x8, ans, nrow, ncol)
    REAL(DFP), INTENT(IN) :: xin(:, :)
    !! vertex coordinate of biunit Hexahedron in xij format
    !! SIZE(xin,1) = 3
    REAL(DFP), INTENT(IN) :: x1(:)
    !! vertex x1 of physical domain, size(x1) = nsd
    REAL(DFP), INTENT(IN) :: x2(:)
    !! vertex x2 of physical domain, size(x2) = nsd
    REAL(DFP), INTENT(IN) :: x3(:)
    !! vertex x3 of physical domain, size(x3) = nsd
    REAL(DFP), INTENT(IN) :: x4(:)
    !! vertex x4 of physical domain, size(x4) = nsd
    REAL(DFP), INTENT(IN) :: x5(:)
    !! vertex x5 of physical domain, size(x5) = nsd
    REAL(DFP), INTENT(IN) :: x6(:)
    !! vertex x6 of physical domain, size(x6) = nsd
    REAL(DFP), INTENT(IN) :: x7(:)
    !! vertex x7 of physical domain, size(x7) = nsd
    REAL(DFP), INTENT(IN) :: x8(:)
    !! vertex x8 of physical domain, size(x8) = nsd
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! mapped coordinates of xin in physical domain
    !! shape(ans) = nsd, N
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! nrow = SIZE(x1)
    !! ncol = SIZE(xin, 2)
  END SUBROUTINE FromBiUnitHexahedron2Hexahedron1_
END INTERFACE FromBiUnitHexahedron2Hexahedron_

!----------------------------------------------------------------------------
!                                            FromBiUnitHexahedron2Hexahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-10-19
! summary: Maps points from the biunit to the unit hexahedron.
!
! Description:
!   Maps the points `xin` of the biunit hexahedron [-1,1]^3 to the unit
!   hexahedron [0,1]^3. Each coordinate is mapped by (1 + x) / 2.
!
! Arguments:
!   xin : REAL(DFP), rank(2) - points in the biunit hexahedron,
!     `SHAPE(xin) = [3, N]`
!
! Returns:
!   ans : REAL(DFP), rank(2) - points in the unit hexahedron,
!     `SHAPE(ans) = SHAPE(xin)`
!
! Formula:
!   $$\mathrm{ans}_{ij} = \frac{1 + x_{ij}}{2}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE MappingUtility, ONLY: FromBiUnitHexahedron2UnitHexahedron
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
! REAL(DFP) :: xin(3, 3), ans(3, 3)
!
! xin(:, 1) = [-1.0_DFP, -1.0_DFP, -1.0_DFP]
! xin(:, 2) = [1.0_DFP, 1.0_DFP, 1.0_DFP]
! xin(:, 3) = [0.0_DFP, 0.5_DFP, -0.5_DFP]
!
! ans = FromBiUnitHexahedron2UnitHexahedron(xin)
! CALL OK(ALL(ABS(ans(:, 1)) .LT. tol), "vertex 1 goes to (0,0,0)")
! CALL OK(ALL(ABS(ans(:, 2) - 1.0_DFP) .LT. tol), "vertex 7 goes to (1,1,1)")
! CALL OK(ALL(ABS(ans(:, 3) - [0.5_DFP, 0.75_DFP, 0.25_DFP]) .LT. tol), &
!         "interior point")
!
! END PROGRAM main
!```

INTERFACE FromBiUnitHexahedron2UnitHexahedron
  MODULE PURE FUNCTION FromBiUnitHexahedron2UnitHexahedron1(xin) &
    & RESULT(ans)
    REAL(DFP), INTENT(IN) :: xin(:, :)
    !! vertex coordinate of biunit Hexahedron in xij format
    !! SIZE(xin,1) = 3
    REAL(DFP) :: ans(SIZE(xin, 1), SIZE(xin, 2))
    !! mapped coordinates of xin in physical domain
    !! shape(ans) = nsd, N
  END FUNCTION FromBiUnitHexahedron2UnitHexahedron1
END INTERFACE FromBiUnitHexahedron2UnitHexahedron

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-07-09
! summary: Biunit to unit hexahedron without allocation.
!
! Description:
!   Same as `FromBiUnitHexahedron2UnitHexahedron`, but the result is
!   written to `ans(1:nrow, 1:ncol)`, where `nrow = 3` and
!   `ncol = SIZE(xin, 2)`.
!
! Arguments:
!   xin : REAL(DFP), rank(2) - points in the biunit hexahedron,
!     `SHAPE(xin) = [3, N]`
!   ans : REAL(DFP), rank(2) - output,
!     `SIZE(ans, 1) >= 3`, `SIZE(ans, 2) >= N`
!   nrow : INTEGER(I4B) - number of rows written, 3
!   ncol : INTEGER(I4B) - number of columns written, `SIZE(xin, 2)`
!
! Formula:
!   $$\mathrm{ans}_{ij} = \frac{1 + x_{ij}}{2}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE MappingUtility, ONLY: FromBiUnitHexahedron2UnitHexahedron_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
! REAL(DFP) :: xin(3, 2), ans(3, 5)
! INTEGER(I4B) :: nrow, ncol
!
! xin(:, 1) = 0.0_DFP
! xin(:, 2) = [0.0_DFP, 0.5_DFP, -0.5_DFP]
!
! CALL FromBiUnitHexahedron2UnitHexahedron_(xin, ans, nrow, ncol)
! CALL OK(nrow .EQ. 3_I4B, "nrow is 3")
! CALL OK(ncol .EQ. 2_I4B, "ncol is SIZE(xin, 2)")
! CALL OK(ALL(ABS(ans(1:3, 1) - 0.5_DFP) .LT. tol), "centre")
! CALL OK(ALL(ABS(ans(1:3, 2) - [0.5_DFP, 0.75_DFP, 0.25_DFP]) .LT. tol), &
!         "interior point")
!
! END PROGRAM main
!```

INTERFACE FromBiUnitHexahedron2UnitHexahedron_
  MODULE PURE SUBROUTINE FromBiUnitHexahedron2UnitHexahedron1_(xin, ans, &
                                                               nrow, ncol)
    REAL(DFP), INTENT(IN) :: xin(:, :)
    !! vertex coordinate of biunit Hexahedron in xij format
    !! SIZE(xin,1) = 3
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! mapped coordinates of xin in physical domain
    !! shape(ans) = nsd, N
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! number of rows and columns written in ans
    !! nrow = SIZE(xin, 1)
    !! ncol = SIZE(xin, 2)
  END SUBROUTINE FromBiUnitHexahedron2UnitHexahedron1_
END INTERFACE FromBiUnitHexahedron2UnitHexahedron_

!----------------------------------------------------------------------------
!                                            FromBiUnitHexahedron2Hexahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-10-19
! summary: Maps points from the unit to the biunit hexahedron.
!
! Description:
!   Maps the points `xin` of the unit hexahedron [0,1]^3 to the biunit
!   hexahedron [-1,1]^3. It is the inverse of
!   `FromBiUnitHexahedron2UnitHexahedron`.
!
! Arguments:
!   xin : REAL(DFP), rank(2) - points in the unit hexahedron,
!     `SHAPE(xin) = [3, N]`
!
! Returns:
!   ans : REAL(DFP), rank(2) - points in the biunit hexahedron,
!     `SHAPE(ans) = SHAPE(xin)`
!
! Formula:
!   $$\mathrm{ans}_{ij} = 2 x_{ij} - 1$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE MappingUtility, ONLY: FromUnitHexahedron2BiUnitHexahedron
! USE MappingUtility, ONLY: FromBiUnitHexahedron2UnitHexahedron
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
! REAL(DFP) :: xin(3, 3), ans(3, 3)
!
! xin(:, 1) = [0.0_DFP, 0.0_DFP, 0.0_DFP]
! xin(:, 2) = [1.0_DFP, 1.0_DFP, 1.0_DFP]
! xin(:, 3) = [0.25_DFP, 0.5_DFP, 0.75_DFP]
!
! ans = FromUnitHexahedron2BiUnitHexahedron(xin)
! CALL OK(ALL(ABS(ans(:, 1) + 1.0_DFP) .LT. tol), &
!         "(0,0,0) goes to (-1,-1,-1)")
! CALL OK(ALL(ABS(ans(:, 2) - 1.0_DFP) .LT. tol), "(1,1,1) goes to (1,1,1)")
! CALL OK(ALL(ABS(ans(:, 3) - [-0.5_DFP, 0.0_DFP, 0.5_DFP]) .LT. tol), &
!         "interior point")
!
! ! inverse of FromBiUnitHexahedron2UnitHexahedron
! ans = FromBiUnitHexahedron2UnitHexahedron( &
!       FromUnitHexahedron2BiUnitHexahedron(xin))
! CALL OK(ALL(ABS(ans - xin) .LT. tol), "round trip")
!
! END PROGRAM main
!```

INTERFACE FromUnitHexahedron2BiUnitHexahedron
  MODULE PURE FUNCTION FromUnitHexahedron2BiUnitHexahedron1(xin) &
    & RESULT(ans)
    REAL(DFP), INTENT(IN) :: xin(:, :)
    !! vertex coordinate of biunit Hexahedron in xij format
    !! SIZE(xin,1) = 3
    REAL(DFP) :: ans(SIZE(xin, 1), SIZE(xin, 2))
    !! mapped coordinates of xin in physical domain
    !! shape(ans) = nsd, N
  END FUNCTION FromUnitHexahedron2BiUnitHexahedron1
END INTERFACE FromUnitHexahedron2BiUnitHexahedron

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-07-09
! summary: Unit to biunit hexahedron without allocation.
!
! Description:
!   Same as `FromUnitHexahedron2BiUnitHexahedron`, but the result is
!   written to `ans(1:nrow, 1:ncol)`, where `nrow = SIZE(xin, 1)` and
!   `ncol = SIZE(xin, 2)`.
!
! Arguments:
!   xin : REAL(DFP), rank(2) - points in the unit hexahedron,
!     `SHAPE(xin) = [3, N]`
!   ans : REAL(DFP), rank(2) - output,
!     `SIZE(ans, 1) >= 3`, `SIZE(ans, 2) >= N`
!   nrow : INTEGER(I4B) - number of rows written, `SIZE(xin, 1)`
!   ncol : INTEGER(I4B) - number of columns written, `SIZE(xin, 2)`
!
! Formula:
!   $$\mathrm{ans}_{ij} = 2 x_{ij} - 1$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE MappingUtility, ONLY: FromUnitHexahedron2BiUnitHexahedron_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
! REAL(DFP) :: xin(3, 2), ans(4, 4)
! INTEGER(I4B) :: nrow, ncol
!
! xin(:, 1) = 0.5_DFP
! xin(:, 2) = [0.25_DFP, 0.5_DFP, 0.75_DFP]
!
! CALL FromUnitHexahedron2BiUnitHexahedron_(xin, ans, nrow, ncol)
! CALL OK(nrow .EQ. 3_I4B, "nrow is SIZE(xin, 1)")
! CALL OK(ncol .EQ. 2_I4B, "ncol is SIZE(xin, 2)")
! CALL OK(ALL(ABS(ans(1:3, 1)) .LT. tol), "centre goes to (0,0,0)")
! CALL OK(ALL(ABS(ans(1:3, 2) - [-0.5_DFP, 0.0_DFP, 0.5_DFP]) .LT. tol), &
!         "interior point")
!
! END PROGRAM main
!```

INTERFACE FromUnitHexahedron2BiUnitHexahedron_

  MODULE PURE SUBROUTINE FromUnitHexahedron2BiUnitHexahedron1_(xin, ans, &
                                                               nrow, ncol)
    REAL(DFP), INTENT(IN) :: xin(:, :)
    !! vertex coordinate of biunit Hexahedron in xij format
    !! SIZE(xin,1) = 3
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! mapped coordinates of xin in physical domain
    !! shape(ans) = nsd, N
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! nrow = SIZE(xin, 1)
    !! ncol = SIZE(xin, 2)
  END SUBROUTINE FromUnitHexahedron2BiUnitHexahedron1_
END INTERFACE FromUnitHexahedron2BiUnitHexahedron_

!----------------------------------------------------------------------------
!                                                     FromBiUnitLine2UnitLine
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-10-19
! summary: Maps points from the biunit line to the unit line.
!
! Description:
!   Maps each point of the biunit line [-1, 1] to the unit line
!   [0, 1].
!
! Arguments:
!   xin : REAL(DFP), rank(1) - coordinates in [-1, 1]
!
! Returns:
!   ans : REAL(DFP), rank(1) - coordinates in [0, 1],
!     `SIZE(ans) = SIZE(xin)`
!
! Formula:
!   $$\mathrm{ans}_i = \frac{1 + x_i}{2}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE MappingUtility, ONLY: FromBiUnitLine2UnitLine
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
! REAL(DFP) :: xin(4), ans(4)
!
! xin = [-1.0_DFP, 0.0_DFP, 1.0_DFP, 0.5_DFP]
! ans = FromBiUnitLine2UnitLine(xin)
!
! CALL OK(ABS(ans(1)) .LT. tol, "-1 goes to 0")
! CALL OK(ABS(ans(2) - 0.5_DFP) .LT. tol, "0 goes to 0.5")
! CALL OK(ABS(ans(3) - 1.0_DFP) .LT. tol, "1 goes to 1")
! CALL OK(ABS(ans(4) - 0.75_DFP) .LT. tol, "0.5 goes to 0.75")
!
! END PROGRAM main
!```

INTERFACE
  MODULE PURE FUNCTION FromBiUnitLine2UnitLine(xin) RESULT(ans)
    REAL(DFP), INTENT(IN) :: xin(:)
    !! coordinates in biunit line
    REAL(DFP) :: ans(SIZE(xin))
    !! mapped coordinates of xin in unit line
  END FUNCTION FromBiUnitLine2UnitLine
END INTERFACE

!----------------------------------------------------------------------------
!                                                    FromUnitLine2BiUnitLine
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-10-19
! summary: Maps points from the unit line to the biunit line.
!
! Description:
!   Maps each point of the unit line [0, 1] to the biunit line
!   [-1, 1]. It is the inverse of `FromBiUnitLine2UnitLine`.
!
! Arguments:
!   xin : REAL(DFP), rank(1) - coordinates in [0, 1]
!
! Returns:
!   ans : REAL(DFP), rank(1) - coordinates in [-1, 1],
!     `SIZE(ans) = SIZE(xin)`
!
! Formula:
!   $$\mathrm{ans}_i = 2 x_i - 1$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE MappingUtility, ONLY: FromUnitLine2BiUnitLine
! USE MappingUtility, ONLY: FromBiUnitLine2UnitLine
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
! REAL(DFP) :: xin(4), ans(4)
!
! xin = [0.0_DFP, 0.5_DFP, 1.0_DFP, 0.25_DFP]
! ans = FromUnitLine2BiUnitLine(xin)
! CALL OK(ALL(ABS(ans - [-1.0_DFP, 0.0_DFP, 1.0_DFP, -0.5_DFP]) .LT. tol), &
!         "mapped values")
!
! ! inverse of FromBiUnitLine2UnitLine
! ans = FromBiUnitLine2UnitLine(FromUnitLine2BiUnitLine(xin))
! CALL OK(ALL(ABS(ans - xin) .LT. tol), "round trip")
!
! END PROGRAM main
!```

INTERFACE
  MODULE PURE FUNCTION FromUnitLine2BiUnitLine(xin) RESULT(ans)
    REAL(DFP), INTENT(IN) :: xin(:)
    !! coordinates in  unit line
    REAL(DFP) :: ans(SIZE(xin))
    !! mapped coordinates of xin in biunit line
  END FUNCTION FromUnitLine2BiUnitLine
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-07-03
! summary: Maps the unit line to the biunit line without allocation.
!
! Description:
!   Same as `FromUnitLine2BiUnitLine`, but the result is written to
!   `ans(1:tsize)`, where `tsize = SIZE(xin)`. Entries of `ans` after
!   `tsize` are not changed.
!
! Arguments:
!   xin : REAL(DFP), rank(1) - coordinates in [0, 1]
!   ans : REAL(DFP), rank(1) - output, `SIZE(ans) >= SIZE(xin)`
!   tsize : INTEGER(I4B) - number of entries written, `SIZE(xin)`
!
! Formula:
!   $$\mathrm{ans}_i = 2 x_i - 1$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE MappingUtility, ONLY: FromUnitLine2BiUnitLine_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
! REAL(DFP) :: xin(3), ans(5)
! INTEGER(I4B) :: tsize
!
! xin = [0.0_DFP, 0.5_DFP, 1.0_DFP]
!
! ans = -100.0_DFP
! CALL FromUnitLine2BiUnitLine_(xin, ans, tsize)
! CALL OK(tsize .EQ. 3_I4B, "tsize is SIZE(xin)")
! CALL OK(ALL(ABS(ans(1:tsize) - [-1.0_DFP, 0.0_DFP, 1.0_DFP]) .LT. tol), &
!         "mapped values")
! CALL OK(ALL(ABS(ans(4:5) + 100.0_DFP) .LT. tol), &
!         "extra entries are not changed")
!
! END PROGRAM main
!```

INTERFACE
  MODULE PURE SUBROUTINE FromUnitLine2BiUnitLine_(xin, ans, tsize)
    REAL(DFP), INTENT(IN) :: xin(:)
    !! coordinates in  unit line
    REAL(DFP), INTENT(INOUT) :: ans(:)
    !! mapped coordinates of xin in biunit line
    INTEGER(I4B), INTENT(OUT) :: tsize
  END SUBROUTINE FromUnitLine2BiUnitLine_
END INTERFACE

!----------------------------------------------------------------------------
!                                                             FromLine2Line_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-04-27
! summary: Maps points between the unit and the biunit line.
!
! Description:
!   Maps the points `xin` from the reference line `from` to the
!   reference line `to`, and writes them to `ans(1:SIZE(xin))`.
!
!   Only the first letter of `from` and `to` is used:
!
!   - "B" (or "b") is the biunit line [-1, 1],
!   - "U" (or "u") is the unit line [0, 1].
!
!   If `from` and `to` are the same line, `xin` is copied to `ans`; in
!   this case both letters must have the same case ("BB" or "bb", but
!   not "Bb"). For any other value, `ans` is not changed.
!
! Arguments:
!   xin : REAL(DFP), rank(1) - coordinates in the `from` line
!   ans : REAL(DFP), rank(1) - output, `SIZE(ans) >= SIZE(xin)`
!   from : CHARACTER(*) - "BIUNIT" or "UNIT"
!   to : CHARACTER(*) - "BIUNIT" or "UNIT"
!
! Formula:
!   $$\text{B to U: } \mathrm{ans}_i = \frac{1 + x_i}{2}, \quad
!     \text{U to B: } \mathrm{ans}_i = 2 x_i - 1$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE MappingUtility, ONLY: FromLine2Line_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
! REAL(DFP) :: xb(3), xu(3), ans(3)
!
! xb = [-1.0_DFP, 0.0_DFP, 1.0_DFP]
! xu = [0.0_DFP, 0.5_DFP, 1.0_DFP]
!
! ! biunit to unit
! CALL FromLine2Line_(xin=xb, ans=ans, from="BIUNIT", to="UNIT")
! CALL OK(ALL(ABS(ans - xu) .LT. tol), "BIUNIT to UNIT")
!
! ! unit to biunit
! CALL FromLine2Line_(xin=xu, ans=ans, from="UNIT", to="BIUNIT")
! CALL OK(ALL(ABS(ans - xb) .LT. tol), "UNIT to BIUNIT")
!
! ! same reference line, values are copied
! CALL FromLine2Line_(xin=xb, ans=ans, from="BIUNIT", to="BIUNIT")
! CALL OK(ALL(ABS(ans - xb) .LT. tol), "BIUNIT to BIUNIT")
!
! ! only the first letter is used, lower case is accepted
! CALL FromLine2Line_(xin=xb, ans=ans, from="b", to="u")
! CALL OK(ALL(ABS(ans - xu) .LT. tol), "b to u")
!
! END PROGRAM main
!```

INTERFACE
  MODULE PURE SUBROUTINE FromLine2Line_(xin, ans, from, to)
    REAL(DFP), INTENT(IN) :: xin(:)
    !! coordinates in  unit line
    REAL(DFP), INTENT(INOUT) :: ans(:)
    !! mapped coordinates of xin in biunit line
    CHARACTER(*), INTENT(IN) :: from
    CHARACTER(*), INTENT(IN) :: to
  END SUBROUTINE FromLine2Line_
END INTERFACE

!----------------------------------------------------------------------------
!                                             FromBiUnitTriangle2BiUnitSqr
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-10-19
! summary: Maps points from the biunit triangle to the biunit square.
!
! Description:
!   Maps the points `xin` of the biunit triangle (-1,-1), (1,-1),
!   (-1,1) to the biunit square [-1,1]^2 by the collapsed (Duffy)
!   coordinates. The generic name is
!   `FromBiUnitTriangle2BiUnitQuadrangle`.
!
!   - Vertex 1 goes to (-1,-1) and vertex 2 goes to (1,-1).
!   - Vertex 3, (-1,1), is the collapsed point. There `1 - y = 0`, so
!     the first coordinate is not defined (division by zero).
!
!   It is the inverse of `FromBiUnitQuadrangle2BiUnitTriangle` away
!   from vertex 3.
!
! Arguments:
!   xin : REAL(DFP), rank(2) - points in the biunit triangle,
!     `SHAPE(xin) = [2, N]`
!
! Returns:
!   ans : REAL(DFP), rank(2) - points in the biunit square,
!     `SHAPE(ans) = [2, N]`
!
! Formula:
!   $$\xi = \frac{1 + 2x + y}{1 - y}, \quad \eta = y$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE MappingUtility, ONLY: FromBiUnitTriangle2BiUnitQuadrangle
! USE MappingUtility, ONLY: FromBiUnitTriangle2BiUnitSqr
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
! REAL(DFP), PARAMETER :: third = 1.0_DFP / 3.0_DFP
! REAL(DFP) :: xin(2, 4), ans(2, 4), expected(2, 4)
!
! ! vertex 1, vertex 2, centroid and mid point of edge 1-2
! ! vertex 3, (-1, 1), is the collapsed point and is not used here
! xin = RESHAPE([-1.0_DFP, -1.0_DFP, 1.0_DFP, -1.0_DFP, -third, -third, &
!                0.0_DFP, -1.0_DFP], [2_I4B, 4_I4B])
! expected = RESHAPE([-1.0_DFP, -1.0_DFP, 1.0_DFP, -1.0_DFP, &
!                     0.0_DFP, -third, 0.0_DFP, -1.0_DFP], [2_I4B, 4_I4B])
!
! ans = FromBiUnitTriangle2BiUnitQuadrangle(xin)
! CALL OK(ALL(ABS(ans - expected) .LT. tol), "mapped points")
!
! ! the specific name gives the same result
! ans = FromBiUnitTriangle2BiUnitSqr(xin)
! CALL OK(ALL(ABS(ans - expected) .LT. tol), "FromBiUnitTriangle2BiUnitSqr")
!
! END PROGRAM main
!```

INTERFACE FromBiUnitTriangle2BiUnitQuadrangle
  MODULE PURE FUNCTION FromBiUnitTriangle2BiUnitSqr(xin) RESULT(ans)
    REAL(DFP), INTENT(IN) :: xin(:, :)
    !! coordinates in biunit triangle in xij format
    !! bi unit triangle is defined by
    !! (-1,-1), (1,-1), (-1,1)
    REAL(DFP) :: ans(2, SIZE(xin, 2))
    !! mapped coordinates of xin in biunit sqr
  END FUNCTION FromBiUnitTriangle2BiUnitSqr
END INTERFACE FromBiUnitTriangle2BiUnitQuadrangle

!----------------------------------------------------------------------------
!                                             FromBiUnitSqr2BiUnitTriangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-10-19
! summary: Maps points from the biunit square to the biunit triangle.
!
! Description:
!   Maps the points `xin` of the biunit square [-1,1]^2 to the biunit
!   triangle (-1,-1), (1,-1), (-1,1). The generic name is
!   `FromBiUnitQuadrangle2BiUnitTriangle`.
!
!   - Square vertices 1 and 2 go to triangle vertices 1 and 2.
!   - The top edge of the square, eta = 1, collapses to vertex 3.
!
! Arguments:
!   xin : REAL(DFP), rank(2) - points in the biunit square,
!     `SHAPE(xin) = [2, N]`
!
! Returns:
!   ans : REAL(DFP), rank(2) - points in the biunit triangle,
!     `SHAPE(ans) = [2, N]`
!
! Formula:
!   $$x = \frac{(1 + \xi)(1 - \eta)}{2} - 1, \quad y = \eta$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE MappingUtility, ONLY: FromBiUnitQuadrangle2BiUnitTriangle
! USE MappingUtility, ONLY: FromBiUnitSqr2BiUnitTriangle
! USE MappingUtility, ONLY: FromBiUnitTriangle2BiUnitQuadrangle
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
! REAL(DFP), PARAMETER :: third = 1.0_DFP / 3.0_DFP
! REAL(DFP) :: xin(2, 5), ans(2, 5), expected(2, 5)
!
! ! vertices of the biunit square and the image of the centroid
! xin = RESHAPE([-1.0_DFP, -1.0_DFP, 1.0_DFP, -1.0_DFP, 1.0_DFP, 1.0_DFP, &
!                -1.0_DFP, 1.0_DFP, 0.0_DFP, -third], [2_I4B, 5_I4B])
!
! ! the top edge of the square collapses to vertex 3, (-1, 1)
! expected = RESHAPE([-1.0_DFP, -1.0_DFP, 1.0_DFP, -1.0_DFP, &
!                     -1.0_DFP, 1.0_DFP, -1.0_DFP, 1.0_DFP, &
!                     -third, -third], [2_I4B, 5_I4B])
!
! ans = FromBiUnitQuadrangle2BiUnitTriangle(xin)
! CALL OK(ALL(ABS(ans - expected) .LT. tol), "mapped points")
!
! ans = FromBiUnitSqr2BiUnitTriangle(xin)
! CALL OK(ALL(ABS(ans - expected) .LT. tol), "FromBiUnitSqr2BiUnitTriangle")
!
! ! inverse of FromBiUnitTriangle2BiUnitQuadrangle (away from vertex 3)
! ans(:, 1:2) = FromBiUnitTriangle2BiUnitQuadrangle( &
!               FromBiUnitQuadrangle2BiUnitTriangle(xin(:, [2, 5])))
! CALL OK(ALL(ABS(ans(:, 1:2) - xin(:, [2, 5])) .LT. tol), "round trip")
!
! END PROGRAM main
!```

INTERFACE FromBiUnitQuadrangle2BiUnitTriangle
  MODULE PURE FUNCTION FromBiUnitSqr2BiUnitTriangle(xin) RESULT(ans)
    REAL(DFP), INTENT(IN) :: xin(:, :)
    !! coordinates in bi-unit square in xij coordinate
    REAL(DFP) :: ans(2, SIZE(xin, 2))
    !! coordinates in biunit triangle
  END FUNCTION FromBiUnitSqr2BiUnitTriangle
END INTERFACE FromBiUnitQuadrangle2BiUnitTriangle

!----------------------------------------------------------------------------
!                                             FromUnitTriangle2BiUnitSqr
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-10-19
! summary: Maps points from the unit triangle to the biunit square.
!
! Description:
!   Maps the points `xin` of the unit triangle (0,0), (1,0), (0,1) to
!   the biunit square [-1,1]^2 by the collapsed (Duffy) coordinates.
!   The generic name is `FromUnitTriangle2BiUnitQuadrangle`.
!
!   - Vertex 1 goes to (-1,-1) and vertex 2 goes to (1,-1).
!   - Vertex 3, (0,1), is the collapsed point. There `1 - y = 0`, so
!     the first coordinate is not defined (division by zero).
!
! Arguments:
!   xin : REAL(DFP), rank(2) - points in the unit triangle,
!     `SHAPE(xin) = [2, N]`
!
! Returns:
!   ans : REAL(DFP), rank(2) - points in the biunit square,
!     `SHAPE(ans) = [2, N]`
!
! Formula:
!   $$\xi = \frac{2x + y - 1}{1 - y}, \quad \eta = 2y - 1$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE MappingUtility, ONLY: FromUnitTriangle2BiUnitQuadrangle
! USE MappingUtility, ONLY: FromUnitTriangle2BiUnitSqr
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
! REAL(DFP), PARAMETER :: third = 1.0_DFP / 3.0_DFP
! REAL(DFP) :: xin(2, 4), ans(2, 4), expected(2, 4)
!
! ! vertex 1, vertex 2, centroid and mid point of edge 1-2
! ! vertex 3, (0, 1), is the collapsed point and is not used here
! xin = RESHAPE([0.0_DFP, 0.0_DFP, 1.0_DFP, 0.0_DFP, third, third, &
!                0.5_DFP, 0.0_DFP], [2_I4B, 4_I4B])
! expected = RESHAPE([-1.0_DFP, -1.0_DFP, 1.0_DFP, -1.0_DFP, &
!                     0.0_DFP, -third, 0.0_DFP, -1.0_DFP], [2_I4B, 4_I4B])
!
! ans = FromUnitTriangle2BiUnitQuadrangle(xin)
! CALL OK(ALL(ABS(ans - expected) .LT. tol), "mapped points")
!
! ans = FromUnitTriangle2BiUnitSqr(xin)
! CALL OK(ALL(ABS(ans - expected) .LT. tol), "FromUnitTriangle2BiUnitSqr")
!
! END PROGRAM main
!```

INTERFACE FromUnitTriangle2BiUnitQuadrangle
  MODULE PURE FUNCTION FromUnitTriangle2BiUnitSqr(xin) RESULT(ans)
    REAL(DFP), INTENT(IN) :: xin(:, :)
    !! coordinates in biunit triangle in xij format
    !! bi unit triangle is defined by
    !! (-1,-1), (1,-1), (-1,1)
    REAL(DFP) :: ans(2, SIZE(xin, 2))
    !! mapped coordinates of xin in biunit sqr
  END FUNCTION FromUnitTriangle2BiUnitSqr
END INTERFACE FromUnitTriangle2BiUnitQuadrangle

!----------------------------------------------------------------------------
!                                             FromBiUnitSqr2UnitTriangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-10-19
! summary: Maps points from the biunit square to the unit triangle.
!
! Description:
!   Maps the points `xin` of the biunit square [-1,1]^2 to the unit
!   triangle (0,0), (1,0), (0,1). The generic name is
!   `FromBiUnitQuadrangle2UnitTriangle`.
!
!   - Square vertices 1 and 2 go to triangle vertices 1 and 2.
!   - The top edge of the square, eta = 1, collapses to vertex 3.
!
! Arguments:
!   xin : REAL(DFP), rank(2) - points in the biunit square,
!     `SHAPE(xin) = [2, N]`
!
! Returns:
!   ans : REAL(DFP), rank(2) - points in the unit triangle,
!     `SHAPE(ans) = [2, N]`
!
! Formula:
!   $$x = \frac{(1 + \xi)(1 - \eta)}{4}, \quad y = \frac{1 + \eta}{2}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE MappingUtility, ONLY: FromBiUnitQuadrangle2UnitTriangle
! USE MappingUtility, ONLY: FromBiUnitSqr2UnitTriangle
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
! REAL(DFP), PARAMETER :: third = 1.0_DFP / 3.0_DFP
! REAL(DFP) :: xin(2, 5), ans(2, 5), expected(2, 5)
!
! ! vertices of the biunit square and the image of the centroid
! xin = RESHAPE([-1.0_DFP, -1.0_DFP, 1.0_DFP, -1.0_DFP, 1.0_DFP, 1.0_DFP, &
!                -1.0_DFP, 1.0_DFP, 0.0_DFP, -third], [2_I4B, 5_I4B])
!
! ! the top edge of the square collapses to vertex 3, (0, 1)
! expected = RESHAPE([0.0_DFP, 0.0_DFP, 1.0_DFP, 0.0_DFP, 0.0_DFP, 1.0_DFP, &
!                     0.0_DFP, 1.0_DFP, third, third], [2_I4B, 5_I4B])
!
! ans = FromBiUnitQuadrangle2UnitTriangle(xin)
! CALL OK(ALL(ABS(ans - expected) .LT. tol), "mapped points")
!
! ans = FromBiUnitSqr2UnitTriangle(xin)
! CALL OK(ALL(ABS(ans - expected) .LT. tol), "FromBiUnitSqr2UnitTriangle")
!
! END PROGRAM main
!```

INTERFACE FromBiUnitQuadrangle2UnitTriangle
  MODULE PURE FUNCTION FromBiUnitSqr2UnitTriangle(xin) RESULT(ans)
    REAL(DFP), INTENT(IN) :: xin(:, :)
    !! coordinates in bi-unit square in xij coordinate
    REAL(DFP) :: ans(2, SIZE(xin, 2))
    !! coordinates in biunit triangle
  END FUNCTION FromBiUnitSqr2UnitTriangle
END INTERFACE FromBiUnitQuadrangle2UnitTriangle

!----------------------------------------------------------------------------
!                                                     FromTriangle2Triangle_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-10-19
! summary: Maps points between triangles without allocation.
!
! Description:
!   Maps the points `xin` from the triangle `from` to the triangle
!   `to`, and writes them to `ans(1:nrow, 1:ncol)`. Only the first
!   letter of `from` and `to` is used, and the case does not matter.
!   The supported cases are:
!
!   - "BIUNIT" to "UNIT": `nrow = SIZE(xin, 1)`,
!   - "UNIT" to "BIUNIT": `nrow = SIZE(xin, 1)`,
!   - "UNIT" to "TRIANGLE": the triangle with vertices `x1`, `x2`,
!     `x3`, which must be present; `nrow = SIZE(x1)` and `nrow <= 3`.
!
!   In all cases `ncol = SIZE(xin, 2)`. For any other case `ans` and
!   `nrow` are not set.
!
! Arguments:
!   xin : REAL(DFP), rank(2) - points, `SHAPE(xin) = [2, N]`
!   ans : REAL(DFP), rank(2) - output,
!     `SIZE(ans, 1) >= nrow`, `SIZE(ans, 2) >= N`
!   nrow : INTEGER(I4B) - number of rows written
!   ncol : INTEGER(I4B) - number of columns written, `SIZE(xin, 2)`
!   from : CHARACTER(*) - "BIUNIT" or "UNIT"
!   to : CHARACTER(*) - "BIUNIT", "UNIT" or "TRIANGLE"
!   x1, x2, x3 : REAL(DFP), rank(1), OPTIONAL - vertices of the
!     triangle, needed when `to` is "TRIANGLE", `SIZE(xk) = nsd`
!
! Formula:
!   $$\text{B to U: } \frac{1 + \mathbf{x}}{2}, \quad
!     \text{U to B: } 2\mathbf{x} - 1, \quad
!     \text{U to T: } \mathbf{x}_1 + (\mathbf{x}_2 - \mathbf{x}_1)\xi
!     + (\mathbf{x}_3 - \mathbf{x}_1)\eta$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE MappingUtility, ONLY: FromTriangle2Triangle_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
! REAL(DFP), PARAMETER :: third = 1.0_DFP / 3.0_DFP
! REAL(DFP) :: xb(2, 4), xu(2, 4), ans(3, 4)
! REAL(DFP) :: x1(2), x2(2), x3(2)
! INTEGER(I4B) :: nrow, ncol
!
! ! vertices and centroid of the biunit and unit triangles
! xb = RESHAPE([-1.0_DFP, -1.0_DFP, 1.0_DFP, -1.0_DFP, -1.0_DFP, 1.0_DFP, &
!               -third, -third], [2_I4B, 4_I4B])
! xu = RESHAPE([0.0_DFP, 0.0_DFP, 1.0_DFP, 0.0_DFP, 0.0_DFP, 1.0_DFP, &
!               third, third], [2_I4B, 4_I4B])
!
! ! biunit to unit
! CALL FromTriangle2Triangle_(xin=xb, ans=ans, nrow=nrow, ncol=ncol, &
!                             from="BIUNIT", to="UNIT")
! CALL OK(nrow .EQ. 2_I4B .AND. ncol .EQ. 4_I4B, "nrow and ncol")
! CALL OK(ALL(ABS(ans(1:2, 1:4) - xu) .LT. tol), "BIUNIT to UNIT")
!
! ! unit to biunit
! CALL FromTriangle2Triangle_(xin=xu, ans=ans, nrow=nrow, ncol=ncol, &
!                             from="UNIT", to="BIUNIT")
! CALL OK(ALL(ABS(ans(1:2, 1:4) - xb) .LT. tol), "UNIT to BIUNIT")
!
! ! unit to a general triangle, x1, x2, x3 are needed
! x1 = [1.0_DFP, 1.0_DFP]
! x2 = [4.0_DFP, 1.0_DFP]
! x3 = [1.0_DFP, 3.0_DFP]
! CALL FromTriangle2Triangle_(xin=xu, ans=ans, nrow=nrow, ncol=ncol, &
!                             from="UNIT", to="TRIANGLE", x1=x1, x2=x2, x3=x3)
! CALL OK(nrow .EQ. 2_I4B, "nrow is SIZE(x1)")
! CALL OK(ALL(ABS(ans(1:2, 2) - x2) .LT. tol), "(1,0) goes to x2")
! CALL OK(ALL(ABS(ans(1:2, 4) - (x1 + x2 + x3) / 3.0_DFP) .LT. tol), &
!         "centroid goes to centroid")
!
! END PROGRAM main
!```

INTERFACE
  MODULE PURE SUBROUTINE FromTriangle2Triangle_(xin, ans, nrow, ncol, &
                                                from, to, x1, x2, x3)
    REAL(DFP), INTENT(IN) :: xin(:, :)
    !! coordinates in bi-unit square in xij coordinate
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! ans(2, SIZE(xin, 2))
    !! coordinates in biunit triangle
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! number of rows and columns written in ans
    !! nrow=2
    !! ncol=SIZE(xin, 2)
    CHARACTER(*), INTENT(IN) :: from
    CHARACTER(*), INTENT(IN) :: to
    REAL(DFP), OPTIONAL, INTENT(IN) :: x1(:)
    !! x1 of physical domain, size(x1) = nsd
    REAL(DFP), OPTIONAL, INTENT(IN) :: x2(:)
    !! x2 of physical domain, size(x2) = nsd
    REAL(DFP), OPTIONAL, INTENT(IN) :: x3(:)
    !! x3 of physical domain, size(x3) = nsd
  END SUBROUTINE FromTriangle2Triangle_
END INTERFACE

!----------------------------------------------------------------------------
!                                                       FromTriangle2Square_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-10-19
! summary: Maps points from a reference triangle to the biunit square.
!
! Description:
!   Maps the points `xin` of the reference triangle `from` to the
!   biunit square [-1,1]^2 by the collapsed (Duffy) coordinates. Only
!   the first letter of `from` and `to` is used. The supported cases
!   are "B" to "B" (biunit triangle) and "U" to "B" (unit triangle).
!   Both letters must have the same case ("BB", "bb", "UB", "ub"). For
!   any other case `ans` is not changed.
!
!   - The whole rows `ans(1, :)` and `ans(2, :)` are assigned, so
!     `SIZE(ans, 2)` must be equal to `SIZE(xin, 2)`.
!   - Vertex 3 of the triangle is the collapsed point. There the first
!     coordinate is not defined (division by zero).
!
! Arguments:
!   xin : REAL(DFP), rank(2) - points in the triangle,
!     `SHAPE(xin) = [2, N]`
!   ans : REAL(DFP), rank(2) - points in the biunit square,
!     `SHAPE(ans) = [2, N]`
!   from : CHARACTER(*) - "BIUNIT" or "UNIT"
!   to : CHARACTER(*) - "BIUNIT"
!
! Formula:
!   $$\text{B to B: } \xi = \frac{1 + 2x + y}{1 - y},\ \eta = y; \quad
!     \text{U to B: } \xi = \frac{2x + y - 1}{1 - y},\ \eta = 2y - 1$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE MappingUtility, ONLY: FromTriangle2Square_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
! REAL(DFP), PARAMETER :: third = 1.0_DFP / 3.0_DFP
! REAL(DFP) :: xb(2, 3), xu(2, 3), ans(2, 3), expected(2, 3)
!
! ! vertex 1, vertex 2 and centroid of the biunit triangle
! xb = RESHAPE([-1.0_DFP, -1.0_DFP, 1.0_DFP, -1.0_DFP, -third, -third], &
!              [2_I4B, 3_I4B])
! ! vertex 1, vertex 2 and centroid of the unit triangle
! xu = RESHAPE([0.0_DFP, 0.0_DFP, 1.0_DFP, 0.0_DFP, third, third], &
!              [2_I4B, 3_I4B])
! expected = RESHAPE([-1.0_DFP, -1.0_DFP, 1.0_DFP, -1.0_DFP, &
!                     0.0_DFP, -third], [2_I4B, 3_I4B])
!
! ! biunit triangle to biunit square
! CALL FromTriangle2Square_(xin=xb, ans=ans, from="B", to="B")
! CALL OK(ALL(ABS(ans - expected) .LT. tol), "BIUNIT triangle to square")
!
! ! unit triangle to biunit square
! CALL FromTriangle2Square_(xin=xu, ans=ans, from="UNIT", to="BIUNIT")
! CALL OK(ALL(ABS(ans - expected) .LT. tol), "UNIT triangle to square")
!
! END PROGRAM main
!```

INTERFACE
  MODULE PURE SUBROUTINE FromTriangle2Square_(xin, ans, from, to)
    REAL(DFP), INTENT(IN) :: xin(:, :)
    !! coordinates in bi-unit square in xij coordinate
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! ans(2, SIZE(xin, 2))
    !! coordinates in biunit triangle
    CHARACTER(*), INTENT(IN) :: from
    CHARACTER(*), INTENT(IN) :: to
  END SUBROUTINE FromTriangle2Square_
END INTERFACE

!----------------------------------------------------------------------------
!                                                       FromSquare2Triangle_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-10-19
! summary: Maps points from the biunit square to a reference triangle.
!
! Description:
!   Maps the points `xin` of the biunit square [-1,1]^2 to the
!   reference triangle `to`, and writes them to `ans(1:nrow, 1:ncol)`.
!   Only the first letter of `from` and `to` is used, and the case does
!   not matter. The supported cases are "B" to "B" (biunit triangle)
!   and "B" to "U" (unit triangle). For any other case `ans` is not
!   changed. The top edge of the square collapses to vertex 3.
!
! Arguments:
!   xin : REAL(DFP), rank(2) - points in the biunit square,
!     `SHAPE(xin) = [2, N]`
!   ans : REAL(DFP), rank(2) - output,
!     `SIZE(ans, 1) >= 2`, `SIZE(ans, 2) >= N`
!   from : CHARACTER(*) - "BIUNIT"
!   to : CHARACTER(*) - "BIUNIT" or "UNIT"
!   nrow : INTEGER(I4B) - number of rows written, 2
!   ncol : INTEGER(I4B) - number of columns written, `SIZE(xin, 2)`
!
! Formula:
!   $$\text{B to B: } x = \frac{(1 + \xi)(1 - \eta)}{2} - 1,\ y = \eta;
!     \quad \text{B to U: } x = \frac{(1 + \xi)(1 - \eta)}{4},\
!     y = \frac{1 + \eta}{2}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE MappingUtility, ONLY: FromSquare2Triangle_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
! REAL(DFP), PARAMETER :: third = 1.0_DFP / 3.0_DFP
! REAL(DFP) :: xin(2, 3), ans(2, 5), eb(2, 3), eu(2, 3)
! INTEGER(I4B) :: nrow, ncol
!
! ! vertex 1, vertex 2 and the image of the centroid in the biunit square
! xin = RESHAPE([-1.0_DFP, -1.0_DFP, 1.0_DFP, -1.0_DFP, 0.0_DFP, -third], &
!               [2_I4B, 3_I4B])
! eb = RESHAPE([-1.0_DFP, -1.0_DFP, 1.0_DFP, -1.0_DFP, -third, -third], &
!              [2_I4B, 3_I4B])
! eu = RESHAPE([0.0_DFP, 0.0_DFP, 1.0_DFP, 0.0_DFP, third, third], &
!              [2_I4B, 3_I4B])
!
! ! biunit square to biunit triangle
! CALL FromSquare2Triangle_(xin=xin, ans=ans, from="BIUNIT", to="BIUNIT", &
!                           nrow=nrow, ncol=ncol)
! CALL OK(nrow .EQ. 2_I4B, "nrow is 2")
! CALL OK(ncol .EQ. 3_I4B, "ncol is SIZE(xin, 2)")
! CALL OK(ALL(ABS(ans(1:nrow, 1:ncol) - eb) .LT. tol), "to BIUNIT triangle")
!
! ! biunit square to unit triangle, lower case is accepted
! CALL FromSquare2Triangle_(xin=xin, ans=ans, from="biunit", to="unit", &
!                           nrow=nrow, ncol=ncol)
! CALL OK(ALL(ABS(ans(1:nrow, 1:ncol) - eu) .LT. tol), "to UNIT triangle")
!
! END PROGRAM main
!```

INTERFACE
  MODULE PURE SUBROUTINE FromSquare2Triangle_(xin, ans, from, to, nrow, ncol)
    REAL(DFP), INTENT(IN) :: xin(:, :)
    !! coordinates in bi-unit square in xij coordinate
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! ans(2, SIZE(xin, 2))
    !! coordinates in biunit triangle
    CHARACTER(*), INTENT(IN) :: from
    CHARACTER(*), INTENT(IN) :: to
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! number of rows and columns written in ans
    !! nrow = 2
    !! ncol = SIZE(xin, 2)
  END SUBROUTINE FromSquare2Triangle_
END INTERFACE

!----------------------------------------------------------------------------
!                                              BarycentricCoordUnitTriangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-10-28
! summary: Barycentric coordinates of points in the unit triangle.
!
! Description:
!   Returns the barycentric coordinates of the points `xin` with
!   respect to the unit triangle (0,0), (1,0), (0,1). `ans(k, j)` is
!   the coordinate of point `j` for vertex `k`. The three coordinates
!   of a point sum to one, and at vertex k they are the unit vector
!   e_k.
!
! Arguments:
!   xin : REAL(DFP), rank(2) - points, `SHAPE(xin) = [2, N]`
!
! Returns:
!   ans : REAL(DFP), rank(2) - barycentric coordinates,
!     `SHAPE(ans) = [3, N]`
!
! Formula:
!   $$\lambda_1 = 1 - x - y, \quad \lambda_2 = x, \quad \lambda_3 = y$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE MappingUtility, ONLY: BarycentricCoordUnitTriangle
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
! REAL(DFP), PARAMETER :: third = 1.0_DFP / 3.0_DFP
! REAL(DFP) :: xin(2, 5), ans(3, 5), eye(3, 3)
!
! ! vertices, centroid and the point (0.2, 0.3)
! xin = RESHAPE([0.0_DFP, 0.0_DFP, 1.0_DFP, 0.0_DFP, 0.0_DFP, 1.0_DFP, &
!                third, third, 0.2_DFP, 0.3_DFP], [2_I4B, 5_I4B])
! eye = RESHAPE([1.0_DFP, 0.0_DFP, 0.0_DFP, 0.0_DFP, 1.0_DFP, 0.0_DFP, &
!                0.0_DFP, 0.0_DFP, 1.0_DFP], [3_I4B, 3_I4B])
!
! ans = BarycentricCoordUnitTriangle(xin)
! CALL OK(ALL(ABS(ans(:, 1:3) - eye) .LT. tol), "vertices")
! CALL OK(ALL(ABS(ans(:, 4) - third) .LT. tol), "centroid")
! CALL OK(ALL(ABS(ans(:, 5) - [0.5_DFP, 0.2_DFP, 0.3_DFP]) .LT. tol), &
!         "point (0.2, 0.3)")
! CALL OK(ALL(ABS(SUM(ans, 1) - 1.0_DFP) .LT. tol), "coordinates sum to 1")
!
! END PROGRAM main
!```

INTERFACE
  MODULE PURE FUNCTION BarycentricCoordUnitTriangle(xin) RESULT(ans)
    REAL(DFP), INTENT(IN) :: xin(:, :)
    REAL(DFP) :: ans(3, SIZE(xin, 2))
  END FUNCTION BarycentricCoordUnitTriangle
END INTERFACE

!----------------------------------------------------------------------------
!                                            BarycentricCoordBiUnitTriangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-10-28
! summary: Barycentric coordinates of points in the biunit triangle.
!
! Description:
!   Returns the barycentric coordinates of the points `xin` with
!   respect to the biunit triangle (-1,-1), (1,-1), (-1,1).
!   `ans(k, j)` is the coordinate of point `j` for vertex `k`. The
!   three coordinates of a point sum to one.
!
! Arguments:
!   xin : REAL(DFP), rank(2) - points, `SHAPE(xin) = [2, N]`
!
! Returns:
!   ans : REAL(DFP), rank(2) - barycentric coordinates,
!     `SHAPE(ans) = [3, N]`
!
! Formula:
!   $$\lambda_1 = -\frac{x + y}{2}, \quad \lambda_2 = \frac{1 + x}{2},
!     \quad \lambda_3 = \frac{1 + y}{2}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE MappingUtility, ONLY: BarycentricCoordBiUnitTriangle
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
! REAL(DFP), PARAMETER :: third = 1.0_DFP / 3.0_DFP
! REAL(DFP) :: xin(2, 5), ans(3, 5), eye(3, 3)
!
! ! vertices, centroid and mid point of edge 1-2
! xin = RESHAPE([-1.0_DFP, -1.0_DFP, 1.0_DFP, -1.0_DFP, -1.0_DFP, 1.0_DFP, &
!                -third, -third, 0.0_DFP, -1.0_DFP], [2_I4B, 5_I4B])
! eye = RESHAPE([1.0_DFP, 0.0_DFP, 0.0_DFP, 0.0_DFP, 1.0_DFP, 0.0_DFP, &
!                0.0_DFP, 0.0_DFP, 1.0_DFP], [3_I4B, 3_I4B])
!
! ans = BarycentricCoordBiUnitTriangle(xin)
! CALL OK(ALL(ABS(ans(:, 1:3) - eye) .LT. tol), "vertices")
! CALL OK(ALL(ABS(ans(:, 4) - third) .LT. tol), "centroid")
! CALL OK(ALL(ABS(ans(:, 5) - [0.5_DFP, 0.5_DFP, 0.0_DFP]) .LT. tol), &
!         "mid point of edge 1-2")
! CALL OK(ALL(ABS(SUM(ans, 1) - 1.0_DFP) .LT. tol), "coordinates sum to 1")
!
! END PROGRAM main
!```

INTERFACE
  MODULE PURE FUNCTION BarycentricCoordBiUnitTriangle(xin) RESULT(ans)
    REAL(DFP), INTENT(IN) :: xin(:, :)
    REAL(DFP) :: ans(3, SIZE(xin, 2))
  END FUNCTION BarycentricCoordBiUnitTriangle
END INTERFACE

!----------------------------------------------------------------------------
!                                                   BarycentricCoordTriangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-07-03
! summary: Barycentric coordinates in a reference triangle.
!
! Description:
!   Returns the barycentric coordinates of the points `xin` with
!   respect to the reference triangle `refTriangle`. Only the first
!   letter of `refTriangle` is used, and the case does not matter:
!
!   - "UNIT": same as `BarycentricCoordUnitTriangle`,
!   - "BIUNIT": same as `BarycentricCoordBiUnitTriangle`.
!
!   For any other value the result is not defined.
!
! Arguments:
!   xin : REAL(DFP), rank(2) - points, `SHAPE(xin) = [2, N]`
!   refTriangle : CHARACTER(*) - "UNIT" or "BIUNIT"
!
! Returns:
!   ans : REAL(DFP), rank(2) - barycentric coordinates,
!     `SHAPE(ans) = [3, N]`
!
! Formula:
!   $$\sum_{k=1}^{3} \lambda_k = 1, \quad
!     \mathbf{x} = \sum_{k=1}^{3} \lambda_k \mathbf{v}_k$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE MappingUtility, ONLY: BarycentricCoordTriangle
! USE MappingUtility, ONLY: BarycentricCoordUnitTriangle
! USE MappingUtility, ONLY: BarycentricCoordBiUnitTriangle
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
! REAL(DFP) :: xu(2, 2), xb(2, 2), ans(3, 2)
!
! xu = RESHAPE([0.2_DFP, 0.3_DFP, 0.5_DFP, 0.0_DFP], [2_I4B, 2_I4B])
! xb = RESHAPE([-0.6_DFP, -0.4_DFP, 0.0_DFP, -1.0_DFP], [2_I4B, 2_I4B])
!
! ans = BarycentricCoordTriangle(xu, "UNIT")
! CALL OK(ALL(ABS(ans - BarycentricCoordUnitTriangle(xu)) .LT. tol), &
!         "UNIT")
! CALL OK(ALL(ABS(ans(:, 1) - [0.5_DFP, 0.2_DFP, 0.3_DFP]) .LT. tol), &
!         "UNIT, point (0.2, 0.3)")
!
! ans = BarycentricCoordTriangle(xb, "BIUNIT")
! CALL OK(ALL(ABS(ans - BarycentricCoordBiUnitTriangle(xb)) .LT. tol), &
!         "BIUNIT")
!
! ! only the first letter is used, lower case is accepted
! ans = BarycentricCoordTriangle(xu, "unit")
! CALL OK(ALL(ABS(ans - BarycentricCoordUnitTriangle(xu)) .LT. tol), &
!         "lower case")
!
! END PROGRAM main
!```

INTERFACE
  MODULE PURE FUNCTION BarycentricCoordTriangle(xin, refTriangle) RESULT(ans)
    REAL(DFP), INTENT(IN) :: xin(:, :)
    REAL(DFP) :: ans(3, SIZE(xin, 2))
    CHARACTER(*), INTENT(IN) :: refTriangle
    !! "UNIT"
    !! "BIUNIT"
  END FUNCTION BarycentricCoordTriangle
END INTERFACE

!----------------------------------------------------------------------------
!                                                   BarycentricCoordTriangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-04-22
! summary: Barycentric coordinates in a triangle, no allocation.
!
! Description:
!   Same as `BarycentricCoordTriangle`, but the result is written to
!   `ans`. The whole rows `ans(1, :)`, `ans(2, :)` and `ans(3, :)` are
!   assigned, so `SIZE(ans, 2)` must be equal to `SIZE(xin, 2)`. For a
!   value of `refTriangle` other than "UNIT" or "BIUNIT", `ans` is not
!   changed.
!
! Arguments:
!   xin : REAL(DFP), rank(2) - points, `SHAPE(xin) = [2, N]`
!   refTriangle : CHARACTER(*) - "UNIT" or "BIUNIT"
!   ans : REAL(DFP), rank(2) - barycentric coordinates,
!     `SIZE(ans, 1) >= 3`, `SIZE(ans, 2) = N`
!
! Formula:
!   $$\sum_{k=1}^{3} \lambda_k = 1, \quad
!     \mathbf{x} = \sum_{k=1}^{3} \lambda_k \mathbf{v}_k$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE MappingUtility, ONLY: BarycentricCoordTriangle_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
! REAL(DFP) :: xin(2, 2), ans(3, 2)
!
! xin = RESHAPE([0.2_DFP, 0.3_DFP, 0.5_DFP, 0.0_DFP], [2_I4B, 2_I4B])
!
! ! ans must have 3 rows and SIZE(xin, 2) columns
! CALL BarycentricCoordTriangle_(xin, "UNIT", ans)
! CALL OK(ALL(ABS(ans(:, 1) - [0.5_DFP, 0.2_DFP, 0.3_DFP]) .LT. tol), &
!         "point (0.2, 0.3)")
! CALL OK(ALL(ABS(ans(:, 2) - [0.5_DFP, 0.5_DFP, 0.0_DFP]) .LT. tol), &
!         "point (0.5, 0.0)")
!
! END PROGRAM main
!```

INTERFACE
  MODULE PURE SUBROUTINE BarycentricCoordTriangle_(xin, refTriangle, ans)
    REAL(DFP), INTENT(IN) :: xin(:, :)
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    ! REAL(DFP) :: ans(3, SIZE(xin, 2))
    CHARACTER(*), INTENT(IN) :: refTriangle
    !! "UNIT"
    !! "BIUNIT"
  END SUBROUTINE BarycentricCoordTriangle_
END INTERFACE

!----------------------------------------------------------------------------
!                                            FromBiUnitTriangle2UnitTriangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-10-28
! summary: Maps points from the biunit to the unit triangle.
!
! Description:
!   Maps the points `xin` of the biunit triangle (-1,-1), (1,-1),
!   (-1,1) to the unit triangle (0,0), (1,0), (0,1). Each coordinate is
!   mapped by (1 + x) / 2.
!
! Arguments:
!   xin : REAL(DFP), rank(2) - points in the biunit triangle,
!     `SHAPE(xin) = [2, N]`
!
! Returns:
!   ans : REAL(DFP), rank(2) - points in the unit triangle,
!     `SHAPE(ans) = [2, N]`
!
! Formula:
!   $$\mathrm{ans}_{ij} = \frac{1 + x_{ij}}{2}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE MappingUtility, ONLY: FromBiUnitTriangle2UnitTriangle
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
! REAL(DFP), PARAMETER :: third = 1.0_DFP / 3.0_DFP
! REAL(DFP) :: xb(2, 4), xu(2, 4), ans(2, 4)
!
! ! vertices and centroid
! xb = RESHAPE([-1.0_DFP, -1.0_DFP, 1.0_DFP, -1.0_DFP, -1.0_DFP, 1.0_DFP, &
!               -third, -third], [2_I4B, 4_I4B])
! xu = RESHAPE([0.0_DFP, 0.0_DFP, 1.0_DFP, 0.0_DFP, 0.0_DFP, 1.0_DFP, &
!               third, third], [2_I4B, 4_I4B])
!
! ans = FromBiUnitTriangle2UnitTriangle(xb)
! CALL OK(ALL(ABS(ans - xu) .LT. tol), "vertices and centroid")
!
! END PROGRAM main
!```

INTERFACE
  MODULE PURE FUNCTION FromBiUnitTriangle2UnitTriangle(xin) RESULT(ans)
    REAL(DFP), INTENT(IN) :: xin(:, :)
    REAL(DFP) :: ans(2, SIZE(xin, 2))
  END FUNCTION FromBiUnitTriangle2UnitTriangle
END INTERFACE

!----------------------------------------------------------------------------
!                                            FromUnitTriangle2BiUnitTriangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-10-28
! summary: Maps points from the unit to the biunit triangle.
!
! Description:
!   Maps the points `xin` of the unit triangle (0,0), (1,0), (0,1) to
!   the biunit triangle (-1,-1), (1,-1), (-1,1). It is the inverse of
!   `FromBiUnitTriangle2UnitTriangle`.
!
! Arguments:
!   xin : REAL(DFP), rank(2) - points in the unit triangle,
!     `SHAPE(xin) = [2, N]`
!
! Returns:
!   ans : REAL(DFP), rank(2) - points in the biunit triangle,
!     `SHAPE(ans) = [2, N]`
!
! Formula:
!   $$\mathrm{ans}_{ij} = 2 x_{ij} - 1$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE MappingUtility, ONLY: FromUnitTriangle2BiUnitTriangle
! USE MappingUtility, ONLY: FromBiUnitTriangle2UnitTriangle
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
! REAL(DFP), PARAMETER :: third = 1.0_DFP / 3.0_DFP
! REAL(DFP) :: xb(2, 4), xu(2, 4), ans(2, 4)
!
! ! vertices and centroid
! xb = RESHAPE([-1.0_DFP, -1.0_DFP, 1.0_DFP, -1.0_DFP, -1.0_DFP, 1.0_DFP, &
!               -third, -third], [2_I4B, 4_I4B])
! xu = RESHAPE([0.0_DFP, 0.0_DFP, 1.0_DFP, 0.0_DFP, 0.0_DFP, 1.0_DFP, &
!               third, third], [2_I4B, 4_I4B])
!
! ans = FromUnitTriangle2BiUnitTriangle(xu)
! CALL OK(ALL(ABS(ans - xb) .LT. tol), "vertices and centroid")
!
! ! inverse of FromBiUnitTriangle2UnitTriangle
! ans = FromBiUnitTriangle2UnitTriangle(FromUnitTriangle2BiUnitTriangle(xu))
! CALL OK(ALL(ABS(ans - xu) .LT. tol), "round trip")
!
! END PROGRAM main
!```

INTERFACE
  MODULE PURE FUNCTION FromUnitTriangle2BiUnitTriangle(xin) RESULT(ans)
    REAL(DFP), INTENT(IN) :: xin(:, :)
    REAL(DFP) :: ans(2, SIZE(xin, 2))
  END FUNCTION FromUnitTriangle2BiUnitTriangle
END INTERFACE

!----------------------------------------------------------------------------
!                                       FromBiUnitTetrahedron2UnitTetrahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-10-28
! summary: Maps points from the biunit to the unit tetrahedron.
!
! Description:
!   Maps the points `xin` of the biunit tetrahedron (-1,-1,-1),
!   (1,-1,-1), (-1,1,-1), (-1,-1,1) to the unit tetrahedron (0,0,0),
!   (1,0,0), (0,1,0), (0,0,1). Each coordinate is mapped by
!   (1 + x) / 2.
!
! Arguments:
!   xin : REAL(DFP), rank(2) - points in the biunit tetrahedron,
!     `SHAPE(xin) = [3, N]`
!
! Returns:
!   ans : REAL(DFP), rank(2) - points in the unit tetrahedron,
!     `SHAPE(ans) = [3, N]`
!
! Formula:
!   $$\mathrm{ans}_{ij} = \frac{1 + x_{ij}}{2}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE MappingUtility, ONLY: FromBiUnitTetrahedron2UnitTetrahedron
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
! REAL(DFP) :: xb(3, 5), xu(3, 5), ans(3, 5)
!
! ! vertices and centroid of the biunit tetrahedron
! xb(:, 1) = [-1.0_DFP, -1.0_DFP, -1.0_DFP]
! xb(:, 2) = [1.0_DFP, -1.0_DFP, -1.0_DFP]
! xb(:, 3) = [-1.0_DFP, 1.0_DFP, -1.0_DFP]
! xb(:, 4) = [-1.0_DFP, -1.0_DFP, 1.0_DFP]
! xb(:, 5) = -0.5_DFP
!
! ! vertices and centroid of the unit tetrahedron
! xu(:, 1) = [0.0_DFP, 0.0_DFP, 0.0_DFP]
! xu(:, 2) = [1.0_DFP, 0.0_DFP, 0.0_DFP]
! xu(:, 3) = [0.0_DFP, 1.0_DFP, 0.0_DFP]
! xu(:, 4) = [0.0_DFP, 0.0_DFP, 1.0_DFP]
! xu(:, 5) = 0.25_DFP
!
! ans = FromBiUnitTetrahedron2UnitTetrahedron(xb)
! CALL OK(ALL(ABS(ans - xu) .LT. tol), "vertices and centroid")
!
! END PROGRAM main
!```

INTERFACE
  MODULE PURE FUNCTION FromBiUnitTetrahedron2UnitTetrahedron(xin) RESULT(ans)
    REAL(DFP), INTENT(IN) :: xin(:, :)
    REAL(DFP) :: ans(3, SIZE(xin, 2))
  END FUNCTION FromBiUnitTetrahedron2UnitTetrahedron
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-07-09
! summary: Biunit to unit tetrahedron without allocation.
!
! Description:
!   Same as `FromBiUnitTetrahedron2UnitTetrahedron`, but the result is
!   written to `ans(1:nrow, 1:ncol)`, where `nrow = SIZE(xin, 1)` and
!   `ncol = SIZE(xin, 2)`.
!
!   This routine is not public; it is used inside the module.
!
! Arguments:
!   xin : REAL(DFP), rank(2) - points in the biunit tetrahedron,
!     `SHAPE(xin) = [3, N]`
!   ans : REAL(DFP), rank(2) - output,
!     `SIZE(ans, 1) >= 3`, `SIZE(ans, 2) >= N`
!   nrow : INTEGER(I4B) - number of rows written, `SIZE(xin, 1)`
!   ncol : INTEGER(I4B) - number of columns written, `SIZE(xin, 2)`
!
! Formula:
!   $$\mathrm{ans}_{ij} = \frac{1 + x_{ij}}{2}$$

INTERFACE
  MODULE PURE SUBROUTINE FromBiUnitTetrahedron2UnitTetrahedron_(xin, &
                                                              ans, nrow, ncol)
    REAL(DFP), INTENT(IN) :: xin(:, :)
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    ! REAL(DFP) :: ans(3, SIZE(xin, 2))
  END SUBROUTINE FromBiUnitTetrahedron2UnitTetrahedron_
END INTERFACE

!----------------------------------------------------------------------------
!                                      FromUnitTetrahedron2BiUnitTetrahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-10-28
! summary: Maps points from the unit to the biunit tetrahedron.
!
! Description:
!   Maps the points `xin` of the unit tetrahedron to the biunit
!   tetrahedron. It is the inverse of
!   `FromBiUnitTetrahedron2UnitTetrahedron`.
!
! Arguments:
!   xin : REAL(DFP), rank(2) - points in the unit tetrahedron,
!     `SHAPE(xin) = [3, N]`
!
! Returns:
!   ans : REAL(DFP), rank(2) - points in the biunit tetrahedron,
!     `SHAPE(ans) = [3, N]`
!
! Formula:
!   $$\mathrm{ans}_{ij} = 2 x_{ij} - 1$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE MappingUtility, ONLY: FromUnitTetrahedron2BiUnitTetrahedron
! USE MappingUtility, ONLY: FromBiUnitTetrahedron2UnitTetrahedron
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
! REAL(DFP) :: xb(3, 5), xu(3, 5), ans(3, 5)
!
! ! vertices and centroid of the biunit tetrahedron
! xb(:, 1) = [-1.0_DFP, -1.0_DFP, -1.0_DFP]
! xb(:, 2) = [1.0_DFP, -1.0_DFP, -1.0_DFP]
! xb(:, 3) = [-1.0_DFP, 1.0_DFP, -1.0_DFP]
! xb(:, 4) = [-1.0_DFP, -1.0_DFP, 1.0_DFP]
! xb(:, 5) = -0.5_DFP
!
! ! vertices and centroid of the unit tetrahedron
! xu(:, 1) = [0.0_DFP, 0.0_DFP, 0.0_DFP]
! xu(:, 2) = [1.0_DFP, 0.0_DFP, 0.0_DFP]
! xu(:, 3) = [0.0_DFP, 1.0_DFP, 0.0_DFP]
! xu(:, 4) = [0.0_DFP, 0.0_DFP, 1.0_DFP]
! xu(:, 5) = 0.25_DFP
!
! ans = FromUnitTetrahedron2BiUnitTetrahedron(xu)
! CALL OK(ALL(ABS(ans - xb) .LT. tol), "vertices and centroid")
!
! ! inverse of FromBiUnitTetrahedron2UnitTetrahedron
! ans = FromBiUnitTetrahedron2UnitTetrahedron( &
!       FromUnitTetrahedron2BiUnitTetrahedron(xu))
! CALL OK(ALL(ABS(ans - xu) .LT. tol), "round trip")
!
! END PROGRAM main
!```

INTERFACE
  MODULE PURE FUNCTION FromUnitTetrahedron2BiUnitTetrahedron(xin) RESULT(ans)
    REAL(DFP), INTENT(IN) :: xin(:, :)
    REAL(DFP) :: ans(3, SIZE(xin, 2))
  END FUNCTION FromUnitTetrahedron2BiUnitTetrahedron
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-07-09
! summary: Unit to biunit tetrahedron without allocation.
!
! Description:
!   Same as `FromUnitTetrahedron2BiUnitTetrahedron`, but the result is
!   written to `ans(1:nrow, 1:ncol)`, where `nrow = SIZE(xin, 1)` and
!   `ncol = SIZE(xin, 2)`.
!
! Arguments:
!   xin : REAL(DFP), rank(2) - points in the unit tetrahedron,
!     `SHAPE(xin) = [3, N]`
!   ans : REAL(DFP), rank(2) - output,
!     `SIZE(ans, 1) >= 3`, `SIZE(ans, 2) >= N`
!   nrow : INTEGER(I4B) - number of rows written, `SIZE(xin, 1)`
!   ncol : INTEGER(I4B) - number of columns written, `SIZE(xin, 2)`
!
! Formula:
!   $$\mathrm{ans}_{ij} = 2 x_{ij} - 1$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE MappingUtility, ONLY: FromUnitTetrahedron2BiUnitTetrahedron_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
! REAL(DFP) :: xin(3, 2), ans(4, 4)
! INTEGER(I4B) :: nrow, ncol
!
! ! centroid and vertex 2 of the unit tetrahedron
! xin(:, 1) = 0.25_DFP
! xin(:, 2) = [1.0_DFP, 0.0_DFP, 0.0_DFP]
!
! CALL FromUnitTetrahedron2BiUnitTetrahedron_(xin, ans, nrow, ncol)
! CALL OK(nrow .EQ. 3_I4B, "nrow is SIZE(xin, 1)")
! CALL OK(ncol .EQ. 2_I4B, "ncol is SIZE(xin, 2)")
! CALL OK(ALL(ABS(ans(1:3, 1) + 0.5_DFP) .LT. tol), "centroid")
! CALL OK(ALL(ABS(ans(1:3, 2) - [1.0_DFP, -1.0_DFP, -1.0_DFP]) .LT. tol), &
!         "vertex 2")
!
! END PROGRAM main
!```

INTERFACE
  MODULE PURE SUBROUTINE FromUnitTetrahedron2BiUnitTetrahedron_(xin, &
                                                              ans, nrow, ncol)
    REAL(DFP), INTENT(IN) :: xin(:, :)
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    ! REAL(DFP) :: ans(3, SIZE(xin, 2))
  END SUBROUTINE FromUnitTetrahedron2BiUnitTetrahedron_
END INTERFACE

!----------------------------------------------------------------------------
!                                          FromBiUnitTetrahedron2Tetrahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-10-28
! summary: Maps points from the biunit tetrahedron to a tetrahedron.
!
! Description:
!   Maps the points `xin` of the biunit tetrahedron to the tetrahedron
!   with vertices `x1`, `x2`, `x3`, `x4` in 3D. The map is affine, so
!   vertex k of the biunit tetrahedron goes to `xk`. The weights are
!   the barycentric coordinates of the biunit tetrahedron.
!
! Arguments:
!   xin : REAL(DFP), rank(2) - points in the biunit tetrahedron,
!     `SHAPE(xin) = [3, N]`
!   x1, x2, x3, x4 : REAL(DFP), rank(1) - vertices, `SIZE(xk) = 3`
!
! Returns:
!   ans : REAL(DFP), rank(2) - mapped points, `SHAPE(ans) = [3, N]`
!
! Formula:
!   $$\mathbf{x} = -\frac{1 + \xi + \eta + \zeta}{2}\mathbf{x}_1
!     + \frac{1 + \xi}{2}\mathbf{x}_2 + \frac{1 + \eta}{2}\mathbf{x}_3
!     + \frac{1 + \zeta}{2}\mathbf{x}_4$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE MappingUtility, ONLY: FromBiUnitTetrahedron2Tetrahedron
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
! REAL(DFP) :: xb(3, 5), x1(3), x2(3), x3(3), x4(3), ans(3, 5)
!
! ! vertices and centroid of the biunit tetrahedron
! xb(:, 1) = [-1.0_DFP, -1.0_DFP, -1.0_DFP]
! xb(:, 2) = [1.0_DFP, -1.0_DFP, -1.0_DFP]
! xb(:, 3) = [-1.0_DFP, 1.0_DFP, -1.0_DFP]
! xb(:, 4) = [-1.0_DFP, -1.0_DFP, 1.0_DFP]
! xb(:, 5) = -0.5_DFP
!
! ! vertices of a tetrahedron
! x1 = [1.0_DFP, 1.0_DFP, 1.0_DFP]
! x2 = [3.0_DFP, 1.0_DFP, 1.0_DFP]
! x3 = [1.0_DFP, 4.0_DFP, 1.0_DFP]
! x4 = [1.0_DFP, 1.0_DFP, 5.0_DFP]
!
! ans = FromBiUnitTetrahedron2Tetrahedron(xb, x1, x2, x3, x4)
! CALL OK(ALL(ABS(ans(:, 1) - x1) .LT. tol), "vertex 1 goes to x1")
! CALL OK(ALL(ABS(ans(:, 2) - x2) .LT. tol), "vertex 2 goes to x2")
! CALL OK(ALL(ABS(ans(:, 3) - x3) .LT. tol), "vertex 3 goes to x3")
! CALL OK(ALL(ABS(ans(:, 4) - x4) .LT. tol), "vertex 4 goes to x4")
! CALL OK(ALL(ABS(ans(:, 5) - [1.5_DFP, 1.75_DFP, 2.0_DFP]) .LT. tol), &
!         "centroid goes to centroid")
!
! END PROGRAM main
!```

INTERFACE
  MODULE PURE FUNCTION FromBiUnitTetrahedron2Tetrahedron( &
    & xin, &
    & x1, &
    & x2, &
    & x3, &
    & x4) RESULT(ans)
    REAL(DFP), INTENT(IN) :: xin(:, :)
    REAL(DFP), INTENT(IN) :: x1(3)
    !! Coordinate of tetrahedron node 1
    REAL(DFP), INTENT(IN) :: x2(3)
    !! Coordinate of tetrahedron node 2
    REAL(DFP), INTENT(IN) :: x3(3)
    !! Coordinate of tetrahedron node 3
    REAL(DFP), INTENT(IN) :: x4(3)
    !! Coordinate of tetrahedron node 4
    REAL(DFP) :: ans(3, SIZE(xin, 2))
  END FUNCTION FromBiUnitTetrahedron2Tetrahedron
END INTERFACE

!----------------------------------------------------------------------------
!                                          FromUnitTetrahedron2Tetrahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-07-27
! summary: Maps points from the unit tetrahedron to a tetrahedron.
!
! Description:
!   Maps the points `xin` of the unit tetrahedron to the tetrahedron
!   with vertices `x1`, `x2`, `x3`, `x4` in 3D. The map is affine, so
!   vertex k of the unit tetrahedron goes to `xk`.
!
! Arguments:
!   xin : REAL(DFP), rank(2) - points in the unit tetrahedron,
!     `SHAPE(xin) = [3, N]`
!   x1, x2, x3, x4 : REAL(DFP), rank(1) - vertices, `SIZE(xk) = 3`
!
! Returns:
!   ans : REAL(DFP), rank(2) - mapped points, `SHAPE(ans) = [3, N]`
!
! Formula:
!   $$\mathbf{x} = (1 - \xi - \eta - \zeta)\mathbf{x}_1
!     + \xi\mathbf{x}_2 + \eta\mathbf{x}_3 + \zeta\mathbf{x}_4$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE MappingUtility, ONLY: FromUnitTetrahedron2Tetrahedron
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
! REAL(DFP) :: xu(3, 5), x1(3), x2(3), x3(3), x4(3), ans(3, 5)
!
! ! vertices and centroid of the unit tetrahedron
! xu(:, 1) = [0.0_DFP, 0.0_DFP, 0.0_DFP]
! xu(:, 2) = [1.0_DFP, 0.0_DFP, 0.0_DFP]
! xu(:, 3) = [0.0_DFP, 1.0_DFP, 0.0_DFP]
! xu(:, 4) = [0.0_DFP, 0.0_DFP, 1.0_DFP]
! xu(:, 5) = 0.25_DFP
!
! ! vertices of a tetrahedron
! x1 = [1.0_DFP, 1.0_DFP, 1.0_DFP]
! x2 = [3.0_DFP, 1.0_DFP, 1.0_DFP]
! x3 = [1.0_DFP, 4.0_DFP, 1.0_DFP]
! x4 = [1.0_DFP, 1.0_DFP, 5.0_DFP]
!
! ans = FromUnitTetrahedron2Tetrahedron(xu, x1, x2, x3, x4)
! CALL OK(ALL(ABS(ans(:, 1) - x1) .LT. tol), "vertex 1 goes to x1")
! CALL OK(ALL(ABS(ans(:, 2) - x2) .LT. tol), "vertex 2 goes to x2")
! CALL OK(ALL(ABS(ans(:, 3) - x3) .LT. tol), "vertex 3 goes to x3")
! CALL OK(ALL(ABS(ans(:, 4) - x4) .LT. tol), "vertex 4 goes to x4")
! CALL OK(ALL(ABS(ans(:, 5) - [1.5_DFP, 1.75_DFP, 2.0_DFP]) .LT. tol), &
!         "centroid goes to centroid")
!
! END PROGRAM main
!```

INTERFACE
  MODULE PURE FUNCTION FromUnitTetrahedron2Tetrahedron(xin, x1, x2, x3, x4) &
    RESULT(ans)
    REAL(DFP), INTENT(IN) :: xin(:, :)
    REAL(DFP), INTENT(IN) :: x1(3)
    !! Coordinate of tetrahedron node 1
    REAL(DFP), INTENT(IN) :: x2(3)
    !! Coordinate of tetrahedron node 2
    REAL(DFP), INTENT(IN) :: x3(3)
    !! Coordinate of tetrahedron node 3
    REAL(DFP), INTENT(IN) :: x4(3)
    !! Coordinate of tetrahedron node 4
    REAL(DFP) :: ans(3, SIZE(xin, 2))
  END FUNCTION FromUnitTetrahedron2Tetrahedron
END INTERFACE

!----------------------------------------------------------------------------
!                                           FromUnitTetrahedron2Tetrahedron_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-06-28
! summary: Unit tetrahedron to a tetrahedron without allocation.
!
! Description:
!   Same as `FromUnitTetrahedron2Tetrahedron`, but the result is
!   written to `ans(1:nrow, 1:ncol)`, where `nrow = 3` and
!   `ncol = SIZE(xin, 2)`.
!
! Arguments:
!   xin : REAL(DFP), rank(2) - points in the unit tetrahedron,
!     `SHAPE(xin) = [3, N]`
!   x1, x2, x3, x4 : REAL(DFP), rank(1) - vertices, `SIZE(xk) = 3`
!   ans : REAL(DFP), rank(2) - output,
!     `SIZE(ans, 1) >= 3`, `SIZE(ans, 2) >= N`
!   nrow : INTEGER(I4B) - number of rows written, 3
!   ncol : INTEGER(I4B) - number of columns written, `SIZE(xin, 2)`
!
! Formula:
!   $$\mathbf{x} = (1 - \xi - \eta - \zeta)\mathbf{x}_1
!     + \xi\mathbf{x}_2 + \eta\mathbf{x}_3 + \zeta\mathbf{x}_4$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE MappingUtility, ONLY: FromUnitTetrahedron2Tetrahedron_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
! REAL(DFP) :: xin(3, 2), x1(3), x2(3), x3(3), x4(3), ans(4, 4)
! INTEGER(I4B) :: nrow, ncol
!
! ! centroid and vertex 4 of the unit tetrahedron
! xin(:, 1) = 0.25_DFP
! xin(:, 2) = [0.0_DFP, 0.0_DFP, 1.0_DFP]
!
! x1 = [1.0_DFP, 1.0_DFP, 1.0_DFP]
! x2 = [3.0_DFP, 1.0_DFP, 1.0_DFP]
! x3 = [1.0_DFP, 4.0_DFP, 1.0_DFP]
! x4 = [1.0_DFP, 1.0_DFP, 5.0_DFP]
!
! CALL FromUnitTetrahedron2Tetrahedron_(xin, x1, x2, x3, x4, ans, nrow, ncol)
! CALL OK(nrow .EQ. 3_I4B, "nrow is 3")
! CALL OK(ncol .EQ. 2_I4B, "ncol is SIZE(xin, 2)")
! CALL OK(ALL(ABS(ans(1:3, 1) - [1.5_DFP, 1.75_DFP, 2.0_DFP]) .LT. tol), &
!         "centroid")
! CALL OK(ALL(ABS(ans(1:3, 2) - x4) .LT. tol), "vertex 4")
!
! END PROGRAM main
!```

INTERFACE
MODULE PURE SUBROUTINE FromUnitTetrahedron2Tetrahedron_(xin, x1, x2, x3, x4, &
                                                          ans, nrow, ncol)
    REAL(DFP), INTENT(IN) :: xin(:, :)
    REAL(DFP), INTENT(IN) :: x1(3)
    !! Coordinate of tetrahedron node 1
    REAL(DFP), INTENT(IN) :: x2(3)
    !! Coordinate of tetrahedron node 2
    REAL(DFP), INTENT(IN) :: x3(3)
    !! Coordinate of tetrahedron node 3
    REAL(DFP), INTENT(IN) :: x4(3)
    !! Coordinate of tetrahedron node 4
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! ans(3, SIZE(xin, 2))
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  END SUBROUTINE FromUnitTetrahedron2Tetrahedron_
END INTERFACE

!----------------------------------------------------------------------------
!                                            BarycentricCoordUnitTetrahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-10-28
! summary: Barycentric coordinates of points in the unit tetrahedron.
!
! Description:
!   Returns the barycentric coordinates of the points `xin` with
!   respect to the unit tetrahedron (0,0,0), (1,0,0), (0,1,0),
!   (0,0,1). `ans(k, j)` is the coordinate of point `j` for vertex
!   `k`. The four coordinates of a point sum to one.
!
! Arguments:
!   xin : REAL(DFP), rank(2) - points, `SHAPE(xin) = [3, N]`
!
! Returns:
!   ans : REAL(DFP), rank(2) - barycentric coordinates,
!     `SHAPE(ans) = [4, N]`
!
! Formula:
!   $$\lambda_1 = 1 - x - y - z, \quad \lambda_2 = x, \quad
!     \lambda_3 = y, \quad \lambda_4 = z$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE MappingUtility, ONLY: BarycentricCoordUnitTetrahedron
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
! REAL(DFP) :: xu(3, 5), ans(4, 5), eye(4, 4)
! INTEGER(I4B) :: ii
!
! ! vertices and centroid of the unit tetrahedron
! xu(:, 1) = [0.0_DFP, 0.0_DFP, 0.0_DFP]
! xu(:, 2) = [1.0_DFP, 0.0_DFP, 0.0_DFP]
! xu(:, 3) = [0.0_DFP, 1.0_DFP, 0.0_DFP]
! xu(:, 4) = [0.0_DFP, 0.0_DFP, 1.0_DFP]
! xu(:, 5) = 0.25_DFP
!
! eye = 0.0_DFP
! DO ii = 1_I4B, 4_I4B
!   eye(ii, ii) = 1.0_DFP
! END DO
!
! ans = BarycentricCoordUnitTetrahedron(xu)
! CALL OK(ALL(ABS(ans(:, 1:4) - eye) .LT. tol), "vertices")
! CALL OK(ALL(ABS(ans(:, 5) - 0.25_DFP) .LT. tol), "centroid")
! CALL OK(ALL(ABS(SUM(ans, 1) - 1.0_DFP) .LT. tol), "coordinates sum to 1")
!
! END PROGRAM main
!```

INTERFACE
  MODULE PURE FUNCTION BarycentricCoordUnitTetrahedron(xin) RESULT(ans)
    REAL(DFP), INTENT(IN) :: xin(:, :)
    REAL(DFP) :: ans(4, SIZE(xin, 2))
  END FUNCTION BarycentricCoordUnitTetrahedron
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-06-30
! summary: Barycentric coordinates in the unit tetrahedron, no alloc.
!
! Description:
!   Same as `BarycentricCoordUnitTetrahedron`, but the result is
!   written to `ans(1:nrow, 1:ncol)`, where `nrow = 4` and
!   `ncol = SIZE(xin, 2)`.
!
! Arguments:
!   xin : REAL(DFP), rank(2) - points, `SHAPE(xin) = [3, N]`
!   ans : REAL(DFP), rank(2) - output,
!     `SIZE(ans, 1) >= 4`, `SIZE(ans, 2) >= N`
!   nrow : INTEGER(I4B) - number of rows written, 4
!   ncol : INTEGER(I4B) - number of columns written, `SIZE(xin, 2)`
!
! Formula:
!   $$\lambda_1 = 1 - x - y - z, \quad \lambda_2 = x, \quad
!     \lambda_3 = y, \quad \lambda_4 = z$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE MappingUtility, ONLY: BarycentricCoordUnitTetrahedron
! USE MappingUtility, ONLY: BarycentricCoordUnitTetrahedron_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
! REAL(DFP) :: xu(3, 5), ans(5, 6)
! INTEGER(I4B) :: nrow, ncol
!
! ! vertices and centroid of the unit tetrahedron
! xu(:, 1) = [0.0_DFP, 0.0_DFP, 0.0_DFP]
! xu(:, 2) = [1.0_DFP, 0.0_DFP, 0.0_DFP]
! xu(:, 3) = [0.0_DFP, 1.0_DFP, 0.0_DFP]
! xu(:, 4) = [0.0_DFP, 0.0_DFP, 1.0_DFP]
! xu(:, 5) = 0.25_DFP
!
! ! ans can be larger, only ans(1:nrow, 1:ncol) is written
! CALL BarycentricCoordUnitTetrahedron_(xu, ans, nrow, ncol)
! CALL OK(nrow .EQ. 4_I4B, "nrow is 4")
! CALL OK(ncol .EQ. 5_I4B, "ncol is SIZE(xin, 2)")
! CALL OK(ALL(ABS(ans(1:nrow, 1:ncol) - &
!                 BarycentricCoordUnitTetrahedron(xu)) .LT. tol), &
!         "same as BarycentricCoordUnitTetrahedron")
!
! END PROGRAM main
!```

INTERFACE
  MODULE PURE SUBROUTINE BarycentricCoordUnitTetrahedron_(xin, ans, &
                                                          nrow, ncol)
    REAL(DFP), INTENT(IN) :: xin(:, :)
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! ans(4, SIZE(xin, 2))
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  END SUBROUTINE BarycentricCoordUnitTetrahedron_
END INTERFACE

!----------------------------------------------------------------------------
!                                            BarycentricCoordBiUnitTetrahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-10-28
! summary: Barycentric coordinates of points in the biunit tetrahedron.
!
! Description:
!   Returns the barycentric coordinates of the points `xin` with
!   respect to the biunit tetrahedron (-1,-1,-1), (1,-1,-1),
!   (-1,1,-1), (-1,-1,1). `ans(k, j)` is the coordinate of point `j`
!   for vertex `k`. The four coordinates of a point sum to one.
!
! Arguments:
!   xin : REAL(DFP), rank(2) - points, `SHAPE(xin) = [3, N]`
!
! Returns:
!   ans : REAL(DFP), rank(2) - barycentric coordinates,
!     `SHAPE(ans) = [4, N]`
!
! Formula:
!   $$\lambda_1 = -\frac{1 + x + y + z}{2}, \quad
!     \lambda_2 = \frac{1 + x}{2}, \quad \lambda_3 = \frac{1 + y}{2},
!     \quad \lambda_4 = \frac{1 + z}{2}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE MappingUtility, ONLY: BarycentricCoordBiUnitTetrahedron
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
! REAL(DFP) :: xb(3, 5), ans(4, 5), eye(4, 4)
! INTEGER(I4B) :: ii
!
! ! vertices and centroid of the biunit tetrahedron
! xb(:, 1) = [-1.0_DFP, -1.0_DFP, -1.0_DFP]
! xb(:, 2) = [1.0_DFP, -1.0_DFP, -1.0_DFP]
! xb(:, 3) = [-1.0_DFP, 1.0_DFP, -1.0_DFP]
! xb(:, 4) = [-1.0_DFP, -1.0_DFP, 1.0_DFP]
! xb(:, 5) = -0.5_DFP
!
! eye = 0.0_DFP
! DO ii = 1_I4B, 4_I4B
!   eye(ii, ii) = 1.0_DFP
! END DO
!
! ans = BarycentricCoordBiUnitTetrahedron(xb)
! CALL OK(ALL(ABS(ans(:, 1:4) - eye) .LT. tol), "vertices")
! CALL OK(ALL(ABS(ans(:, 5) - 0.25_DFP) .LT. tol), "centroid")
! CALL OK(ALL(ABS(SUM(ans, 1) - 1.0_DFP) .LT. tol), "coordinates sum to 1")
!
! END PROGRAM main
!```

INTERFACE
  MODULE PURE FUNCTION BarycentricCoordBiUnitTetrahedron(xin) RESULT(ans)
    REAL(DFP), INTENT(IN) :: xin(:, :)
    REAL(DFP) :: ans(4, SIZE(xin, 2))
  END FUNCTION BarycentricCoordBiUnitTetrahedron
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-06-30
! summary: Barycentric coordinates in biunit tetrahedron, no alloc.
!
! Description:
!   Same as `BarycentricCoordBiUnitTetrahedron`, but the result is
!   written to `ans(1:nrow, 1:ncol)`, where `nrow = 4` and
!   `ncol = SIZE(xin, 2)`.
!
! Arguments:
!   xin : REAL(DFP), rank(2) - points, `SHAPE(xin) = [3, N]`
!   ans : REAL(DFP), rank(2) - output,
!     `SIZE(ans, 1) >= 4`, `SIZE(ans, 2) >= N`
!   nrow : INTEGER(I4B) - number of rows written, 4
!   ncol : INTEGER(I4B) - number of columns written, `SIZE(xin, 2)`
!
! Formula:
!   $$\lambda_1 = -\frac{1 + x + y + z}{2}, \quad
!     \lambda_2 = \frac{1 + x}{2}, \quad \lambda_3 = \frac{1 + y}{2},
!     \quad \lambda_4 = \frac{1 + z}{2}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE MappingUtility, ONLY: BarycentricCoordBiUnitTetrahedron
! USE MappingUtility, ONLY: BarycentricCoordBiUnitTetrahedron_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
! REAL(DFP) :: xb(3, 5), ans(5, 6)
! INTEGER(I4B) :: nrow, ncol
!
! ! vertices and centroid of the biunit tetrahedron
! xb(:, 1) = [-1.0_DFP, -1.0_DFP, -1.0_DFP]
! xb(:, 2) = [1.0_DFP, -1.0_DFP, -1.0_DFP]
! xb(:, 3) = [-1.0_DFP, 1.0_DFP, -1.0_DFP]
! xb(:, 4) = [-1.0_DFP, -1.0_DFP, 1.0_DFP]
! xb(:, 5) = -0.5_DFP
!
! ! ans can be larger, only ans(1:nrow, 1:ncol) is written
! CALL BarycentricCoordBiUnitTetrahedron_(xb, ans, nrow, ncol)
! CALL OK(nrow .EQ. 4_I4B, "nrow is 4")
! CALL OK(ncol .EQ. 5_I4B, "ncol is SIZE(xin, 2)")
! CALL OK(ALL(ABS(ans(1:nrow, 1:ncol) - &
!                 BarycentricCoordBiUnitTetrahedron(xb)) .LT. tol), &
!         "same as BarycentricCoordBiUnitTetrahedron")
!
! END PROGRAM main
!```

INTERFACE
  MODULE PURE SUBROUTINE BarycentricCoordBiUnitTetrahedron_(xin, &
                                                            ans, nrow, ncol)
    REAL(DFP), INTENT(IN) :: xin(:, :)
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! ans(4, SIZE(xin, 2))
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  END SUBROUTINE BarycentricCoordBiUnitTetrahedron_
END INTERFACE

!----------------------------------------------------------------------------
!                                                BarycentricCoordTetrahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-07-28
! summary: Barycentric coordinates in a reference tetrahedron.
!
! Description:
!   Returns the barycentric coordinates of the points `xin` with
!   respect to the reference tetrahedron `refTetrahedron`. Only the
!   first letter of `refTetrahedron` is used, and the case does not
!   matter:
!
!   - "UNIT": same as `BarycentricCoordUnitTetrahedron`,
!   - "BIUNIT": same as `BarycentricCoordBiUnitTetrahedron`.
!
!   For any other value the result is not defined.
!
! Arguments:
!   xin : REAL(DFP), rank(2) - points, `SHAPE(xin) = [3, N]`
!   refTetrahedron : CHARACTER(*) - "UNIT" or "BIUNIT"
!
! Returns:
!   ans : REAL(DFP), rank(2) - barycentric coordinates,
!     `SHAPE(ans) = [4, N]`
!
! Formula:
!   $$\sum_{k=1}^{4} \lambda_k = 1, \quad
!     \mathbf{x} = \sum_{k=1}^{4} \lambda_k \mathbf{v}_k$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE MappingUtility, ONLY: BarycentricCoordTetrahedron
! USE MappingUtility, ONLY: BarycentricCoordUnitTetrahedron
! USE MappingUtility, ONLY: BarycentricCoordBiUnitTetrahedron
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
! REAL(DFP) :: xu(3, 2), xb(3, 2), ans(4, 2)
!
! xu(:, 1) = [0.1_DFP, 0.2_DFP, 0.3_DFP]
! xu(:, 2) = 0.25_DFP
! xb(:, 1) = [-0.8_DFP, -0.6_DFP, -0.4_DFP]
! xb(:, 2) = -0.5_DFP
!
! ans = BarycentricCoordTetrahedron(xu, "UNIT")
! CALL OK(ALL(ABS(ans - BarycentricCoordUnitTetrahedron(xu)) .LT. tol), &
!         "UNIT")
! CALL OK(ALL(ABS(ans(:, 1) - [0.4_DFP, 0.1_DFP, 0.2_DFP, 0.3_DFP]) &
!             .LT. tol), "UNIT, point (0.1, 0.2, 0.3)")
!
! ans = BarycentricCoordTetrahedron(xb, "BIUNIT")
! CALL OK(ALL(ABS(ans - BarycentricCoordBiUnitTetrahedron(xb)) .LT. tol), &
!         "BIUNIT")
!
! ! only the first letter is used, lower case is accepted
! ans = BarycentricCoordTetrahedron(xb, "biunit")
! CALL OK(ALL(ABS(ans(:, 2) - 0.25_DFP) .LT. tol), "lower case")
!
! END PROGRAM main
!```

INTERFACE
  MODULE PURE FUNCTION BarycentricCoordTetrahedron(xin, refTetrahedron) &
    RESULT(ans)
    REAL(DFP), INTENT(IN) :: xin(:, :)
    REAL(DFP) :: ans(4, SIZE(xin, 2))
    CHARACTER(*), INTENT(IN) :: refTetrahedron
    !! "UNIT"
    !! "BIUNIT"
  END FUNCTION BarycentricCoordTetrahedron
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-06-30
! summary: Barycentric coordinates in a tetrahedron, no allocation.
!
! Description:
!   Same as `BarycentricCoordTetrahedron`, but the result is written
!   to `ans(1:nrow, 1:ncol)`, where `nrow = 4` and
!   `ncol = SIZE(xin, 2)`. For a value of `refTetrahedron` other than
!   "UNIT" or "BIUNIT", `ans`, `nrow` and `ncol` are not set.
!
! Arguments:
!   xin : REAL(DFP), rank(2) - points, `SHAPE(xin) = [3, N]`
!   refTetrahedron : CHARACTER(*) - "UNIT" or "BIUNIT"
!   ans : REAL(DFP), rank(2) - output,
!     `SIZE(ans, 1) >= 4`, `SIZE(ans, 2) >= N`
!   nrow : INTEGER(I4B) - number of rows written, 4
!   ncol : INTEGER(I4B) - number of columns written, `SIZE(xin, 2)`
!
! Formula:
!   $$\sum_{k=1}^{4} \lambda_k = 1, \quad
!     \mathbf{x} = \sum_{k=1}^{4} \lambda_k \mathbf{v}_k$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE MappingUtility, ONLY: BarycentricCoordTetrahedron_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
! REAL(DFP) :: xin(3, 2), ans(4, 3)
! INTEGER(I4B) :: nrow, ncol
!
! xin(:, 1) = [0.1_DFP, 0.2_DFP, 0.3_DFP]
! xin(:, 2) = 0.25_DFP
!
! CALL BarycentricCoordTetrahedron_(xin, "UNIT", ans, nrow, ncol)
! CALL OK(nrow .EQ. 4_I4B, "nrow is 4")
! CALL OK(ncol .EQ. 2_I4B, "ncol is SIZE(xin, 2)")
! CALL OK(ALL(ABS(ans(1:4, 1) - [0.4_DFP, 0.1_DFP, 0.2_DFP, 0.3_DFP]) &
!             .LT. tol), "point (0.1, 0.2, 0.3)")
! CALL OK(ALL(ABS(ans(1:4, 2) - 0.25_DFP) .LT. tol), "centroid")
!
! END PROGRAM main
!```

INTERFACE
  MODULE PURE SUBROUTINE BarycentricCoordTetrahedron_(xin, refTetrahedron, &
                                                      ans, nrow, ncol)
    REAL(DFP), INTENT(IN) :: xin(:, :)
    CHARACTER(*), INTENT(IN) :: refTetrahedron
    !! "UNIT" "BIUNIT"
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! ans(4, SIZE(xin, 2))
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  END SUBROUTINE BarycentricCoordTetrahedron_
END INTERFACE

!----------------------------------------------------------------------------
!                                     FromBiUnitTetrahedron2BiUnitHexahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-07-27
! summary: Maps points from the biunit tetrahedron to the biunit cube.
!
! Description:
!   Maps the points `xin` of the biunit tetrahedron to the biunit
!   hexahedron [-1,1]^3 by the collapsed coordinates. It is the
!   inverse of `FromBiUnitHexahedron2BiUnitTetrahedron` inside the
!   cube.
!
!   - Vertices 1 and 2 of the tetrahedron are not changed.
!   - Where `y + z = 0` (the edge from vertex 3 to vertex 4), the
!     first coordinate is set to -1.
!   - Where `z = 1` (vertex 4), the second coordinate is set to -1.
!   - A value is treated as zero when its absolute value is at most
!     1.0E-12.
!
! Arguments:
!   xin : REAL(DFP), rank(2) - points in the biunit tetrahedron,
!     `SHAPE(xin) = [3, N]`
!
! Returns:
!   ans : REAL(DFP), rank(2) - points in the biunit hexahedron,
!     `SHAPE(ans) = [3, N]`
!
! Formula:
!   $$\xi = -\frac{2(1 + x)}{y + z} - 1, \quad
!     \eta = \frac{2(1 + y)}{1 - z} - 1, \quad \zeta = z$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE MappingUtility, ONLY: FromBiUnitTetrahedron2BiUnitHexahedron
! USE MappingUtility, ONLY: FromBiUnitHexahedron2BiUnitTetrahedron
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
! REAL(DFP), PARAMETER :: third = 1.0_DFP / 3.0_DFP
! REAL(DFP) :: xb(3, 5), ans(3, 5), xh(3, 1)
!
! ! vertices and centroid of the biunit tetrahedron
! xb(:, 1) = [-1.0_DFP, -1.0_DFP, -1.0_DFP]
! xb(:, 2) = [1.0_DFP, -1.0_DFP, -1.0_DFP]
! xb(:, 3) = [-1.0_DFP, 1.0_DFP, -1.0_DFP]
! xb(:, 4) = [-1.0_DFP, -1.0_DFP, 1.0_DFP]
! xb(:, 5) = -0.5_DFP
!
! ans = FromBiUnitTetrahedron2BiUnitHexahedron(xb)
! ! vertices 1 and 2 are not changed
! CALL OK(ALL(ABS(ans(:, 1:2) - xb(:, 1:2)) .LT. tol), "vertices 1 and 2")
! ! at the collapsed points, ans(1) = -1 (and ans(2) = -1 at vertex 4)
! CALL OK(ALL(ABS(ans(:, 3) - xb(:, 3)) .LT. tol), "vertex 3")
! CALL OK(ALL(ABS(ans(:, 4) - xb(:, 4)) .LT. tol), "vertex 4")
! CALL OK(ALL(ABS(ans(:, 5) - [0.0_DFP, -third, -0.5_DFP]) .LT. tol), &
!         "centroid")
!
! ! inverse of FromBiUnitHexahedron2BiUnitTetrahedron, inside the cube
! xh(:, 1) = [0.2_DFP, -0.4_DFP, 0.3_DFP]
! ans(:, 1:1) = FromBiUnitTetrahedron2BiUnitHexahedron( &
!               FromBiUnitHexahedron2BiUnitTetrahedron(xh))
! CALL OK(ALL(ABS(ans(:, 1:1) - xh) .LT. tol), "round trip")
!
! END PROGRAM main
!```

INTERFACE
  MODULE PURE FUNCTION FromBiUnitTetrahedron2BiUnitHexahedron(xin) RESULT(ans)
    REAL(DFP), INTENT(IN) :: xin(:, :)
    !! coordinates in biunit tetrahedron in xij format
    REAL(DFP) :: ans(3, SIZE(xin, 2))
    !! mapped coordinates of xin in biunit hexahedron
  END FUNCTION FromBiUnitTetrahedron2BiUnitHexahedron
END INTERFACE

!----------------------------------------------------------------------------
!                                      FromBiUnitHexahedron2BiUnitTetrahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-07-27
! summary: Maps points from the biunit cube to the biunit tetrahedron.
!
! Description:
!   Maps the points `xin` of the biunit hexahedron [-1,1]^3 to the
!   biunit tetrahedron by the collapsed coordinates.
!
!   - Cube vertices 1 and 2 go to tetrahedron vertices 1 and 2.
!   - Cube vertices 3 and 4 both go to tetrahedron vertex 3.
!   - The top face of the cube (vertices 5 to 8) goes to vertex 4.
!
! Arguments:
!   xin : REAL(DFP), rank(2) - points in the biunit hexahedron,
!     `SHAPE(xin) = [3, N]`
!
! Returns:
!   ans : REAL(DFP), rank(2) - points in the biunit tetrahedron,
!     `SHAPE(ans) = [3, N]`
!
! Formula:
!   $$x = \frac{(1 + \xi)(1 - \eta)(1 - \zeta)}{4} - 1, \quad
!     y = \frac{(1 + \eta)(1 - \zeta)}{2} - 1, \quad z = \zeta$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE MappingUtility, ONLY: FromBiUnitHexahedron2BiUnitTetrahedron
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
! REAL(DFP), PARAMETER :: third = 1.0_DFP / 3.0_DFP
! REAL(DFP) :: xh(3, 9), xb(3, 4), ans(3, 9)
! INTEGER(I4B) :: ii
!
! ! vertices of the biunit hexahedron
! xh(:, 1) = [-1.0_DFP, -1.0_DFP, -1.0_DFP]
! xh(:, 2) = [1.0_DFP, -1.0_DFP, -1.0_DFP]
! xh(:, 3) = [1.0_DFP, 1.0_DFP, -1.0_DFP]
! xh(:, 4) = [-1.0_DFP, 1.0_DFP, -1.0_DFP]
! xh(:, 5:8) = xh(:, 1:4)
! xh(3, 5:8) = 1.0_DFP
! ! image of the centroid of the biunit tetrahedron
! xh(:, 9) = [0.0_DFP, -third, -0.5_DFP]
!
! ! vertices of the biunit tetrahedron
! xb(:, 1) = [-1.0_DFP, -1.0_DFP, -1.0_DFP]
! xb(:, 2) = [1.0_DFP, -1.0_DFP, -1.0_DFP]
! xb(:, 3) = [-1.0_DFP, 1.0_DFP, -1.0_DFP]
! xb(:, 4) = [-1.0_DFP, -1.0_DFP, 1.0_DFP]
!
! ans = FromBiUnitHexahedron2BiUnitTetrahedron(xh)
! CALL OK(ALL(ABS(ans(:, 1:2) - xb(:, 1:2)) .LT. tol), "vertices 1 and 2")
! ! the edge 3-4 of the cube collapses to vertex 3
! CALL OK(ALL(ABS(ans(:, 3) - xb(:, 3)) .LT. tol), "vertex 3 of cube")
! CALL OK(ALL(ABS(ans(:, 4) - xb(:, 3)) .LT. tol), "vertex 4 of cube")
! ! the top face of the cube collapses to vertex 4
! DO ii = 5_I4B, 8_I4B
!   CALL OK(ALL(ABS(ans(:, ii) - xb(:, 4)) .LT. tol), "top face of cube")
! END DO
! CALL OK(ALL(ABS(ans(:, 9) + 0.5_DFP) .LT. tol), "centroid")
!
! END PROGRAM main
!```

INTERFACE
  MODULE PURE FUNCTION FromBiUnitHexahedron2BiUnitTetrahedron(xin) RESULT(ans)
    REAL(DFP), INTENT(IN) :: xin(:, :)
    !! coordinates in bi-unit hexahedron in xij coordinate
    REAL(DFP) :: ans(3, SIZE(xin, 2))
    !! coordinates in biunit tetrahedron
  END FUNCTION FromBiUnitHexahedron2BiUnitTetrahedron
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-07-09
! summary: Biunit cube to biunit tetrahedron without allocation.
!
! Description:
!   Same as `FromBiUnitHexahedron2BiUnitTetrahedron`, but the result
!   is written to `ans(1:nrow, 1:ncol)`, where `nrow = 3` and
!   `ncol = SIZE(xin, 2)`.
!
! Arguments:
!   xin : REAL(DFP), rank(2) - points in the biunit hexahedron,
!     `SHAPE(xin) = [3, N]`
!   ans : REAL(DFP), rank(2) - output,
!     `SIZE(ans, 1) >= 3`, `SIZE(ans, 2) >= N`
!   nrow : INTEGER(I4B) - number of rows written, 3
!   ncol : INTEGER(I4B) - number of columns written, `SIZE(xin, 2)`
!
! Formula:
!   $$x = \frac{(1 + \xi)(1 - \eta)(1 - \zeta)}{4} - 1, \quad
!     y = \frac{(1 + \eta)(1 - \zeta)}{2} - 1, \quad z = \zeta$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE MappingUtility, ONLY: FromBiUnitHexahedron2BiUnitTetrahedron_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
! REAL(DFP), PARAMETER :: third = 1.0_DFP / 3.0_DFP
! REAL(DFP) :: xh(3, 2), ans(4, 4)
! INTEGER(I4B) :: nrow, ncol
!
! xh(:, 1) = [0.0_DFP, -third, -0.5_DFP]
! xh(:, 2) = [1.0_DFP, -1.0_DFP, -1.0_DFP]
!
! CALL FromBiUnitHexahedron2BiUnitTetrahedron_(xh, ans, nrow, ncol)
! CALL OK(nrow .EQ. 3_I4B, "nrow is 3")
! CALL OK(ncol .EQ. 2_I4B, "ncol is SIZE(xin, 2)")
! CALL OK(ALL(ABS(ans(1:3, 1) + 0.5_DFP) .LT. tol), "centroid")
! CALL OK(ALL(ABS(ans(1:3, 2) - xh(:, 2)) .LT. tol), "vertex 2")
!
! END PROGRAM main
!```

INTERFACE
  MODULE PURE SUBROUTINE FromBiUnitHexahedron2BiUnitTetrahedron_(xin, ans, &
                                                                 nrow, ncol)
    REAL(DFP), INTENT(IN) :: xin(:, :)
    !! coordinates in bi-unit hexahedron in xij coordinate
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    ! REAL(DFP) :: ans(3, SIZE(xin, 2))
    !! coordinates in biunit tetrahedron
  END SUBROUTINE FromBiUnitHexahedron2BiUnitTetrahedron_
END INTERFACE

!----------------------------------------------------------------------------
!                                      FromUnitTetrahedron2BiUnitHexahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-07-27
! summary: Maps points from the unit tetrahedron to the biunit cube.
!
! Description:
!   Maps the points `xin` of the unit tetrahedron to the biunit
!   hexahedron [-1,1]^3. It first maps the points to the biunit
!   tetrahedron (`FromUnitTetrahedron2BiUnitTetrahedron`) and then
!   to the cube (`FromBiUnitTetrahedron2BiUnitHexahedron`).
!
! Arguments:
!   xin : REAL(DFP), rank(2) - points in the unit tetrahedron,
!     `SHAPE(xin) = [3, N]`
!
! Returns:
!   ans : REAL(DFP), rank(2) - points in the biunit hexahedron,
!     `SHAPE(ans) = [3, N]`
!
! Formula:
!   $$\mathbf{x}_b = 2\mathbf{x} - 1, \quad
!     \xi = -\frac{2(1 + x_b)}{y_b + z_b} - 1, \quad
!     \eta = \frac{2(1 + y_b)}{1 - z_b} - 1, \quad \zeta = z_b$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE MappingUtility, ONLY: FromUnitTetrahedron2BiUnitHexahedron
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
! REAL(DFP), PARAMETER :: third = 1.0_DFP / 3.0_DFP
! REAL(DFP) :: xu(3, 5), ans(3, 5), expected(3, 5)
!
! ! vertices and centroid of the unit tetrahedron
! xu(:, 1) = [0.0_DFP, 0.0_DFP, 0.0_DFP]
! xu(:, 2) = [1.0_DFP, 0.0_DFP, 0.0_DFP]
! xu(:, 3) = [0.0_DFP, 1.0_DFP, 0.0_DFP]
! xu(:, 4) = [0.0_DFP, 0.0_DFP, 1.0_DFP]
! xu(:, 5) = 0.25_DFP
!
! expected(:, 1) = [-1.0_DFP, -1.0_DFP, -1.0_DFP]
! expected(:, 2) = [1.0_DFP, -1.0_DFP, -1.0_DFP]
! expected(:, 3) = [-1.0_DFP, 1.0_DFP, -1.0_DFP]
! expected(:, 4) = [-1.0_DFP, -1.0_DFP, 1.0_DFP]
! expected(:, 5) = [0.0_DFP, -third, -0.5_DFP]
!
! ans = FromUnitTetrahedron2BiUnitHexahedron(xu)
! CALL OK(ALL(ABS(ans - expected) .LT. tol), "vertices and centroid")
!
! END PROGRAM main
!```

INTERFACE
  MODULE PURE FUNCTION FromUnitTetrahedron2BiUnitHexahedron(xin) RESULT(ans)
    REAL(DFP), INTENT(IN) :: xin(:, :)
    !! coordinates in unit tetrahedron in xij format
    REAL(DFP) :: ans(3, SIZE(xin, 2))
    !! mapped coordinates of xin in biunit hexahedron
  END FUNCTION FromUnitTetrahedron2BiUnitHexahedron
END INTERFACE

!----------------------------------------------------------------------------
!                                      FromBiUnitHexahedron2BiUnitTetrahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-07-27
! summary: Maps points from the biunit cube to the unit tetrahedron.
!
! Description:
!   Maps the points `xin` of the biunit hexahedron [-1,1]^3 to the
!   unit tetrahedron. It first maps the points to the biunit
!   tetrahedron (`FromBiUnitHexahedron2BiUnitTetrahedron`) and then
!   to the unit tetrahedron.
!
! Arguments:
!   xin : REAL(DFP), rank(2) - points in the biunit hexahedron,
!     `SHAPE(xin) = [3, N]`
!
! Returns:
!   ans : REAL(DFP), rank(2) - points in the unit tetrahedron,
!     `SHAPE(ans) = [3, N]`
!
! Formula:
!   $$x = \frac{(1 + \xi)(1 - \eta)(1 - \zeta)}{8}, \quad
!     y = \frac{(1 + \eta)(1 - \zeta)}{4}, \quad
!     z = \frac{1 + \zeta}{2}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE MappingUtility, ONLY: FromBiUnitHexahedron2UnitTetrahedron
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
! REAL(DFP), PARAMETER :: third = 1.0_DFP / 3.0_DFP
! REAL(DFP) :: xh(3, 9), ans(3, 9)
!
! ! vertices of the biunit hexahedron
! xh(:, 1) = [-1.0_DFP, -1.0_DFP, -1.0_DFP]
! xh(:, 2) = [1.0_DFP, -1.0_DFP, -1.0_DFP]
! xh(:, 3) = [1.0_DFP, 1.0_DFP, -1.0_DFP]
! xh(:, 4) = [-1.0_DFP, 1.0_DFP, -1.0_DFP]
! xh(:, 5:8) = xh(:, 1:4)
! xh(3, 5:8) = 1.0_DFP
! ! image of the centroid of the biunit tetrahedron
! xh(:, 9) = [0.0_DFP, -third, -0.5_DFP]
!
! ans = FromBiUnitHexahedron2UnitTetrahedron(xh)
! CALL OK(ALL(ABS(ans(:, 1)) .LT. tol), "vertex 1 goes to (0,0,0)")
! CALL OK(ALL(ABS(ans(:, 2) - [1.0_DFP, 0.0_DFP, 0.0_DFP]) .LT. tol), &
!         "vertex 2 goes to (1,0,0)")
! CALL OK(ALL(ABS(ans(:, 3) - [0.0_DFP, 1.0_DFP, 0.0_DFP]) .LT. tol), &
!         "vertex 3 goes to (0,1,0)")
! CALL OK(ALL(ABS(ans(:, 7) - [0.0_DFP, 0.0_DFP, 1.0_DFP]) .LT. tol), &
!         "vertex 7 goes to (0,0,1)")
! CALL OK(ALL(ABS(ans(:, 9) - 0.25_DFP) .LT. tol), "centroid")
!
! END PROGRAM main
!```

INTERFACE
  MODULE PURE FUNCTION FromBiUnitHexahedron2UnitTetrahedron(xin) RESULT(ans)
    REAL(DFP), INTENT(IN) :: xin(:, :)
    !! coordinates in biunit hexahedron in xij coordinate
    REAL(DFP) :: ans(3, SIZE(xin, 2))
    !! coordinates in unit tetrahedron
  END FUNCTION FromBiUnitHexahedron2UnitTetrahedron
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-07-09
! summary: Biunit cube to unit tetrahedron without allocation.
!
! Description:
!   Same as `FromBiUnitHexahedron2UnitTetrahedron`, but the result is
!   written to `ans`.
!
!   `ans` should have at least 3 rows and `SIZE(xin, 2)` columns.
!   Only `ans(1:3, 1:SIZE(xin, 2))` is written.
!
! Arguments:
!   xin : REAL(DFP), rank(2) - points in the biunit hexahedron,
!     `SHAPE(xin) = [3, N]`
!   ans : REAL(DFP), rank(2) - output,
!     `SIZE(ans, 1) >= 3`, `SIZE(ans, 2) >= N`
!   nrow : INTEGER(I4B) - number of rows written, 3
!   ncol : INTEGER(I4B) - number of columns written, `SIZE(xin, 2)`
!
! Formula:
!   $$x = \frac{(1 + \xi)(1 - \eta)(1 - \zeta)}{8}, \quad
!     y = \frac{(1 + \eta)(1 - \zeta)}{4}, \quad
!     z = \frac{1 + \zeta}{2}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE MappingUtility, ONLY: FromBiUnitHexahedron2UnitTetrahedron_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
! REAL(DFP), PARAMETER :: third = 1.0_DFP / 3.0_DFP
! REAL(DFP) :: xh(3, 2), ans(3, 2)
! INTEGER(I4B) :: nrow, ncol
!
! xh(:, 1) = [0.0_DFP, -third, -0.5_DFP]
! xh(:, 2) = [1.0_DFP, -1.0_DFP, -1.0_DFP]
!
! ! ans must have exactly 3 rows and SIZE(xin, 2) columns
! CALL FromBiUnitHexahedron2UnitTetrahedron_(xh, ans, nrow, ncol)
! CALL OK(nrow .EQ. 3_I4B, "nrow is 3")
! CALL OK(ncol .EQ. 2_I4B, "ncol is SIZE(xin, 2)")
! CALL OK(ALL(ABS(ans(1:3, 1) - 0.25_DFP) .LT. tol), "centroid")
! CALL OK(ALL(ABS(ans(1:3, 2) - [1.0_DFP, 0.0_DFP, 0.0_DFP]) .LT. tol), &
!         "vertex 2")
!
! END PROGRAM main
!```

INTERFACE

  MODULE PURE SUBROUTINE FromBiUnitHexahedron2UnitTetrahedron_(xin, ans, &
                                                               nrow, ncol)
    REAL(DFP), INTENT(IN) :: xin(:, :)
    !! coordinates in biunit hexahedron in xij coordinate
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !!
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! nrow = 3
    !! ncol = SIZE(xin, 2)
    !! coordinates in unit tetrahedron
  END SUBROUTINE FromBiUnitHexahedron2UnitTetrahedron_
END INTERFACE

!----------------------------------------------------------------------------
!                                                             JacobianLine
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-08-07
! summary: Jacobian of the map between two lines.
!
! Description:
!   Returns the Jacobian of the affine map from the line `from` to the
!   line `to`, that is the length of `to` divided by the length of
!   `from`. The values of `from` and `to` are (upper case only,
!   trailing blanks are ignored):
!
!   - "BIUNIT": the biunit line [-1, 1], length 2,
!   - "UNIT": the unit line [0, 1], length 1,
!   - "LINE": the segment from `xij(:, 1)` to `xij(:, 2)`.
!
!   - `xij` must be present when `from` or `to` is "LINE"; this is not
!     checked.
!   - `from` and `to` should not both be "LINE" (the result is 1).
!   - For any other value the result is not defined.
!
! Arguments:
!   from : CHARACTER(*) - "BIUNIT", "UNIT" or "LINE"
!   to : CHARACTER(*) - "BIUNIT", "UNIT" or "LINE"
!   xij : REAL(DFP), rank(2), OPTIONAL - end points of the segment,
!     `SHAPE(xij) = [nsd, 2]`
!
! Returns:
!   ans : REAL(DFP) - Jacobian of the map
!
! Formula:
!   $$J = \frac{\mathrm{length}(\mathrm{to})}
!     {\mathrm{length}(\mathrm{from})}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE MappingUtility, ONLY: JacobianLine
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
! REAL(DFP) :: xij(2, 2)
!
! ! between the reference lines
! CALL OK(ABS(JacobianLine("UNIT", "BIUNIT") - 2.0_DFP) .LT. tol, &
!         "UNIT to BIUNIT")
! CALL OK(ABS(JacobianLine("BIUNIT", "UNIT") - 0.5_DFP) .LT. tol, &
!         "BIUNIT to UNIT")
! CALL OK(ABS(JacobianLine("BIUNIT", "BIUNIT") - 1.0_DFP) .LT. tol, &
!         "BIUNIT to BIUNIT")
! CALL OK(ABS(JacobianLine("UNIT", "UNIT") - 1.0_DFP) .LT. tol, &
!         "UNIT to UNIT")
!
! ! a segment from (0, 0) to (3, 4), its length is 5
! xij(:, 1) = [0.0_DFP, 0.0_DFP]
! xij(:, 2) = [3.0_DFP, 4.0_DFP]
! CALL OK(ABS(JacobianLine("BIUNIT", "LINE", xij) - 2.5_DFP) .LT. tol, &
!         "BIUNIT to LINE")
! CALL OK(ABS(JacobianLine("UNIT", "LINE", xij) - 5.0_DFP) .LT. tol, &
!         "UNIT to LINE")
! CALL OK(ABS(JacobianLine("LINE", "BIUNIT", xij) - 0.4_DFP) .LT. tol, &
!         "LINE to BIUNIT")
! CALL OK(ABS(JacobianLine("LINE", "UNIT", xij) - 0.2_DFP) .LT. tol, &
!         "LINE to UNIT")
!
! END PROGRAM main
!```

INTERFACE
  MODULE PURE FUNCTION JacobianLine(from, to, xij) RESULT(ans)
    CHARACTER(*), INTENT(IN) :: from
    !! BIUNIT
    !! UNIT
    !! LINE
    CHARACTER(*), INTENT(IN) :: to
    !! BIUNIT
    !! UNIT
    !! LINE
    REAL(DFP), OPTIONAL, INTENT(IN) :: xij(:, :)
    !! nodal coordinates of general line (segment)
    !! number of rows=nsd
    !! number of cols=2
    !! xij is needed when from or to are LINE
    !! both from and to cannot be LINE
    REAL(DFP) :: ans
  END FUNCTION JacobianLine
END INTERFACE

!----------------------------------------------------------------------------
!                                                         JacobianTriangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-08-07
! summary: Jacobian of the map between two triangles.
!
! Description:
!   Returns the Jacobian of the affine map from the triangle `from` to
!   the triangle `to`, that is the area of `to` divided by the area of
!   `from`. The values of `from` and `to` are (upper case only,
!   trailing blanks are ignored):
!
!   - "BIUNIT": the biunit triangle, area 2,
!   - "UNIT": the unit triangle, area 1/2,
!   - "TRIANGLE": the triangle with vertices `xij(:, 1:3)`, in 2D or
!     3D.
!
!   - `xij` is needed when `from` or `to` is "TRIANGLE". If it is
!     absent, the result is 1.
!   - In 2D the area is signed, so the result is negative when the
!     vertices of `xij` are in clockwise order.
!   - If `from` and `to` are the same, the result is 1.
!   - For any other value the result is not meaningful.
!
! Arguments:
!   from : CHARACTER(*) - "BIUNIT", "UNIT" or "TRIANGLE"
!   to : CHARACTER(*) - "BIUNIT", "UNIT" or "TRIANGLE"
!   xij : REAL(DFP), rank(2), OPTIONAL - vertices of the triangle,
!     `SHAPE(xij) = [nsd, 3]`, `nsd` is 2 or 3
!
! Returns:
!   ans : REAL(DFP) - Jacobian of the map
!
! Formula:
!   $$J = \frac{\mathrm{area}(\mathrm{to})}{\mathrm{area}(\mathrm{from})}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE MappingUtility, ONLY: JacobianTriangle
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
! REAL(DFP) :: xij(2, 3), xij3(3, 3)
!
! ! between the reference triangles
! CALL OK(ABS(JacobianTriangle("UNIT", "BIUNIT") - 4.0_DFP) .LT. tol, &
!         "UNIT to BIUNIT")
! CALL OK(ABS(JacobianTriangle("BIUNIT", "UNIT") - 0.25_DFP) .LT. tol, &
!         "BIUNIT to UNIT")
! CALL OK(ABS(JacobianTriangle("UNIT", "UNIT") - 1.0_DFP) .LT. tol, &
!         "UNIT to UNIT")
!
! ! a triangle with vertices (0,0), (2,0), (0,3), its area is 3
! xij(:, 1) = [0.0_DFP, 0.0_DFP]
! xij(:, 2) = [2.0_DFP, 0.0_DFP]
! xij(:, 3) = [0.0_DFP, 3.0_DFP]
! CALL OK(ABS(JacobianTriangle("UNIT", "TRIANGLE", xij) - 6.0_DFP) &
!         .LT. tol, "UNIT to TRIANGLE")
! CALL OK(ABS(JacobianTriangle("BIUNIT", "TRIANGLE", xij) - 1.5_DFP) &
!         .LT. tol, "BIUNIT to TRIANGLE")
! CALL OK(ABS(JacobianTriangle("TRIANGLE", "UNIT", xij) - 1.0_DFP / 6.0_DFP) &
!         .LT. tol, "TRIANGLE to UNIT")
! CALL OK(ABS(JacobianTriangle("TRIANGLE", "BIUNIT", xij) - &
!             2.0_DFP / 3.0_DFP) .LT. tol, "TRIANGLE to BIUNIT")
!
! ! the same triangle in the plane z = 1
! xij3(1:2, :) = xij
! xij3(3, :) = 1.0_DFP
! CALL OK(ABS(JacobianTriangle("UNIT", "TRIANGLE", xij3) - 6.0_DFP) &
!         .LT. tol, "UNIT to TRIANGLE in 3D")
!
! ! a map from a triangle to itself has Jacobian 1
! CALL OK(ABS(JacobianTriangle("TRIANGLE", "TRIANGLE", xij) - 1.0_DFP) &
!         .LT. tol, "TRIANGLE to TRIANGLE")
! CALL OK(ABS(JacobianTriangle("TRIANGLE", "TRIANGLE", xij3) - 1.0_DFP) &
!         .LT. tol, "TRIANGLE to TRIANGLE in 3D")
!
! END PROGRAM main
!```

INTERFACE
  MODULE PURE FUNCTION JacobianTriangle(from, to, xij) RESULT(ans)
    CHARACTER(*), INTENT(IN) :: from
    !! BIUNIT
    !! UNIT
    !! TRIANGLE
    CHARACTER(*), INTENT(IN) :: to
    !! BIUNIT
    !! UNIT
    !! TRIANGLE
    REAL(DFP), OPTIONAL, INTENT(IN) :: xij(:, :)
    !! nodal coordinates of general TRIANGLE
    !! number of rows=nsd
    !! number of cols=3
    !! xij is needed when `from` or `to` is TRIANGLE
    !! `from` and `to` cannot both be TRIANGLE
    REAL(DFP) :: ans
  END FUNCTION JacobianTriangle
END INTERFACE

!----------------------------------------------------------------------------
!                                                       JacobianQuadrangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-08-07
! summary: Jacobian of the map between two quadrangles.
!
! Description:
!   Returns the area of the quadrangle `to` divided by the area of the
!   quadrangle `from`. For a parallelogram this is the Jacobian of the
!   bilinear map; for a general quadrangle it is the mean value of the
!   Jacobian. The values of `from` and `to` are (upper case only,
!   trailing blanks are ignored):
!
!   - "BIUNIT": the biunit quadrangle, area 4,
!   - "UNIT": the unit quadrangle, area 1,
!   - "QUADRANGLE": the quadrangle with vertices `xij(:, 1:4)`, in 2D
!     or 3D.
!
!   - `xij` is needed when `from` or `to` is "QUADRANGLE". If it is
!     absent, the result is 1.
!   - If `from` and `to` are the same, the result is 1.
!   - For any other value the result is not meaningful.
!
! Arguments:
!   from : CHARACTER(*) - "BIUNIT", "UNIT" or "QUADRANGLE"
!   to : CHARACTER(*) - "BIUNIT", "UNIT" or "QUADRANGLE"
!   xij : REAL(DFP), rank(2), OPTIONAL - vertices of the quadrangle,
!     `SHAPE(xij) = [nsd, 4]`, `nsd` is 2 or 3
!
! Returns:
!   ans : REAL(DFP) - Jacobian of the map
!
! Formula:
!   $$J = \frac{\mathrm{area}(\mathrm{to})}{\mathrm{area}(\mathrm{from})}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE MappingUtility, ONLY: JacobianQuadrangle
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
! REAL(DFP) :: xij(2, 4)
!
! ! between the reference quadrangles
! CALL OK(ABS(JacobianQuadrangle("UNIT", "BIUNIT") - 4.0_DFP) .LT. tol, &
!         "UNIT to BIUNIT")
! CALL OK(ABS(JacobianQuadrangle("BIUNIT", "UNIT") - 0.25_DFP) .LT. tol, &
!         "BIUNIT to UNIT")
! CALL OK(ABS(JacobianQuadrangle("BIUNIT", "BIUNIT") - 1.0_DFP) .LT. tol, &
!         "BIUNIT to BIUNIT")
!
! ! the rectangle [0,2] x [0,3], its area is 6
! xij(:, 1) = [0.0_DFP, 0.0_DFP]
! xij(:, 2) = [2.0_DFP, 0.0_DFP]
! xij(:, 3) = [2.0_DFP, 3.0_DFP]
! xij(:, 4) = [0.0_DFP, 3.0_DFP]
! CALL OK(ABS(JacobianQuadrangle("BIUNIT", "QUADRANGLE", xij) - 1.5_DFP) &
!         .LT. tol, "BIUNIT to QUADRANGLE")
! CALL OK(ABS(JacobianQuadrangle("UNIT", "QUADRANGLE", xij) - 6.0_DFP) &
!         .LT. tol, "UNIT to QUADRANGLE")
! CALL OK(ABS(JacobianQuadrangle("QUADRANGLE", "BIUNIT", xij) - &
!             4.0_DFP / 6.0_DFP) .LT. tol, "QUADRANGLE to BIUNIT")
! CALL OK(ABS(JacobianQuadrangle("QUADRANGLE", "UNIT", xij) - &
!             1.0_DFP / 6.0_DFP) .LT. tol, "QUADRANGLE to UNIT")
!
! ! a map from a quadrangle to itself has Jacobian 1
! CALL OK(ABS(JacobianQuadrangle("QUADRANGLE", "QUADRANGLE", xij) &
!             - 1.0_DFP) .LT. tol, "QUADRANGLE to QUADRANGLE")
!
! END PROGRAM main
!```

INTERFACE
  MODULE PURE FUNCTION JacobianQuadrangle(from, to, xij) RESULT(ans)
    CHARACTER(*), INTENT(IN) :: from
    !! BIUNIT
    !! UNIT
    !! QUADRANGLE
    CHARACTER(*), INTENT(IN) :: to
    !! BIUNIT
    !! UNIT
    !! QUADRANGLE
    REAL(DFP), OPTIONAL, INTENT(IN) :: xij(:, :)
    !! nodal coordinates of general QUADRANGLE
    !! number of rows=nsd
    !! number of cols=4
    !! xij is needed when `from` or `to` is QUADRANGLE
    !! `from` and `to` cannot both be QUADRANGLE
    REAL(DFP) :: ans
  END FUNCTION JacobianQuadrangle
END INTERFACE

!----------------------------------------------------------------------------
!                                                       JacobianTetrahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-08-07
! summary: Jacobian of the map between two tetrahedrons.
!
! Description:
!   Returns the Jacobian of the affine map from the tetrahedron `from`
!   to the tetrahedron `to`, that is the volume of `to` divided by the
!   volume of `from`. The values of `from` and `to` are (upper case
!   only, trailing blanks are ignored):
!
!   - "BIUNIT": the biunit tetrahedron, volume 4/3,
!   - "UNIT": the unit tetrahedron, volume 1/6,
!   - "TETRAHEDRON": the tetrahedron with vertices `xij(:, 1:4)`.
!
!   - `xij` is needed when `from` or `to` is "TETRAHEDRON". If it is
!     absent, the result is 1.
!   - `from` and `to` should not both be "TETRAHEDRON".
!   - For any other value the result is 1.
!
! Arguments:
!   from : CHARACTER(*) - "BIUNIT", "UNIT" or "TETRAHEDRON"
!   to : CHARACTER(*) - "BIUNIT", "UNIT" or "TETRAHEDRON"
!   xij : REAL(DFP), rank(2), OPTIONAL - vertices of the tetrahedron,
!     `SHAPE(xij) = [3, 4]`
!
! Returns:
!   ans : REAL(DFP) - Jacobian of the map
!
! Formula:
!   $$J = \frac{\mathrm{vol}(\mathrm{to})}{\mathrm{vol}(\mathrm{from})}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE MappingUtility, ONLY: JacobianTetrahedron
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
! REAL(DFP) :: xij(3, 4)
!
! ! between the reference tetrahedrons
! CALL OK(ABS(JacobianTetrahedron("UNIT", "BIUNIT") - 8.0_DFP) .LT. tol, &
!         "UNIT to BIUNIT")
! CALL OK(ABS(JacobianTetrahedron("BIUNIT", "UNIT") - 0.125_DFP) .LT. tol, &
!         "BIUNIT to UNIT")
! CALL OK(ABS(JacobianTetrahedron("UNIT", "UNIT") - 1.0_DFP) .LT. tol, &
!         "UNIT to UNIT")
!
! ! vertices (0,0,0), (2,0,0), (0,3,0), (0,0,4), the volume is 4
! xij = 0.0_DFP
! xij(1, 2) = 2.0_DFP
! xij(2, 3) = 3.0_DFP
! xij(3, 4) = 4.0_DFP
! CALL OK(ABS(JacobianTetrahedron("UNIT", "TETRAHEDRON", xij) - 24.0_DFP) &
!         .LT. tol, "UNIT to TETRAHEDRON")
! CALL OK(ABS(JacobianTetrahedron("BIUNIT", "TETRAHEDRON", xij) - 3.0_DFP) &
!         .LT. tol, "BIUNIT to TETRAHEDRON")
! CALL OK(ABS(JacobianTetrahedron("TETRAHEDRON", "UNIT", xij) - &
!             1.0_DFP / 24.0_DFP) .LT. tol, "TETRAHEDRON to UNIT")
! CALL OK(ABS(JacobianTetrahedron("TETRAHEDRON", "BIUNIT", xij) - &
!             1.0_DFP / 3.0_DFP) .LT. tol, "TETRAHEDRON to BIUNIT")
!
! END PROGRAM main
!```

INTERFACE
  MODULE PURE FUNCTION JacobianTetrahedron(from, to, xij) RESULT(ans)
    CHARACTER(*), INTENT(IN) :: from
    !! BIUNIT
    !! UNIT
    !! TETRAHEDRON
    CHARACTER(*), INTENT(IN) :: to
    !! BIUNIT
    !! UNIT
    !! TETRAHEDRON
    REAL(DFP), OPTIONAL, INTENT(IN) :: xij(:, :)
    !! nodal coordinates of general TETRAHEDRON
    !! number of rows=nsd
    !! number of cols=4
    !! xij is needed when `from` or `to` is TETRAHEDRON
    !! `from` and `to` cannot both be TETRAHEDRON
    REAL(DFP) :: ans
  END FUNCTION JacobianTetrahedron
END INTERFACE

!----------------------------------------------------------------------------
!                                                         JacobianHexahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-08-07
! summary: Jacobian of the map between two hexahedrons.
!
! Description:
!   Returns the volume of the hexahedron `to` divided by the volume of
!   the hexahedron `from`. For a parallelepiped this is the Jacobian of
!   the trilinear map; for a general hexahedron it is the mean value
!   of the Jacobian. The values of `from` and `to` are (upper case
!   only, trailing blanks are ignored):
!
!   - "BIUNIT": the biunit hexahedron, volume 8,
!   - "UNIT": the unit hexahedron, volume 1,
!   - "HEXAHEDRON": the hexahedron with vertices `xij(:, 1:8)`.
!
!   - `xij` is needed when `from` or `to` is "HEXAHEDRON". If it is
!     absent, the result is 1.
!   - `from` and `to` should not both be "HEXAHEDRON".
!   - For any other value the result is 1.
!
! Arguments:
!   from : CHARACTER(*) - "BIUNIT", "UNIT" or "HEXAHEDRON"
!   to : CHARACTER(*) - "BIUNIT", "UNIT" or "HEXAHEDRON"
!   xij : REAL(DFP), rank(2), OPTIONAL - vertices of the hexahedron,
!     `SHAPE(xij) = [3, 8]`
!
! Returns:
!   ans : REAL(DFP) - Jacobian of the map
!
! Formula:
!   $$J = \frac{\mathrm{vol}(\mathrm{to})}{\mathrm{vol}(\mathrm{from})}$$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE MappingUtility, ONLY: JacobianHexahedron
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
! REAL(DFP) :: xij(3, 8)
!
! ! between the reference hexahedrons
! CALL OK(ABS(JacobianHexahedron("UNIT", "BIUNIT") - 8.0_DFP) .LT. tol, &
!         "UNIT to BIUNIT")
! CALL OK(ABS(JacobianHexahedron("BIUNIT", "UNIT") - 0.125_DFP) .LT. tol, &
!         "BIUNIT to UNIT")
! CALL OK(ABS(JacobianHexahedron("BIUNIT", "BIUNIT") - 1.0_DFP) .LT. tol, &
!         "BIUNIT to BIUNIT")
! CALL OK(ABS(JacobianHexahedron("UNIT", "UNIT") - 1.0_DFP) .LT. tol, &
!         "UNIT to UNIT")
!
! ! the box [0,2] x [0,3] x [0,4], its volume is 24
! xij(:, 1) = [0.0_DFP, 0.0_DFP, 0.0_DFP]
! xij(:, 2) = [2.0_DFP, 0.0_DFP, 0.0_DFP]
! xij(:, 3) = [2.0_DFP, 3.0_DFP, 0.0_DFP]
! xij(:, 4) = [0.0_DFP, 3.0_DFP, 0.0_DFP]
! xij(:, 5:8) = xij(:, 1:4)
! xij(3, 5:8) = 4.0_DFP
! CALL OK(ABS(JacobianHexahedron("BIUNIT", "HEXAHEDRON", xij) - 3.0_DFP) &
!         .LT. tol, "BIUNIT to HEXAHEDRON")
! CALL OK(ABS(JacobianHexahedron("UNIT", "HEXAHEDRON", xij) - 24.0_DFP) &
!         .LT. tol, "UNIT to HEXAHEDRON")
! CALL OK(ABS(JacobianHexahedron("HEXAHEDRON", "BIUNIT", xij) - &
!             1.0_DFP / 3.0_DFP) .LT. tol, "HEXAHEDRON to BIUNIT")
! CALL OK(ABS(JacobianHexahedron("HEXAHEDRON", "UNIT", xij) - &
!             1.0_DFP / 24.0_DFP) .LT. tol, "HEXAHEDRON to UNIT")
!
! END PROGRAM main
!```

INTERFACE
  MODULE PURE FUNCTION JacobianHexahedron(from, to, xij) RESULT(ans)
    CHARACTER(*), INTENT(IN) :: from
    !! BIUNIT
    !! UNIT
    !! HEXAHEDRON
    CHARACTER(*), INTENT(IN) :: to
    !! BIUNIT
    !! UNIT
    !! HEXAHEDRON
    REAL(DFP), OPTIONAL, INTENT(IN) :: xij(:, :)
    !! nodal coordinates of general HEXAHEDRON
    !! number of rows=nsd
    !! number of cols=8
    !! xij is needed when `from` or `to` is HEXAHEDRON
    !! `from` and `to` cannot both be HEXAHEDRON
    REAL(DFP) :: ans
  END FUNCTION JacobianHexahedron
END INTERFACE

!----------------------------------------------------------------------------
!                                                            JacobianPrism
!----------------------------------------------------------------------------

! INTERFACE
!   MODULE PURE FUNCTION JacobianPrism(from, to) RESULT(ans)
!     CHARACTER(*), INTENT(IN) :: from
!     CHARACTER(*), INTENT(IN) :: to
!     REAL(DFP) :: ans
!   END FUNCTION JacobianPrism
! END INTERFACE

!----------------------------------------------------------------------------
!                                                            JacobianPyramid
!----------------------------------------------------------------------------

! INTERFACE
!   MODULE PURE FUNCTION JacobianPyramid(from, to) RESULT(ans)
!     CHARACTER(*), INTENT(IN) :: from
!     CHARACTER(*), INTENT(IN) :: to
!     REAL(DFP) :: ans
!   END FUNCTION JacobianPyramid
! END INTERFACE

END MODULE MappingUtility
