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
! date:  19 Oct 2022
! summary: Some methods related to standard mapping are defined
!
!{!pages/MappingUtility_.md!}

MODULE MappingUtility
USE GlobalData
IMPLICIT NONE
PRIVATE

PUBLIC :: FromBiunitLine2Segment
PUBLIC :: FromBiunitLine2Segment_
PUBLIC :: FromBiUnitLine2UnitLine
PUBLIC :: FromUnitLine2BiUnitLine
PUBLIC :: FromUnitLine2BiUnitLine_
PUBLIC :: FromLine2Line_

PUBLIC :: FromBiUnitQuadrangle2Quadrangle
PUBLIC :: FromBiUnitQuadrangle2Quadrangle_

PUBLIC :: FromBiUnitQuadrangle2UnitQuadrangle
PUBLIC :: FromBiUnitQuadrangle2UnitQuadrangle_
PUBLIC :: FromUnitQuadrangle2BiUnitQuadrangle

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
PUBLIC :: FromUnitTetrahedron2BiUnitHexahedron
PUBLIC :: FromBiUnitHexahedron2UnitTetrahedron

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
! date: 19 Oct 2022
! summary: Map from unit line to physical space

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
!                                                    FromBiunitLine2Segment_
!----------------------------------------------------------------------------

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
!                                                     FromBiunitLine2Segment
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 19 Oct 2022
! summary: Map from unit line to physical space

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
! date: 19 Oct 2022
! summary: from bi unit line to segment wo allocation

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
! date: 19 Oct 2022
! summary: Map from unit line to physical space

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
! date:  2024-06-26
! summary:  from unit triangle to triangle without allocation

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
! date: 19 Oct 2022
! summary: Map from unit line to physical space

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
! date: 19 Oct 2022
! summary: Map from unit line to physical space

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
! date: 19 Oct 2022
! summary: Map from unit line to physical space

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
! date: 19 Oct 2022
! summary: Map from unit line to physical space

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
! date: 19 Oct 2022
! summary: Map from unit line to physical space

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
!                                            FromBiUnitHexahedron2Hexahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 19 Oct 2022
! summary: Map from biunit hexahedron to unit hexahedron

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
!                                            FromBiUnitHexahedron2Hexahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 19 Oct 2022
! summary: Map from unit hexahedron to biunit hexahedron

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
!                                                     FromBiUnitLine2UnitLine
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 19 Oct 2022
! summary: Map from biunit line to unit line
!
!# Introduction
!
!- Bi unit line is defined by -1 to 1.
!- Unit line is defined by 0 to 1

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
! date: 19 Oct 2022
! summary: Map from unit line to biunit line
!
!# Introduction
!
!- Bi unit line is defined by -1 to 1.
!- Unit line is defined by 0 to 1

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
! summary:  from unit line to bi unit line without allocation

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
! date:  2024-04-27
! summary: Map line to line

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
! date: 19 Oct 2022
! summary: Map from biunit triangle to bi-unit square
!
!# Introduction
!
!- Bi unit triangle is defined by (-1,-1), (1,-1), and (-1,1)
!- Bi unit square is defined by (-1,-1), (1,-1), (1,1), and (-1,1)

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
! date: 19 Oct 2022
! summary: Map from biunit triangle to bi-unit square
!
!# Introduction
!
!- Bi unit triangle is defined by (-1,-1), (1,-1), and (-1,1)
!- Bi unit square is defined by (-1,-1), (1,-1), (1,1), and (-1,1)

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
! date: 19 Oct 2022
! summary: Map from biunit triangle to bi-unit square
!
!# Introduction
!
!- Unit triangle is defined by (0,0), (0,1), and (1,0)
!- Biunit square is defined by (-1,-1), (1,-1), (1,1), and (-1,1)

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
! date: 19 Oct 2022
! summary: Map from biunit triangle to bi-unit square
!
!# Introduction
!
!- Unit triangle is defined by (0,0), (0,1), and (1,0)
!- Bi unit square is defined by (-1,-1), (1,-1), (1,1), and (-1,1)

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
! date: 19 Oct 2022
! summary: Map from triangle to square

INTERFACE
 MODULE PURE SUBROUTINE FromTriangle2Triangle_(xin, ans, from, to, x1, x2, x3)
    REAL(DFP), INTENT(IN) :: xin(:, :)
    !! coordinates in bi-unit square in xij coordinate
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! ans(2, SIZE(xin, 2))
    !! coordinates in biunit triangle
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
! date: 19 Oct 2022
! summary: Map from triangle to square

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
! date: 19 Oct 2022
! summary: Map from triangle to square

INTERFACE
  MODULE PURE SUBROUTINE FromSquare2Triangle_(xin, ans, from, to)
    REAL(DFP), INTENT(IN) :: xin(:, :)
    !! coordinates in bi-unit square in xij coordinate
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! ans(2, SIZE(xin, 2))
    !! coordinates in biunit triangle
    CHARACTER(*), INTENT(IN) :: from
    CHARACTER(*), INTENT(IN) :: to
  END SUBROUTINE FromSquare2Triangle_
END INTERFACE

!----------------------------------------------------------------------------
!                                              BarycentricCoordUnitTriangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 Oct 2022
! summary: Returns barycentric coord of unit triangle

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
! date: 28 Oct 2022
! summary: Returns barycentric coord of unit triangle

INTERFACE
  MODULE PURE FUNCTION BarycentricCoordBiUnitTriangle(xin) RESULT(ans)
    REAL(DFP), INTENT(IN) :: xin(:, :)
    REAL(DFP) :: ans(3, SIZE(xin, 2))
  END FUNCTION BarycentricCoordBiUnitTriangle
END INTERFACE

!----------------------------------------------------------------------------
!                                                   BarycentricCoordTriangle
!----------------------------------------------------------------------------

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
! date: 28 Oct 2022
! summary: Returns barycentric coord of unit triangle

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
! date: 28 Oct 2022
! summary: Returns barycentric coord of unit triangle

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
! date: 28 Oct 2022
! summary: Biunit Tetrahedron to Unit tetrahedron

INTERFACE
  MODULE PURE FUNCTION FromBiUnitTetrahedron2UnitTetrahedron(xin) RESULT(ans)
    REAL(DFP), INTENT(IN) :: xin(:, :)
    REAL(DFP) :: ans(3, SIZE(xin, 2))
  END FUNCTION FromBiUnitTetrahedron2UnitTetrahedron
END INTERFACE

!----------------------------------------------------------------------------
!                                      FromUnitTetrahedron2BiUnitTetrahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 Oct 2022
! summary: Unit Tetrahedron to biunit tetrahedron

INTERFACE
  MODULE PURE FUNCTION FromUnitTetrahedron2BiUnitTetrahedron(xin) RESULT(ans)
    REAL(DFP), INTENT(IN) :: xin(:, :)
    REAL(DFP) :: ans(3, SIZE(xin, 2))
  END FUNCTION FromUnitTetrahedron2BiUnitTetrahedron
END INTERFACE

!----------------------------------------------------------------------------
!                                          FromBiUnitTetrahedron2Tetrahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 Oct 2022
! summary: Biunit Tetrahedron to tetrahedron

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
! summary: Unit Tetrahedron to tetrahedron

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
! date:  2024-06-28
! summary:  No allocation

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
! date: 28 Oct 2022
! summary: Returns barycentric coord of unit triangle

INTERFACE
  MODULE PURE FUNCTION BarycentricCoordUnitTetrahedron(xin) RESULT(ans)
    REAL(DFP), INTENT(IN) :: xin(:, :)
    REAL(DFP) :: ans(4, SIZE(xin, 2))
  END FUNCTION BarycentricCoordUnitTetrahedron
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

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
! date: 28 Oct 2022
! summary: Returns barycentric coord of unit triangle

INTERFACE
  MODULE PURE FUNCTION BarycentricCoordBiUnitTetrahedron(xin) RESULT(ans)
    REAL(DFP), INTENT(IN) :: xin(:, :)
    REAL(DFP) :: ans(4, SIZE(xin, 2))
  END FUNCTION BarycentricCoordBiUnitTetrahedron
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

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
!                                                   BarycentricCoordTetrahedron
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION BarycentricCoordTetrahedron(xin, refTetrahedron) RESULT(ans)
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
! summary: Map from biunit tetrahedron to bi-unit Hexahedron

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
! summary: Map from biunit hexahedron to biunit tetrahedron

INTERFACE
  MODULE PURE FUNCTION FromBiUnitHexahedron2BiUnitTetrahedron(xin) RESULT(ans)
    REAL(DFP), INTENT(IN) :: xin(:, :)
    !! coordinates in bi-unit hexahedron in xij coordinate
    REAL(DFP) :: ans(3, SIZE(xin, 2))
    !! coordinates in biunit tetrahedron
  END FUNCTION FromBiUnitHexahedron2BiUnitTetrahedron
END INTERFACE

!----------------------------------------------------------------------------
!                                      FromUnitTetrahedron2BiUnitHexahedron
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-07-27
! summary: Map from unit tetrahedron to bi-unit Hexahedron

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
! summary: Map from unit hexahedron to biunit tetrahedron

INTERFACE
  MODULE PURE FUNCTION FromBiUnitHexahedron2UnitTetrahedron(xin) RESULT(ans)
    REAL(DFP), INTENT(IN) :: xin(:, :)
    !! coordinates in biunit hexahedron in xij coordinate
    REAL(DFP) :: ans(3, SIZE(xin, 2))
    !! coordinates in unit tetrahedron
  END FUNCTION FromBiUnitHexahedron2UnitTetrahedron
END INTERFACE

!----------------------------------------------------------------------------
!                                                             JacobianLine
!----------------------------------------------------------------------------

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
    !! number of rows=1
    !! number of cols=2
    !! xij is needed when from or to are LINE
    !! both from and to cannot be LINE
    REAL(DFP) :: ans
  END FUNCTION JacobianLine
END INTERFACE

!----------------------------------------------------------------------------
!                                                         JacobianTriangle
!----------------------------------------------------------------------------

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
    !! both `from` and to `cannot` be TRIANGLE
    REAL(DFP) :: ans
  END FUNCTION JacobianTriangle
END INTERFACE

!----------------------------------------------------------------------------
!                                                       JacobianQuadrangle
!----------------------------------------------------------------------------

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
    !! both `from` and to `cannot` be QUADRANGLE
    REAL(DFP) :: ans
  END FUNCTION JacobianQuadrangle
END INTERFACE

!----------------------------------------------------------------------------
!                                                       JacobianTetrahedron
!----------------------------------------------------------------------------

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
    !! both `from` and to `cannot` be TETRAHEDRON
    REAL(DFP) :: ans
  END FUNCTION JacobianTetrahedron
END INTERFACE

!----------------------------------------------------------------------------
!                                                         JacobianHexahedron
!----------------------------------------------------------------------------

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
    !! number of cols=4
    !! xij is needed when `from` or `to` is HEXAHEDRON
    !! both `from` and to `cannot` be HEXAHEDRON
    REAL(DFP) :: ans
  END FUNCTION JacobianHexahedron
END INTERFACE

! !----------------------------------------------------------------------------
! !                                                            JacobianPrism
! !----------------------------------------------------------------------------
!
! INTERFACE
!   MODULE PURE FUNCTION JacobianPrism(from, to) RESULT(ans)
!     CHARACTER(*), INTENT(IN) :: from
!     CHARACTER(*), INTENT(IN) :: to
!     REAL(DFP) :: ans
!   END FUNCTION JacobianPrism
! END INTERFACE
!
! !----------------------------------------------------------------------------
! !                                                            JacobianPyramid
! !----------------------------------------------------------------------------
!
! INTERFACE
!   MODULE PURE FUNCTION JacobianPyramid(from, to) RESULT(ans)
!     CHARACTER(*), INTENT(IN) :: from
!     CHARACTER(*), INTENT(IN) :: to
!     REAL(DFP) :: ans
!   END FUNCTION JacobianPyramid
! END INTERFACE

END MODULE MappingUtility
