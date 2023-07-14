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
! summary:         Some methods related to standard mapping are defined
!
!{!pages/MappingUtility_.md!}

MODULE MappingUtility
USE GlobalData
IMPLICIT NONE
PRIVATE

PUBLIC :: FromBiunitLine2Segment
PUBLIC :: FromUnitTriangle2Triangle
PUBLIC :: FromBiUnitQuadrangle2Quadrangle
PUBLIC :: FromBiUnitHexahedron2Hexahedron
PUBLIC :: FromBiUnitLine2UnitLine
PUBLIC :: FromUnitLine2BiUnitLine
PUBLIC :: FromBiUnitTriangle2BiUnitSqr
PUBLIC :: FromBiUnitSqr2BiUnitTriangle
PUBLIC :: FromUnitTriangle2BiUnitSqr
PUBLIC :: FromBiUnitSqr2UnitTriangle
PUBLIC :: BarycentricCoordUnitTriangle
PUBLIC :: BarycentricCoordBiUnitTriangle
PUBLIC :: BarycentricCoordTriangle

!----------------------------------------------------------------------------
!                                                     FromBiunitLine2Segment
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 19 Oct 2022
! summary: Map from unit line to physical space

INTERFACE
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
END INTERFACE

INTERFACE FromBiunitLine2Segment
  MODULE PROCEDURE FromBiunitLine2Segment1
END INTERFACE FromBiunitLine2Segment

!----------------------------------------------------------------------------
!                                                     FromBiunitLine2Segment
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 19 Oct 2022
! summary: Map from unit line to physical space

INTERFACE
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
END INTERFACE

INTERFACE FromBiunitLine2Segment
  MODULE PROCEDURE FromBiunitLine2Segment2
END INTERFACE FromBiunitLine2Segment

!----------------------------------------------------------------------------
!                                                 FromUnitTriangle2Triangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 19 Oct 2022
! summary: Map from unit line to physical space

INTERFACE
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
END INTERFACE

INTERFACE FromUnitTriangle2Triangle
  MODULE PROCEDURE FromUnitTriangle2Triangle1
END INTERFACE FromUnitTriangle2Triangle

!----------------------------------------------------------------------------
!                                            FromBiUnitQuadrangle2Quadrangle
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 19 Oct 2022
! summary: Map from unit line to physical space

INTERFACE FromBiUnitQuadrangle2Quadrangle
  MODULE PURE FUNCTION FromBiUnitQuadrangle2Quadrangle1(xin, x1, x2, x3, x4) &
    & RESULT(ans)
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

INTERFACE
  MODULE PURE FUNCTION FromBiUnitTriangle2BiUnitSqr(xin) RESULT(ans)
    REAL(DFP), INTENT(IN) :: xin(:, :)
    !! coordinates in biunit triangle in xij format
    !! bi unit triangle is defined by
    !! (-1,-1), (1,-1), (-1,1)
    REAL(DFP) :: ans(2, SIZE(xin, 2))
    !! mapped coordinates of xin in biunit sqr
  END FUNCTION FromBiUnitTriangle2BiUnitSqr
END INTERFACE

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

INTERFACE
  MODULE PURE FUNCTION FromBiUnitSqr2BiUnitTriangle(xin) RESULT(ans)
    REAL(DFP), INTENT(IN) :: xin(:, :)
    !! coordinates in bi-unit square in xij coordinate
    REAL(DFP) :: ans(2, SIZE(xin, 2))
    !! coordinates in biunit triangle
  END FUNCTION FromBiUnitSqr2BiUnitTriangle
END INTERFACE

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

INTERFACE
  MODULE PURE FUNCTION FromUnitTriangle2BiUnitSqr(xin) RESULT(ans)
    REAL(DFP), INTENT(IN) :: xin(:, :)
    !! coordinates in biunit triangle in xij format
    !! bi unit triangle is defined by
    !! (-1,-1), (1,-1), (-1,1)
    REAL(DFP) :: ans(2, SIZE(xin, 2))
    !! mapped coordinates of xin in biunit sqr
  END FUNCTION FromUnitTriangle2BiUnitSqr
END INTERFACE

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

INTERFACE
  MODULE PURE FUNCTION FromBiUnitSqr2UnitTriangle(xin) RESULT(ans)
    REAL(DFP), INTENT(IN) :: xin(:, :)
    !! coordinates in bi-unit square in xij coordinate
    REAL(DFP) :: ans(2, SIZE(xin, 2))
    !! coordinates in biunit triangle
  END FUNCTION FromBiUnitSqr2UnitTriangle
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

END MODULE MappingUtility
