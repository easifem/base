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

PUBLIC :: FromBiunitLine2Segment

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

PUBLIC :: FromUnitTriangle2Triangle

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

PUBLIC :: FromBiUnitLine2UnitLine

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

PUBLIC :: FromUnitLine2BiUnitLine

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

PUBLIC :: FromBiUnitTriangle2BiUnitSqr

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

PUBLIC :: FromBiUnitSqr2BiUnitTriangle

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

PUBLIC :: FromUnitTriangle2BiUnitSqr

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

PUBLIC :: FromBiUnitSqr2UnitTriangle

END MODULE MappingUtility