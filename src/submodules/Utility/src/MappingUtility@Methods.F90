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

SUBMODULE(MappingUtility) Methods
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                       FromBiunitLine2Segment
!----------------------------------------------------------------------------

MODULE PROCEDURE FromBiunitLine2Segment1
ans = 0.5_DFP * (x1 + x2) + 0.5_DFP * (x2 - x1) * xin
END PROCEDURE FromBiunitLine2Segment1

!----------------------------------------------------------------------------
!                                                       FromBiunitLine2Segment
!----------------------------------------------------------------------------

MODULE PROCEDURE FromBiunitLine2Segment2
INTEGER(I4B) :: ii
DO ii = 1, SIZE(xin)
  ans(:, ii) = 0.5_DFP * (x1 + x2) + 0.5_DFP * (x2 - x1) * xin(ii)
END DO
END PROCEDURE FromBiunitLine2Segment2

!----------------------------------------------------------------------------
!                                                   FromBiUnitLine2UnitLine
!----------------------------------------------------------------------------

MODULE PROCEDURE FromBiUnitLine2UnitLine
ans = 0.5_DFP * (1.0_DFP + xin)
END PROCEDURE FromBiUnitLine2UnitLine

!----------------------------------------------------------------------------
!                                                   FromUnitLine2BiUnitLine
!----------------------------------------------------------------------------

MODULE PROCEDURE FromUnitLine2BiUnitLine
ans = 2.0_DFP * xin - 1.0_DFP
END PROCEDURE FromUnitLine2BiUnitLine

!----------------------------------------------------------------------------
!                                                  FromUnitTriangle2Triangle
!----------------------------------------------------------------------------

MODULE PROCEDURE FromUnitTriangle2Triangle1
INTEGER(I4B) :: ii
DO ii = 1, size(ans, 2)
  ans(:, ii) = x1 + (x2 - x1) * xin(1, ii) + (x3 - x1) * xin(2, ii)
END DO
END PROCEDURE FromUnitTriangle2Triangle1

!----------------------------------------------------------------------------
!                                             FromBiUnitTriangle2BiUnitSqr
!----------------------------------------------------------------------------

MODULE PROCEDURE FromBiUnitTriangle2BiUnitSqr
ans(1, :) = 2.0_DFP * (1.0_DFP + xin(1, :)) / (1.0_DFP - xin(2, :)) - 1.0_DFP
ans(2, :) = xin(2, :)
END PROCEDURE FromBiUnitTriangle2BiUnitSqr

!----------------------------------------------------------------------------
!                                             FromBiUnitSqr2BiUnitTriangle
!----------------------------------------------------------------------------

MODULE PROCEDURE FromBiUnitSqr2BiUnitTriangle
ans(1, :) = 0.5_DFP * (1.0_DFP + xin(1, :)) * (1.0_DFP - xin(2, :)) &
  & - 1.0_DFP
ans(2, :) = xin(2, :)
END PROCEDURE FromBiUnitSqr2BiUnitTriangle

!----------------------------------------------------------------------------
!                                                 FromUnitTriangle2BiUnitSqr
!----------------------------------------------------------------------------

MODULE PROCEDURE FromUnitTriangle2BiUnitSqr
ans(1, :) = (2.0_DFP * xin(1,:) + xin(2,:) - 1.0_DFP) / (1.0_DFP - xin(2, :))
ans(2, :) = 2.0_DFP * xin(2, :) - 1.0_DFP
END PROCEDURE FromUnitTriangle2BiUnitSqr

!----------------------------------------------------------------------------
!                                                 FromBiUnitSqr2UnitTriangle
!----------------------------------------------------------------------------

MODULE PROCEDURE FromBiUnitSqr2UnitTriangle
ans(1, :) = 0.25_DFP * (1.0_DFP + xin(1, :)) * (1.0_DFP - xin(2, :))
ans(2, :) = 0.5_DFP * (xin(2, :) + 1.0_DFP)
END PROCEDURE FromBiUnitSqr2UnitTriangle

!----------------------------------------------------------------------------
!                                               BarycentricCoordUnitTriangle
!----------------------------------------------------------------------------

MODULE PROCEDURE BarycentricCoordUnitTriangle
ans(1, :) = 1.0_DFP - xin(1, :) - xin(2, :)
ans(2, :) = xin(1, :)
ans(3, :) = xin(2, :)
END PROCEDURE BarycentricCoordUnitTriangle

!----------------------------------------------------------------------------
!                                             BarycentricCoordBiUnitTriangle
!----------------------------------------------------------------------------

MODULE PROCEDURE BarycentricCoordBiUnitTriangle
ans(1, :) = -0.5_DFP * (xin(1, :) + xin(2, :))
ans(2, :) = 0.5_DFP * (1.0_DFP + xin(1, :))
ans(3, :) = 0.5_DFP * (1.0_DFP + xin(2, :))
END PROCEDURE BarycentricCoordBiUnitTriangle

END SUBMODULE Methods
