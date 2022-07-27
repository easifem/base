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

MODULE InterpolationUtility
USE GlobalData, ONLY: I4B, DFP, Real32, Real64
IMPLICIT NONE
PRIVATE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 27 July 2022
! summary: Returns vandermonde matrix

INTERFACE
MODULE PURE FUNCTION VandermondeMatrix_Real32( order, points ) RESULT( ans )
  INTEGER( I4B ), INTENT( IN ) :: order
  REAL( Real32 ), INTENT( IN ) :: points( : )
  REAL( Real32 ) :: ans( SIZE(points), order+1 )
END FUNCTION VandermondeMatrix_Real32

MODULE PURE FUNCTION VandermondeMatrix_Real64( order, points ) RESULT( ans )
  INTEGER( I4B ), INTENT( IN ) :: order
  REAL( Real64 ), INTENT( IN ) :: points( : )
  REAL( Real64 ) :: ans( SIZE(points), order+1 )
END FUNCTION VandermondeMatrix_Real64
END INTERFACE

INTERFACE VandermondeMatrix
  MODULE PROCEDURE VandermondeMatrix_Real32, VandermondeMatrix_Real64
END INTERFACE VandermondeMatrix

PUBLIC :: VandermondeMatrix


END MODULE InterpolationUtility