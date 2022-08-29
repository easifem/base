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

SUBMODULE(InterpolationUtility) Methods
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                         VandermondeMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE VandermondeMatrix_Real32
  INTEGER( I4B ) :: ii
  !!
  ans(:,1) = 1.0_Real32
  !!
  DO ii = 2, order+1
    ans(:,ii) = x**(ii-1)
  END DO
  !!
END PROCEDURE VandermondeMatrix_Real32

!----------------------------------------------------------------------------
!                                                         VandermondeMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE VandermondeMatrix_Real64
  INTEGER( I4B ) :: ii
  !!
  ans(:,1) = 1.0_Real64
  !!
  DO ii = 2, order+1
    ans(:,ii) = x**(ii-1)
  END DO
  !!
END PROCEDURE VandermondeMatrix_Real64


END SUBMODULE Methods