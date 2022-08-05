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
! date: 	23 Feb 2021
! summary: This submodule of [[BoundingBox_Method]] implements set methods defined in parent module

SUBMODULE(BoundingBox_Method) SetMethods
IMPLICIT NONE
CONTAINS

!-----------------------------------------------------------------------------
!                                                                     setXmin
!-----------------------------------------------------------------------------

MODULE PROCEDURE setXmin
  obj%Box( 1, 1 ) = Val
END PROCEDURE setXmin

!-----------------------------------------------------------------------------
!                                                                     setXmax
!-----------------------------------------------------------------------------

MODULE PROCEDURE setXmax
  obj%Box( 2, 1 ) = Val
END PROCEDURE setXmax

!-----------------------------------------------------------------------------
!                                                                     setYmin
!-----------------------------------------------------------------------------

MODULE PROCEDURE setYmin
  obj%Box( 1, 2 ) = Val
END PROCEDURE setYmin

!-----------------------------------------------------------------------------
!                                                                     setYmax
!-----------------------------------------------------------------------------

MODULE PROCEDURE setYmax
  obj%Box( 2, 2 ) = Val
END PROCEDURE setYmax

!-----------------------------------------------------------------------------
!                                                                     setZmin
!-----------------------------------------------------------------------------

MODULE PROCEDURE setZmin
  obj%Box( 1, 3 ) = Val
END PROCEDURE setZmin

!-----------------------------------------------------------------------------
!                                                                     setZmax
!-----------------------------------------------------------------------------

MODULE PROCEDURE setZmax
  obj%Box( 2, 3 ) = Val
END PROCEDURE setZmax

END SUBMODULE SetMethods