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

SUBMODULE(BoundingBox_Method) SetMethods
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                     setXmin
!----------------------------------------------------------------------------

MODULE PROCEDURE setXmin
obj%box(1, 1) = val
END PROCEDURE setXmin

!----------------------------------------------------------------------------
!                                                                     setXmax
!----------------------------------------------------------------------------

MODULE PROCEDURE setXmax
obj%box(2, 1) = val
END PROCEDURE setXmax

!----------------------------------------------------------------------------
!                                                                     setYmin
!----------------------------------------------------------------------------

MODULE PROCEDURE setYmin
obj%box(1, 2) = val
END PROCEDURE setYmin

!----------------------------------------------------------------------------
!                                                                     setYmax
!----------------------------------------------------------------------------

MODULE PROCEDURE setYmax
obj%box(2, 2) = val
END PROCEDURE setYmax

!----------------------------------------------------------------------------
!                                                                     setZmin
!----------------------------------------------------------------------------

MODULE PROCEDURE setZmin
obj%box(1, 3) = val
END PROCEDURE setZmin

!----------------------------------------------------------------------------
!                                                                     setZmax
!----------------------------------------------------------------------------

MODULE PROCEDURE setZmax
obj%box(2, 3) = val
END PROCEDURE setZmax

END SUBMODULE SetMethods
