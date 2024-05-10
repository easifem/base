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

SUBMODULE(BoundingBox_Method) IOMethods
USE Display_Method
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                   Display
!----------------------------------------------------------------------------

MODULE PROCEDURE display_obj
CALL Display(msg, unitNo=unitNo)
CALL Display(obj%nsd, msg="NSD :: ", unitNo=unitNo)
CALL Display(.Xmin.obj, msg="Xmin :: ", unitNo=unitNo)
CALL Display(.Xmax.obj, msg="Xmax :: ", unitNo=unitNo)
CALL Display(.Ymin.obj, msg="Ymin :: ", unitNo=unitNo)
CALL Display(.Ymax.obj, msg="Ymax :: ", unitNo=unitNo)
CALL Display(.Zmin.obj, msg="Zmin :: ", unitNo=unitNo)
CALL Display(.Zmax.obj, msg="Zmax :: ", unitNo=unitNo)
CALL Display(obj%l(1), msg="Lx :: ", unitNo=unitNo)
CALL Display(obj%l(2), msg="Ly :: ", unitNo=unitNo)
CALL Display(obj%l(3), msg="Lz :: ", unitNo=unitNo)
END PROCEDURE display_obj

END SUBMODULE IOMethods
