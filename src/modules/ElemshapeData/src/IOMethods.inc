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

PUBLIC :: Display

!----------------------------------------------------------------------------
!                                                                Display@IO
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 4 March 2021
! summary: Display the content of [[elemshapedata_]] and [[stelemshapedata_]]
!
!# Introduction
!
! This subroutine displays the content of [[elemshapedata_]] and
! [[stelemshapedata_]] on screen. this routine belongs to `Display()`.

INTERFACE
  MODULE SUBROUTINE elemsd_display_1(obj, Msg, UnitNo)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    CHARACTER(LEN=*), INTENT(IN) :: Msg
    INTEGER(I4B), INTENT(IN), OPTIONAL :: UnitNo
  END SUBROUTINE elemsd_display_1
END INTERFACE

INTERFACE Display
  MODULE PROCEDURE elemsd_display_1
END INTERFACE Display

!----------------------------------------------------------------------------
!                                                                Display@IO
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 4 March 2021
! summary: Display the content of [[elemshapedata_]] and [[stelemshapedata_]]

INTERFACE
  MODULE SUBROUTINE elemsd_display_2(obj, Msg, UnitNo)
    CLASS(STElemshapeData_), INTENT(IN) :: obj(:)
    CHARACTER(LEN=*), INTENT(IN) :: Msg
    INTEGER(I4B), INTENT(IN), OPTIONAL :: UnitNo
  END SUBROUTINE elemsd_display_2
END INTERFACE

INTERFACE Display
  MODULE PROCEDURE elemsd_display_2
END INTERFACE Display
