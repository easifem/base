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

module test_FACE
use easifemBase
implicit none
contains

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine test1

  CALL Display( &
    & "Hello World", &
    & colorize( "msg : ", color_fg="WHITE", color_bg= "MAGENTA", style="BOLD_ON") )

  CALL Display( &
    & "Hello World", &
    & colorize( "msg : ", color_fg="BLACK", color_bg= "RED") )

  CALL Display( &
    & "Hello World", &
    & colorize( "msg : ", color_fg="BLACK", color_bg= "CYAN") )

  ! colorize("BLACK          ", color_fg="BLACK          ", color_bg="")
  ! colorize("RED            ", color_fg="RED            ")
  ! colorize("GREEN          ", color_fg="GREEN          ")
  ! colorize("YELLOW         ", color_fg="YELLOW         ")
  ! colorize("BLUE           ", color_fg="BLUE           ")
  ! colorize("MAGENTA        ", color_fg="MAGENTA        ")
  ! colorize("CYAN           ", color_fg="CYAN           ")
  ! colorize("WHITE          ", color_fg="WHITE          ")
  ! colorize("DEFAULT        ", color_fg="DEFAULT        ")
  ! colorize("BLACK_INTENSE  ", color_fg="BLACK_INTENSE  ")
  ! colorize("RED_INTENSE    ", color_fg="RED_INTENSE    ")
  ! colorize("GREEN_INTENSE  ", color_fg="GREEN_INTENSE  ")
  ! colorize("YELLOW_INTENSE ", color_fg="YELLOW_INTENSE ")
  ! colorize("BLUE_INTENSE   ", color_fg="BLUE_INTENSE   ")
  ! colorize("MAGENTA_INTENSE", color_fg="MAGENTA_INTENSE")
  ! colorize("CYAN_INTENSE   ", color_fg="CYAN_INTENSE   ")
  ! colorize("WHITE_INTENSE  ", color_fg="WHITE_INTENSE  ")
end

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

end

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

program main
use test_FACE
implicit none

call test1
end program main

