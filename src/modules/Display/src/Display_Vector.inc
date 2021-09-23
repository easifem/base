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

!! vector of real numbers
CHARACTER( LEN = * ), INTENT( IN ) :: msg
!! message
INTEGER( I4B ), INTENT( IN ), OPTIONAL :: unitNo
!! Unit number
CHARACTER( LEN = * ), OPTIONAL, INTENT( IN ) :: orient
!! orient=row => rowwise printing
!! orient=col =>  columnwise printing
LOGICAL( LGT ), INTENT( IN ), OPTIONAL :: full
!! logical variable to print the whole vector
! Define internal variables
INTEGER( I4B ) :: I
CHARACTER( LEN = 3 ) :: orient_
!> main
IF( PRESENT( unitNo ) ) THEN
  I = unitNo
ELSE
  I = stdout
END IF
IF( PRESENT( full ) ) THEN
  ! do nothing for now
END IF
IF( PRESENT( orient ) ) THEN
  IF( orient(1:1) .EQ. "r" .OR. orient(1:1) .EQ. "R" ) THEN
    orient_ = "row"
  ELSE
    orient_ = "col"
  END IF
ELSE
  orient_ = "col"
END IF
CALL DISP( &
  & title=TRIM(colorize( msg, color_fg=COLOR_FG, color_bg=COLOR_BG, &
  & style=COLOR_STYLE)), &
  & x=val, unit = I, orient = orient_ )