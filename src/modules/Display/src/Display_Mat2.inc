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

CHARACTER( LEN = * ), INTENT( IN ) :: msg
INTEGER( I4B ), INTENT( IN ), OPTIONAL :: unitNo
LOGICAL( LGT ), INTENT( IN ), OPTIONAL :: full
!   Define internal variables
INTEGER( I4B ) :: I
CALL setDefaultSettings
IF( PRESENT( unitNo ) ) THEN; I = unitNo; ELSE; I = stdout; END IF
CALL DISP( &
  & title = TRIM(colorize( msg, color_fg=COLOR_FG, color_bg=COLOR_BG, &
  & style=COLOR_STYLE)), &
  & x= val, unit = I )
