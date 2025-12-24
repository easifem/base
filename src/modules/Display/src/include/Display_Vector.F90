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

INTEGER(I4B) :: I
CHARACTER(3) :: orient_
LOGICAL(LGT) :: full_
INTEGER(I4B) :: ii, ff, ss
LOGICAL(LGT) :: isok, abool

CALL setDefaultSettings
!> main

I = stdout
full_ = .FALSE.
orient_ = "col"

isok = PRESENT(unitNo); IF (isok) I = unitNo
isok = PRESENT(full); IF (isok) full_ = full
isok = (I .NE. stdout) .OR. (I .NE. stderr)
IF (isok) full_ = .TRUE.

isok = PRESENT(orient)
IF (isok) THEN
  abool = (orient(1:1) .EQ. "r") .OR. (orient(1:1) .EQ. "R")
  IF (abool) orient_ = "row"
END IF

ss = SIZE(val)
abool = ss .LE. (minRow + minRow)
IF (full_ .OR. abool) THEN

#ifdef COLOR_DISP
  CALL DISP( &
    title=TRIM(colorize(msg, color_fg=COLOR_FG, color_bg=COLOR_BG, &
                        style=COLOR_STYLE)), &
    x=val, unit=I, orient=orient_, advance=advance)
#else
  CALL DISP(title=msg, x=val, unit=I, orient=orient_, advance=advance)
#endif

ELSE
  IF (orient_ .EQ. "row") THEN
    CALL Disp(title=msg, unit=I, advance="YES")
    CALL Disp(title="", x=val(1:minRow), unit=I, orient=orient_, advance="NO")
    CALL Display("...", unitNo=I, advance=.FALSE.)
    CALL Disp(title="", x=val(ss - minRow + 1:ss), unit=I, orient=orient_, &
              advance=advance)
  ELSE
    CALL Disp(title=msg, unit=I, advance="YES")
    CALL Disp(title="", x=val(1:minRow), unit=I, orient=orient_, &
              advance="YES")
    CALL Display("."//CHAR_LF//"."//CHAR_LF//".", unitNo=I, advance=.TRUE.)
    CALL Disp(title="", x=val(ss - minRow + 1:ss), unit=I, orient=orient_, &
              advance=advance)
  END IF
END IF
