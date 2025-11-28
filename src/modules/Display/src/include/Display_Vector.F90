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

CALL setDefaultSettings
!> main

IF (PRESENT(unitNo)) THEN
  I = unitNo
ELSE
  I = stdout
END IF
IF (PRESENT(full)) THEN
  full_ = full
ELSE
  full_ = .FALSE.
  ! do nothing for now
END IF
IF (I .NE. stdout .OR. (I .NE. stderr)) THEN
  full_ = .TRUE.
END IF

IF (PRESENT(orient)) THEN
  IF (orient(1:1) .EQ. "r" .OR. orient(1:1) .EQ. "R") THEN
    orient_ = "row"
  ELSE
    orient_ = "col"
  END IF
ELSE
  orient_ = "col"
END IF

ss = SIZE(val)
IF (full_ .OR. ss .LE. (minRow + minRow)) THEN
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
    CALL DISP(title="", x=val(1:minRow), unit=I, orient=orient_, advance="NO")
    CALL Display("...", unitNo=I, advance=.FALSE.)
    CALL DISP(title="", x=val(ss-minRow+1:ss), unit=I, orient=orient_, advance=advance)
  ELSE
    CALL Disp(title=msg, unit=I, advance="YES")
   CALL DISP(title="", x=val(1:minRow), unit=I, orient=orient_, advance="YES")
    CALL Display("."//CHAR_LF//"."//CHAR_LF//".", unitNo=I, advance=.TRUE.)
    CALL DISP(title="", x=val(ss-minRow+1:ss), unit=I, orient=orient_, advance=advance)
  END IF
END IF
