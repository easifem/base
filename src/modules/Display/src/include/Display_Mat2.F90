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

LOGICAL(LGT) :: full_
INTEGER(I4B) :: ii, ff, mm, nn
CHARACTER(3) :: orient_

CALL setDefaultSettings
IF (PRESENT(unitNo)) THEN; I = unitNo; ELSE; I = stdout; END IF

IF (PRESENT(full)) THEN
  full_ = full
ELSE
  full_ = .FALSE.
  ! do nothing for now
END IF
IF (I .NE. stdout .OR. (I .NE. stderr)) THEN
  full_ = .TRUE.
END IF

! IF (PRESENT(orient)) THEN
!   IF (orient(1:1) .EQ. "r" .OR. orient(1:1) .EQ. "R") THEN
!     orient_ = "row"
!   ELSE
!     orient_ = "col"
!   END IF
! ELSE
!   orient_ = "col"
! END IF
orient_ = "row"

mm = SIZE(val, 1)
nn = SIZE(val, 2)
IF (full_ .OR. mm .LE. (minRow + minRow) .OR. (nn .LE. (minCol + minCol))) THEN
#ifdef COLOR_DISP
  CALL DISP( &
    & title=TRIM(colorize(msg, color_fg=COLOR_FG, color_bg=COLOR_BG, &
    & style=COLOR_STYLE)), &
    & x=val, unit=I, advance=advance)
#else
  CALL DISP(title=msg, x=val, unit=I, advance=advance)
#endif
ELSE
  CALL Disp(title=msg, unit=I, advance="YES")
  CALL DISP(title="", x=val(1:minRow, 1:minCol), unit=I, advance="NO")
  CALL Display("...", unitNo=I, advance=.FALSE.)
  CALL DISP(title="", x=val(1:minRow, nn-minCol+1:nn), unit=I, advance="YES")
  CALL Display("."//CHAR_LF//"."//CHAR_LF//".", unitNo=I, advance=.TRUE.)
CALL DISP(title="", x=val(mm - minRow + 1:mm, 1:minCol), unit=I, advance="NO")
  CALL Display("...", unitNo=I, advance=.FALSE.)
  CALL DISP(title="", x=val(mm-minRow+1:mm, nn-minCol+1:nn), unit=I, advance=advance)
END IF
