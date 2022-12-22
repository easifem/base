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
! along with array program.  If not, see <https: //www.gnu.org/licenses/>
!

INTEGER(I4B) :: start, bottom
INTEGER(I4B) :: N

N = SIZE(arg)
arg = arange(1_I4B, n, 1_I4B)

DO start = ((n - 2) / 2), 0, -1
  CALL argSiftdown(arg, start, n);
END DO

DO bottom = n - 1, 1, -1
  CALL Swap(arg(0), arg(bottom))
  CALL argSiftdown(arg, 0, bottom)
END DO

CONTAINS

PURE SUBROUTINE argSiftdown(arg, start, bottom)
  INTEGER(I4B), INTENT(INOUT) :: arg(0:)
  INTEGER(I4B), INTENT(IN) :: start
  INTEGER(I4B), INTENT(IN) :: bottom
  !
  INTEGER(I4B) :: i, j
  i = start
  DO WHILE ((i * 2) + 1 < bottom)
    j = (i * 2) + 1
    IF (j + 1 < bottom) THEN
      IF (array(arg(j)) < array(arg(j + 1))) j = j + 1
    END IF
    IF (array(arg(i)) < array(arg(j))) THEN
      CALL Swap(arg(i), arg(j))
      i = j
    ELSE
      EXIT
    END IF
  END DO

END SUBROUTINE

! subroutine argSiftdown(array, arg, start, bottom)
!   real(Real32) :: array(:)
!   INTEGER(Int32) :: arg(0:)
!   INTEGER(Int32) :: start
!   INTEGER(Int32) :: bottom
!   INTEGER(Int32) :: i, j
!   i = start
!   DO WHILE ((i * 2) + 1 < bottom)
!     j = (i * 2) + 1
!     IF (j + 1 < bottom) THEN
!       IF (array(arg(j)) < array(arg(j + 1))) j = j + 1
!     END IF
!     IF (array(arg(i)) < array(arg(j))) THEN
!       CALL swap(arg(i), arg(j))
!       i = j
!     ELSE
!       RETURN
!     END IF
!   END DO
! END subroutine
