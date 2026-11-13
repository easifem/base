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

INTEGER(I4B) :: na, nb
!!
nb = SIZE( B )
na = SIZE( A )
!!
IF( na .NE. 0 ) THEN
  !!
  !! na not zero
  !!
  IF ( nb .NE. 0) THEN
    !!
    !! nb not zero
    !!
    CALL Reallocate( C, na+nb )
    C(1:na) = A
    C(na + 1:) = B
    !!
  ELSE
    C = A
  END IF
  !!
ELSE
  !!
  !! na is zero
  !!
  IF (nb .NE. 0) THEN
    !!
    !! nb is zero
    !!
    C = B
  ELSE
    CALL Reallocate( C, 0 )
  END IF
  !!
END IF
!!