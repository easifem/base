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

INTEGER(I4B) :: n, m, na, nb, nm
!!
na = SIZE( A )
nb = SIZE( B )
!!
IF( na .EQ. 0 ) THEN
  !!
  !! na zero
  !!
  IF ( nb .EQ. 0 ) THEN
    !!
    !! nb is zero
    !!
    CALL REALLOCATE( C, 0 )
    !!
  ELSE
    !!
    !! nb is not zero
    !!
    nm = COUNT(mask)
    !!
    IF( nm .EQ. 0 ) THEN
      CALL REALLOCATE( C, 0 )
    ELSE
      C = PACK( B, mask )
    END IF
  END IF
  !!
ELSE
  !!
  !! na is not zero
  !!
  IF ( nb .EQ. 0 ) THEN
    C = A
  ELSE
    nm = COUNT(mask)
    IF( nm .EQ. 0 ) THEN
      C = A
    ELSE
      CALL Reallocate( C, na + nm )
      C(1:na) = A
      C(na + 1:) = PACK( B, mask )
    END IF
  END IF
END IF
!!