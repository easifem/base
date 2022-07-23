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

! Define internal variables
INTEGER(I4B) :: i, k, j, N
!!
IF (ALLOCATED(obj)) THEN
  !!
  N = SIZE(obj)
  ALLOCATE (Res(N))
  Res = 0
  Res(1) = obj(1)
  k = 1
  !!
  DO i = 2, N
    IF (.NOT. ANY(Res .EQ. obj(i))) THEN
      k = k + 1
      Res(k) = obj(i)
    END IF
  END DO
  !!
  obj = Res(1:k)
  DEALLOCATE (Res)
  !!
END IF
!!