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
INTEGER(I4B) :: ii, n, tsize

IF (.NOT. ALLOCATED(obj)) THEN
  RETURN
END IF

tsize = SIZE(obj)

if(tsize .LE. 1) RETURN


CALL QUICKSORT(obj, 1_I4B, tsize)

temp = obj
DEALLOCATE (obj)

n = 1
obj = [temp(1)]

DO ii = 2, tsize
  IF (temp(ii) .NE. temp(ii - 1)) THEN
    CALL Expand(vec=obj, n=n, chunk_size=tsize, &
      & val=temp(ii))
  END IF
END DO

CALL Expand(vec=obj, n=n, chunk_size=tsize, finished=.TRUE.)

