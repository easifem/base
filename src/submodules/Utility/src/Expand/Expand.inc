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
!!
!! temporary array
!!
IF( PRESENT(val) ) THEN
  IF( ALLOCATED(vec) ) THEN
    IF( n .EQ. SIZE(vec)) THEN
      ! have to add another chunk:
      ALLOCATE(tmp(SIZE(vec)+chunk_size))
      tmp(1:SIZE(vec)) = vec
      CALL MOVE_ALLOC(tmp,vec)
    END IF
    n = n + 1
  ELSE
    ! the first element:
    ALLOCATE(vec(chunk_size))
    n = 1
  END IF
  vec(n) = val
END IF
!!
!!
!!
IF (PRESENT(finished)) THEN
  IF (finished) THEN
    ! set vec to actual size (n):
    IF (ALLOCATED(tmp)) DEALLOCATE(tmp)
    ALLOCATE(tmp(n))
    tmp = vec(1:n)
    CALL MOVE_ALLOC(tmp,vec)
  END IF
END IF