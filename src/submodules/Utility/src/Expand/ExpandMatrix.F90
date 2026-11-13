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

INTEGER( I4B ) :: dimSize1, dimSize2, otherdim


IF (PRESENT(val)) THEN
  IF (dim .EQ. 1) THEN
  ELSE
  END IF

  IF (ALLOCATED(mat)) THEN
    IF (n .EQ. SIZE(mat, dim)) THEN
      ! have to add another chunk:
      ALLOCATE (tmp(SIZE(mat, dim) + chunk_size, SIZE(mat, otherdim)))
      tmp(1:SIZE(mat, 1), 1:SIZE(mat, 2) ) = mat
      CALL MOVE_ALLOC(tmp, mat)
    END IF
    n = n + 1
  ELSE
    ! the first element:
    ALLOCATE (mat(chunk_size))
    n = 1
  END IF
  mat(n) = val
END IF
!!
!!
!!
IF (PRESENT(finished)) THEN
  IF (finished) THEN
    ! set mat to actual size (n):
    IF (ALLOCATED(tmp)) DEALLOCATE (tmp)
    ALLOCATE (tmp(n))
    tmp = mat(1:n)
    CALL MOVE_ALLOC(tmp, mat)
  END IF
END IF
