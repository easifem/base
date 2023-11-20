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

SUBMODULE(BoundingBox_Method) TomlMethods
USE tomlf, ONLY: &
  ! & toml_error,  &
  ! & toml_load,  &
  ! & toml_parser_config,  &
  ! & toml_serialize,  &
  & toml_get => get_value, &
  & toml_len => len, &
  ! & toml_context,  &
  ! & toml_terminal,  &
  ! & toml_load,  &
  ! & toml_stat, &
  & toml_array
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE toml_get_bbox_r0
REAL(DFP) :: lim(6)
lim = 0.0
CALL toml_get(table, "xmin", lim(1), origin=origin, stat=stat)
CALL toml_get(table, "ymin", lim(2), origin=origin, stat=stat)
CALL toml_get(table, "zmin", lim(3), origin=origin, stat=stat)
CALL toml_get(table, "xmax", lim(4), origin=origin, stat=stat)
CALL toml_get(table, "ymax", lim(5), origin=origin, stat=stat)
CALL toml_get(table, "zmax", lim(6), origin=origin, stat=stat)
CALL Initiate(obj=VALUE, nsd=3_I4B, lim=lim)
END PROCEDURE toml_get_bbox_r0

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE toml_get_bbox_r1
TYPE(toml_array), POINTER :: array
TYPE(toml_table), POINTER :: child
LOGICAL(LGT) :: isFound0
INTEGER(I4B) :: ii, tsize

child => NULL()
array => NULL()
isFound0 = .FALSE.
CALL DEALLOCATE (VALUE)

CALL toml_get(table, key, array, origin=origin,  &
  & stat=stat, requested=.FALSE.)

IF (ASSOCIATED(array)) THEN
  isFound0 = .TRUE.
  tsize = toml_len(array)
  ALLOCATE (VALUE(tsize))
  DO ii = 1, tsize
    CALL toml_get(array, ii, child)
    CALL toml_get_bbox_r0(table=child, key="", VALUE=VALUE(ii), &
      &  origin=origin, stat=stat)
  END DO
END IF

IF (PRESENT(isFound)) isFound = isFound0
NULLIFY (array, child)
END PROCEDURE toml_get_bbox_r1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE TomlMethods
