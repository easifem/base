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

SUBMODULE(RealMatrix_Method) SetMethods
USE GlobalData, ONLY: MATRIX_DIAGONAL
USE GlobalData, ONLY: MATRIX_ROW
USE GlobalData, ONLY: MATRIX_COLUMN
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                 setValues
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set1
obj%val = val
END PROCEDURE obj_Set1

!----------------------------------------------------------------------------
!                                                                 setValues
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set2
obj%val(row, col) = val
END PROCEDURE obj_Set2

!----------------------------------------------------------------------------
!                                                                 setValues
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set3
obj%val(row, col) = val
END PROCEDURE obj_Set3

!----------------------------------------------------------------------------
!                                                                 setValues
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set4
INTEGER(I4B) :: i

SELECT CASE (ExtraOption)

CASE (MATRIX_DIAGONAL)
  ! diagonal
  IF (indx .LT. 0) THEN
    DO i = 1, SIZE(obj%val, 2) + indx
      obj%val(i - indx, i) = val(i)
    END DO
  ELSE
    DO i = 1, SIZE(obj%val, 1) - indx
      obj%val(i, i + indx) = val(i)
    END DO
  END IF

CASE (MATRIX_ROW)
  ! row
  IF (indx .LE. SIZE(obj%val, 1)) THEN
    obj%val(indx, 1:SIZE(val)) = val
  END IF

CASE (MATRIX_COLUMN)
  IF (indx .LE. SIZE(obj%val, 2)) THEN
    obj%val(1:SIZE(val), indx) = val
  END IF

CASE DEFAULT
END SELECT
END PROCEDURE obj_Set4

!----------------------------------------------------------------------------
!                                                                  setValues
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set5
INTEGER(I4B) :: i, j

SELECT CASE (extraOption)

CASE (MATRIX_DIAGONAL)

  ! diagonal
  DO j = 1, SIZE(indx)
    IF (indx(j) .LT. 0) THEN
      DO i = 1, SIZE(obj%val, 2) + indx(j)
        obj%val(i - indx(j), i) = val(i, j)
      END DO
    ELSE
      DO i = 1, SIZE(obj%val, 1) - indx(j)
        obj%val(i, i + indx(j)) = val(i, j)
      END DO
    END IF
  END DO

CASE (MATRIX_ROW)
  ! row
  DO j = 1, SIZE(indx)
    obj%val(indx(j), :) = val(j, :)
  END DO

CASE (MATRIX_COLUMN)
  ! col
  DO j = 1, SIZE(indx)
    obj%val(:, indx(j)) = val(:, j)
  END DO

CASE DEFAULT

END SELECT
END PROCEDURE obj_Set5

!----------------------------------------------------------------------------
!                                                                  setValue
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set6
obj%val = 1.0_DFP
END PROCEDURE obj_Set6

END SUBMODULE SetMethods
