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

SUBMODULE(RealMatrix_Method) AddMethods
USE GlobalData, ONLY: MATRIX_DIAGONAL
USE GlobalData, ONLY: MATRIX_ROW
USE GlobalData, ONLY: MATRIX_COLUMN
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                        Add
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Add1
INTEGER(I4B) :: acase

acase = IACHAR(op)

SELECT CASE (acase)
CASE (43)
  ! +
  obj%val = obj%val + scale * val
CASE (45)
  ! -
  obj%val = obj%val - scale * val
CASE (42)
  ! *
  obj%val = scale * (obj%val * val)
CASE (47)
  ! /
  obj%val = (obj%val / val) / scale
CASE DEFAULT
  ! default is +
  obj%val = obj%val + scale * val
END SELECT
END PROCEDURE obj_Add1

!----------------------------------------------------------------------------
!                                                            addContribution
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Add2
INTEGER(I4B) :: acase

acase = IACHAR(Op)

SELECT CASE (acase)
CASE (43)
  ! +
  obj%val(row, col) = obj%val(row, col) + scale * val
CASE (45)
  ! -
  obj%val(row, col) = obj%val(row, col) - scale * val
CASE (42)
  ! *
  obj%val(row, col) = scale * obj%val(row, col) * val
CASE (47)
  ! /
  obj%val(row, col) = obj%val(row, col) / val / scale
CASE DEFAULT
  ! +
  obj%val(row, col) = obj%val(row, col) + scale * val
END SELECT
END PROCEDURE obj_Add2

!----------------------------------------------------------------------------
!                                                           realmat_add_3
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Add3
INTEGER(I4B) :: acase

acase = IACHAR(Op)

SELECT CASE (acase)
CASE (43)
  ! +
  obj%val(row, col) = obj%val(row, col) + scale * val

CASE (45)
  ! -
  obj%val(row, col) = obj%val(row, col) - scale * val

CASE (42)
  ! *
  obj%val(row, col) = scale * obj%val(row, col) * val

CASE (47)
  ! /
  obj%val(row, col) = obj%val(row, col) / val / scale

CASE DEFAULT
  ! +
  obj%val(row, col) = obj%val(row, col) + scale * val
END SELECT
END PROCEDURE obj_Add3

!----------------------------------------------------------------------------
!                                                           realmat_add_4
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Add4
INTEGER(I4B) :: i, acase, tsize
LOGICAL(LGT) :: isok

acase = IACHAR(Op)

SELECT CASE (acase)

CASE (43)
  SELECT CASE (extraOption)
  CASE (MATRIX_DIAGONAL)

    ! diagonal
    isok = indx .LT. 0
    IF (isok) THEN
      tsize = SIZE(obj%val, 2) + indx
      DO i = 1, tsize
        obj%val(i - indx, i) = obj%val(i - indx, i) + scale * val(i)
      END DO

    ELSE

      tsize = SIZE(obj%val, 1) - indx
      DO i = 1, tsize
        obj%val(i, i + indx) = obj%val(i, i + indx) + scale * val(i)
      END DO
    END IF

  CASE (MATRIX_ROW)

    ! row
    isok = indx .LE. SIZE(obj%val, 1)
    IF (isok) THEN
      obj%val(indx, 1:SIZE(val)) = obj%val(indx, 1:SIZE(val)) &
                                   + scale * val
    END IF

  CASE (MATRIX_COLUMN)

    isok = indx .LE. SIZE(obj%val, 2)
    tsize = SIZE(val)
    IF (isok) THEN
      obj%val(1:tsize, indx) = obj%val(1:tsize, indx) &
                               + scale * val
    END IF

  CASE DEFAULT
  END SELECT

CASE (45)

  SELECT CASE (ExtraOption)
  CASE (0)

    ! diagonal
    isok = indx .LT. 0
    tsize = SIZE(obj%val, 2) + indx
    IF (isok) THEN
      DO i = 1, tsize
        obj%val(i - indx, i) = obj%val(i - indx, i) - scale * val(i)
      END DO

    ELSE

      tsize = SIZE(obj%val, 1) - indx
      DO i = 1, tsize
        obj%val(i, i + indx) = obj%val(i, i + indx) - scale * val(i)
      END DO

    END IF

  CASE (1)

    ! row
    isok = indx .LE. SIZE(obj%val, 1)
    tsize = SIZE(val)
    IF (isok) THEN
      obj%val(indx, 1:tsize) = obj%val(indx, 1:tsize) &
                               - scale * val
    END IF

  CASE (2)

    isok = indx .LE. SIZE(obj%val, 2)
    tsize = SIZE(val)
    IF (isok) THEN
      obj%val(1:tsize, indx) = obj%val(1:tsize, indx) &
                               - scale * val
    END IF

  CASE DEFAULT
  END SELECT

CASE (42)

  SELECT CASE (extraOption)

  CASE (MATRIX_DIAGONAL)

    ! diagonal
    isok = indx .LT. 0
    IF (isok) THEN
      tsize = SIZE(obj%val, 2) + indx
      DO i = 1, tsize
        obj%val(i - indx, i) = obj%val(i - indx, i) * scale * val(i)
      END DO

    ELSE

      tsize = SIZE(obj%val, 1) - indx
      DO i = 1, tsize
        obj%val(i, i + indx) = obj%val(i, i + indx) * scale * val(i)
      END DO

    END IF

  CASE (MATRIX_ROW)

    ! row
    isok = indx .LE. SIZE(obj%val, 1)
    tsize = SIZE(val)
    IF (isok) THEN
      obj%val(indx, 1:tsize) = obj%val(indx, 1:tsize) &
                               * scale * val
    END IF

  CASE (MATRIX_COLUMN)

    isok = indx .LE. SIZE(obj%val, 2)
    tsize = SIZE(val)
    IF (isok) THEN
      obj%val(1:tsize, indx) = obj%val(1:tsize, indx) * scale * val
    END IF

  CASE DEFAULT

  END SELECT

CASE (47)

  SELECT CASE (extraOption)

  CASE (MATRIX_DIAGONAL)

    ! diagonal
    isok = indx .LT. 0
    IF (isok) THEN
      tsize = SIZE(obj%val, 2) + indx
      DO i = 1, tsize
        obj%val(i - indx, i) = obj%val(i - indx, i) / scale / val(i)
      END DO

    ELSE
      tsize = SIZE(obj%val, 1) - indx
      DO i = 1, tsize
        obj%val(i, i + indx) = obj%val(i, i + indx) / scale / val(i)
      END DO

    END IF

  CASE (MATRIX_ROW)

    ! row
    isok = indx .LE. SIZE(obj%val, 1)
    tsize = SIZE(val)
    IF (isok) THEN
      obj%val(indx, 1:tsize) = obj%val(indx, 1:tsize) &
                               / scale / val
    END IF

  CASE (MATRIX_COLUMN)

    isok = indx .LE. SIZE(obj%val, 2)
    tsize = SIZE(val)
    IF (isok) THEN
      obj%val(1:tsize, indx) = obj%val(1:tsize, indx) &
                               / scale / val
    END IF

  CASE DEFAULT
  END SELECT

CASE DEFAULT
END SELECT
END PROCEDURE obj_Add4

!----------------------------------------------------------------------------
!                                                           addContribution
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Add5
INTEGER(I4B) :: i, j, acase, tsize
LOGICAL(LGT) :: isok

acase = IACHAR(op)

SELECT CASE (acase)

CASE (43)

  SELECT CASE (extraOption)

  CASE (MATRIX_DIAGONAL)
    ! diagonal
    tsize = SIZE(indx)

    DO j = 1, tsize

      isok = indx(j) .LT. 0
      IF (isok) THEN
        tsize = SIZE(obj%val, 2) + indx(j)
        DO i = 1, tsize
          obj%val(i - indx(j), i) = obj%val(i - indx(j), i) &
                                    + scale * val(i, j)
        END DO

      ELSE

        tsize = SIZE(obj%val, 1) - indx(j)
        DO i = 1, tsize
          obj%val(i, i + indx(j)) = obj%val(i, i + indx(j)) + &
                                    scale * val(i, j)
        END DO
      END IF
    END DO

  CASE (MATRIX_ROW)
    ! row
    tsize = SIZE(indx)

    DO j = 1, tsize
      obj%val(indx(j), :) = obj%val(indx(j), :) + scale * val(j, :)
    END DO

  CASE (MATRIX_COLUMN)
    ! col
    tsize = SIZE(indx)
    DO j = 1, tsize
      obj%val(:, indx(j)) = obj%val(:, indx(j)) + scale * val(:, j)
    END DO

  CASE DEFAULT

  END SELECT

CASE (45)
  ! -
  SELECT CASE (extraOption)
  CASE (MATRIX_DIAGONAL)

    ! diagonal
    tsize = SIZE(indx)
    DO j = 1, tsize

      isok = indx(j) .LT. 0
      IF (isok) THEN

        tsize = SIZE(obj%val, 2) + indx(j)
        DO i = 1, tsize
          obj%val(i - indx(j), i) = obj%val(i - indx(j), i) &
                                    - scale * val(i, j)
        END DO

      ELSE

        tsize = SIZE(obj%val, 1) - indx(j)
        DO i = 1, tsize
          obj%val(i, i + indx(j)) = obj%val(i, i + indx(j)) - &
                                    scale * val(i, j)
        END DO

      END IF
    END DO

  CASE (MATRIX_ROW)

    ! row
    tsize = SIZE(indx)
    DO j = 1, tsize
      obj%val(indx(j), :) = obj%val(indx(j), :) - &
                            scale * val(j, :)
    END DO

  CASE (MATRIX_COLUMN)

    ! col
    tsize = SIZE(indx)
    DO j = 1, tsize
      obj%val(:, indx(j)) = obj%val(:, indx(j)) - &
                            scale * val(:, j)
    END DO

  CASE DEFAULT

  END SELECT

CASE (42)
  ! *
  SELECT CASE (extraOption)

  CASE (MATRIX_DIAGONAL)
    ! diagonal
    tsize = SIZE(indx)
    DO j = 1, tsize

      isok = indx(j) .LT. 0
      IF (isok) THEN
        tsize = SIZE(obj%val, 2) + indx(j)
        DO i = 1, tsize
          obj%val(i - indx(j), i) = obj%val(i - indx(j), i) * &
                                    scale * val(i, j)
        END DO

      ELSE

        tsize = SIZE(obj%val, 1) - indx(j)
        DO i = 1, tsize
          obj%val(i, i + indx(j)) = obj%val(i, i + indx(j)) * &
                                    scale * val(i, j)
        END DO

      END IF

    END DO

  CASE (MATRIX_ROW)

    ! row
    tsize = SIZE(indx)
    DO j = 1, tsize
      obj%val(indx(j), :) = obj%val(indx(j), :) * &
                            scale * val(j, :)
    END DO

  CASE (MATRIX_COLUMN)

    ! col
    tsize = SIZE(indx)
    DO j = 1, tsize
      obj%val(:, indx(j)) = obj%val(:, indx(j)) * &
                            scale * val(:, j)
    END DO

  CASE DEFAULT

  END SELECT

CASE (47)
  ! /
  SELECT CASE (extraOption)
  CASE (MATRIX_DIAGONAL)

    ! diagonal
    tsize = SIZE(indx)
    DO j = 1, tsize

      isok = indx(j) .LT. 0
      IF (isok) THEN
        tsize = SIZE(obj%val, 2) + indx(j)
        DO i = 1, tsize
          obj%val(i - indx(j), i) = obj%val(i - indx(j), i) / &
                                    scale / val(i, j)
        END DO

      ELSE

        tsize = SIZE(obj%val, 1) - indx(j)
        DO i = 1, tsize
          obj%val(i, i + indx(j)) = obj%val(i, i + indx(j)) / &
                                    scale / val(i, j)
        END DO

      END IF

    END DO

  CASE (MATRIX_ROW)

    ! row
    tsize = SIZE(indx)
    DO j = 1, tsize
      obj%val(indx(j), :) = obj%val(indx(j), :) / &
                            scale / val(j, :)
    END DO

  CASE (MATRIX_COLUMN)

    ! col
    tsize = SIZE(indx)
    DO j = 1, tsize
      obj%val(:, indx(j)) = obj%val(:, indx(j)) / &
                            scale / val(:, j)
    END DO

  CASE DEFAULT

  END SELECT

CASE DEFAULT
END SELECT
END PROCEDURE obj_Add5

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE AddMethods
