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

SUBMODULE( RealMatrix_Method ) setValues
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                 setValues
!----------------------------------------------------------------------------

MODULE PROCEDURE realmat_setValues_1
  obj%Val = Val
END PROCEDURE realmat_setValues_1

!----------------------------------------------------------------------------
!                                                                 setValues
!----------------------------------------------------------------------------

MODULE PROCEDURE realmat_setValues_2
  obj%Val( Row, Col ) = Val
END PROCEDURE realmat_setValues_2

!----------------------------------------------------------------------------
!                                                                 setValues
!----------------------------------------------------------------------------

MODULE PROCEDURE realmat_setValues_3
  obj%Val( Row, Col ) = Val
END PROCEDURE realmat_setValues_3

!----------------------------------------------------------------------------
!                                                                 setValues
!----------------------------------------------------------------------------

MODULE PROCEDURE realmat_setValues_4
  INTEGER( I4B ) :: i
  !
  SELECT CASE( ExtraOption )
  CASE( MATRIX_DIAGONAL )
    ! diagonal
    IF( Indx .LT. 0 ) THEN
      DO i = 1, SIZE( obj%Val, 2 ) + Indx
        obj%Val( i-Indx, i ) = Val( i )
      END DO
    ELSE
      DO i = 1, SIZE( obj%Val, 1 ) - Indx
        obj%Val( i, i+Indx ) = Val( i )
      END DO
    END IF
  CASE( MATRIX_ROW )
    ! row
    IF( Indx .LE. SIZE( obj%Val, 1 ) ) THEN
      obj%Val( Indx, 1:SIZE( Val ) ) = Val
    END IF
  CASE( MATRIX_COLUMN )
    IF( Indx .LE. SIZE( obj%Val, 2 ) ) THEN
      obj%Val( 1:SIZE( Val ), Indx ) = Val
    END IF
  END SELECT
END PROCEDURE realmat_setValues_4

!----------------------------------------------------------------------------
!                                                                  setValues
!----------------------------------------------------------------------------

MODULE PROCEDURE realmat_setValues_5
  INTEGER( I4B ) :: i, j
  !
  SELECT CASE( ExtraOption )
  CASE( MATRIX_DIAGONAL )
    ! diagonal
    DO j = 1, SIZE( Indx )
      IF( Indx( j ) .LT. 0 ) THEN
        DO i = 1, SIZE( obj%Val, 2 ) + Indx( j )
          obj%Val( i-Indx( j ), i ) = Val( i, j )
        END DO
      ELSE
        DO i = 1, SIZE( obj%Val, 1 ) - Indx( j )
          obj%Val( i, i+Indx( j ) ) = Val( i, j )
        END DO
      END IF
    END DO
  CASE( MATRIX_ROW )
    ! row
    DO j = 1, SIZE( Indx )
      obj%Val( Indx( j ), : ) = Val( j, : )
    END DO
  CASE( MATRIX_COLUMN )
    ! col
    DO j = 1, SIZE( Indx )
      obj%Val( :, Indx( j ) ) = Val( :, j )
    END DO
  END SELECT
END PROCEDURE realmat_setValues_5

!----------------------------------------------------------------------------
!                                                            addContribution
!----------------------------------------------------------------------------

MODULE PROCEDURE realmat_addVal_1
  SELECT CASE( IACHAR( Op ) )
  CASE( 43 )
    ! +
    obj%Val = obj%Val + Scale * Val
  CASE( 45 )
    ! -
    obj%Val = obj%Val - Scale * Val
  CASE( 42 )
    ! *
    obj%Val = Scale * (obj%Val  * Val)
  CASE( 47 )
    ! /
    obj%Val = ( obj%Val / Val ) / Scale
  END SELECT
END PROCEDURE realmat_addVal_1

!----------------------------------------------------------------------------
!                                                            addContribution
!----------------------------------------------------------------------------

MODULE PROCEDURE realmat_addVal_2
  SELECT CASE( IACHAR( Op ) )
  CASE( 43 )
    ! +
    obj%Val( Row, Col ) = obj%Val( Row, Col ) + Scale * Val
  CASE( 45 )
    ! -
    obj%Val( Row, Col ) = obj%Val( Row, Col ) - Scale * Val
  CASE( 42 )
    ! *
    obj%Val( Row, Col ) = Scale * obj%Val( Row, Col ) * Val
  CASE( 47 )
    ! /
    obj%Val( Row, Col ) = obj%Val( Row, Col ) / Val / Scale
  END SELECT
END PROCEDURE realmat_addVal_2

!----------------------------------------------------------------------------
!                                                           realmat_addVal_3
!----------------------------------------------------------------------------

MODULE PROCEDURE realmat_addVal_3
  SELECT CASE( IACHAR( Op ) )
  CASE( 43 )
    ! +
    obj%Val( Row, Col ) = obj%Val( Row, Col ) + Scale * Val
  CASE( 45 )
    ! -
    obj%Val( Row, Col ) = obj%Val( Row, Col ) - Scale * Val
  CASE( 42 )
    ! *
    obj%Val( Row, Col ) = Scale * obj%Val( Row, Col ) * Val
  CASE( 47 )
    ! /
    obj%Val( Row, Col ) = obj%Val( Row, Col ) / Val / Scale
  END SELECT
END PROCEDURE realmat_addVal_3

!----------------------------------------------------------------------------
!                                                           realmat_addVal_4
!----------------------------------------------------------------------------

MODULE PROCEDURE realmat_addVal_4
  INTEGER( I4B ) :: i
  SELECT CASE( IACHAR( Op ) )
  CASE( 43 )
    SELECT CASE( ExtraOption )
    CASE( MATRIX_DIAGONAL )
      ! diagonal
      IF( Indx .LT. 0 ) THEN
        DO i = 1, SIZE( obj%Val, 2 ) + Indx
          obj%Val( i-Indx, i ) = obj%Val( i-Indx, i ) + Scale * Val( i )
        END DO
      ELSE
        DO i = 1, SIZE( obj%Val, 1 ) - Indx
          obj%Val( i, i+Indx ) = obj%Val( i, i+Indx ) + Scale * Val( i )
        END DO
      END IF
    CASE( MATRIX_ROW )
      ! row
      IF( Indx .LE. SIZE( obj%Val, 1 ) ) THEN
        obj%Val( Indx, 1:SIZE( Val ) ) = obj%Val( Indx, 1:SIZE( Val ) ) &
          & + Scale * Val
      END IF
    CASE( MATRIX_COLUMN )
      IF( Indx .LE. SIZE( obj%Val, 2 ) ) THEN
        obj%Val( 1:SIZE( Val ), Indx ) = obj%Val( 1:SIZE( Val ), Indx ) &
          & + Scale * Val
      END IF
    END SELECT
  CASE( 45 )
    SELECT CASE( ExtraOption )
    CASE( 0 )
      ! diagonal
      IF( Indx .LT. 0 ) THEN
        DO i = 1, SIZE( obj%Val, 2 ) + Indx
          obj%Val( i-Indx, i ) = obj%Val( i-Indx, i ) - Scale * Val( i )
        END DO
      ELSE
        DO i = 1, SIZE( obj%Val, 1 ) - Indx
          obj%Val( i, i+Indx ) = obj%Val( i, i+Indx ) - Scale * Val( i )
        END DO
      END IF
    CASE( 1 )
      ! row
      IF( Indx .LE. SIZE( obj%Val, 1 ) ) THEN
        obj%Val( Indx, 1:SIZE( Val ) ) = obj%Val( Indx, 1:SIZE( Val ) ) &
          & - Scale * Val
      END IF
    CASE( 2 )
      IF( Indx .LE. SIZE( obj%Val, 2 ) ) THEN
        obj%Val( 1:SIZE( Val ), Indx ) = obj%Val( 1:SIZE( Val ), Indx ) &
          & - Scale * Val
      END IF
    END SELECT
  CASE( 42 )
    SELECT CASE( ExtraOption )
    CASE( MATRIX_DIAGONAL )
      ! diagonal
      IF( Indx .LT. 0 ) THEN
        DO i = 1, SIZE( obj%Val, 2 ) + Indx
          obj%Val( i-Indx, i ) = obj%Val( i-Indx, i ) * Scale * Val( i )
        END DO
      ELSE
        DO i = 1, SIZE( obj%Val, 1 ) - Indx
          obj%Val( i, i+Indx ) = obj%Val( i, i+Indx ) * Scale * Val( i )
        END DO
      END IF
    CASE( MATRIX_ROW )
      ! row
      IF( Indx .LE. SIZE( obj%Val, 1 ) ) THEN
        obj%Val( Indx, 1:SIZE( Val ) ) = obj%Val( Indx, 1:SIZE( Val ) ) &
          & * Scale * Val
      END IF
    CASE( MATRIX_COLUMN )
      IF( Indx .LE. SIZE( obj%Val, 2 ) ) THEN
        obj%Val( 1:SIZE( Val ), Indx ) = obj%Val( 1:SIZE( Val ), Indx ) &
          & * Scale * Val
      END IF
    END SELECT
  CASE( 47 )
    SELECT CASE( ExtraOption )
    CASE( MATRIX_DIAGONAL )
      ! diagonal
      IF( Indx .LT. 0 ) THEN
        DO i = 1, SIZE( obj%Val, 2 ) + Indx
          obj%Val( i-Indx, i ) = obj%Val( i-Indx, i ) / Scale / Val( i )
        END DO
      ELSE
        DO i = 1, SIZE( obj%Val, 1 ) - Indx
          obj%Val( i, i+Indx ) = obj%Val( i, i+Indx ) / Scale / Val( i )
        END DO
      END IF
    CASE( MATRIX_ROW )
      ! row
      IF( Indx .LE. SIZE( obj%Val, 1 ) ) THEN
        obj%Val( Indx, 1:SIZE( Val ) ) = obj%Val( Indx, 1:SIZE( Val ) ) &
          & / Scale / Val
      END IF
    CASE( MATRIX_COLUMN )
      IF( Indx .LE. SIZE( obj%Val, 2 ) ) THEN
        obj%Val( 1:SIZE( Val ), Indx ) = obj%Val( 1:SIZE( Val ), Indx ) &
          & / Scale / Val
      END IF
    END SELECT
  END SELECT
END PROCEDURE realmat_addVal_4

!----------------------------------------------------------------------------
!                                                           addContribution
!----------------------------------------------------------------------------

MODULE PROCEDURE realmat_addVal_5
  INTEGER( I4B ) :: i, j
  !
  SELECT CASE( IACHAR( Op ) )
  CASE( 43 )
    SELECT CASE( ExtraOption )
    CASE( MATRIX_DIAGONAL )
      ! diagonal
      DO j = 1, SIZE( Indx )
        IF( Indx( j ) .LT. 0 ) THEN
          DO i = 1, SIZE( obj%Val, 2 ) + Indx( j )
            obj%Val( i-Indx( j ), i ) = obj%Val( i-Indx( j ), i ) &
              & + Scale * Val( i, j )
          END DO
        ELSE
          DO i = 1, SIZE( obj%Val, 1 ) - Indx( j )
            obj%Val( i, i+Indx( j ) ) = obj%Val( i, i+Indx( j ) ) + &
              & Scale * Val( i, j )
          END DO
        END IF
      END DO
    CASE( MATRIX_ROW )
      ! row
      DO j = 1, SIZE( Indx )
        obj%Val( Indx( j ), : ) = obj%Val( Indx( j ), : ) + &
          & Scale * Val( j, : )
      END DO
    CASE( MATRIX_COLUMN )
      ! col
      DO j = 1, SIZE( Indx )
        obj%Val( :, Indx( j ) ) = obj%Val( :, Indx( j ) ) + &
          & Scale  * Val( :, j )
      END DO
    END SELECT
  CASE( 45 )
    ! -
    SELECT CASE( ExtraOption )
    CASE( MATRIX_DIAGONAL )
      ! diagonal
      DO j = 1, SIZE( Indx )
        IF( Indx( j ) .LT. 0 ) THEN
          DO i = 1, SIZE( obj%Val, 2 ) + Indx( j )
            obj%Val( i-Indx( j ), i ) = obj%Val( i-Indx( j ), i ) &
              & - Scale * Val( i, j )
          END DO
        ELSE
          DO i = 1, SIZE( obj%Val, 1 ) - Indx( j )
            obj%Val( i, i+Indx( j ) ) = obj%Val( i, i+Indx( j ) ) - &
              & Scale * Val( i, j )
          END DO
        END IF
      END DO
    CASE( MATRIX_ROW )
      ! row
      DO j = 1, SIZE( Indx )
        obj%Val( Indx( j ), : ) = obj%Val( Indx( j ), : ) - &
          & Scale * Val( j, : )
      END DO
    CASE( MATRIX_COLUMN )
      ! col
      DO j = 1, SIZE( Indx )
        obj%Val( :, Indx( j ) ) = obj%Val( :, Indx( j ) ) - &
          & Scale  * Val( :, j )
      END DO
    END SELECT
  CASE( 42 )
    ! *
    SELECT CASE( ExtraOption )
    CASE( MATRIX_DIAGONAL )
      ! diagonal
      DO j = 1, SIZE( Indx )
        IF( Indx( j ) .LT. 0 ) THEN
          DO i = 1, SIZE( obj%Val, 2 ) + Indx( j )
            obj%Val( i-Indx( j ), i ) = obj%Val( i-Indx( j ), i ) * &
              & Scale * Val( i, j )
          END DO
        ELSE
          DO i = 1, SIZE( obj%Val, 1 ) - Indx( j )
            obj%Val( i, i+Indx( j ) ) = obj%Val( i, i+Indx( j ) ) * &
              & Scale * Val( i, j )
          END DO
        END IF
      END DO
    CASE( MATRIX_ROW )
      ! row
      DO j = 1, SIZE( Indx )
        obj%Val( Indx( j ), : ) = obj%Val( Indx( j ), : ) * &
          & Scale * Val( j, : )
      END DO
    CASE( MATRIX_COLUMN )
      ! col
      DO j = 1, SIZE( Indx )
        obj%Val( :, Indx( j ) ) = obj%Val( :, Indx( j ) ) * &
          & Scale  * Val( :, j )
      END DO
    END SELECT
  CASE( 47 )
    ! /
    SELECT CASE( ExtraOption )
    CASE( MATRIX_DIAGONAL )
      ! diagonal
      DO j = 1, SIZE( Indx )
        IF( Indx( j ) .LT. 0 ) THEN
          DO i = 1, SIZE( obj%Val, 2 ) + Indx( j )
            obj%Val( i-Indx( j ), i ) = obj%Val( i-Indx( j ), i ) / &
              & Scale / Val( i, j )
          END DO
        ELSE
          DO i = 1, SIZE( obj%Val, 1 ) - Indx( j )
            obj%Val( i, i+Indx( j ) ) = obj%Val( i, i+Indx( j ) ) / &
              & Scale / Val( i, j )
          END DO
        END IF
      END DO
    CASE( MATRIX_ROW )
      ! row
      DO j = 1, SIZE( Indx )
        obj%Val( Indx( j ), : ) = obj%Val( Indx( j ), : ) / &
          & Scale / Val( j, : )
      END DO
    CASE( MATRIX_COLUMN )
      ! col
      DO j = 1, SIZE( Indx )
        obj%Val( :, Indx( j ) ) = obj%Val( :, Indx( j ) ) / &
          & Scale / Val( :, j )
      END DO
    END SELECT
  END SELECT
END PROCEDURE realmat_addVal_5

END SUBMODULE setValues