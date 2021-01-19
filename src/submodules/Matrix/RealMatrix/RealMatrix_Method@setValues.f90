SUBMODULE( RealMatrix_Method ) setValues
USE BaseMethod
IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                                 setValues
!----------------------------------------------------------------------------

MODULE PROCEDURE realmat_setValues_1
  Obj % Val = Val
END PROCEDURE realmat_setValues_1

!----------------------------------------------------------------------------
!                                                                 setValues
!----------------------------------------------------------------------------

MODULE PROCEDURE realmat_setValues_2
  Obj % Val( Row, Col ) = Val
END PROCEDURE realmat_setValues_2

!----------------------------------------------------------------------------
!                                                                 setValues
!----------------------------------------------------------------------------

MODULE PROCEDURE realmat_setValues_3
  Obj % Val( Row, Col ) = Val
END PROCEDURE realmat_setValues_3

!----------------------------------------------------------------------------
!                                                                 setValues
!----------------------------------------------------------------------------

MODULE PROCEDURE realmat_setValues_4
  INTEGER( I4B ) :: i
  !
  SELECT CASE( ExtraOption )
  CASE( 0 )
    ! diagonal
    IF( Indx .LT. 0 ) THEN
      DO i = 1, SIZE( Obj % Val, 2 ) + Indx
        Obj % Val( i-Indx, i ) = Val( i )
      END DO
    ELSE
      DO i = 1, SIZE( Obj % Val, 1 ) - Indx
        Obj % Val( i, i+Indx ) = Val( i )
      END DO
    END IF
  CASE( 1 )
    ! row
    IF( Indx .LE. SIZE( Obj % Val, 1 ) ) THEN
      Obj % Val( Indx, 1:SIZE( Val ) ) = Val
    END IF
  CASE( 2 )
    IF( Indx .LE. SIZE( Obj % Val, 2 ) ) THEN
      Obj % Val( 1:SIZE( Val ), Indx ) = Val
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
  CASE( 0 )
    ! diagonal
    DO j = 1, SIZE( Indx )
      IF( Indx( j ) .LT. 0 ) THEN
        DO i = 1, SIZE( Obj % Val, 2 ) + Indx( j )
          Obj % Val( i-Indx( j ), i ) = Val( i, j )
        END DO
      ELSE
        DO i = 1, SIZE( Obj % Val, 1 ) - Indx( j )
          Obj % Val( i, i+Indx( j ) ) = Val( i, j )
        END DO
      END IF
    END DO
  CASE( 1 )
    ! row
    DO j = 1, SIZE( Indx )
      Obj % Val( Indx( j ), : ) = Val( j, : )
    END DO
  CASE( 2 )
    ! col
    DO j = 1, SIZE( Indx )
      Obj % Val( :, Indx( j ) ) = Val( :, j )
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
    Obj % Val = Obj % Val + Scale * Val
  CASE( 45 )
    ! -
    Obj % Val = Obj % Val - Scale * Val
  CASE( 42 )
    ! *
    Obj % Val = Scale * (Obj % Val  * Val)
  CASE( 47 )
    ! /
    Obj % Val = ( Obj % Val / Val ) / Scale
  END SELECT
END PROCEDURE realmat_addVal_1

!----------------------------------------------------------------------------
!                                                            addContribution
!----------------------------------------------------------------------------

MODULE PROCEDURE realmat_addVal_2
  SELECT CASE( IACHAR( Op ) )
  CASE( 43 )
    ! +
    Obj % Val( Row, Col ) = Obj % Val( Row, Col ) + Scale * Val
  CASE( 45 )
    ! -
    Obj % Val( Row, Col ) = Obj % Val( Row, Col ) - Scale * Val
  CASE( 42 )
    ! *
    Obj % Val( Row, Col ) = Scale * Obj % Val( Row, Col ) * Val
  CASE( 47 )
    ! /
    Obj % Val( Row, Col ) = Obj % Val( Row, Col ) / Val / Scale
  END SELECT
END PROCEDURE realmat_addVal_2

!----------------------------------------------------------------------------
!                                                           realmat_addVal_3
!----------------------------------------------------------------------------

MODULE PROCEDURE realmat_addVal_3
  SELECT CASE( IACHAR( Op ) )
  CASE( 43 )
    ! +
    Obj % Val( Row, Col ) = Obj % Val( Row, Col ) + Scale * Val
  CASE( 45 )
    ! -
    Obj % Val( Row, Col ) = Obj % Val( Row, Col ) - Scale * Val
  CASE( 42 )
    ! *
    Obj % Val( Row, Col ) = Scale * Obj % Val( Row, Col ) * Val
  CASE( 47 )
    ! /
    Obj % Val( Row, Col ) = Obj % Val( Row, Col ) / Val / Scale
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
    CASE( 0 )
      ! diagonal
      IF( Indx .LT. 0 ) THEN
        DO i = 1, SIZE( Obj % Val, 2 ) + Indx
          Obj % Val( i-Indx, i ) = Obj % Val( i-Indx, i ) + Scale * Val( i )
        END DO
      ELSE
        DO i = 1, SIZE( Obj % Val, 1 ) - Indx
          Obj % Val( i, i+Indx ) = Obj % Val( i, i+Indx ) + Scale * Val( i )
        END DO
      END IF
    CASE( 1 )
      ! row
      IF( Indx .LE. SIZE( Obj % Val, 1 ) ) THEN
        Obj % Val( Indx, 1:SIZE( Val ) ) = Obj % Val( Indx, 1:SIZE( Val ) ) &
          & + Scale * Val
      END IF
    CASE( 2 )
      IF( Indx .LE. SIZE( Obj % Val, 2 ) ) THEN
        Obj % Val( 1:SIZE( Val ), Indx ) = Obj % Val( 1:SIZE( Val ), Indx ) &
          & + Scale * Val
      END IF
    END SELECT
  CASE( 45 )
    SELECT CASE( ExtraOption )
    CASE( 0 )
      ! diagonal
      IF( Indx .LT. 0 ) THEN
        DO i = 1, SIZE( Obj % Val, 2 ) + Indx
          Obj % Val( i-Indx, i ) = Obj % Val( i-Indx, i ) - Scale * Val( i )
        END DO
      ELSE
        DO i = 1, SIZE( Obj % Val, 1 ) - Indx
          Obj % Val( i, i+Indx ) = Obj % Val( i, i+Indx ) - Scale * Val( i )
        END DO
      END IF
    CASE( 1 )
      ! row
      IF( Indx .LE. SIZE( Obj % Val, 1 ) ) THEN
        Obj % Val( Indx, 1:SIZE( Val ) ) = Obj % Val( Indx, 1:SIZE( Val ) ) &
          & - Scale * Val
      END IF
    CASE( 2 )
      IF( Indx .LE. SIZE( Obj % Val, 2 ) ) THEN
        Obj % Val( 1:SIZE( Val ), Indx ) = Obj % Val( 1:SIZE( Val ), Indx ) &
          & - Scale * Val
      END IF
    END SELECT
  CASE( 42 )
    SELECT CASE( ExtraOption )
    CASE( 0 )
      ! diagonal
      IF( Indx .LT. 0 ) THEN
        DO i = 1, SIZE( Obj % Val, 2 ) + Indx
          Obj % Val( i-Indx, i ) = Obj % Val( i-Indx, i ) * Scale * Val( i )
        END DO
      ELSE
        DO i = 1, SIZE( Obj % Val, 1 ) - Indx
          Obj % Val( i, i+Indx ) = Obj % Val( i, i+Indx ) * Scale * Val( i )
        END DO
      END IF
    CASE( 1 )
      ! row
      IF( Indx .LE. SIZE( Obj % Val, 1 ) ) THEN
        Obj % Val( Indx, 1:SIZE( Val ) ) = Obj % Val( Indx, 1:SIZE( Val ) ) &
          & * Scale * Val
      END IF
    CASE( 2 )
      IF( Indx .LE. SIZE( Obj % Val, 2 ) ) THEN
        Obj % Val( 1:SIZE( Val ), Indx ) = Obj % Val( 1:SIZE( Val ), Indx ) &
          & * Scale * Val
      END IF
    END SELECT
  CASE( 47 )
    SELECT CASE( ExtraOption )
    CASE( 0 )
      ! diagonal
      IF( Indx .LT. 0 ) THEN
        DO i = 1, SIZE( Obj % Val, 2 ) + Indx
          Obj % Val( i-Indx, i ) = Obj % Val( i-Indx, i ) / Scale / Val( i )
        END DO
      ELSE
        DO i = 1, SIZE( Obj % Val, 1 ) - Indx
          Obj % Val( i, i+Indx ) = Obj % Val( i, i+Indx ) / Scale / Val( i )
        END DO
      END IF
    CASE( 1 )
      ! row
      IF( Indx .LE. SIZE( Obj % Val, 1 ) ) THEN
        Obj % Val( Indx, 1:SIZE( Val ) ) = Obj % Val( Indx, 1:SIZE( Val ) ) &
          & / Scale / Val
      END IF
    CASE( 2 )
      IF( Indx .LE. SIZE( Obj % Val, 2 ) ) THEN
        Obj % Val( 1:SIZE( Val ), Indx ) = Obj % Val( 1:SIZE( Val ), Indx ) &
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
    CASE( 0 )
      ! diagonal
      DO j = 1, SIZE( Indx )
        IF( Indx( j ) .LT. 0 ) THEN
          DO i = 1, SIZE( Obj % Val, 2 ) + Indx( j )
            Obj % Val( i-Indx( j ), i ) = Obj % Val( i-Indx( j ), i ) &
              & + Scale * Val( i, j )
          END DO
        ELSE
          DO i = 1, SIZE( Obj % Val, 1 ) - Indx( j )
            Obj % Val( i, i+Indx( j ) ) = Obj % Val( i, i+Indx( j ) ) + &
              & Scale * Val( i, j )
          END DO
        END IF
      END DO
    CASE( 1 )
      ! row
      DO j = 1, SIZE( Indx )
        Obj % Val( Indx( j ), : ) = Obj % Val( Indx( j ), : ) + &
          & Scale * Val( j, : )
      END DO
    CASE( 2 )
      ! col
      DO j = 1, SIZE( Indx )
        Obj % Val( :, Indx( j ) ) = Obj % Val( :, Indx( j ) ) + &
          & Scale  * Val( :, j )
      END DO
    END SELECT
  CASE( 45 )
    ! -
    SELECT CASE( ExtraOption )
    CASE( 0 )
      ! diagonal
      DO j = 1, SIZE( Indx )
        IF( Indx( j ) .LT. 0 ) THEN
          DO i = 1, SIZE( Obj % Val, 2 ) + Indx( j )
            Obj % Val( i-Indx( j ), i ) = Obj % Val( i-Indx( j ), i ) &
              & - Scale * Val( i, j )
          END DO
        ELSE
          DO i = 1, SIZE( Obj % Val, 1 ) - Indx( j )
            Obj % Val( i, i+Indx( j ) ) = Obj % Val( i, i+Indx( j ) ) - &
              & Scale * Val( i, j )
          END DO
        END IF
      END DO
    CASE( 1 )
      ! row
      DO j = 1, SIZE( Indx )
        Obj % Val( Indx( j ), : ) = Obj % Val( Indx( j ), : ) - &
          & Scale * Val( j, : )
      END DO
    CASE( 2 )
      ! col
      DO j = 1, SIZE( Indx )
        Obj % Val( :, Indx( j ) ) = Obj % Val( :, Indx( j ) ) - &
          & Scale  * Val( :, j )
      END DO
    END SELECT
  CASE( 42 )
    ! *
    SELECT CASE( ExtraOption )
    CASE( 0 )
      ! diagonal
      DO j = 1, SIZE( Indx )
        IF( Indx( j ) .LT. 0 ) THEN
          DO i = 1, SIZE( Obj % Val, 2 ) + Indx( j )
            Obj % Val( i-Indx( j ), i ) = Obj % Val( i-Indx( j ), i ) * &
              & Scale * Val( i, j )
          END DO
        ELSE
          DO i = 1, SIZE( Obj % Val, 1 ) - Indx( j )
            Obj % Val( i, i+Indx( j ) ) = Obj % Val( i, i+Indx( j ) ) * &
              & Scale * Val( i, j )
          END DO
        END IF
      END DO
    CASE( 1 )
      ! row
      DO j = 1, SIZE( Indx )
        Obj % Val( Indx( j ), : ) = Obj % Val( Indx( j ), : ) * &
          & Scale * Val( j, : )
      END DO
    CASE( 2 )
      ! col
      DO j = 1, SIZE( Indx )
        Obj % Val( :, Indx( j ) ) = Obj % Val( :, Indx( j ) ) * &
          & Scale  * Val( :, j )
      END DO
    END SELECT
  CASE( 47 )
    ! /
    SELECT CASE( ExtraOption )
    CASE( 0 )
      ! diagonal
      DO j = 1, SIZE( Indx )
        IF( Indx( j ) .LT. 0 ) THEN
          DO i = 1, SIZE( Obj % Val, 2 ) + Indx( j )
            Obj % Val( i-Indx( j ), i ) = Obj % Val( i-Indx( j ), i ) / &
              & Scale / Val( i, j )
          END DO
        ELSE
          DO i = 1, SIZE( Obj % Val, 1 ) - Indx( j )
            Obj % Val( i, i+Indx( j ) ) = Obj % Val( i, i+Indx( j ) ) / &
              & Scale / Val( i, j )
          END DO
        END IF
      END DO
    CASE( 1 )
      ! row
      DO j = 1, SIZE( Indx )
        Obj % Val( Indx( j ), : ) = Obj % Val( Indx( j ), : ) / &
          & Scale / Val( j, : )
      END DO
    CASE( 2 )
      ! col
      DO j = 1, SIZE( Indx )
        Obj % Val( :, Indx( j ) ) = Obj % Val( :, Indx( j ) ) / &
          & Scale / Val( :, j )
      END DO
    END SELECT
  END SELECT
END PROCEDURE realmat_addVal_5

END SUBMODULE setValues