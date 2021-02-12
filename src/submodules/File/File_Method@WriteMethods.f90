SUBMODULE(File_Method) WriteMethods
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                 WriteData
!----------------------------------------------------------------------------

MODULE PROCEDURE write_data_ascii_scalar
  SELECT TYPE( Val )
  TYPE IS( CHARACTER( * ))
    WRITE( Obj%UnitNo, "(A)") TRIM(Val)
  TYPE IS( Real(Real64) )
    WRITE( Obj%UnitNo, "(A)" ) STR(Val)
  TYPE IS( Real(Real32) )
    WRITE( Obj%UnitNo, "(A)" ) STR(Val)
  TYPE IS( Integer(INT32) )
    WRITE( Obj%UnitNo, "(A)" ) STR(Val)
  TYPE IS( Integer(INT64) )
    WRITE( Obj%UnitNo, "(A)" ) STR(Val)
  END SELECT
END PROCEDURE write_data_ascii_scalar

!----------------------------------------------------------------------------
!                                                                 WriteData
!----------------------------------------------------------------------------

MODULE PROCEDURE write_data_ascii_r1
  INTEGER( I4B ) :: ii

  SELECT TYPE( Val )
  TYPE IS ( Integer(Int32) )
    IF( PRESENT( col ) ) THEN
      DO ii =1, SIZE(Val)
        WRITE(Obj%UnitNo, "(A)") STR(Val( ii ))
      END DO
      RETURN
    END IF

    WRITE(Obj%UnitNo, "(A)") STR(Val, Separator = Obj%Separator)

  TYPE IS ( Integer(Int64) )
    IF( PRESENT( col ) ) THEN
      DO ii =1, SIZE(Val)
        WRITE(Obj%UnitNo, "(A)") STR(Val( ii ))
      END DO
      RETURN
    END IF

    WRITE(Obj%UnitNo, "(A)") STR(Val, Separator = Obj%Separator)

  TYPE IS ( Real(Real32) )
    IF( PRESENT( col ) ) THEN
      DO ii =1, SIZE(Val)
        WRITE(Obj%UnitNo, "(A)") STR(Val( ii ))
      END DO
      RETURN
    END IF

    WRITE(Obj%UnitNo, "(A)") STR(Val, Separator = Obj%Separator)

  TYPE IS ( Real(Real64) )
    IF( PRESENT( col ) ) THEN
      DO ii =1, SIZE(Val)
        WRITE(Obj%UnitNo, "(A)") STR(Val( ii ))
      END DO
      RETURN
    END IF

    WRITE(Obj%UnitNo, "(A)") STR(Val, Separator = Obj%Separator)
  END SELECT

END PROCEDURE write_data_ascii_r1

!----------------------------------------------------------------------------
!                                                                 WriteData
!----------------------------------------------------------------------------

MODULE PROCEDURE write_data_ascii_r2
  INTEGER( I4B ) :: ii, jj, m, n

  SELECT TYPE( Val )

  TYPE IS ( Integer(Int32) )
    ! print column wise
    m = SIZE( Val, 1 )
    n = SIZE( Val, 2 )
    IF(transpose) THEN

      DO jj=1,n
        DO ii = 1, m
          IF( ii .EQ. m ) THEN
            WRITE(Obj%UnitNo, "(A)", ADVANCE="YES" ) TRIM(STR(val( ii, jj )))
          ELSE
            WRITE(Obj%UnitNo, "(A)", ADVANCE="NO" ) &
              & TRIM(STR(val( ii, jj ))) // Obj%Separator
          END IF
        END DO
      END DO

    ELSE
      ! print rowwise
      DO ii =1, m
        DO jj=1,n
          IF( jj .EQ. n ) THEN
            WRITE(Obj%UnitNo, "(A)", ADVANCE="YES" ) TRIM(STR(val( ii, jj )))
          ELSE
            WRITE(Obj%UnitNo, "(A)", ADVANCE="NO" ) &
              & TRIM(STR(val( ii, jj ))) // Obj%Separator
          END IF
        END DO
      END DO
    END IF

  TYPE IS ( Integer(Int64) )
    ! print column wise
    m = SIZE( Val, 1 )
    n = SIZE( Val, 2 )
    IF(transpose) THEN

      DO jj=1,n
        DO ii = 1, m
          IF( ii .EQ. m ) THEN
            WRITE(Obj%UnitNo, "(A)", ADVANCE="YES" ) TRIM(STR(val( ii, jj )))
          ELSE
            WRITE(Obj%UnitNo, "(A)", ADVANCE="NO" ) &
              & TRIM(STR(val( ii, jj ))) // Obj%Separator
          END IF
        END DO
      END DO

    ELSE
      ! print rowwise
      DO ii =1, m
        DO jj=1,n
          IF( jj .EQ. n ) THEN
            WRITE(Obj%UnitNo, "(A)", ADVANCE="YES" ) TRIM(STR(val( ii, jj )))
          ELSE
            WRITE(Obj%UnitNo, "(A)", ADVANCE="NO" ) &
              & TRIM(STR(val( ii, jj ))) // Obj%Separator
          END IF
        END DO
      END DO
    END IF

  TYPE IS ( Real(Real32) )
    ! print column wise
    m = SIZE( Val, 1 )
    n = SIZE( Val, 2 )
    IF(transpose) THEN

      DO jj=1,n
        DO ii = 1, m
          IF( ii .EQ. m ) THEN
            WRITE(Obj%UnitNo, "(A)", ADVANCE="YES" ) TRIM(STR(val( ii, jj )))
          ELSE
            WRITE(Obj%UnitNo, "(A)", ADVANCE="NO" ) &
              & TRIM(STR(val( ii, jj ))) // Obj%Separator
          END IF
        END DO
      END DO

    ELSE
      ! print rowwise
      DO ii =1, m
        DO jj=1,n
          IF( jj .EQ. n ) THEN
            WRITE(Obj%UnitNo, "(A)", ADVANCE="YES" ) TRIM(STR(val( ii, jj )))
          ELSE
            WRITE(Obj%UnitNo, "(A)", ADVANCE="NO" ) &
              & TRIM(STR(val( ii, jj ))) // Obj%Separator
          END IF
        END DO
      END DO
    END IF

  TYPE IS ( Real(Real64) )
    ! print column wise
    m = SIZE( Val, 1 )
    n = SIZE( Val, 2 )
    IF(transpose) THEN

      DO jj=1,n
        DO ii = 1, m
          IF( ii .EQ. m ) THEN
            WRITE(Obj%UnitNo, "(A)", ADVANCE="YES" ) TRIM(STR(val( ii, jj )))
          ELSE
            WRITE(Obj%UnitNo, "(A)", ADVANCE="NO" ) &
              & TRIM(STR(val( ii, jj ))) // Obj%Separator
          END IF
        END DO
      END DO

    ELSE
      ! print rowwise
      DO ii =1, m
        DO jj=1,n
          IF( jj .EQ. n ) THEN
            WRITE(Obj%UnitNo, "(A)", ADVANCE="YES" ) TRIM(STR(val( ii, jj )))
          ELSE
            WRITE(Obj%UnitNo, "(A)", ADVANCE="NO" ) &
              & TRIM(STR(val( ii, jj ))) // Obj%Separator
          END IF
        END DO
      END DO
    END IF
  END SELECT

END PROCEDURE write_data_ascii_r2

!----------------------------------------------------------------------------
!                                                                 WriteLine
!----------------------------------------------------------------------------

MODULE PROCEDURE writeLine_a
  INTEGER( I4B ) :: iunit, istat

  IF( PRESENT( unitNo ) ) THEN
    iunit = unitNo
  ELSE
    iunit = stdout
  END IF

  WRITE( iunit, '(a)' ) STR( a )

END PROCEDURE writeLine_a

!----------------------------------------------------------------------------
!                                                                 WriteLine
!----------------------------------------------------------------------------

MODULE PROCEDURE writeLine_ab
  INTEGER( I4B ) :: iunit, istat

  IF( PRESENT( unitNo ) ) THEN
    iunit = unitNo
  ELSE
    iunit = stdout
  END IF

  WRITE( iunit, '(a)' ) STR( a ) // STR( b )

END PROCEDURE writeLine_ab

!----------------------------------------------------------------------------
!                                                                 WriteLine
!----------------------------------------------------------------------------

MODULE PROCEDURE writeLine_abc
  INTEGER( I4B ) :: iunit, istat

  IF( PRESENT( unitNo ) ) THEN
    iunit = unitNo
  ELSE
    iunit = stdout
  END IF

  WRITE( iunit, '(a)' ) STR( a ) // STR( b ) // STR( c )

END PROCEDURE writeLine_abc

!----------------------------------------------------------------------------
!                                                                 WriteLine
!----------------------------------------------------------------------------

MODULE PROCEDURE writeLine_abcd
  INTEGER( I4B ) :: iunit, istat

  IF( PRESENT( unitNo ) ) THEN
    iunit = unitNo
  ELSE
    iunit = stdout
  END IF

  WRITE( iunit, '(a)' ) STR( a ) // STR( b ) // STR( c ) // STR( d )

END PROCEDURE writeLine_abcd

!----------------------------------------------------------------------------
!                                                                 WriteLine
!----------------------------------------------------------------------------

MODULE PROCEDURE writeLine_abcde
  INTEGER( I4B ) :: iunit, istat

  IF( PRESENT( unitNo ) ) THEN
    iunit = unitNo
  ELSE
    iunit = stdout
  END IF

  WRITE( iunit, '(a)' ) STR( a ) // STR( b ) // STR( c ) // STR( d ) // STR(e)

END PROCEDURE writeLine_abcde

!----------------------------------------------------------------------------
!                                                                 WriteLine
!----------------------------------------------------------------------------

MODULE PROCEDURE writeLine_av
  INTEGER( I4B ) :: iunit, istat

  IF( PRESENT( unitNo ) ) THEN
    iunit = unitNo
  ELSE
    iunit = stdout
  END IF

  WRITE( iunit, '(a)' ) STR( a )

END PROCEDURE writeLine_av

!----------------------------------------------------------------------------
!                                                                 WriteLine
!----------------------------------------------------------------------------

MODULE PROCEDURE writeLine_avbv
  INTEGER( I4B ) :: iunit, istat

  IF( PRESENT( unitNo ) ) THEN
    iunit = unitNo
  ELSE
    iunit = stdout
  END IF

  WRITE( iunit, '(a)' ) STR( a ) // STR( b )

END PROCEDURE writeLine_avbv

!----------------------------------------------------------------------------
!                                                                 WriteLine
!----------------------------------------------------------------------------

MODULE PROCEDURE writeLine_avbvcv
  INTEGER( I4B ) :: iunit, istat

  IF( PRESENT( unitNo ) ) THEN
    iunit = unitNo
  ELSE
    iunit = stdout
  END IF

  WRITE( iunit, '(a)' ) STR( a ) // STR( b ) // STR( c )

END PROCEDURE writeLine_avbvcv


END SUBMODULE WriteMethods