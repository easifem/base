
!----------------------------------------------------------------------------
!                                                        WriteLine@WriteData
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE writeLine_a( a, fileName, unitNo )
  REAL( DFP ), INTENT( IN ) :: a
  CHARACTER( LEN = * ), INTENT( IN ) :: fileName
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: unitno
END SUBROUTINE writeLine_a
END INTERFACE

!----------------------------------------------------------------------------
!                                                        WriteLine@WriteData
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE writeLine_ab( a, b, fileName, unitNo )
  REAL( DFP ), INTENT( IN ) :: a, b
  CHARACTER( LEN = * ), INTENT( IN ) :: fileName
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: unitno
END SUBROUTINE writeLine_ab
END INTERFACE

!----------------------------------------------------------------------------
!                                                        WriteLine@WriteData
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE writeLine_abc( a, b, c, fileName, unitNo )
  REAL( DFP ), INTENT( IN ) :: a, b, c
  CHARACTER( LEN = * ), INTENT( IN ) :: fileName
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: unitno
END SUBROUTINE writeLine_abc
END INTERFACE

!----------------------------------------------------------------------------
!                                                        WriteLine@WriteData
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE writeLine_abcd( a, b, c, d, fileName, unitNo )
  REAL( DFP ), INTENT( IN ) :: a, b, c, d
  CHARACTER( LEN = * ), INTENT( IN ) :: fileName
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: unitno
END SUBROUTINE writeLine_abcd
END INTERFACE

!----------------------------------------------------------------------------
!                                                        WriteLine@WriteData
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE writeLine_abcde( a, b, c, d, e, fileName, unitNo )
  REAL( DFP ), INTENT( IN ) :: a, b, c, d, e
  CHARACTER( LEN = * ), INTENT( IN ) :: fileName
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: unitno
END SUBROUTINE writeLine_abcde
END INTERFACE

!----------------------------------------------------------------------------
!                                                        WriteLine@WriteData
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE writeLine_av( a, fileName, unitNo )
  REAL( DFP ), INTENT( IN ) :: a( : )
  CHARACTER( LEN = * ), INTENT( IN ) :: fileName
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: unitno
END SUBROUTINE writeLine_av
END INTERFACE

!----------------------------------------------------------------------------
!                                                        WriteLine@WriteData
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE writeLine_avbv( a, b, fileName, unitNo )
  REAL( DFP ), INTENT( IN ) :: a( : ), b( : )
  CHARACTER( LEN = * ), INTENT( IN ) :: fileName
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: unitno
END SUBROUTINE writeLine_avbv
END INTERFACE

!----------------------------------------------------------------------------
!                                                        WriteLine@WriteData
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE writeLine_avbvcv( a, b, c, fileName, unitNo )
  REAL( DFP ), INTENT( IN ) :: a( : ), b( : ), c( : )
  CHARACTER( LEN = * ), INTENT( IN ) :: fileName
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: unitno
END SUBROUTINE writeLine_avbvcv
END INTERFACE

INTERFACE WriteLine
  MODULE PROCEDURE writeLine_a, writeLine_ab, writeLine_abc, writeLine_abcd,&
    & writeLine_abcde, writeLine_av, writeLine_avbv, writeLine_avbvcv
END INTERFACE WriteLine

PUBLIC :: WriteLine


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
