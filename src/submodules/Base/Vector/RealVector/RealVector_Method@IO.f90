SUBMODULE( RealVector_Method ) IO
USE BaseMethod
USE h5fortran
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                    Display
!----------------------------------------------------------------------------

MODULE PROCEDURE RealVectorDisplay
  INTEGER( I4B ) :: i, max_size
  type(hdf5_file) :: h5f
  type(File_) :: aFile
  INTEGER( I4B ) :: sizes(SIZE(Obj))
  REAL( DFP ) :: val( SIZE( Obj ) )

  DO i = 1, SIZE( Obj )
    sizes(i) = SIZE(Obj(i))
  END DO

  max_size = MAXVAL( sizes )

  IF( PRESENT( UnitNo ) ) THEN
    CALL Write_data( UnitNo )
    RETURN
  END IF

  IF( PRESENT( filename ) ) THEN
    SELECT CASE( TRIM( extension ) )
    CASE( '.hdf5' )
      call ExecuteCommand( 'mkdir -p '//trim(path), &
        & __FILE__ // "Line num :: " // TRIM(INT2STR(__LINE__)) &
        & // "  RealVectorDisplay()" )

      call h5f%initialize( &
        & filename= trim(path)//trim(filename)//trim(extension), &
        & status='new', action='w', comp_lvl=1)

      DO i = 1, SIZE(Obj)
        call h5f%write( '/' // TRIM(msg) // '/comp[' &
          & // TRIM(INT2STR(i)) // ']', Obj(i)%Val )
      END DO
      call h5f%finalize()

    CASE( '.txt' )
      CALL OpenFileToWrite(Obj=afile, filename=filename, path=path, &
        & extension='.txt')
      CALL Write_data( afile%UnitNo )
      CALL CloseFile(afile)

    CASE( '.md' )
      CALL Display( __FILE__, 'ERROR in File :: ' )
      CALL Display( __LINE__, '          in LINE :: ' )
      CALL Display( '        Message :: Cannot write to .txt file')
      STOP
    END SELECT

    RETURN

  END IF

  CALL Write_data( stdout )

  CONTAINS
  SUBROUTINE Write_data( unitno )
    INTEGER( I4B ), INTENT( IN ) :: unitno
    INTEGER( I4B ) :: i, j

    DO i = 1, max_size
      val = 0.0_DFP
      DO j = 1, SIZE( Obj )
        IF( i .LE. sizes( j ) ) val( j ) = Obj(j)%Val(i)
      END DO

      WRITE( UnitNo, * ) val

    END DO
  END SUBROUTINE

END PROCEDURE RealVectorDisplay

!----------------------------------------------------------------------------
!                                                                    Display
!----------------------------------------------------------------------------

MODULE PROCEDURE RealscalarDisplay
  IF( PRESENT( UnitNo ) ) THEN
    CALL Display(Vec=Obj%Val, UnitNo = UnitNo, msg=msg )
    RETURN
  END IF

  IF( PRESENT( filename ) ) THEN
    CALL Display(Vec=Obj%Val, msg=msg, filename=filename, &
      & extension=extension, path=path )
    RETURN
  END IF

  CALL Display(Vec=Obj%Val, msg=msg)

END PROCEDURE RealscalarDisplay

!----------------------------------------------------------------------------
!                                                                    Display
!----------------------------------------------------------------------------

MODULE PROCEDURE Display_Vector_Real
  INTEGER( I4B ) :: i
  type(hdf5_file) :: h5f

  IF( PRESENT( UnitNo ) ) THEN
    IF( LEN_TRIM( msg ) .NE. 0 ) THEN
      WRITE( UnitNo, "(A)" ) TRIM( msg )
    END IF

    CALL Write_data( UnitNo )
    RETURN
  END IF

  IF( PRESENT( filename ) ) THEN
    SELECT CASE( TRIM( extension ) )
    CASE( '.hdf5' )
      call ExecuteCommand( 'mkdir -p ' // trim(path), &
        & __FILE__ // "Line num :: " // TRIM(INT2STR(__LINE__)) &
        & // "  Display_Vector_Real()" )
      call h5f%initialize( &
        & filename= trim(path)//trim(filename)//trim(extension), &
        & status='new', action='w', comp_lvl=1)
      call h5f%write( '/' // TRIM(msg), Vec )
      call h5f%finalize()
    END SELECT
    RETURN
  END IF

  IF( LBOUND( vec, 1 ) .EQ. 1 ) THEN
  call disp( title = msg, &
    & x= vec, &
    & unit = I, &
    & style = 'underline & pad', &
    & orient = 'row' )
  ELSE
    call disp( title = msg, &
    & x= vec, &
    & unit = I, &
    & style = 'underline & pad', &
    & sep = ' ---> ', &
    & lbound = LBOUND( Vec ) )
  END IF

  CONTAINS
  SUBROUTINE Write_data( unitno )
    INTEGER( I4B ), INTENT( IN ) :: unitno
    INTEGER( I4B ) :: i
    DO i = 1, SIZE( Vec )
      WRITE( UnitNo, * ) Vec(i)
    END DO
  END SUBROUTINE

END PROCEDURE Display_Vector_Real

!----------------------------------------------------------------------------
!                                                                 ReadLine
!----------------------------------------------------------------------------

MODULE PROCEDURE readline_a
  INTEGER( I4B ) :: istat

  IF (present(buffer)) THEN
    READ(buffer,*,iostat=istat) a
  ELSE
    READ(unitNo,*,iostat=istat) a
    CALL FileError(istat=istat,fname=fileName,flg=IO_READ, File=__FILE__, &
      & Routine="readline_a()", Line=__LINE__ )
  ENDIF

END PROCEDURE readline_a

!----------------------------------------------------------------------------
!                                                                 ReadLine
!----------------------------------------------------------------------------

MODULE PROCEDURE readline_ab
  INTEGER( I4B ) :: istat

  IF (present(buffer)) THEN
    READ(buffer,*,iostat=istat) a, b
  ELSE
    READ(unitNo,*,iostat=istat) a, b
    CALL FileError(istat=istat,fname=fileName,flg=IO_READ, File=__FILE__, &
    & Routine="readline_ab()", Line=__LINE__ )
  ENDIF

END PROCEDURE readline_ab

!----------------------------------------------------------------------------
!                                                                 ReadLine
!----------------------------------------------------------------------------

MODULE PROCEDURE readline_abc
  INTEGER( I4B ) :: istat

  IF (present(buffer)) THEN
    READ(buffer,*,iostat=istat) a, b, c
  ELSE
    READ(unitNo,*,iostat=istat) a, b, c
    CALL FileError(istat=istat,fname=fileName,flg=IO_READ, File=__FILE__, &
    & Routine="readline_abc()", Line=__LINE__ )
  ENDIF

END PROCEDURE readline_abc

!----------------------------------------------------------------------------
!                                                                 ReadLine
!----------------------------------------------------------------------------

MODULE PROCEDURE readline_abcd
  INTEGER( I4B ) :: istat

  IF (present(buffer)) THEN
    READ(buffer,*,iostat=istat) a, b, c, d
  ELSE
    READ(unitNo,*,iostat=istat) a, b, c, d
    CALL FileError(istat=istat,fname=fileName,flg=IO_READ, File=__FILE__, &
    & Routine="readline_abcd()", Line=__LINE__ )
  ENDIF
END PROCEDURE readline_abcd

!----------------------------------------------------------------------------
!                                                                 ReadLine
!----------------------------------------------------------------------------

MODULE PROCEDURE readline_abcde
  INTEGER( I4B ) :: istat

  IF (present(buffer)) THEN
    READ(buffer,*,iostat=istat) a, b, c, d, e
  ELSE
    READ(unitNo,*,iostat=istat) a, b, c, d, e
    CALL FileError(istat=istat,fname=fileName,flg=IO_READ, File=__FILE__, &
    & Routine="readline_abcde()", Line=__LINE__ )
  ENDIF
END PROCEDURE readline_abcde

!----------------------------------------------------------------------------
!                                                                 ReadLine
!----------------------------------------------------------------------------

MODULE PROCEDURE readline_av
  INTEGER( I4B ) :: i,Na,istat

  Na=size(a)
  IF ( PRESENT(buffer) ) THEN
    READ( buffer,*,iostat=istat ) ( a(i),i=1,Na )
  ELSE
    READ( unitNo,*,iostat=istat ) ( a(i),i=1,Na )
    CALL FileError(istat=istat,fname=fileName,flg=IO_READ, File=__FILE__, &
      & Routine="readline_av()", Line=__LINE__ )
  ENDIF
END PROCEDURE readline_av

!----------------------------------------------------------------------------
!                                                                 ReadLine
!----------------------------------------------------------------------------

MODULE PROCEDURE readline_avbv
  INTEGER( I4B ) :: i,Na,istat, Nb

  Na=size(a); Nb=size(b)
  IF ( PRESENT(buffer) ) THEN
    READ( buffer,*,iostat=istat ) ( a(i),i=1,Na ) ( b(i),i=1,Nb )
  ELSE
    READ( unitNo,*,iostat=istat ) ( a(i),i=1,Na ) ( b(i),i=1,Nb )
    CALL FileError(istat=istat,fname=fileName,flg=IO_READ, File=__FILE__, &
      & Routine="readline_avbv()", Line=__LINE__ )
  ENDIF
END PROCEDURE readline_avbv

!----------------------------------------------------------------------------
!                                                                 ReadLine
!----------------------------------------------------------------------------

MODULE PROCEDURE readline_avbvcv
  INTEGER( I4B ) :: i,Na,istat, Nb, Nc

  Na=size(a); Nb=size(b); Nc=size(c)
  IF ( PRESENT(buffer) ) THEN
    READ( buffer,*,iostat=istat ) ( a(i),i=1,Na ) ( b(i),i=1,Nb ) &
      & ( c(i),i=1,Nc )
  ELSE
    READ( unitNo,*,iostat=istat ) ( a(i),i=1,Na ) ( b(i),i=1,Nb ) &
      & ( c(i),i=1,Nc )
    CALL FileError(istat=istat,fname=fileName,flg=IO_READ, File=__FILE__, &
      & Routine="readline_avbvcv()", Line=__LINE__ )
  ENDIF
END PROCEDURE readline_avbvcv

!----------------------------------------------------------------------------
!                                                                 ReadLine
!----------------------------------------------------------------------------

MODULE PROCEDURE readline_abv
  INTEGER( I4B ) :: i,Nb,istat

  Nb=size(b)
  IF ( PRESENT(buffer) ) THEN
    READ( buffer,*,iostat=istat ) a, ( b(i),i=1,Nb )
  ELSE
    READ( unitNo,*,iostat=istat ) a, ( b(i),i=1,Nb )
    CALL FileError(istat=istat,fname=fileName,flg=IO_READ, File=__FILE__, &
      & Routine="readline_abv()", Line=__LINE__ )
  ENDIF
END PROCEDURE readline_abv

!----------------------------------------------------------------------------
!                                                                 ReadLine
!----------------------------------------------------------------------------

MODULE PROCEDURE readline_abvcv
  INTEGER( I4B ) :: i,Nb,istat, Nc

  Nb=size(b)
  Nc=size(c)
  IF ( PRESENT(buffer) ) THEN
    READ( buffer,*,iostat=istat ) a, ( b(i),i=1,Nb ), ( c(i),i=1,Nc )
  ELSE
    READ( unitNo,*,iostat=istat ) a, ( b(i),i=1,Nb ), ( c(i),i=1,Nc )
    CALL FileError(istat=istat,fname=fileName,flg=IO_READ, File=__FILE__, &
      & Routine="readline_abvcv()", Line=__LINE__ )
  ENDIF
END PROCEDURE readline_abvcv

!----------------------------------------------------------------------------
!                                                                 ReadLine
!----------------------------------------------------------------------------

MODULE PROCEDURE readline_abcv
  INTEGER( I4B ) :: i,istat, Nc

  Nc=size(c)
  IF ( PRESENT(buffer) ) THEN
    READ( buffer,*,iostat=istat ) a, b, ( c(i),i=1,Nc )
  ELSE
    READ( unitNo,*,iostat=istat ) a, b, ( c(i),i=1,Nc )
    CALL FileError(istat=istat,fname=fileName,flg=IO_READ, File=__FILE__, &
      & Routine="readline_abcv()", Line=__LINE__ )
  ENDIF
END PROCEDURE readline_abcv

!----------------------------------------------------------------------------
!                                                                 ReadLine
!----------------------------------------------------------------------------

MODULE PROCEDURE readline_abcvdv
  INTEGER( I4B ) :: i,istat, Nc, Nd

  Nc=size(c)
  Nd=size(d)
  IF ( PRESENT(buffer) ) THEN
    READ( buffer,*,iostat=istat ) a, b, ( c(i),i=1,Nc ), ( d(i),i=1,Nd )
  ELSE
    READ( unitNo,*,iostat=istat ) a, b, ( c(i),i=1,Nc ), ( d(i),i=1,Nd )
    CALL FileError(istat=istat,fname=fileName,flg=IO_READ, File=__FILE__, &
      & Routine="readline_abcvdv()", Line=__LINE__ )
  ENDIF
END PROCEDURE readline_abcvdv

!----------------------------------------------------------------------------
!                                                                 ReadLine
!----------------------------------------------------------------------------

MODULE PROCEDURE readline_abcdv
  INTEGER( I4B ) :: i,istat, Nd

  Nd=size(d)
  IF ( PRESENT(buffer) ) THEN
    READ( buffer,*,iostat=istat ) a, b, c, ( d(i),i=1,Nd )
  ELSE
    READ( unitNo,*,iostat=istat ) a, b, c, ( d(i),i=1,Nd )
    CALL FileError(istat=istat,fname=fileName,flg=IO_READ, File=__FILE__, &
      & Routine="readline_abcdv()", Line=__LINE__ )
  ENDIF
END PROCEDURE readline_abcdv

!----------------------------------------------------------------------------
!                                                                 ReadLine
!----------------------------------------------------------------------------

MODULE PROCEDURE readline_abcdvev
  INTEGER( I4B ) :: i,istat, Nd, Ne

  Nd=size(d)
  Ne=size(e)
  IF ( PRESENT(buffer) ) THEN
    READ( buffer,*,iostat=istat ) a, b, c, ( d(i),i=1,Nd ), ( e(i),i=1,Ne )
  ELSE
    READ( unitNo,*,iostat=istat ) a, b, c, ( d(i),i=1,Nd ), ( e(i),i=1,Ne )
    CALL FileError(istat=istat,fname=fileName,flg=IO_READ, File=__FILE__, &
      & Routine="readline_abcdvev()", Line=__LINE__ )
  ENDIF
END PROCEDURE readline_abcdvev

!----------------------------------------------------------------------------
!                                                                 ReadLine
!----------------------------------------------------------------------------

MODULE PROCEDURE readline_abcdev
  INTEGER( I4B ) :: i,istat, Ne

  Ne=size(e)
  IF ( PRESENT(buffer) ) THEN
    READ( buffer,*,iostat=istat ) a, b, c, d, ( e(i),i=1,Ne )
  ELSE
    READ( unitNo,*,iostat=istat ) a, b, c, d, ( e(i),i=1,Ne )
    CALL FileError(istat=istat,fname=fileName,flg=IO_READ, File=__FILE__, &
      & Routine="readline_abcdev()", Line=__LINE__ )
  ENDIF
END PROCEDURE readline_abcdev

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE IO