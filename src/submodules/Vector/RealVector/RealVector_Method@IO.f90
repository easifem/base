SUBMODULE( RealVector_Method ) IO
  !! This submodule is related to the input output operations of real vector
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
    CALL Display( Obj%Val, UnitNo = UnitNo, msg=msg )
    RETURN
  END IF

  IF( PRESENT( filename ) ) THEN
    CALL Display( Obj%Val, msg=msg, filename=filename, &
      & extension=extension, path=path )
    RETURN
  END IF

  CALL Display( Obj%Val, msg=msg, unitNo = stdout)

END PROCEDURE RealscalarDisplay

!----------------------------------------------------------------------------
!                                                                    Display
!----------------------------------------------------------------------------

MODULE PROCEDURE Display_Vector_Real
  INTEGER( I4B ) :: i
  type(hdf5_file) :: h5f

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
  CASE DEFAULT
    CALL Display( Val=__FILE__, msg="Error: In file :: ", unitNo = stdout )
    CALL Display( Val=__LINE__, msg="In line number :: ", UnitNo = stdout )
    CALL Display( Msg= "No match found for given extension", UnitNo=stdout )
    STOP
  END SELECT

END PROCEDURE Display_Vector_Real

END SUBMODULE IO