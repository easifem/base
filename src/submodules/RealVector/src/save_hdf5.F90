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

!> authors: Vikas Sharma, Ph. D.
! date: 	25 Feb 2021
! summary: 	This submodule implements IO methods of [[RealVector_]]

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
  INTEGER( I4B ) :: sizes(SIZE(obj))
  REAL( DFP ) :: val( SIZE( obj ) )

  DO i = 1, SIZE( obj )
    sizes(i) = SIZE(obj(i))
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

      DO i = 1, SIZE(obj)
        call h5f%write( '/' // TRIM(msg) // '/comp[' &
          & // TRIM(INT2STR(i)) // ']', obj(i)%Val )
      END DO
      call h5f%finalize()

    CASE( '.txt' )
      CALL OpenFileToWrite(obj=afile, filename=filename, path=path, &
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
      DO j = 1, SIZE( obj )
        IF( i .LE. sizes( j ) ) val( j ) = obj(j)%Val(i)
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
    CALL Display( obj%Val, UnitNo = UnitNo, msg=msg )
    RETURN
  END IF

  IF( PRESENT( filename ) ) THEN
    CALL Display( obj%Val, msg=msg, filename=filename, &
      & extension=extension, path=path )
    RETURN
  END IF

  CALL Display( obj%Val, msg=msg, unitNo = stdout)

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