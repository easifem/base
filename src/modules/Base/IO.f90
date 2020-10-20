MODULE IO
USE GlobalData
USE DISPMODULE
USE H5Fortran
USE BaseType, ONLY: FILE_

IMPLICIT NONE

PRIVATE

INTEGER( I4B ), PARAMETER :: minRow = 4, minCol = 4
PUBLIC :: Display, BlankLines, DashLine, DebugTag
PUBLIC :: DotLine, EqualLine, Err_Msg, ERR_OPEN_FILE
PUBLIC :: TIMESTAMP

INTERFACE Display

  MODULE PROCEDURE &
    & Display_Str, &
    & Display_Str2, &
    & Display_Real, &
    & Display_Int, &
    & Display_Logical, &
    & Display_Vector_Real, &
    & Display_Vector_Int, &
    & Display_Mat2_Real, &
    & Display_Mat2_Int, &
    & Display_Mat3_Real, &
    & Display_Mat4_Real
END INTERFACE

CONTAINS

!----------------------------------------------------------------------------
!                                                                    Display
!----------------------------------------------------------------------------

SUBROUTINE Display_Str( msg, unitno )
  CHARACTER( LEN = * ), INTENT( IN ) :: msg
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: unitno

  INTEGER( I4B ) :: i

  if( PRESENT( unitno ) ) then
    i = unitno
  ELSE
    i = stdout
  END IF

  ! write( i, "(A)" ) TRIM( msg )

  call disp( title = '', &
    & x = msg, &
    & FMT = 'a', &
    & unit = i, &
    & style = 'left' )

END SUBROUTINE Display_Str

!----------------------------------------------------------------------------
!                                                                    Display
!----------------------------------------------------------------------------

SUBROUTINE Display_Str2( val, msg, unitno )
  CHARACTER( LEN = * ), INTENT( IN ) :: val
  CHARACTER( LEN = * ), INTENT( IN ) :: msg
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: unitno

  INTEGER( I4B ) :: i

  if( PRESENT( unitno ) ) then
    i = unitno
  ELSE
    i = stdout
  END IF

  call disp( title = '', &
    & x = trim(msg) // trim(val), &
    & FMT = 'a', &
    & unit = i, &
    & style = 'left' )

END SUBROUTINE Display_Str2

!----------------------------------------------------------------------------
!                                                                    Display
!----------------------------------------------------------------------------

SUBROUTINE Display_Real( vec, msg, unitNo )
  ! Define intent of dummy variables
  INTEGER( I4B ), INTENT( IN ), OPTIONAL :: unitNo
  REAL( DFP ), INTENT( IN ) :: vec
  CHARACTER( LEN = * ), INTENT( IN ) :: msg
  ! Define internal variables
  INTEGER( I4B ) :: I
  !
  I = stdout
  IF( PRESENT( unitNo ) ) I = unitNo
  call disp( title = msg, x = vec, unit = I, style = 'left' )

END SUBROUTINE Display_Real

!----------------------------------------------------------------------------
!                                                                    Display
!----------------------------------------------------------------------------

SUBROUTINE Display_Int( vec, msg, unitNo )
  ! Define intent of dummy variables
  INTEGER( I4B ), INTENT( IN ) :: vec
  CHARACTER( LEN = * ), INTENT( IN ) :: msg
  INTEGER( I4B ), INTENT( IN ), OPTIONAL :: unitNo
  ! Define internal variables
  INTEGER( I4B ) :: I
  I = stdout
  IF( PRESENT( unitNo ) ) I = unitNo
  call disp( title = msg, x = vec, unit = I, style = 'left' )

END SUBROUTINE Display_Int

!----------------------------------------------------------------------------
!                                                                    Display
!----------------------------------------------------------------------------

SUBROUTINE Display_Logical( vec, msg, unitNo )
  ! Define intent of dummy variables
  INTEGER( I4B ), INTENT( IN ), OPTIONAL :: unitNo
  LOGICAL( LGT ), INTENT( IN ) :: vec
  CHARACTER( LEN = * ), INTENT( IN ) :: msg
  ! Define internal variables
  INTEGER( I4B ) :: I
  !
  I = stdout
  IF( PRESENT( unitNo ) ) I = unitNo
  call disp( title = msg, x = vec, unit = I, style = 'left' )

END SUBROUTINE Display_Logical

!----------------------------------------------------------------------------
!                                                                    Display
!----------------------------------------------------------------------------

SUBROUTINE Display_Vector_Real( vec, msg, unitNo, path, filename, &
  & extension )
  ! Define intent of dummy variables
  INTEGER( I4B ), INTENT( IN ), OPTIONAL :: unitNo
  REAL( DFP ), INTENT( IN ) :: vec( : )
  CHARACTER( LEN = * ), INTENT( IN ) :: msg
  CHARACTER( LEN = * ), OPTIONAL, INTENT( IN ) :: path
  CHARACTER( LEN = * ), OPTIONAL, INTENT( IN ) :: filename
  CHARACTER( LEN = * ), OPTIONAL, INTENT( IN ) :: extension
  !

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

END SUBROUTINE Display_Vector_Real

!----------------------------------------------------------------------------
!                                                                    Display
!----------------------------------------------------------------------------

SUBROUTINE Display_Vector_Int( vec, msg, unitNo, full )

  ! Define intent of dummy variables
  INTEGER( I4B ), INTENT( IN ), OPTIONAL :: unitNo
  INTEGER( I4B ), DIMENSION( : ), INTENT( IN ) :: vec
  CHARACTER( LEN = * ), INTENT( IN ) :: msg
  LOGICAL( LGT ), INTENT( IN ), OPTIONAL :: full


  ! Define internal variables
  INTEGER( I4B ) :: I
  ! INTEGER( I4B ) :: I, j, n
  ! CHARACTER( LEN = 120 ) :: fmt
  I = stdout
  IF( PRESENT( unitNo ) ) I = unitNo

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

END SUBROUTINE Display_Vector_Int

!----------------------------------------------------------------------------
!                                                                    Display
!----------------------------------------------------------------------------

SUBROUTINE Display_Mat2_Real( Mat, msg, unitNo, full )

  !   Define intent of dummy variables
  INTEGER( I4B ), INTENT( IN ), OPTIONAL :: unitNo
  REAL( DFP ), DIMENSION( :, : ), INTENT( IN ) :: Mat
  CHARACTER( LEN = * ), INTENT( IN ) :: msg
  LOGICAL( LGT ), INTENT( IN ), OPTIONAL :: full
  !   Define internal variables
  INTEGER( I4B ) :: I, j, r
  !
  I = stdout
  IF( PRESENT( unitNo ) ) I = unitNo

  call disp( title = msg, &
    & x= mat, &
    & unit = I, &
    & style = 'underline & pad', &
    & sep = ', ', &
    & advance = 'double' )

  ! IF( LEN_TRIM( msg ) .NE. 0 ) THEN
  !   WRITE( I, "(A)" ) TRIM( msg )
  ! END IF
  ! !
  ! r = SIZE( Mat, 1 )
  ! IF( I .EQ. stdout ) THEN
  !   IF( PRESENT( full ) ) THEN
  !     DO j = 1, r
  !       CALL Display( Mat( j, : ), "", I, full )
  !     END DO
  !   ELSE
  !     IF( r .LE. 10 ) THEN
  !       DO j = 1, r
  !         CALL Display( Mat( j, : ), "", I )
  !       END DO
  !     ELSE
  !       DO j = 1, minRow
  !         CALL Display( Mat( j, : ), "", I )
  !       END DO
  !       WRITE( I, "(A)") "."
  !       WRITE( I, "(A)") "."
  !       WRITE( I, "(A)") "."
  !       DO j = r, r-minRow, -1
  !         CALL Display( Mat( j, : ), "", I )
  !       END DO
  !     END IF
  !   END IF
  ! ELSE
  !   DO j = 1, r
  !     CALL Display( Mat( j, : ), "", I )
  !   END DO
  ! END IF
END SUBROUTINE Display_Mat2_Real

!----------------------------------------------------------------------------
!                                                                    Display
!----------------------------------------------------------------------------

SUBROUTINE Display_Mat2_Int( Mat, msg, unitNo, full )

  !   Define intent of dummy variables
  INTEGER( I4B ), INTENT( IN ), OPTIONAL :: unitNo
  INTEGER( I4B ), INTENT( IN ) :: Mat( :, : )
  CHARACTER( LEN = * ), INTENT( IN ) :: msg
  LOGICAL( LGT ), INTENT( IN ), OPTIONAL :: full

  !   Define internal variables
  INTEGER( I4B ) :: I, j, r
  !
  I = stdout
  IF( PRESENT( unitNo ) ) I = unitNo

  call disp( title = msg, &
    & x= mat, &
    & unit = I, &
    & style = 'underline & pad', &
    & sep = ', ', &
    & advance = 'double' )

  ! IF( LEN_TRIM( msg ) .NE. 0 ) THEN
  !   WRITE( I, "(A)" ) TRIM( msg )
  ! END IF
  ! !
  ! r = SIZE( Mat, 1 )
  ! IF( I .EQ. stdout ) THEN
  !   IF( PRESENT( full ) ) THEN
  !     DO j = 1, r
  !       CALL Display( Mat( j, : ), "", I, full )
  !     END DO
  !   ELSE
  !     IF( r .LE. 10 ) THEN
  !       DO j = 1, r
  !         CALL Display( Mat( j, : ), "", I )
  !       END DO
  !     ELSE
  !       DO j = 1, minRow
  !         CALL Display( Mat( j, : ), "", I )
  !       END DO
  !       WRITE( I, "(A)") "."
  !       WRITE( I, "(A)") "."
  !       WRITE( I, "(A)") "."
  !       DO j = r, r-minRow, -1
  !         CALL Display( Mat( j, : ), "", I )
  !       END DO
  !     END IF
  !   END IF
  ! ELSE
  !   DO j = 1, r
  !     CALL Display( Mat( j, : ), "", I )
  !   END DO
  ! END IF
END SUBROUTINE Display_Mat2_Int

!----------------------------------------------------------------------------
!                                                                    Display
!----------------------------------------------------------------------------

SUBROUTINE Display_Mat3_Real( Mat, msg, unitNo )

  !   Define intent of dummy variables
  INTEGER( I4B ), INTENT( IN ), OPTIONAL :: unitNo
  REAL( DFP ), INTENT( IN ) :: Mat( :, :, : )
  CHARACTER( LEN = * ), INTENT( IN ) :: msg
  !   Define internal variables
  INTEGER( I4B ) :: I, J
  !
  I = stdout
  IF( PRESENT( unitNo ) ) I = unitNo
  !
  DO J = 1, SIZE( Mat, 3 )
    CALL Display( Mat( :, :, J ), &
      & TRIM( msg ) //"( :, :, "//TRIM( Int2Str( J ) ) // " )", I )
  END DO

END SUBROUTINE Display_Mat3_Real

!----------------------------------------------------------------------------
!                                                                    Display
!----------------------------------------------------------------------------

SUBROUTINE Display_Mat4_Real( Mat, msg, unitNo )

  !   Define intent of dummy variables
  INTEGER( I4B ), INTENT( IN ), OPTIONAL :: unitNo
  REAL( DFP ), INTENT( IN ) :: Mat( :, :, :, : )
  CHARACTER( LEN = * ), INTENT( IN ) :: msg
  !   Define internal variables
  INTEGER( I4B ) :: I, J, K
  !
  I = stdout
  IF( PRESENT( unitNo ) ) I = unitNo
  !
  DO K = 1, SIZE( Mat, 4 )
    DO J = 1, SIZE( Mat, 3 )
    CALL Display( Mat( :, :, J, K ), &
      & TRIM( msg ) &
      & // "( :, :, " &
      & // TRIM( Int2Str( J ) ) &
      & // ", " &
      & // TRIM( Int2Str( K ) ) &
      & // " )" &
      & , I )
    END DO
  END DO

END SUBROUTINE Display_Mat4_Real

!------------------------------------------------------------------------------
!                                                                       Int2Str
!------------------------------------------------------------------------------
!
FUNCTION Int2Str( I )
  ! Define intent of dummy arguments
  INTEGER( I4B ), INTENT( IN ) :: I
  CHARACTER( LEN = 15 ) :: Int2Str
  ! Define internal variables
  CHARACTER( LEN = 15 ) :: Str

  WRITE( Str, "(I15)" ) I
  Int2Str = TRIM( ADJUSTL( Str ) )
END FUNCTION Int2Str

!------------------------------------------------------------------------------
!
!------------------------------------------------------------------------------

SUBROUTINE DebugTag( Tag, unitNo )
	! INTENT OF DUMMY VARIABLES
  INTEGER( I4B ), INTENT( IN ), OPTIONAL :: Tag, unitNo

	! Define internal variables
  INTEGER( I4B ) :: M = 1
  IF( PRESENT( Tag ) ) M = Tag

  IF( PRESENT( unitNo ) ) THEN
    WRITE( unitNo, "(A,I4)") "**Debug**", M
  ELSE
    WRITE( stdout, "(A,I4)") "**Debug**", M
  END IF
END SUBROUTINE DebugTag

!------------------------------------------------------------------------------
!
!------------------------------------------------------------------------------

SUBROUTINE BlankLines( unitNo, NOL )
  ! INTENT OF DUMMY VARIABLES
  INTEGER( I4B ), INTENT( IN ), OPTIONAL :: NOL, unitNo
  ! Define internal variables
  INTEGER( I4B ) :: M = 1, I

  IF( PRESENT( NOL ) ) M = NOL

  IF( PRESENT( unitNo ) ) THEN
    DO I = 1, M
    WRITE( unitNo, "(A)") ""
    END DO
  ELSE
    DO I = 1, M
    WRITE( stdout, * ) ""
    END DO
  END IF
END SUBROUTINE BlankLines

!------------------------------------------------------------------------------
!
!------------------------------------------------------------------------------

SUBROUTINE DashLine( unitNo )
  ! INTENT OF DUMMY VARIABLES
  INTEGER( I4B ), INTENT( IN ), OPTIONAL :: unitNo

  IF( PRESENT( unitNo ) ) THEN
    WRITE( unitNo, "(A)")"-------------------------------------------------"
  ELSE
    WRITE( stdout, "(A)") "-------------------------------------------------"
  END IF
END SUBROUTINE DashLine

!------------------------------------------------------------------------------
!
!------------------------------------------------------------------------------

SUBROUTINE DotLine( unitNo )
  ! INTENT OF DUMMY VARIABLES
  INTEGER( I4B ), INTENT( IN ), OPTIONAL :: unitNo

  IF( PRESENT( unitNo ) ) THEN
    WRITE( unitNo, "(A)")"................................................."
  ELSE
    WRITE( stdout, "(A)") "................................................."
  END IF
END SUBROUTINE DotLine

!------------------------------------------------------------------------------
!
!------------------------------------------------------------------------------

SUBROUTINE EqualLine( unitNo )
  ! INTENT OF DUMMY VARIABLES
  INTEGER( I4B ), INTENT( IN ), OPTIONAL :: unitNo

  IF( PRESENT( unitNo ) ) THEN
    WRITE( unitNo, "(A)")"================================================="
  ELSE
    WRITE( stdout, "(A)") "================================================="
  END IF

END SUBROUTINE EqualLine

!------------------------------------------------------------------------------
!
!------------------------------------------------------------------------------

SUBROUTINE check_error(pgm,name1)
  ! INTENT OF DUMMY VARIABLES
  CHARACTER(*), INTENT(IN):: name1, pgm

  IF(Error_Flag) THEN
    WRITE(*,100) TRIM(pgm), TRIM(name1)
    STOP
  END IF

  ! Format
  100 FORMAT(1X,"=======================================================",/&
  T6,"ERROR:: in PROGRAM :: ", a, /&
  T6,"and SUBROUTINE :: ", a, /&
  T6, "Please See The Message Return By the subroutine",/ &
  T6, "Program STOPPED ", / 1X, &
  "=======================================================")

END SUBROUTINE check_error

!------------------------------------------------------------------------------
!
!------------------------------------------------------------------------------

SUBROUTINE Err_msg(pgm, str1, str2)
!
!   Description
!-----------------------------------------------------------------------------
!       1.  This procedure returns the formatted error message
!
!       2.  Arguments
!               -   str1; subroutine name
!               -   str2; error message
!               -   pgm = name of main program name that contains subroutine
!
!-----------------------------------------------------------------------------
!
! Define Intent of dummy Variable
  CHARACTER(*), INTENT(IN):: str1, str2, pgm
!
!
  WRITE(*,*)
  WRITE(*,121)TRIM(pgm),TRIM(str1), TRIM(str2)
!
! Format
121 FORMAT(1X,"------------------------------------------------------",/&
T6,"ERROR!!:: in PROGRAM::= ",a, /&
T6,"& SUBROUTINE:: ",a, /&
T6, "Message: ",a, / &
1X, "------------------------------------------------------",/)
!
END SUBROUTINE Err_msg
!
!
!------------------------------------------------------------------------------
!
!------------------------------------------------------------------------------
!
SUBROUTINE Error_Msg(pgm, str1, str2)
!
!   Description
!-----------------------------------------------------------------------------
!       1.  This procedure returns the formatted error message
!
!       2.  Arguments
!               -   str1; subroutine name
!               -   str2; error message
!               -   pgm = name of main program name that contains subroutine
!
!-----------------------------------------------------------------------------
!
! Define Intent of dummy Variable
  CHARACTER(*), INTENT(IN):: str1, str2, pgm
!
!
  WRITE(*,*)
  WRITE(*,121)TRIM(pgm),TRIM(str1), TRIM(str2)
!
! Format
121 FORMAT(1X,"------------------------------------------------------",/&
T6,"ERROR!!:: in PROGRAM::= ",a, /&
T6,"& SUBROUTINE:: ",a, /&
T6, "Message: ",a, / &
1X, "------------------------------------------------------",/)
!
END SUBROUTINE Error_Msg
!------------------------------------------------------------------------------
!
!------------------------------------------------------------------------------
!
SUBROUTINE ERR_OPEN_FILE(Main_pgm,Routine_name,file_name, open_IOstat)
!
!   Description
!------------------------------------------------------------------------------
!       1.  This procedure checks the success of file opening operation
!           If opening opertation is failed then subroutine stops
!           the execution of program with message on the screen.
!
!       2.  Arguments
!               -   Main_pgm    :   Name of main program
!               -   Routine_name:   Name of subroutine
!               -   file_name   :   Name of the file which we are trying to open.
!               -   open_IOstat :   IOSTAT returned by open function.
!
!------------------------------------------------------------------------------
!
! Define Intent of dummy Variable
  CHARACTER(*), INTENT(IN):: Main_pgm,Routine_name, file_name
  INTEGER(I4B), INTENT(IN):: open_IOstat

  IF(open_IOstat .NE. 0)THEN
    WRITE(*,100)TRIM(Main_pgm),TRIM(Routine_name), TRIM(file_name)
    100 FORMAT(2X, "FATAL ERROR!!:: in PROGRAM= ", a,/ &
  2X," & SUBROUTINE = ",a,/ &
    2X, "could not found the file::= ", a, /, 2X, "PROGRAM STOPPED !", /)
    WRITE(*,*)"======================================================="
    STOP
  END IF
!
END SUBROUTINE ERR_OPEN_FILE
!
!------------------------------------------------------------------------------
!
!------------------------------------------------------------------------------
!
SUBROUTINE TIMESTAMP ( )
!
!   Description
!------------------------------------------------------------------------------
!       1.  This subroutine prints the current YMDHMS date as a time stamp.
!
!------------------------------------------------------------------------------
!
! Define Intent of dummy Variable
  CHARACTER (LEN = 8 ):: ampm
  INTEGER (I4B)::   d
  INTEGER (I4B)::   h
  INTEGER (I4B)::   m
  INTEGER (I4B)::   mm
  CHARACTER ( LEN = 9 ), PARAMETER, DIMENSION(12) :: month = (/ &
            'January  ', 'February ', 'March    ', 'April    ', &
            'May      ', 'June     ', 'July     ', 'August   ', &
            'September', 'October  ', 'November ', 'December ' /)
  INTEGER (I4B):: n
  INTEGER (I4B):: s
  INTEGER (I4B):: values(8)
  INTEGER (I4B):: y

  CALL date_and_time ( values = values )

  y = values(1)
  m = values(2)
  d = values(3)
  h = values(5)
  n = values(6)
  s = values(7)
  mm = values(8)

  IF ( h < 12 ) THEN
    ampm = 'AM'
!
  ELSE IF ( h == 12 ) THEN
!
    IF( n == 0 .and. s == 0 ) THEN
      ampm = 'Noon'
    ELSE
      ampm = 'PM'
    END IF
!
  ELSE
    h = h - 12
!
    IF ( h < 12 ) THEN
      ampm = 'PM'
    ELSE IF ( h == 12 ) THEN
      IF ( n == 0 .and. s == 0 ) THEN
        ampm = 'Midnight'
      ELSE
        ampm = 'AM'
      END IF
    END IF
!
  END IF

  WRITE( *, '(8x, i2,1x,a,1x,i4,2x,i2,a1,i2.2,a1,i2.2,a1,i3.3,1x,a)' ) &
  d, TRIM( month(m) ), y, h, ':', n, ':', s, '.', mm, TRIM( ampm )
!
!
END SUBROUTINE TIMESTAMP

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------
!> authors: Dr. Vikas Sharma
!
! This subroutine run a system commoand on terminal
SUBROUTINE ExecuteCommand( CMD, Str )
  CHARACTER( LEN = * ), INTENT( IN ) :: CMD, Str

  ! Define internal variables
  INTEGER( I4B ) :: CMDSTAT, EXITSTAT
  LOGICAL( LGT ) :: WAIT = .TRUE.
  CHARACTER( LEN = 300 ) :: CMDMSG = ""

  CALL EXECUTE_COMMAND_LINE( TRIM(CMD), CMDSTAT = CMDSTAT, &
    & EXITSTAT = EXITSTAT, &
    WAIT = WAIT, CMDMSG = CMDMSG )

  IF( CMDSTAT .NE. 0 ) THEN
    IF( CMDSTAT .EQ. -1 ) THEN
      CALL Err_Msg( &
        & Str, &
        & "exe_cmd()", &
        & "The Command " //TRIM( CMD ) // " FAILED; Program Stopped" )
      STOP
    END IF
    CALL Err_Msg( &
      & Str, &
      & "exe_cmd()", &
      & "Returned Error Message; Program Stopped"//TRIM( CMDMSG ) )

    STOP
  END IF
END SUBROUTINE ExecuteCommand

END MODULE IO
