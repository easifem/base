MODULE IO
USE GlobalData
USE DISPMODULE
USE H5Fortran
USE BaseType, ONLY: FILE_

IMPLICIT NONE

PRIVATE

INTEGER( I4B ), PARAMETER :: minRow = 4, minCol = 4
PUBLIC :: Display, BlankLines, DashLine, DebugTag
PUBLIC :: DotLine, EqualLine
PUBLIC :: TIMESTAMP

INTERFACE Display

  MODULE PROCEDURE &
    & Display_Str, &
    & Display_Str2, &
    & Display_Real, &
    & Display_Int, &
    & Display_Logical, &
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
!
SUBROUTINE TIMESTAMP ( )
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
  ELSE IF ( h == 12 ) THEN
    IF( n == 0 .and. s == 0 ) THEN
      ampm = 'Noon'
    ELSE
      ampm = 'PM'
    END IF
  ELSE
    h = h - 12
    IF ( h < 12 ) THEN
      ampm = 'PM'
    ELSE IF ( h == 12 ) THEN
      IF ( n == 0 .and. s == 0 ) THEN
        ampm = 'Midnight'
      ELSE
        ampm = 'AM'
      END IF
    END IF
  END IF

  WRITE( *, '(8x, i2,1x,a,1x,i4,2x,i2,a1,i2.2,a1,i2.2,a1,i3.3,1x,a)' ) &
  d, TRIM( month(m) ), y, h, ':', n, ':', s, '.', mm, TRIM( ampm )

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
      STOP
    END IF
    STOP
  END IF
END SUBROUTINE ExecuteCommand

END MODULE IO
