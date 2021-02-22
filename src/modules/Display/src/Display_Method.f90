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


!> authors: Dr. Vikas Sharma
!
! [[Display_Method]] module consists small routines related
! to displaying the fortran variables on the screen


MODULE Display_Method
USE GlobalData
USE DISPMODULE
IMPLICIT NONE
PRIVATE
INTEGER( I4B ), PARAMETER :: minRow = 4, minCol = 4
PUBLIC :: Display, BlankLines, DashLine
PUBLIC :: DotLine, EqualLine
PUBLIC :: TIMESTAMP

CHARACTER( LEN = 30 ) :: equal = "=============================="
CHARACTER( LEN = 30 ) :: dot  =  ".............................."
CHARACTER( LEN = 30 ) :: dash =  "------------------------------"

!----------------------------------------------------------------------------
!                                                                    Display
!----------------------------------------------------------------------------
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

!> authors: Dr. Vikas Sharma
!
! This subroutine Displays the string
!
! ## usage
! ```fortran
! CALL Display( msg="hello world", unitno=stdout )
! ```
!

SUBROUTINE Display_Str( msg, unitno )
  ! Dummt arguments
  CHARACTER( LEN = * ), INTENT( IN ) :: msg
  !! input message
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: unitno
  !! unit no

  ! Internal variables
  INTEGER( I4B ) :: i

  if( PRESENT( unitno ) ) then
    i = unitno
  ELSE
    i = stdout
  END IF

  call disp( title = '', &
    & x = msg, &
    & FMT = 'a', &
    & unit = i, &
    & style = 'left' )

END SUBROUTINE Display_Str

!----------------------------------------------------------------------------
!                                                                    Display
!----------------------------------------------------------------------------

!> authors: Dr. Vikas Sharma
!
!  This routine prints a string; msg=val
!
! ## Usage
! ```fortran
!   CALL Display( val=" world!", msg="hello", stdout)
! ```

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

!> authors: Dr. Vikas Sharma
!
! This subroutine display a scalar real number
!
! ## Usage
!
! ```fortran
! call display( val=1.0_DFP, msg="var=", unitno=stdout)
! ```

SUBROUTINE Display_Real( val, msg, unitNo )
  ! Define intent of dummy variables
  INTEGER( I4B ), INTENT( IN ), OPTIONAL :: unitNo
  REAL( DFP ), INTENT( IN ) :: val
  CHARACTER( LEN = * ), INTENT( IN ) :: msg
  ! Define internal variables
  INTEGER( I4B ) :: I
  !
  I = stdout
  IF( PRESENT( unitNo ) ) I = unitNo
  call disp( title = msg, x = val, unit = I, style = 'left' )

END SUBROUTINE Display_Real

!----------------------------------------------------------------------------
!                                                                    Display
!----------------------------------------------------------------------------

!> authors: Dr. Vikas Sharma
!
! This subroutine display a scalar integer number
!
! ## Usage
!
! ```fortran
! call display( val=1.0_I4B, msg="var=", unitno=stdout)
! ```

SUBROUTINE Display_Int( val, msg, unitNo )
  ! Define intent of dummy variables
  INTEGER( I4B ), INTENT( IN ) :: val
  CHARACTER( LEN = * ), INTENT( IN ) :: msg
  INTEGER( I4B ), INTENT( IN ), OPTIONAL :: unitNo
  ! Define internal variables
  INTEGER( I4B ) :: I
  I = stdout
  IF( PRESENT( unitNo ) ) I = unitNo
  call disp( title = msg, x = val, unit = I, style = 'left' )

END SUBROUTINE Display_Int

!----------------------------------------------------------------------------
!                                                                    Display
!----------------------------------------------------------------------------

!> authors: Dr. Vikas Sharma
!
! This subroutine display a scalar logical variable
!
! ## Usage
!
! ```fortran
! call display( val=.TRUE., msg="var=", unitno=stdout)
! ```

SUBROUTINE Display_Logical( val, msg, unitNo )
  ! Define intent of dummy variables
  INTEGER( I4B ), INTENT( IN ), OPTIONAL :: unitNo
  LOGICAL( LGT ), INTENT( IN ) :: val
  CHARACTER( LEN = * ), INTENT( IN ) :: msg
  ! Define internal variables
  INTEGER( I4B ) :: I
  !
  I = stdout
  IF( PRESENT( unitNo ) ) I = unitNo
  call disp( title = msg, x = val, unit = I, style = 'left' )

END SUBROUTINE Display_Logical

!----------------------------------------------------------------------------
!                                                                    Display
!----------------------------------------------------------------------------

!> authors: Dr. Vikas Sharma
!
! This subroutine display a vector of real numbers
!
! ## Usage
! ```fortran
! real( dfp ) :: vec(10)
! call RANDOM_NUMBER(vec)
! call display( val=vec, msg="var=", unitno=stdout)
! call display( val=vec, msg="var=", unitno=stdout, orient="col")
! ```

SUBROUTINE Display_Vector_Real( val, msg, unitNo, orient, full )

  ! Define intent of dummy variables
  INTEGER( I4B ), INTENT( IN ), OPTIONAL :: unitNo
  !! Unit number
  REAL( DFP ), INTENT( IN ) :: val( : )
  !! vector of real numbers
  CHARACTER( LEN = * ), INTENT( IN ) :: msg
  !! message
  CHARACTER( LEN = * ), OPTIONAL, INTENT( IN ) :: orient
  !! orient=row => rowwise printing
  !! orient=col =>  columnwise printing
  LOGICAL( LGT ), INTENT( IN ), OPTIONAL :: full
  !! logical variable to print the whole vector

  ! Define internal variables
  INTEGER( I4B ) :: I
  CHARACTER( LEN = 3 ) :: orient_

  IF( PRESENT( unitNo ) ) THEN
    I = unitNo
  ELSE
    I = stdout
  END IF

  IF( PRESENT( full ) ) THEN
    ! do nothing for now
  END IF

  IF( PRESENT( orient ) ) THEN
    IF( orient(1:1) .EQ. "r" .OR. orient(1:1) .EQ. "R" ) THEN
      orient_ = "row"
    ELSE
      orient_ = "col"
    END IF
  ELSE
    orient_ = "row"
  END IF

  call disp( &
    & title = msg, &
    & x= val, &
    & unit = I, &
    ! & style = 'underline & pad', &
    & style = 'left', &
    & orient = orient_ &
  )

END SUBROUTINE Display_Vector_Real

!----------------------------------------------------------------------------
!                                                                    Display
!----------------------------------------------------------------------------

!> authors: Dr. Vikas Sharma
!
! This subroutine display a vector of integer numbers
!
! ## Usage
! ```fortran
! real( dfp ) :: vec(10)
! call RANDOM_NUMBER(vec)
! call display( val=vec, msg="var=", unitno=stdout)
! call display( val=vec, msg="var=", unitno=stdout, orient="col")
! ```

SUBROUTINE Display_Vector_Int( val, msg, unitNo, orient, full )

  ! Define intent of dummy variables
  INTEGER( I4B ), INTENT( IN ), OPTIONAL :: unitNo
  !! Unit number
  INTEGER( I4B ), INTENT( IN ) :: val( : )
  !! vector of real numbers
  CHARACTER( LEN = * ), INTENT( IN ) :: msg
  !! message
  CHARACTER( LEN = * ), OPTIONAL, INTENT( IN ) :: orient
  !! orient=row => rowwise printing
  !! orient=col =>  columnwise printing
  LOGICAL( LGT ), INTENT( IN ), OPTIONAL :: full
  !! logical variable to print the whole vector

  ! Define internal variables
  INTEGER( I4B ) :: I
  CHARACTER( LEN = 3 ) :: orient_

  IF( PRESENT( unitNo ) ) THEN
    I = unitNo
  ELSE
    I = stdout
  END IF

  IF( PRESENT( full ) ) THEN
    ! do nothing for now
  END IF

  IF( PRESENT( orient ) ) THEN
    IF( orient(1:1) .EQ. "r" .OR. orient(1:1) .EQ. "R" ) THEN
      orient_ = "row"
    ELSE
      orient_ = "col"
    END IF
  ELSE
    orient_ = "row"
  END IF

  call disp( &
    & title = msg, &
    & x= val, &
    & unit = I, &
    ! & style = 'underline & pad', &
    & style = 'left', &
    & orient = orient_ &
  )
END SUBROUTINE Display_Vector_Int

!----------------------------------------------------------------------------
!                                                                    Display
!----------------------------------------------------------------------------

!> authors: Dr. Vikas Sharma
!
! This subroutine display a matrix of real numbers
!
! ## Usage
! ```fortran
! real( dfp ) :: mat(10, 10)
! call RANDOM_NUMBER(mat)
! call display( val=mat, msg="var=", unitno=stdout)
! ```

SUBROUTINE Display_Mat2_Real( Val, msg, unitNo, full )
  !   Define intent of dummy variables
  INTEGER( I4B ), INTENT( IN ), OPTIONAL :: unitNo
  REAL( DFP ), DIMENSION( :, : ), INTENT( IN ) :: Val
  CHARACTER( LEN = * ), INTENT( IN ) :: msg
  LOGICAL( LGT ), INTENT( IN ), OPTIONAL :: full
  !   Define internal variables
  INTEGER( I4B ) :: I

  IF( PRESENT( unitNo ) ) THEN
    I = unitNo
  ELSE
    I = stdout
  END IF

  call disp( title = msg, &
    & x= val, &
    & unit = I, &
    & style = 'left', &
    & sep = ' ', &
    & advance = 'double' )
END SUBROUTINE Display_Mat2_Real

!----------------------------------------------------------------------------
!                                                                    Display
!----------------------------------------------------------------------------


!> authors: Dr. Vikas Sharma
!
! This subroutine display a matrix of real numbers
!
! ## Usage
! ```fortran
! integer( i4b ) :: mat(10, 10)
! call RANDOM_NUMBER(mat)
! call display( val=mat, msg="var=", unitno=stdout)
! ```

SUBROUTINE Display_Mat2_Int( Val, msg, unitNo, full )

  !   Define intent of dummy variables
  INTEGER( I4B ), INTENT( IN ), OPTIONAL :: unitNo
  INTEGER( I4B ), INTENT( IN ) :: Val( :, : )
  CHARACTER( LEN = * ), INTENT( IN ) :: msg
  LOGICAL( LGT ), INTENT( IN ), OPTIONAL :: full

  !   Define internal variables
  INTEGER( I4B ) :: I

  I = stdout
  IF( PRESENT( unitNo ) ) I = unitNo

  call disp( &
    & title = msg, &
    & x= val, &
    & unit = I, &
    & style = 'left', &
    & sep = ' ', &
    & advance = 'double' &
  )
END SUBROUTINE Display_Mat2_Int

!----------------------------------------------------------------------------
!                                                                    Display
!----------------------------------------------------------------------------

!> authors: Dr. Vikas Sharma
!
! This subroutine displays the contents of a rank 3 array.
!
! ## Usage
!```fortran
! real( dfp ) :: mat(5, 5, 2)
! call RANDOM_NUMBER(mat)
! call display( val=mat, msg="var=", unitno=stdout)
!```

SUBROUTINE Display_Mat3_Real( Val, msg, unitNo )

  !   Define intent of dummy variables
  INTEGER( I4B ), INTENT( IN ), OPTIONAL :: unitNo
  REAL( DFP ), INTENT( IN ) :: Val( :, :, : )
  CHARACTER( LEN = * ), INTENT( IN ) :: msg
  !   Define internal variables
  INTEGER( I4B ) :: I, J

  IF( PRESENT( unitNo ) ) THEN
    I = unitNo
  ELSE
    I =stdout
  END IF

  DO J = 1, SIZE( Val, 3 )
    CALL Display( Val( :, :, J ), &
      & TRIM( msg ) //"( :, :, "//TRIM( Int2Str( J ) ) // " ) = ", I )
  END DO

END SUBROUTINE Display_Mat3_Real

!----------------------------------------------------------------------------
!                                                                    Display
!----------------------------------------------------------------------------

!> authors: Dr. Vikas Sharma
!
! This subroutine displays the content of rank4 matrix
!
! ## Usage
!
!```fortran
! real( dfp ) :: mat(3, 3, 2, 2)
! call RANDOM_NUMBER(mat)
! call display( val=mat, msg="var=", unitno=stdout)
!```

SUBROUTINE Display_Mat4_Real( Val, msg, unitNo )

  !   Define intent of dummy variables
  INTEGER( I4B ), INTENT( IN ), OPTIONAL :: unitNo
  REAL( DFP ), INTENT( IN ) :: Val( :, :, :, : )
  CHARACTER( LEN = * ), INTENT( IN ) :: msg
  !   Define internal variables
  INTEGER( I4B ) :: I, J, K
  !
  I = stdout
  IF( PRESENT( unitNo ) ) I = unitNo
  !
  DO K = 1, SIZE( Val, 4 )
    DO J = 1, SIZE( Val, 3 )
    CALL Display( Val( :, :, J, K ), &
      & TRIM( msg ) &
      & // "( :, :, " &
      & // TRIM( Int2Str( J ) ) &
      & // ", " &
      & // TRIM( Int2Str( K ) ) &
      & // " ) = " &
      & , I )
    END DO
  END DO

END SUBROUTINE Display_Mat4_Real

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> authors: Dr. Vikas Sharma
!
! This function converts integer to character
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

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> authors: Dr. Vikas Sharma
!
! This subroutine prints blankline

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

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> authors: Dr. Vikas Sharma
!
! This subroutine prints dash line

SUBROUTINE DashLine( unitNo )
  ! INTENT OF DUMMY VARIABLES
  INTEGER( I4B ), INTENT( IN ), OPTIONAL :: unitNo

  IF( PRESENT( unitNo ) ) THEN
    WRITE( unitNo, "(A)") dash
  ELSE
    WRITE( stdout, "(A)") dash
  END IF
END SUBROUTINE DashLine

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> authors: Dr. Vikas Sharma
!
! This subroutine prints dot line

SUBROUTINE DotLine( unitNo )
  ! INTENT OF DUMMY VARIABLES
  INTEGER( I4B ), INTENT( IN ), OPTIONAL :: unitNo

  IF( PRESENT( unitNo ) ) THEN
    WRITE( unitNo, "(A)") dot
  ELSE
    WRITE( stdout, "(A)") dot
  END IF

END SUBROUTINE DotLine

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> authors: Dr. Vikas Sharma
!
! This subroutine prints equal line

SUBROUTINE EqualLine( unitNo )
  ! INTENT OF DUMMY VARIABLES
  INTEGER( I4B ), INTENT( IN ), OPTIONAL :: unitNo

  IF( PRESENT( unitNo ) ) THEN
    WRITE( unitNo, "(A)") equal
  ELSE
    WRITE( stdout, "(A)") equal
  END IF

END SUBROUTINE EqualLine

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> authors: Dr. Vikas Sharma
!
! This subroutine prints the time stamp

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
END MODULE Display_Method
