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
! date: 	3 April 2021
! summary: 	Assert functions

SUBMODULE(Utility ) Assert
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                  Assert_EQ
!----------------------------------------------------------------------------

MODULE PROCEDURE assert_eq2
  IF (n1 .EQ. n2) THEN
    assert_eq2=n1
  ELSE
    CALL ErrorMsg( &
      & File = __FILE__, &
      & Routine = "Assert_Eq()", &
      & Line = __LINE__, &
      & MSG = " Sizes of Matrices are not the same; Program Stopped " )
    STOP
  END IF
END PROCEDURE assert_eq2

!----------------------------------------------------------------------------
!                                                                  Assert_EQ
!----------------------------------------------------------------------------

MODULE PROCEDURE assert_eq3
  IF (n1 == n2 .and. n2 == n3) THEN
    assert_eq3=n1
  ELSE
    CALL ErrorMsg( &
      & File = __FILE__, &
      & Routine = "Assert_Eq()", &
      & Line = __LINE__, &
      & MSG = " Sizes of Matrices are not the same; Program Stopped " )
    STOP
  END IF
END PROCEDURE assert_eq3

!----------------------------------------------------------------------------
!                                                                  Assert_EQ
!----------------------------------------------------------------------------

MODULE PROCEDURE assert_eq4
  IF (n1 == n2 .and. n2 == n3 .and. n3 == n4) THEN
    assert_eq4=n1
  ELSE
    CALL ErrorMsg( &
      & File = __FILE__, &
      & Routine = "Assert_Eq()", &
      & Line = __LINE__, &
      & MSG = " Sizes of Matrices are not the same; Program Stopped " )
    STOP
  END IF
END PROCEDURE assert_eq4

!----------------------------------------------------------------------------
!                                                                  Assert_EQ
!----------------------------------------------------------------------------

MODULE PROCEDURE assert_eqn
  IF (all(nn(2:) == nn(1))) THEN
    assert_eqn=nn(1)
  ELSE
    CALL ErrorMsg( &
      & File = __FILE__, &
      & Routine = "Assert_Eq()", &
      & Line = __LINE__, &
      & MSG = " Sizes of Matrices are not the same; Program Stopped " )
    STOP
  END IF
END PROCEDURE assert_eqn

!----------------------------------------------------------------------------
!                                                               ASSERT_SHAPE
!----------------------------------------------------------------------------

MODULE PROCEDURE assert_shape_2
  INTEGER( I4B ) :: shape_mat( 2 )
  shape_mat = SHAPE( Mat )
  IF (all(shape_mat == s) ) THEN
    RETURN
  ELSE
    CALL ErrorMsg( &
      & File = file, &
      & Routine = routine, &
      & Line = line, &
      & MSG = msg )
    STOP
  END IF
END PROCEDURE assert_shape_2

!----------------------------------------------------------------------------
!                                                               ASSERT_SHAPE
!----------------------------------------------------------------------------

MODULE PROCEDURE assert_shape_3
  INTEGER( I4B ) :: shape_mat( 3 )
  shape_mat = SHAPE( Mat )
  IF (all(shape_mat == s) ) THEN
    RETURN
  ELSE
    CALL ErrorMsg( File = file, Routine = routine, Line = line, &
      & MSG = msg )
    STOP
  END IF
END PROCEDURE assert_shape_3

!----------------------------------------------------------------------------
!                                                               ASSERT_SHAPE
!----------------------------------------------------------------------------

MODULE PROCEDURE assert_shape_4
  INTEGER( I4B ) :: shape_mat( 4 )
  shape_mat = SHAPE( Mat )
  IF (all(shape_mat == s) ) THEN
    RETURN
  ELSE
    CALL ErrorMsg( File = file, Routine = routine, Line = line, &
      & MSG = msg )
    STOP
  END IF
END PROCEDURE assert_shape_4

END SUBMODULE Assert