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

SUBMODULE(AssertUtility) Methods
USE ErrorHandling, ONLY: ErrorMsg
USE Display_Method, ONLY: ToString
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                  Assert_EQ
!----------------------------------------------------------------------------

MODULE PROCEDURE assert_eq2
IF (n1 .EQ. n2) THEN
  ans = n1
ELSE
#ifdef DEBUG_VER
  CALL ErrorMsg( &
    file=__FILE__, routine="Assert_Eq2()", line=__LINE__, &
    msg="n1 and n2 are not same; n1= "//ToString(n1)//" n2= "//ToString(n2))
  STOP
#endif
END IF
END PROCEDURE assert_eq2

!----------------------------------------------------------------------------
!                                                                  Assert_EQ
!----------------------------------------------------------------------------

MODULE PROCEDURE assert_eq3
IF (n1 == n2 .AND. n2 == n3) THEN
  ans = n1
ELSE
#ifdef DEBUG_VER
  CALL ErrorMsg( &
    file=__FILE__, routine="Assert_Eq2()", line=__LINE__, &
    msg="n1, n2, n3 are not same; n1= "//ToString(n1)// &
    " n2= "//ToString(n2)//" n3= "//ToString(n3))
  STOP
#endif
END IF
END PROCEDURE assert_eq3

!----------------------------------------------------------------------------
!                                                                  Assert_EQ
!----------------------------------------------------------------------------

MODULE PROCEDURE assert_eq4
IF (n1 == n2 .AND. n2 == n3 .AND. n3 == n4) THEN
  ans = n1
ELSE
#ifdef DEBUG_VER
  CALL ErrorMsg( &
    file=__FILE__, routine="Assert_Eq2()", line=__LINE__, &
    msg="n1, n2, n3, n4 are not same; n1= "//ToString(n1)// &
    " n2= "//ToString(n2)// &
    " n3= "//ToString(n3)// &
    " n4= "//ToString(n4))
  STOP
#endif
END IF
END PROCEDURE assert_eq4

!----------------------------------------------------------------------------
!                                                                  Assert_EQ
!----------------------------------------------------------------------------

MODULE PROCEDURE assert_eqn
IF (ALL(nn(2:) == nn(1))) THEN
  ans = nn(1)
ELSE
#ifdef DEBUG_VER
  CALL ErrorMsg( &
    file=__FILE__, routine="Assert_Eq2()", line=__LINE__, &
    msg="n1, n2, n3, n4 are not same")
  STOP
#endif
END IF
END PROCEDURE assert_eqn

!----------------------------------------------------------------------------
!                                                               ASSERT_SHAPE
!----------------------------------------------------------------------------

MODULE PROCEDURE assert_shape_2
#ifdef DEBUG_VER
INTEGER(I4B) :: shape_mat(2)
shape_mat = SHAPE(Mat)
IF (.NOT. ALL(shape_mat == s)) THEN
  CALL ErrorMsg(file=file, routine=routine, line=line, msg=msg)
  STOP
END IF
#endif
END PROCEDURE assert_shape_2

!----------------------------------------------------------------------------
!                                                               ASSERT_SHAPE
!----------------------------------------------------------------------------

MODULE PROCEDURE assert_shape_3
#ifdef DEBUG_VER
INTEGER(I4B) :: shape_mat(3)
shape_mat = SHAPE(Mat)
IF (.NOT. ALL(shape_mat == s)) THEN
  CALL ErrorMsg(file=file, routine=routine, line=line, msg=msg)
  STOP
END IF
#endif
END PROCEDURE assert_shape_3

!----------------------------------------------------------------------------
!                                                               ASSERT_SHAPE
!----------------------------------------------------------------------------

MODULE PROCEDURE assert_shape_4
#ifdef DEBUG_VER
INTEGER(I4B) :: shape_mat(4)
shape_mat = SHAPE(Mat)
IF (.NOT. ALL(shape_mat == s)) THEN
  CALL ErrorMsg(file=file, routine=routine, line=line, msg=msg)
  STOP
END IF
#endif
END PROCEDURE assert_shape_4

!----------------------------------------------------------------------------
!                                                                     Assert
!----------------------------------------------------------------------------

MODULE PROCEDURE assert_2
#ifdef DEBUG_VER
IF (n1 .NE. n2) THEN
  CALL ErrorMsg(file=file, routine=routine, line=line, msg=msg)
  STOP
END IF
#endif
END PROCEDURE assert_2

!----------------------------------------------------------------------------
!                                                                  Assert_EQ
!----------------------------------------------------------------------------

MODULE PROCEDURE assert_3
#ifdef DEBUG_VER
IF (n1 .NE. n2 .OR. n2 .NE. n3) THEN
  CALL ErrorMsg(file=file, routine=routine, line=line, msg=msg)
  STOP
END IF
#endif
END PROCEDURE assert_3

!----------------------------------------------------------------------------
!                                                                  Assert_EQ
!----------------------------------------------------------------------------

MODULE PROCEDURE assert_4
#ifdef DEBUG_VER
IF (n1 .NE. n2 .OR. n2 .NE. n3 .OR. n3 .NE. n4) THEN
  CALL ErrorMsg(file=file, routine=routine, line=line, msg=msg)
  STOP
END IF
#endif
END PROCEDURE assert_4

!----------------------------------------------------------------------------
!                                                                  Assert_EQ
!----------------------------------------------------------------------------

MODULE PROCEDURE assert_n
#ifdef DEBUG_VER
IF (.NOT. ALL(nn(2:) == nn(1))) THEN
  CALL ErrorMsg(file=file, routine=routine, line=line, msg=msg)
  STOP
END IF
#endif
END PROCEDURE assert_n

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE Methods
