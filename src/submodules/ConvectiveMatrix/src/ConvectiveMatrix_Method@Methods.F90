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

SUBMODULE(ConvectiveMatrix_Method) Methods
USE BaseMethod
IMPLICIT NONE
CONTAINS

#include "./CM_1.inc"
#include "./CM_2.inc"
#include "./CM_3.inc"
#include "./CM_4.inc"
#include "./CM_5.inc"
#include "./CM_6.inc"
#include "./CM_7.inc"
#include "./CM_8.inc"
#include "./CM_9.inc"
#include "./CM_10.inc"

!----------------------------------------------------------------------------
!                                                           ConvectiveMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE ConvectiveMatrix_1
IF( term1 .EQ. DEL_NONE ) THEN
!!
!!
!!
!!
  IF( term2 .EQ. DEL_X_ALL ) THEN
    !!
    !! del_none
    !! del_x_all
    !!
    CALL CM_9(ans=ans, test=test, trial=trial, &
      & term1=term2, term2=term2, opt=opt)
    !!
  ELSE
    !!
    !! del_none
    !! del_x, del_y, del_z
    !!
    CALL CM_7(ans=ans, test=test, trial=trial, &
      & term1=term2, term2=term2, opt=opt)
    !!
  END IF
!!
!!
!!
!!
ELSE
  !!
  !! term2 .eq. del_none
  !!
  IF( term1 .EQ. del_x_all ) THEN
    !!
    !! del_x_all
    !! del_none
    !!
    CALL CM_10(ans=ans, test=test, trial=trial, &
      & term1=term2, term2=term2, opt=opt)
    !!
  ELSE
    !!
    !! del_x, del_y, del_z
    !! del_none
    !!
    CALL CM_8(ans=ans, test=test, trial=trial, &
      & term1=term2, term2=term2, opt=opt)
    !!
  END IF
END IF
!!
END PROCEDURE ConvectiveMatrix_1

!----------------------------------------------------------------------------
!                                                           ConvectiveMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE ConvectiveMatrix_2
  !!
  !! scalar
  !!
  IF( term1 .EQ. del_none ) THEN
    IF( term2 .EQ. del_x_all ) THEN
      CALL CM_5(ans=ans, test=test, trial=trial, c=c, &
        & term1=term1, term2=term2, opt=opt)
    ELSE
      CALL CM_3(ans=ans, test=test, trial=trial, c=c, &
        & term1=term2, term2=term2, opt=opt)
    END IF
  ELSE
    IF( term1 .EQ. del_x_all ) THEN
      CALL CM_6(ans=ans, test=test, trial=trial, c=c, &
        & term1=term1, term2=term2, opt=opt)
    ELSE
      CALL CM_4(ans=ans, test=test, trial=trial, c=c, &
        & term1=term2, term2=term2, opt=opt)
    END IF
  END IF
  !!
END PROCEDURE ConvectiveMatrix_2

!----------------------------------------------------------------------------
!                                                           ConvectiveMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE ConvectiveMatrix_3
  !!
  IF( term1 .EQ. del_none ) THEN
    CALL CM_1(ans=ans, test=test, trial=trial, c=c, &
      & term1=term1, term2=term2, opt=opt)
  ELSE
    CALL CM_2(ans=ans, test=test, trial=trial, c=c, &
      & term1=term1, term2=term2, opt=opt)
  END IF
  !!
END PROCEDURE ConvectiveMatrix_3

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE Methods
