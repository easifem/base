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

!----------------------------------------------------------------------------
!                                                         STConvectiveMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE Mat4_STConvectiveMatrix_2
!!
SELECT CASE (term1)
!!
CASE (DEL_NONE)
  SELECT CASE (term2)
!!
  CASE (DEL_X_ALL, DEL_X, DEL_Y, DEL_Z)
!!
!! term1 = none
!! term2 = dx/dy/dz
!! term2 = del_x_all
!!
    CALL STCM_1b(ans=ans, test=test, trial=trial, &
        & term1=term1, term2=term2, rho=rho, c=c, &
        & projectOn="trial")
  END SELECT
!!
!!
!!
CASE (DEL_X, DEL_Y, DEL_Z)
!!
  SELECT CASE (term2)
!!
  CASE (DEL_NONE)
!!
!! term1 = dx/dy/dz
!! term2 = none
!!

    CALL STCM_1b(ans=ans, test=test, trial=trial, &
        & term1=term1, term2=term2, rho=rho, c=c, &
        & projectOn="test")
!!
  CASE (DEL_t)
!!
!! term1 = dx/dy/dz
!! term2 = dt
!!
    CALL STCM_8b(ans=ans, test=test, trial=trial, &
        & term1=term1, term2=term2, rho=rho, c=c, &
        & projectOn="test")
!!
  CASE (DEL_x, DEL_y, DEL_z)
!!
!! term1 = dx/dy/dz
!! term2 = dx/dy/dz
!!
    CALL STCM_6b(ans=ans, test=test, trial=trial, &
         & term1=term1, term2=term2, rho=rho, c=c, &
         & projectOn=projectOn)
!!
  CASE (DEL_x_all)
!!
!! term1 = dx/dy/dz
!! term2 = del_x_all
!!
    IF (PRESENT(projectOn)) THEN
      IF (TRIM(projectOn) .EQ. 'trial') THEN
        CALL STCM_6b(ans=ans, test=test, trial=trial, &
             & term1=term1, term2=term2, rho=rho, c=c, &
             & projectOn='trial')
      ELSE
        CALL STCM_7b(ans=ans, test=test, trial=trial, &
            & term1=term1, term2=term2, c=c, rho=rho, &
            & projectOn="test")
      END IF
    ELSE
      CALL STCM_7b(ans=ans, test=test, trial=trial, &
           & term1=term1, term2=term2, c=c, rho=rho, &
           & projectOn="test")
    END IF
!!
  END SELECT
!!
!!
!!
CASE (DEL_X_ALL)
!!
  SELECT CASE (term2)
  CASE (DEL_NONE)
!!
!! term1 = del_x_all
!! term2 = none
!!
    CALL STCM_1b(ans=ans, test=test, trial=trial, &
        & term1=term1, term2=term2, c=c, rho=rho, &
        & projectOn=projectOn)
!!
  CASE (DEL_t)
!!
!! term1 = del_x_all
!! term2 = dt
!!
    CALL STCM_8b(ans=ans, test=test, trial=trial, &
         & term1=term1, term2=term2, c=c, rho=rho, &
         & projectOn='test')
!!
  CASE (DEL_x, DEL_y, DEL_z)
!!
!! term1 = del_x_all
!! term2 = dx/dy/dz
!!
    IF (PRESENT(projectOn)) THEN
      IF (TRIM(projectOn) .EQ. "test") THEN
        CALL STCM_6b(ans=ans, test=test, trial=trial, &
            & term1=term1, term2=term2, c=c, rho=rho, &
            & projectOn="test")
      ELSE
        CALL STCM_7b(ans=ans, test=test, trial=trial, &
             & term1=term1, term2=term2, c=c, rho=rho, &
             & projectOn="trial")
      END IF
    ELSE
      CALL STCM_7b(ans=ans, test=test, trial=trial, &
           & term1=term1, term2=term2, c=c, rho=rho, &
           & projectOn="trial")
    END IF
!!
  CASE (DEL_x_all)
!!
!! term1 = del_x_all
!! term2 = del_x_all
!!
    CALL STCM_7b(ans=ans, test=test, trial=trial, &
         & term1=term1, term2=term2, c=c, rho=rho, &
         & projectOn=projectOn )
!!
  END SELECT
!!
!!
CASE (DEL_t)
!!
  SELECT CASE (term2)
!!
  CASE (DEL_X, DEL_Y, DEL_Z)
!!
!! term1 = dt
!! term2 = dx/dy/dz
!!
    CALL STCM_8b(ans=ans, test=test, trial=trial, &
        & term1=term1, term2=term2, rho=rho, c=c, &
        & projectOn="trial")
!!
  CASE (DEL_X_ALL)
!!
!! term1 = dt
!! term2 = del_x_all
!!
    CALL STCM_8b(ans=ans, test=test, trial=trial, &
         & term1=term1, term2=term2, c=c, rho=rho, &
         & projectOn='trial')
!!
  END SELECT
END SELECT

END PROCEDURE Mat4_STConvectiveMatrix_2
