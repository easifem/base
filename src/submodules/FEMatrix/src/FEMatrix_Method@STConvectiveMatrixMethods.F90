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

SUBMODULE(FEMatrix_Method) STConvectiveMatrixMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

#include "./STCM_1.inc"
#include "./STCM_2.inc"
#include "./STCM_3.inc"
#include "./STCM_4.inc"
#include "./STCM_5.inc"
#include "./STCM_6.inc"
#include "./STCM_7.inc"
#include "./STCM_8.inc"

!----------------------------------------------------------------------------
!                                                         STConvectiveMatrix
!----------------------------------------------------------------------------
MODULE PROCEDURE Mat4_STConvectiveMatrix_1

INTEGER(I4B) :: rankC

IF (PRESENT(c)) THEN
  rankC = .rank.c
ELSE
  rankC = SCALAR
END IF
!!
SELECT CASE (term1)
!!
CASE (DEL_NONE)
  SELECT CASE (term2)
!!
  CASE (DEL_X_ALL)
!!
!! term1 = none
!! term2 = del_x_all
!! c = vector
!!
    IF (rankC .EQ. VECTOR) THEN
      CALL STCM_1(ans=ans, test=test, trial=trial, &
          & term1=term1, term2=term2, c=c, projectOn=projectOn)
!!
!! term1 = none
!! term2 = del_x_all
!! c = scalar
!!
    ELSEIF (rankC .EQ. SCALAR) THEN
      !! none_all_scalar_1()
      CALL STCM_3(ans=ans, test=test, trial=trial, &
          & term1=term1, term2=term2, c=c)
    END IF
!!
  CASE (DEL_X, DEL_Y, DEL_Z)
!!
!! term1 = none
!! term2 = dx/dy/dz
!! c = vector
!!
    IF (rankC .EQ. VECTOR) THEN
      CALL STCM_1(ans=ans, test=test, trial=trial, &
          & term1=term1, term2=term2, c=c, projectOn=projectOn)
!!
!! term1 = none
!! term2 = dx/dy/dz
!! c = scalar
!!
    ELSEIF (rankC .EQ. SCALAR) THEN
      CALL STCM_2(ans=ans, test=test, trial=trial, &
          & term1=term1, term2=term2, dim=term2, c=c)
    END IF
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
!! c = vector
!!
    IF (rankC .EQ. VECTOR) THEN
      CALL STCM_1(ans=ans, test=test, trial=trial, &
          & term1=term1, term2=term2, c=c, projectOn=projectOn)
!!
!! term1 = dx/dy/dz
!! term2 = none
!! c = scalar
!!
    ELSEIF (rankC .EQ. SCALAR) THEN
      CALL STCM_2(ans=ans, test=test, trial=trial, &
           & term1=term1, term2=term2, dim=term1, c=c)
    END IF
!!
  CASE (DEL_t)
!!
!! term1 = dx/dy/dz
!! term2 = dt
!! c = vector
!!
    IF (rankC .EQ. VECTOR) THEN
      CALL STCM_8(ans=ans, test=test, trial=trial, c=c, projectOn='test')
!!
!! term1 = dx/dy/dz
!! term2 = dt
!! c = scalar
!!
    ELSEIF (rankC .EQ. SCALAR) THEN
      CALL STCM_4(ans=ans, test=test, trial=trial, &
           & term1=term1, term2=term2, dim=term1, c=c)
    END IF
!!
  CASE (DEL_x, DEL_y, DEL_z)
!!
!! term1 = dx/dy/dz
!! term2 = dx/dy/dz
!! c = vector
!!
    IF (rankC .EQ. VECTOR) THEN
      CALL STCM_6(ans=ans, test=test, trial=trial, &
           & term1=term1, term2=term2, c=c, projectOn=projectOn)
!!
!! term1 = dx/dy/dz
!! term2 = dx/dy/dz
!! c = scalar
!!
    ELSEIF (rankC .EQ. SCALAR) THEN
       !! TODO
    END IF

  CASE (DEL_x_all)
!!
!! term1 = dx/dy/dz
!! term2 = del_x_all
!! c = vector
!!
    IF (rankC .EQ. VECTOR) THEN
      CALL STCM_7(ans=ans, test=test, trial=trial, &
           & term1=term1, term2=term2, c=c, projectOn="test")
!!
!! term1 = dx/dy/dz
!! term2 = del_x_all
!! c = scalar
!!
    ELSEIF (rankC .EQ. SCALAR) THEN
        !! TODO
    END IF

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
!! c = vector
!!
    IF (rankC .EQ. VECTOR) THEN
      CALL STCM_1(ans=ans, test=test, trial=trial, &
          & term1=term1, term2=term2, c=c, projectOn=projectOn)
!!
!! term1 = del_x_all
!! term2 = none
!! c = scalar
!!
    ELSEIF (rankC .EQ. SCALAR) THEN
      CALL STCM_3(ans=ans, test=test, trial=trial, &
          & term1=term1, term2=term2, c=c)
    END IF
!!
  CASE (DEL_t)
!!
!! term1 = del_x_all
!! term2 = dt
!! c = vector
    IF (rankC .EQ. VECTOR) THEN
      CALL STCM_8(ans=ans, test=test, trial=trial, c=c, projectOn='test')
!!
!! term1 = del_x_all
!! term2 = dt
!! c = scalar
!!
    ELSEIF (rankC .EQ. SCALAR) THEN
      CALL STCM_5(ans=ans, test=test, trial=trial, &
          & term1=term1, term2=term2, c=c)
    END IF
!!
  CASE (DEL_x, DEL_y, DEL_z)
!!
!! term1 = del_x_all
!! term2 = dx/dy/dz
!! c = vector
!!
    IF (rankC .EQ. VECTOR) THEN
      CALL STCM_7(ans=ans, test=test, trial=trial, &
           & term1=term1, term2=term2, c=c, projectOn="trial")
!!
!! term1 = del_x_all
!! term2 = dx/dy/dz
!! c = scalar
!! TODO
    ELSEIF (rankC .EQ. SCALAR) THEN
    END IF
!!
  CASE (DEL_x_all)
!!
!! term1 = del_x_all
!! term2 = del_x_all
!! c = vector
!!
    IF (rankC .EQ. VECTOR) THEN
      CALL STCM_7(ans=ans, test=test, trial=trial, &
           & term1=term1, term2=term2, c=c, projectOn="trial")
!!
!! term1 = del_x_all
!! term2 = del_x_all
!! c = scalar
!! TODO
    ELSEIF (rankC .EQ. SCALAR) THEN
    END IF
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
!! c = vector
!!
    IF (rankC .EQ. VECTOR) THEN
      CALL STCM_8(ans=ans, test=test, trial=trial, c=c, projectOn='trial')
!!
!! term1 = dt
!! term2 = dx/dy/dz
!! c = scalar
!!
    ELSEIF (rankC .EQ. SCALAR) THEN
      CALL STCM_4(ans=ans, test=test, trial=trial, &
          & term1=term1, term2=term2, dim=term2, c=c)
    END IF
!!
  CASE (DEL_X_ALL)
!!
!! term1 = dt
!! term2 = del_x_all
!! c = vector
!!
    IF (rankC .EQ. VECTOR) THEN
      CALL STCM_8(ans=ans, test=test, trial=trial, c=c, projectOn='trial')
!!
!! term1 = dt
!! term2 = del_x_all
!! c = scalar
!!
    ELSEIF (rankC .EQ. SCALAR) THEN
      CALL STCM_5(ans=ans, test=test, trial=trial, &
          & term1=term1, term2=term2, c=c)
    END IF
  END SELECT
END SELECT

END PROCEDURE Mat4_STConvectiveMatrix_1

!----------------------------------------------------------------------------
!                                                           ConvectiveMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE Mat2_STConvectiveMatrix_1
REAL(DFP), ALLOCATABLE :: mat4(:, :, :, :)
mat4 = STConvectiveMatrix(test=test, trial=trial, term1=term1, &
     & term2=term2, c=c, projectOn=projectOn)
CALL convert(from=mat4, to=ans)
IF (ALLOCATED(mat4)) DEALLOCATE (mat4)
END PROCEDURE Mat2_STConvectiveMatrix_1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE STConvectiveMatrixMethods
