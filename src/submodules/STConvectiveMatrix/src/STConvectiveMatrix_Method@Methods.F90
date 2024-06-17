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

SUBMODULE(STConvectiveMatrix_Method) Methods
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
#include "./STCM_9.inc"
#include "./STCM_10.inc"
#include "./STCM_11.inc"
#include "./STCM_12.inc"
#include "./STCM_13.inc"
#include "./STCM_14.inc"
#include "./STCM_15.inc"
#include "./STCM_16.inc"
#include "./STCM_17.inc"

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

PURE SUBROUTINE MakeDiagonalCopiesIJab(ans, ncopy)
  REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: ans(:, :, :, :)
  INTEGER(I4B), INTENT(IN) :: ncopy
  !!
  REAL(DFP), ALLOCATABLE :: m2(:, :), m4(:, :, :, :)
  INTEGER(I4B) :: a, b
  !!
  m4 = ans
  !!
  CALL Reallocate(ans, &
    & ncopy * SIZE(m4, 1), &
    & ncopy * SIZE(m4, 2), &
    & SIZE(m4, 3), &
    & SIZE(m4, 4))
  !!
  DO b = 1, SIZE(m4, 4)
    DO a = 1, SIZE(m4, 3)
      CALL MakeDiagonalCopies(from=m4(:, :, a, b), to=m2, ncopy=ncopy)
      ans(:, :, a, b) = m2
    END DO
  END DO
  !!
  DEALLOCATE (m2, m4)
END SUBROUTINE MakeDiagonalCopiesIJab

!----------------------------------------------------------------------------
!                                                         STConvectiveMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE Mat4_STConvectiveMatrix_1
!!
SELECT CASE (term1)
!!
!!
!!
!!
CASE (DEL_NONE)
  !!
  SELECT CASE( term2 )
  CASE( DEL_X_ALL )
    !!
    !! term1 = none
    !! term2 = del_x_all
    !!
    CALL STCM_3a(ans=ans, test=test, trial=trial, term1=term1, &
      & term2=term2, opt=opt)
    !!
  CASE( DEL_X, DEL_Y, DEL_Z )
    !!
    !! term1 = none
    !! term2 = dx/dy/dz
    !!
    CALL STCM_2a(ans=ans, test=test, trial=trial, term1=term1, &
      & term2=term2, opt=opt)
    !!
  END SELECT
!!
!!
!!
!!
CASE (DEL_X, DEL_Y, DEL_Z)
    !!
  SELECT CASE( term2 )
  CASE( DEL_NONE )
    !!
    !! term1 = dx/dy/dz
    !! term2 = none
    !!
    CALL STCM_2b(ans=ans, test=test, trial=trial, term1=term1, &
      & term2=term2, opt=opt)
    !!
  CASE( DEL_X, DEL_Y, DEL_Z )
    !!
    !! term1 = dx/dy/dz
    !! term2 = DEL_X, DEL_Y, DEL_Z
    !! TODO
    !!
  CASE( DEL_X_ALL )
    !!
    !! term1 = dx/dy/dz
    !! term2 = DEL_X_ALL
    !! TODO
    !!
  CASE( DEL_T )
    !!
    !! term1 = dx/dy/dz
    !! term2 = dt
    !!
    CALL STCM_4b(ans=ans, test=test, trial=trial, term1=term1, &
      & term2=term2, opt=opt)
    !!
  END SELECT
!!
!!
!!
!!
CASE (DEL_X_ALL)
    !!
  SELECT CASE( term2 )
  CASE( DEL_NONE )
    !!
    !! term1 = del_x_all
    !! term2 = del_none
    !!
    CALL STCM_3b(ans=ans, test=test, trial=trial, term1=term1, &
      & term2=term2, opt=opt)
    !!
  CASE( DEL_X, DEL_Y, DEL_Z )
    !!
    !! term1 = del_x_all
    !! term2 = del_x, del_y, del_z
    !! TODO
    !!
  CASE( DEL_X_ALL )
    !!
    !! term1 = del_x_all
    !! term2 = del_x_all
    !! TODO
    !!
  CASE( DEL_T )
    !!
    !! term1 = del_x_all
    !! term2 = del_t
    !!
    CALL STCM_5b(ans=ans, test=test, trial=trial, term1=term1, &
      & term2=term2, opt=opt)
    !!
  END SELECT
!!
!!
!!
!!
CASE (DEL_T)
    !!
  SELECT CASE( term2 )
    !!case( DEL_NONE )
    !!
    !! term1 = del_t
    !! term2 = del_none
    !! NOT POSSIBLE
    !!
  CASE( DEL_X, DEL_Y, DEL_Z )
    !!
    !! term1 = del_t
    !! term2 = del_x, del_y, del_z
    CALL STCM_4a(ans=ans, test=test, trial=trial, term1=term1, &
      & term2=term2, opt=opt)
    !!
  CASE( DEL_X_ALL )
    !!
    !! term1 = del_t
    !! term2 = del_x_all
    !!
    CALL STCM_5a(ans=ans, test=test, trial=trial, term1=term1, &
      & term2=term2, opt=opt)
    !!
    !! case( DEL_T )
    !!
    !! term1 = del_t
    !! term2 = del_t
    !! NOT POSSIBLE
    !!
  END SELECT
!!
END SELECT
!!
!!
!!
!!
END PROCEDURE Mat4_STConvectiveMatrix_1

!----------------------------------------------------------------------------
!                                                         STConvectiveMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE Mat4_STConvectiveMatrix_2
!!
SELECT CASE (term1)
!!
!!
!!
!!
CASE (DEL_NONE)
  !!
  SELECT CASE( term2 )
  CASE( DEL_X_ALL )
    !!
    !! term1 = none
    !! term2 = del_x_all
    !!
    CALL STCM_3a(ans=ans, test=test, trial=trial, term1=term1, &
      & term2=term2, c=c, opt=opt)
    !!
  CASE( DEL_X, DEL_Y, DEL_Z )
    !!
    !! term1 = none
    !! term2 = dx/dy/dz
    !!
    CALL STCM_2a(ans=ans, test=test, trial=trial, term1=term1, &
      & term2=term2, c=c, opt=opt)
    !!
  END SELECT
!!
!!
!!
!!
CASE (DEL_X, DEL_Y, DEL_Z)
    !!
  SELECT CASE( term2 )
  CASE( DEL_NONE )
    !!
    !! term1 = dx/dy/dz
    !! term2 = none
    !!
    CALL STCM_2b(ans=ans, test=test, trial=trial, term1=term1, &
      & term2=term2, c=c, opt=opt)
    !!
  CASE( DEL_X, DEL_Y, DEL_Z )
    !!
    !! term1 = dx/dy/dz
    !! term2 = DEL_X, DEL_Y, DEL_Z
    !! TODO
    !!
  CASE( DEL_X_ALL )
    !!
    !! term1 = dx/dy/dz
    !! term2 = DEL_X_ALL
    !! TODO
    !!
  CASE( DEL_T )
    !!
    !! term1 = dx/dy/dz
    !! term2 = dt
    !!
    CALL STCM_4b(ans=ans, test=test, trial=trial, term1=term1, &
      & term2=term2, c=c, opt=opt)
    !!
  END SELECT
!!
!!
!!
!!
CASE (DEL_X_ALL)
    !!
  SELECT CASE( term2 )
  CASE( DEL_NONE )
    !!
    !! term1 = del_x_all
    !! term2 = del_none
    !!
    CALL STCM_3b(ans=ans, test=test, trial=trial, term1=term1, &
      & term2=term2, c=c, opt=opt)
    !!
  CASE( DEL_X, DEL_Y, DEL_Z )
    !!
    !! term1 = del_x_all
    !! term2 = del_x, del_y, del_z
    !! TODO
    !!
  CASE( DEL_X_ALL )
    !!
    !! term1 = del_x_all
    !! term2 = del_x_all
    !! TODO
    !!
  CASE( DEL_T )
    !!
    !! term1 = del_x_all
    !! term2 = del_t
    !!
    CALL STCM_5b(ans=ans, test=test, trial=trial, term1=term1, &
      & term2=term2, c=c, opt=opt)
    !!
  END SELECT
!!
!!
!!
!!
CASE (DEL_T)
    !!
  SELECT CASE( term2 )
    !!case( DEL_NONE )
    !!
    !! term1 = del_t
    !! term2 = del_none
    !! NOT POSSIBLE
    !!
  CASE( DEL_X, DEL_Y, DEL_Z )
    !!
    !! term1 = del_t
    !! term2 = del_x, del_y, del_z
    !!
    CALL STCM_4a(ans=ans, test=test, trial=trial, term1=term1, &
      & term2=term2, c=c, opt=opt)
    !!
  CASE( DEL_X_ALL )
    !!
    !! term1 = del_t
    !! term2 = del_x_all
    !!
    CALL STCM_5a(ans=ans, test=test, trial=trial, term1=term1, &
      & term2=term2, c=c, opt=opt)
    !!
    !! case( DEL_T )
    !!
    !! term1 = del_t
    !! term2 = del_t
    !! NOT POSSIBLE
    !!
  END SELECT
!!
END SELECT
!!
!!
!!
!!
END PROCEDURE Mat4_STConvectiveMatrix_2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE Mat4_STConvectiveMatrix_3
  !!
SELECT CASE (term1)
CASE (DEL_NONE)
    !!
    !! term1 = none
    !! term2 = del_x, del_y, del_z, del_x_all
    !! projecton = trial (not needed)
    !!
  CALL STCM_1a(ans=ans, test=test, trial=trial, term1=term1, &
    & term2=term2, c=c, opt=opt)
  !!
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
    CALL STCM_1b(ans=ans, test=test, trial=trial, term1=term1, &
      & term2=term2, c=c, opt=opt)
      !!
  CASE (DEL_t)
      !!
      !! term1 = dx/dy/dz
      !! term2 = dt
      !! c = vector
      !!
    CALL STCM_8b(ans=ans, test=test, trial=trial, c=c, term1=term1, &
      & term2=term2, opt=opt)
      !!
  CASE (DEL_x, DEL_y, DEL_z)
      !!
      !! term1 = dx/dy/dz
      !! term2 = dx/dy/dz
      !! c = vector
      !!
    IF (TRIM(projecton) .EQ. "trial") THEN
      CALL STCM_6a(ans=ans, test=test, trial=trial, &
        & term1=term1, term2=term2, c=c, &
        & projecton=projecton, opt=opt)
    ELSE
      CALL STCM_6b(ans=ans, test=test, trial=trial, &
        & term1=term1, term2=term2, c=c, &
        & projecton=projecton, opt=opt)
    END IF
      !!
  CASE (DEL_x_all)
      !!
      !! term1 = dx/dy/dz
      !! term2 = del_x_all
      !!
    CALL STCM_7b(ans=ans, test=test, trial=trial, &
      & term1=term1, term2=term2, c=c, opt=opt)
      !!
  END SELECT
  !!
  !!
  !!
  !!
CASE (DEL_X_ALL)
    !!
  SELECT CASE (term2)
  CASE (DEL_NONE)
      !!
      !! term1 = del_x_all
      !! term2 = del_none
      !!
    CALL STCM_1b(ans=ans, test=test, trial=trial, term1=term1, &
      & term2=term2, c=c, opt=opt)
      !!
  CASE (DEL_T)
      !!
      !! term1 = del_x_all
      !! term2 = del_t
      !!
    CALL STCM_8b(ans=ans, test=test, trial=trial, term1=term1, &
      & term2=term2, c=c, opt=opt)
      !!
  CASE (DEL_X, DEL_Y, DEL_Z)
      !!
      !! term1 = del_x_all
      !! term2 = del_x, del_y, del_z
      !!
    CALL STCM_7a(ans=ans, test=test, trial=trial, &
      & term1=term1, term2=term2, c=c, opt=opt)
      !!
  END SELECT
  !!
  !!
  !!
  !!
CASE (DEL_t)
    !!
  CALL STCM_8a(ans=ans, test=test, trial=trial, c=c, term1=term1, &
    & term2=term2, opt=opt)
    !!
END SELECT
  !!
  !!
  !!
  !!
END PROCEDURE Mat4_STConvectiveMatrix_3

!----------------------------------------------------------------------------
!                                                         STConvectiveMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE Mat4_STConvectiveMatrix_4
  !!
SELECT CASE (term1)
!!
!!
!!
!!
CASE (DEL_NONE)
  !!
  !! term1 = del_none
  !! term2 = del_x/del_y/del_z
  !! term2 = del_x_all
  !!
  CALL STCM_9a(ans=ans, test=test, trial=trial, &
    & term1=term1, term2=term2, rho=c1, c=c2, opt=opt)
!!
!!
!!
!!
CASE (DEL_X, DEL_Y, DEL_Z)
  !!
  SELECT CASE (term2)
  !!
  CASE (DEL_NONE)
    !!
    !! term1 = del_x, del_y, del_z
    !! term2 = del_none
    !!
    CALL STCM_9b(ans=ans, test=test, trial=trial, &
      & term1=term1, term2=term2, rho=c1, c=c2, opt=opt)
    !!
  CASE (DEL_t)
    !!
    !! term1 = del_x, del_y, del_z
    !! term2 = del_t
    !!
    CALL STCM_12b(ans=ans, test=test, trial=trial, &
      & term1=term1, term2=term2, rho=c1, c=c2, opt=opt)
    !!
  CASE (DEL_x, DEL_y, DEL_z)
    !!
    !! term1 = del_x, del_y, del_z
    !! term2 = del_x, del_y, del_z
    !!
    IF (TRIM(projecton) .EQ. "trial") THEN
      CALL STCM_10a(ans=ans, test=test, trial=trial, term1=term1, &
        & term2=term2, rho=c1, c=c2, opt=opt)
    ELSE
      CALL STCM_10b(ans=ans, test=test, trial=trial, term1=term1, &
        & term2=term2, rho=c1, c=c2, opt=opt)
    END IF
    !!
  CASE (DEL_x_all)
    !!
    !! term1 = del_x, del_y, del_z
    !! term2 = del_x_all
    !!
    CALL STCM_11b(ans=ans, test=test, trial=trial, &
      & term1=term1, term2=term2, rho=c1, c=c2, opt=opt)
    !!
  END SELECT
  !!
  !!
  !!
  !!
CASE (DEL_X_ALL)
  !!
  SELECT CASE (term2)
  CASE (DEL_NONE)
    !!
    !! term1 = del_x_all
    !! term2 = del_none
    !!
    CALL STCM_9b(ans=ans, test=test, trial=trial, &
      & term1=term1, term2=term2, rho=c1, c=c2, opt=opt)
    !!
  CASE (DEL_t)
    !!
    !! term1 = del_x_all
    !! term2 = del_t
    !!
    CALL STCM_12b(ans=ans, test=test, trial=trial, &
      & term1=term1, term2=term2, rho=c1, c=c2, opt=opt)
    !!
  CASE (DEL_x, DEL_y, DEL_z)
    !!
    !! term1 = del_x_all
    !! term2 = del_x, del_y, del_z
    !!
    CALL STCM_11a(ans=ans, test=test, trial=trial, &
      & term1=term1, term2=term2, rho=c1, c=c2, opt=opt)
    !!
  CASE (DEL_x_all)
    !!
    !! term1 = del_x_all
    !! term2 = del_x_all
    !!
    IF (TRIM(projecton) .EQ. "trial") THEN
      CALL STCM_11a(ans=ans, test=test, trial=trial, &
        & term1=term1, term2=del_x, rho=c1, c=c2, opt=opt)
    ELSE
      CALL STCM_11b(ans=ans, test=test, trial=trial, &
        & term1=del_x, term2=term2, rho=c1, c=c2, opt=opt)
    END IF
    !!
  END SELECT
  !!
  !!
  !!
  !!
CASE (DEL_t)
  !!
  !! term1 = del_t
  !! term2 = del_x, del_y, del_z
  !! term2 = del_x_all
  !!
  CALL STCM_12a(ans=ans, test=test, trial=trial, &
    & term1=term1, term2=term2, rho=c1, c=c2, opt=opt)
  !!
END SELECT
  !!
END PROCEDURE Mat4_STConvectiveMatrix_4

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE Mat4_STConvectiveMatrix_5
SELECT CASE (term1)
CASE (del_none)
  !!
  !! term1 = del_none
  !! term2 = del_x,y,z,x_all
  !!
  IF (opt(1) .EQ. 1) THEN
    CALL STCM_13a(ans=ans, test=test, trial=trial, &
      & term1=term1, term2=term2, c=c, opt=1)
  ELSE
    CALL STCM_13b(ans=ans, test=test, trial=trial, &
      & term1=term1, term2=term2, c=c, opt=2)
  END IF
  !!
CASE (del_t)
  !!
  !! term1 = del_t
  !! term2 = del_x,y,z,x_all
  !!
  IF (opt(1) .EQ. 1) THEN
    CALL STCM_14a(ans=ans, test=test, trial=trial, &
      & term1=term1, term2=term2, c=c, opt=1)
  ELSE
    CALL STCM_14b(ans=ans, test=test, trial=trial, &
      & term1=term1, term2=term2, c=c, opt=2)
  END IF
  !!
CASE DEFAULT
  !!
  SELECT CASE (term2)
  CASE (del_none)
    !!
    !! term2 = del_x,y,z,x_all
    !! term1 = del_none
    !!
    IF (opt(1) .EQ. 1) THEN
      CALL STCM_13c(ans=ans, test=test, trial=trial, &
        & term1=term1, term2=term2, c=c, opt=1)
    ELSE
      CALL STCM_13d(ans=ans, test=test, trial=trial, &
        & term1=term1, term2=term2, c=c, opt=2)
    END IF
  !!
  CASE (del_t)
    !!
    !! term2 = del_x,y,z,x_all
    !! term1 = del_t
    !!
    IF (opt(1) .EQ. 1) THEN
      CALL STCM_14c(ans=ans, test=test, trial=trial, &
      & term1=term1, term2=term2, c=c, opt=1)
    ELSE
      CALL STCM_14d(ans=ans, test=test, trial=trial, &
      & term1=term1, term2=term2, c=c, opt=2)
    END IF
    !!
  END SELECT
END SELECT
!!
END PROCEDURE Mat4_STConvectiveMatrix_5

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE Mat4_STConvectiveMatrix_6
  !!
  SELECT CASE (term1)
  !!
  !!
  !!
  !!
  CASE (del_none)
    !!
    SELECT CASE( term2 )
      !!
    CASE( del_none )
      !!
      !! not possible
      !!
    CASE( del_t )
      !!
      !! not possible
      !!
    CASE( del_x, del_y, del_z )
      !!
      !! term1 = del_none
      !! term2 = del_x, del_y, del_z, del_x_all
      !!
      IF (opt(1) .EQ. 1) THEN
        CALL STCM_15a(ans=ans, test=test, trial=trial, &
        & term1=term1, term2=term2, rho=c1, c=c2, opt=1)
      ELSE
        CALL STCM_15b(ans=ans, test=test, trial=trial, &
        & term1=term1, term2=term2, rho=c1, c=c2, opt=2)
      END IF
      !!
    END SELECT
  !!
  !!
  !!
  !!
  CASE (del_t)
    !!
    SELECT CASE( term2 )
      !!
    CASE( del_none )
      !!
      !! not possible
      !!
    CASE( del_t )
      !!
      !! not possible
      !!
    CASE( del_x, del_y, del_z )
      !!
      !! term1 = del_t
      !! term2 = del_x, del_y, del_z
      !!
      IF (opt(1) .EQ. 1) THEN
        CALL STCM_16a(ans=ans, test=test, trial=trial, &
        & term1=term1, term2=term2, rho=c1, c=c2, opt=1)
      ELSE
        CALL STCM_16b(ans=ans, test=test, trial=trial, &
        & term1=term1, term2=term2, rho=c1, c=c2, opt=2)
      END IF
      !!
    END SELECT
  !!
  !!
  !!
  !!
  CASE (del_x, del_y, del_z)
    !!
    SELECT CASE ( term2 )
      !!
    CASE( del_none )
      !!
      !! term1 = del_x, del_y, del_z
      !! term2 = del_none
      !!
      IF (opt(1) .EQ. 1) THEN
        CALL STCM_15c(ans=ans, test=test, trial=trial, &
        & term1=term1, term2=term2, rho=c1, c=c2, opt=1)
      ELSE
        CALL STCM_15d(ans=ans, test=test, trial=trial, &
        & term1=term1, term2=term2, rho=c1, c=c2, opt=2)
      END IF
      !!
    CASE( del_t )
      !!
      !! term1 = del_x, del_y, del_z, del_x_all
      !! term2 = del_t
      !!
      IF (opt(1) .EQ. 1) THEN
        CALL STCM_16c(ans=ans, test=test, trial=trial, &
        & term1=term1, term2=term2, rho=c1, c=c2, opt=1)
      ELSE
        CALL STCM_16d(ans=ans, test=test, trial=trial, &
        & term1=term1, term2=term2, rho=c1, c=c2, opt=2)
      END IF
      !!
      !!
    CASE( del_x, del_y, del_z )
      !!
      !! term1 = del_x, del_y, del_z, del_x_all
      !! term2 = del_x, del_y, del_z, del_x_all
      !!
      IF( TRIM(projecton) .EQ. 'test' ) THEN
        IF (opt(1) .EQ. 1) THEN
          CALL STCM_17a(ans=ans, test=test, trial=trial, &
          & term1=term1, term2=term2, rho=c1, c=c2, opt=1)
        ELSE
          CALL STCM_17b(ans=ans, test=test, trial=trial, &
          & term1=term1, term2=term2, rho=c1, c=c2, opt=2)
        END IF
      ELSE
        IF (opt(1) .EQ. 1) THEN
          CALL STCM_17c(ans=ans, test=test, trial=trial, &
          & term1=term1, term2=term2, rho=c1, c=c2, opt=1)
        ELSE
          CALL STCM_17d(ans=ans, test=test, trial=trial, &
          & term1=term1, term2=term2, rho=c1, c=c2, opt=2)
        END IF
      END IF
      !!
    END SELECT
    !!
  END SELECT
  !!
END PROCEDURE Mat4_STConvectiveMatrix_6

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE Methods
