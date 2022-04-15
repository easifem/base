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

SUBMODULE(STMassMatrix_Method) Methods
USE BaseMethod
IMPLICIT NONE
CONTAINS

#include "./STMM_1.inc"
#include "./STMM_2.inc"
#include "./STMM_3.inc"
#include "./STMM_4.inc"

#include "./STMM_5.inc"
#include "./STMM_6.inc"
#include "./STMM_7.inc"
#include "./STMM_8.inc"

#include "./STMM_9a.inc"
#include "./STMM_9b.inc"
#include "./STMM_9c.inc"
#include "./STMM_9d.inc"

#include "./STMM_10a.inc"
#include "./STMM_10b.inc"
#include "./STMM_10c.inc"
#include "./STMM_10d.inc"

#include "./STMM_11a.inc"
#include "./STMM_11b.inc"
#include "./STMM_11c.inc"
#include "./STMM_11d.inc"

#include "./STMM_12a.inc"
#include "./STMM_12b.inc"
#include "./STMM_12c.inc"
#include "./STMM_12d.inc"

#include "./STMM_13.inc"
#include "./STMM_14.inc"
#include "./STMM_15.inc"
#include "./STMM_16.inc"

#include "./STMM_17.inc"
#include "./STMM_18.inc"
#include "./STMM_19.inc"
#include "./STMM_20.inc"

#include "./STMM_21a.inc"
#include "./STMM_21b.inc"
#include "./STMM_21c.inc"
#include "./STMM_21d.inc"

#include "./STMM_22a.inc"
#include "./STMM_22b.inc"
#include "./STMM_22c.inc"
#include "./STMM_22d.inc"

#include "./STMM_23a.inc"
#include "./STMM_23b.inc"
#include "./STMM_23c.inc"
#include "./STMM_23d.inc"

#include "./STMM_24a.inc"
#include "./STMM_24b.inc"
#include "./STMM_24c.inc"
#include "./STMM_24d.inc"

#include "./STMM_25.inc"
#include "./STMM_26.inc"
#include "./STMM_27.inc"
#include "./STMM_28.inc"

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
!                                                                MassMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE mat4_STMassMatrix_1
  SELECT CASE (term1)
  !!
  !!
  !!
  !!
  CASE (del_t)
    !!
    SELECT CASE (term2)
    !!
    CASE (del_t)
      !! del_t
      !! del_t
      CALL STMM_4(ans=ans, test=test, trial=trial, term1=term1, &
        & term2=term2, opt=opt)
      !!
    CASE (del_none)
      !! del_t
      !! del_none
      CALL STMM_2(ans=ans, test=test, trial=trial, term1=term1, &
        & term2=term2, opt=opt)
      !!
    END SELECT
  !!
  !!
  !!
  !!
  CASE (del_none)
    !!
    SELECT CASE (term2)
    !!
    CASE (del_t)
      !! del_none
      !! del_t
      CALL STMM_3(ans=ans, test=test, trial=trial, term1=term1, &
        & term2=term2, opt=opt)
      !!
    CASE (del_none)
      !! del_none
      !! del_none
      CALL STMM_1(ans=ans, test=test, trial=trial, term1=term1, &
        & term2=term2, opt=opt)
      !!
    END SELECT
  END SELECT
  !!
END PROCEDURE mat4_STMassMatrix_1

!----------------------------------------------------------------------------
!                                                                MassMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE mat4_STMassMatrix_2
  !!
  SELECT CASE (term1)
  !!
  !!
  !!
  !!
  CASE (del_t)
    !!
    SELECT CASE (term2)
    !!
    CASE (del_t)
      !!
      !! del_t
      !! del_t
      !!
      CALL STMM_8(ans=ans, test=test, trial=trial, rho=rho, &
        & term1=term1, term2=term2, opt=opt)
    !!
    CASE (del_none)
      !!
      !! del_t
      !! del_none
      !!
      CALL STMM_6(ans=ans, test=test, trial=trial, rho=rho, &
        & term1=term1, term2=term2, opt=opt)
    !!
    END SELECT
  !!
  !!
  !!
  !!
  CASE (del_none)
    !!
    SELECT CASE (term2)
    !!
    CASE (del_t)
      !!
      !! del_none
      !! del_t
      !!
      CALL STMM_7(ans=ans, test=test, trial=trial, rho=rho, &
        & term1=term1, term2=term2, opt=opt)
    !!
    CASE (del_none)
      !!
      !! del_none
      !! del_none
      !!
      CALL STMM_5(ans=ans, test=test, trial=trial, rho=rho, &
        & term1=term1, term2=term2, opt=opt)
    !!
    END SELECT
  END SELECT
  !!
END PROCEDURE mat4_STMassMatrix_2

!----------------------------------------------------------------------------
!                                                                MassMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE mat4_STMassMatrix_3
  !!
  !! main
  !!
  SELECT CASE (term1)
  !!
  !!
  !!
  !!
  CASE (del_t)
    !!
    SELECT CASE (term2)
    !!
    CASE (del_t)
      !!
      !! del_t
      !! del_t
      !!
      SELECT CASE (opt)
      CASE (1)
        CALL STMM_12a(ans=ans, test=test, trial=trial, rho=rho, &
          & term1=term1, term2=term2)
      CASE (2)
        CALL STMM_12b(ans=ans, test=test, trial=trial, rho=rho, &
          & term1=term1, term2=term2)
      CASE (3)
        CALL STMM_12c(ans=ans, test=test, trial=trial, rho=rho, &
          & term1=term1, term2=term2)
      CASE (4)
        CALL STMM_12d(ans=ans, test=test, trial=trial, rho=rho, &
          & term1=term1, term2=term2)
      END SELECT
    !!
    CASE (del_none)
      !!
      !! del_t
      !! del_none
      !!
      SELECT CASE (opt)
      CASE (1)
        CALL STMM_10a(ans=ans, test=test, trial=trial, rho=rho, &
          & term1=term1, term2=term2)
      CASE (2)
        CALL STMM_10b(ans=ans, test=test, trial=trial, rho=rho, &
          & term1=term1, term2=term2)
      CASE (3)
        CALL STMM_10c(ans=ans, test=test, trial=trial, rho=rho, &
          & term1=term1, term2=term2)
      CASE (4)
        CALL STMM_10d(ans=ans, test=test, trial=trial, rho=rho, &
          & term1=term1, term2=term2)
      END SELECT
    !!
    END SELECT
  !!
  !!
  !!
  !!
  CASE (del_none)
    !!
    SELECT CASE (term2)
    !!
    CASE (del_t)
      !!
      !! del_none
      !! del_t
      !!
      SELECT CASE (opt)
      CASE (1)
        CALL STMM_11a(ans=ans, test=test, trial=trial, rho=rho, &
          & term1=term1, term2=term2)
      CASE (2)
        CALL STMM_11b(ans=ans, test=test, trial=trial, rho=rho, &
          & term1=term1, term2=term2)
      CASE (3)
        CALL STMM_11c(ans=ans, test=test, trial=trial, rho=rho, &
          & term1=term1, term2=term2)
      CASE (4)
        CALL STMM_11d(ans=ans, test=test, trial=trial, rho=rho, &
          & term1=term1, term2=term2)
      END SELECT
    !!
    CASE (del_none)
      !!
      !! del_none
      !! del_none
      !!
      SELECT CASE (opt)
      CASE (1)
        CALL STMM_9a(ans=ans, test=test, trial=trial, rho=rho, &
          & term1=term1, term2=term2)
      CASE (2)
        CALL STMM_9b(ans=ans, test=test, trial=trial, rho=rho, &
          & term1=term1, term2=term2)
      CASE (3)
        CALL STMM_9c(ans=ans, test=test, trial=trial, rho=rho, &
          & term1=term1, term2=term2)
      CASE (4)
        CALL STMM_9d(ans=ans, test=test, trial=trial, rho=rho, &
          & term1=term1, term2=term2)
      END SELECT
    !!
    END SELECT
  END SELECT
  !!
END PROCEDURE mat4_STMassMatrix_3

!----------------------------------------------------------------------------
!                                                                MassMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE mat4_STMassMatrix_4
  !!
  !! main
  !!
  SELECT CASE (term1)
  !!
  !!
  !!
  !!
  CASE (del_t)
    !!
    SELECT CASE (term2)
    !!
    CASE (del_t)
      !!
      !! del_t
      !! del_t
      !!
      CALL STMM_16(ans=ans, test=test, trial=trial, rho=rho, &
        & term1=term1, term2=term2)
    !!
    CASE (del_none)
      !!
      !! del_t,
      !! del_none
      !!
      CALL STMM_14(ans=ans, test=test, trial=trial, rho=rho, &
        & term1=term1, term2=term2)
    !!
    END SELECT
  !!
  !!
  !!
  !!
  CASE (del_none)
    !!
    SELECT CASE (term2)
    !!
    CASE (del_t)
      !!
      !! del_none,
      !! del_t
      !!
      CALL STMM_15(ans=ans, test=test, trial=trial, rho=rho, &
        & term1=term1, term2=term2)
    !!
    CASE (del_none)
      !!
      !! del_none,
      !! del_none,
      !!
      CALL STMM_13(ans=ans, test=test, trial=trial, rho=rho, &
        & term1=term1, term2=term2)
    !!
    END SELECT
  END SELECT
END PROCEDURE mat4_STMassMatrix_4

!----------------------------------------------------------------------------
!                                                                MassMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE mat4_STMassMatrix_5
  SELECT CASE (term1)
  !!
  !!
  !!
  !!
  CASE (del_t)
    !!
    SELECT CASE (term2)
    !!
    CASE (del_t)
    !!
    !! scalar
    !! scalar
    !! del_t
    !! del_t
    !!
      CALL STMM_20(ans=ans, test=test, trial=trial, c1=c1, c2=c2, &
        & term1=term1, term2=term2, opt=opt)
    !!
    CASE (del_none)
    !!
    !! scalar
    !! scalar
    !! del_t
    !! del_none
    !!
      CALL STMM_18(ans=ans, test=test, trial=trial, c1=c1, c2=c2, &
        & term1=term1, term2=term2, opt=opt)
    !!
    END SELECT
    !!
  CASE (del_none)
    !!
    SELECT CASE (term2)
    !!
    CASE (del_t)
    !!
    !! scalar
    !! scalar
    !! del_none
    !! del_t
    !!
      CALL STMM_19(ans=ans, test=test, trial=trial, c1=c1, c2=c2, &
        & term1=term1, term2=term2, opt=opt)
    !!
    CASE (del_none)
    !!
    !! scalar
    !! scalar
    !! del_none
    !! del_none
    !!
      CALL STMM_17(ans=ans, test=test, trial=trial, c1=c1, c2=c2, &
        & term1=term1, term2=term2, opt=opt)
    !!
    END SELECT
  END SELECT
  !!
END PROCEDURE mat4_STMassMatrix_5

!----------------------------------------------------------------------------
!                                                                MassMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE mat4_STMassMatrix_6
  !!
  SELECT CASE (term1)
  !!
  !!
  !!
  !!
  CASE (del_t)
    !!
    SELECT CASE (term2)
    !!
    CASE (del_t)
      !!
      !! scalar
      !! vector
      !! del_t
      !! del_t
      !!
      SELECT CASE (opt)
      CASE (1)
        CALL STMM_24a(ans=ans, test=test, trial=trial, c1=c1, c2=c2, &
          & term1=term1, term2=term2)
      CASE (2)
        CALL STMM_24b(ans=ans, test=test, trial=trial, c1=c1, c2=c2, &
          & term1=term1, term2=term2)
      CASE (3)
        CALL STMM_24c(ans=ans, test=test, trial=trial, c1=c1, c2=c2, &
          & term1=term1, term2=term2)
      CASE (4)
        CALL STMM_24d(ans=ans, test=test, trial=trial, c1=c1, c2=c2, &
          & term1=term1, term2=term2)
      END SELECT
    !!
    CASE (del_none)
      !!
      !! scalar
      !! vector
      !! del_t
      !! del_none
      !!
      SELECT CASE (opt)
      CASE (1)
        CALL STMM_22a(ans=ans, test=test, trial=trial, c1=c1, c2=c2, &
          & term1=term1, term2=term2)
      CASE (2)
        CALL STMM_22b(ans=ans, test=test, trial=trial, c1=c1, c2=c2, &
          & term1=term1, term2=term2)
      CASE (3)
        CALL STMM_22c(ans=ans, test=test, trial=trial, c1=c1, c2=c2, &
          & term1=term1, term2=term2)
      CASE (4)
        CALL STMM_22d(ans=ans, test=test, trial=trial, c1=c1, c2=c2, &
          & term1=term1, term2=term2)
      END SELECT
    !!
    END SELECT
  !!
  !!
  !!
  !!
  CASE (del_none)
    !!
    SELECT CASE (term2)
    !!
    CASE (del_t)
      !!
      !! scalar
      !! vector
      !! del_none
      !! del_t
      !!
      SELECT CASE (opt)
      CASE (1)
        CALL STMM_23a(ans=ans, test=test, trial=trial, c1=c1, c2=c2, &
          & term1=term1, term2=term2)
      CASE (2)
        CALL STMM_23b(ans=ans, test=test, trial=trial, c1=c1, c2=c2, &
          & term1=term1, term2=term2)
      CASE (3)
        CALL STMM_23c(ans=ans, test=test, trial=trial, c1=c1, c2=c2, &
          & term1=term1, term2=term2)
      CASE (4)
        CALL STMM_23d(ans=ans, test=test, trial=trial, c1=c1, c2=c2, &
          & term1=term1, term2=term2)
      END SELECT
    !!
    CASE (del_none)
      !!
      !! scalar
      !! vector
      !! del_none
      !! del_none
      !!
      SELECT CASE (opt)
      CASE (1)
        CALL STMM_21a(ans=ans, test=test, trial=trial, c1=c1, c2=c2, &
          & term1=term1, term2=term2)
      CASE (2)
        CALL STMM_21b(ans=ans, test=test, trial=trial, c1=c1, c2=c2, &
          & term1=term1, term2=term2)
      CASE (3)
        CALL STMM_21c(ans=ans, test=test, trial=trial, c1=c1, c2=c2, &
          & term1=term1, term2=term2)
      CASE (4)
        CALL STMM_21d(ans=ans, test=test, trial=trial, c1=c1, c2=c2, &
          & term1=term1, term2=term2)
      END SELECT
    !!
    END SELECT
  END SELECT
  !!
END PROCEDURE mat4_STMassMatrix_6

!----------------------------------------------------------------------------
!                                                                MassMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE mat4_STMassMatrix_7
  SELECT CASE (term1)
  !!
  !!
  !!
  !!
  CASE (del_t)
    !!
    SELECT CASE (term2)
    !!
    CASE (del_t)
      !!
      !! scalar
      !! matrix
      !! del_t
      !! del_t
      !!
      CALL STMM_28(ans=ans, test=test, trial=trial, c1=c1, c2=c2, &
        & term1=term1, term2=term2)
    !!
    CASE (del_none)
      !!
      !! scalar
      !! matrix
      !! del_t
      !! del_none
      !!
      CALL STMM_26(ans=ans, test=test, trial=trial, c1=c1, c2=c2, &
        & term1=term1, term2=term2)
    !!
    END SELECT
    !!
  CASE (del_none)
    !!
    SELECT CASE (term2)
    !!
    CASE (del_t)
      !!
      !! scalar
      !! matrix
      !! del_none
      !! del_t
      !!
      CALL STMM_27(ans=ans, test=test, trial=trial, c1=c1, c2=c2, &
        & term1=term1, term2=term2)
    !!
    CASE (del_none)
      !!
      !! scalar
      !! matrix
      !! del_none
      !! del_none
      !!
      CALL STMM_25(ans=ans, test=test, trial=trial, c1=c1, c2=c2, &
        & term1=term1, term2=term2)
    !!
    END SELECT
  END SELECT
END PROCEDURE mat4_STMassMatrix_7

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE Methods

