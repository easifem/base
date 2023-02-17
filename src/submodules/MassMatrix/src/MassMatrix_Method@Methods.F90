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

SUBMODULE(MassMatrix_Method) Methods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                MassMatrix
!----------------------------------------------------------------------------

PURE SUBROUTINE MM_2a(ans, test, trial, rho)
  REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: ans(:, :)
  CLASS(ElemshapeData_), INTENT(IN) :: test
  ! Shapedata for test function
  CLASS(ElemshapeData_), INTENT(IN) :: trial
  ! Shapedata for trial function
  CLASS(FEVariable_), INTENT(IN) :: rho
  ! vector variable
  ! Define internal variable
  REAL(DFP), ALLOCATABLE :: realval(:)
  REAL(DFP), ALLOCATABLE :: m2(:, :)
  REAL(DFP), ALLOCATABLE :: vbar(:, :)
  REAL(DFP), ALLOCATABLE :: m4(:, :, :, :)
  INTEGER(I4B) :: ii, ips
  !
  ! main
  !
  CALL getInterpolation(obj=trial, interpol=vbar, val=rho)
  !
  CALL reallocate(m4, SIZE(test%N, 1), SIZE(trial%N, 1), SIZE(vbar, 1), 1)
  !
  realval = trial%js * trial%ws * trial%thickness
  !
  DO ips = 1, SIZE(realval)
    m2 = OUTERPROD(a=test%N(:, ips), b=trial%N(:, ips))
    DO ii = 1, SIZE(vbar, 1)
      m4(:, :, ii, 1) = m4(:, :, ii, 1) &
        & + realval(ips) * vbar(ii, ips) * m2
    END DO
  END DO
  !
  CALL Convert(From=m4, To=ans)
  !
  DEALLOCATE (realval, m2, vbar, m4)
  !
END SUBROUTINE MM_2a

!----------------------------------------------------------------------------
!                                                                MassMatrix
!----------------------------------------------------------------------------

PURE SUBROUTINE MM_2b(ans, test, trial, rho)
  REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: ans(:, :)
  CLASS(ElemshapeData_), INTENT(IN) :: test
  ! Shapedata for test function
  CLASS(ElemshapeData_), INTENT(IN) :: trial
  ! Shapedata for trial function
  CLASS(FEVariable_), INTENT(IN) :: rho
  ! vector variable
  !
  ! Define internal variable
  !
  REAL(DFP), ALLOCATABLE :: realval(:)
  REAL(DFP), ALLOCATABLE :: m2(:, :)
  REAL(DFP), ALLOCATABLE :: vbar(:, :)
  REAL(DFP), ALLOCATABLE :: m4(:, :, :, :)
  INTEGER(I4B) :: ii, ips
  !
  ! main
  !
  CALL getInterpolation(obj=trial, interpol=vbar, val=rho)
  !
  CALL reallocate(m4, SIZE(test%N, 1), SIZE(trial%N, 1), 1, SIZE(vbar, 1))
  !
  realval = trial%js * trial%ws * trial%thickness
  !
  DO ips = 1, SIZE(realval)
    m2 = OUTERPROD(a=test%N(:, ips), b=trial%N(:, ips))
    DO ii = 1, SIZE(vbar, 1)
      m4(:, :, 1, ii) = m4(:, :, 1, ii) &
        & + realval(ips) * vbar(ii, ips) * m2
    END DO
  END DO
  !
  CALL Convert(From=m4, To=ans)
  !
  DEALLOCATE (realval, m2, vbar, m4)
  !
END SUBROUTINE MM_2b

!----------------------------------------------------------------------------
!                                                                MassMatrix
!----------------------------------------------------------------------------

PURE SUBROUTINE MM_2c(ans, test, trial, rho)
  REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: ans(:, :)
  CLASS(ElemshapeData_), INTENT(IN) :: test
  ! Shapedata for test function
  CLASS(ElemshapeData_), INTENT(IN) :: trial
  ! Shapedata for trial function
  CLASS(FEVariable_), INTENT(IN) :: rho
  ! vector variable
  ! Define internal variable
  REAL(DFP), ALLOCATABLE :: realval(:)
  REAL(DFP), ALLOCATABLE :: m2(:, :)
  REAL(DFP), ALLOCATABLE :: vbar(:, :)
  REAL(DFP), ALLOCATABLE :: m4(:, :, :, :)
  INTEGER(I4B) :: ips, ii
  !
  ! main
  !
  CALL getInterpolation(obj=trial, interpol=vbar, val=rho)
  !
  CALL reallocate(m4, SIZE(test%N, 1), SIZE(trial%N, 1), &
    & SIZE(vbar, 1), SIZE(vbar, 1))
  !
  realval = trial%js * trial%ws * trial%thickness
  !
  DO ips = 1, SIZE(vbar, 2)
    m2 = OUTERPROD(a=test%N(:, ips), b=trial%N(:, ips))
    DO ii = 1, SIZE(vbar, 1)
      m4(:, :, ii, ii) = m4(:, :, ii, ii) &
        & + realval(ips) * vbar(ii, ips) * m2
    END DO
  END DO
  !
  CALL Convert(from=m4, to=ans)
  !
  DEALLOCATE (realval, m2, vbar, m4)
  !
END SUBROUTINE MM_2c

!----------------------------------------------------------------------------
!                                                                MassMatrix
!----------------------------------------------------------------------------

PURE SUBROUTINE MM_2d(ans, test, trial, rho)
  REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: ans(:, :)
  CLASS(ElemshapeData_), INTENT(IN) :: test
  ! Shapedata for test function
  CLASS(ElemshapeData_), INTENT(IN) :: trial
  ! Shapedata for trial function
  CLASS(FEVariable_), INTENT(IN) :: rho
  ! vector variable
  ! Define internal variable
  REAL(DFP), ALLOCATABLE :: realval(:)
  REAL(DFP), ALLOCATABLE :: m2(:, :)
  REAL(DFP), ALLOCATABLE :: vbar(:, :)
  REAL(DFP), ALLOCATABLE :: m4(:, :, :, :)
  INTEGER(I4B) :: ips, ii, jj
  !
  ! main
  !
  CALL getInterpolation(obj=trial, interpol=vbar, val=rho)
  !
  CALL reallocate(m4, SIZE(test%N, 1), SIZE(trial%N, 1), &
    & SIZE(vbar, 1), SIZE(vbar, 1))
  !
  realval = trial%js * trial%ws * trial%thickness
  !
  DO ips = 1, SIZE(vbar, 2)
    m2 = OUTERPROD(a=test%N(:, ips), b=trial%N(:, ips))
    DO jj = 1, SIZE(vbar, 1)
      DO ii = 1, SIZE(vbar, 1)
        m4(:, :, ii, jj) = m4(:, :, ii, jj) &
          & + realval(ips) * vbar(ii, ips) &
          & * vbar(jj, ips) * m2
      END DO
    END DO
  END DO
  !
  CALL Convert(from=m4, to=ans)
  !
  DEALLOCATE (realval, m2, vbar, m4)
  !
END SUBROUTINE MM_2d

!----------------------------------------------------------------------------
!                                                                MassMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE MassMatrix_1
! Define internal variable
REAL(DFP), ALLOCATABLE :: realval(:)
INTEGER(I4B) :: ips
!
! main
!
CALL reallocate(ans, SIZE(test%N, 1), SIZE(trial%N, 1))
realval = trial%js * trial%ws * trial%thickness
!
DO ips = 1, SIZE(trial%N, 2)
  ans = ans + realval(ips) * &
    & OUTERPROD(a=test%N(:, ips), b=trial%N(:, ips))
END DO
!
IF (PRESENT(opt)) CALL MakeDiagonalCopies(ans, opt)
!
DEALLOCATE (realval)
END PROCEDURE MassMatrix_1

!----------------------------------------------------------------------------
!                                                                MassMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE MassMatrix_2
! CALL MM_1(ans=ans, test=test, trial=trial, rho=rho, opt=opt)
! Internal variable
!
REAL(DFP), ALLOCATABLE :: realval(:)
INTEGER(I4B) :: ips
!
! main
!
CALL reallocate(ans, SIZE(test%N, 1), SIZE(trial%N, 1))
CALL getInterpolation(obj=trial, interpol=realval, val=rho)
realval = trial%js * trial%ws * trial%thickness * realval
!
DO ips = 1, SIZE(realval)
  ans = ans + realval(ips) * &
    & OUTERPROD(a=test%N(:, ips), b=trial%N(:, ips))
END DO
!
IF (PRESENT(opt)) CALL MakeDiagonalCopies(ans, opt)
!
DEALLOCATE (realval)
END PROCEDURE MassMatrix_2

!----------------------------------------------------------------------------
!                                                                MassMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE MassMatrix_3
SELECT CASE (opt)
CASE (1)
  CALL MM_2a(ans=ans, test=test, trial=trial, rho=rho)
CASE (2)
  CALL MM_2b(ans=ans, test=test, trial=trial, rho=rho)
CASE (3)
  CALL MM_2c(ans=ans, test=test, trial=trial, rho=rho)
CASE (4)
  CALL MM_2d(ans=ans, test=test, trial=trial, rho=rho)
END SELECT
END PROCEDURE MassMatrix_3

!----------------------------------------------------------------------------
!                                                                MassMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE MassMatrix_4
! CALL MM_3(ans=ans, test=test, trial=trial, rho=rho, opt=opt)
! Define internal variable
!
REAL(DFP), ALLOCATABLE :: realval(:)
REAL(DFP), ALLOCATABLE :: m2(:, :)
REAL(DFP), ALLOCATABLE :: kbar(:, :, :)
REAL(DFP), ALLOCATABLE :: m4(:, :, :, :)
INTEGER(I4B) :: ii, jj, ips
!
! main
!
CALL getInterpolation(obj=trial, interpol=kbar, val=rho)
CALL reallocate(m4, SIZE(test%N, 1), SIZE(trial%N, 1), &
  & SIZE(kbar, 1), SIZE(kbar, 2))
!
realval = trial%js * trial%ws * trial%thickness
!
DO ips = 1, SIZE(realval)
  m2 = OUTERPROD(a=test%N(:, ips), b=trial%N(:, ips))
  DO jj = 1, SIZE(kbar, 2)
    DO ii = 1, SIZE(kbar, 1)
      m4(:, :, ii, jj) = m4(:, :, ii, jj) &
        & + realval(ips) * kbar(ii, jj, ips) * m2
    END DO
  END DO
END DO
!
CALL Convert(From=m4, To=ans)
!
DEALLOCATE (realval, m2, kbar, m4)
END PROCEDURE MassMatrix_4

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE Methods
