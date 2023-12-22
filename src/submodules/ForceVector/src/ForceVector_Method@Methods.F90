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

SUBMODULE(ForceVector_Method) Methods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                               ForceVector
!----------------------------------------------------------------------------

MODULE PROCEDURE ForceVector_1
! Define internal variable
REAL(DFP), ALLOCATABLE :: realval(:)
INTEGER(I4B) :: ips

! main
realval = test%js * test%ws * test%thickness
CALL Reallocate(ans, SIZE(test%N, 1))

DO ips = 1, SIZE(realval)
  ans = ans + realval(ips) * test%N(:, ips)
END DO

DEALLOCATE (realval)
END PROCEDURE ForceVector_1

!----------------------------------------------------------------------------
!                                                               ForceVector
!----------------------------------------------------------------------------

MODULE PROCEDURE ForceVector_2
! Define internal variable
REAL(DFP), ALLOCATABLE :: realval(:)
INTEGER(I4B) :: ips

! main
CALL GetInterpolation(obj=test, interpol=realval, val=c)
realval = test%js * test%ws * test%thickness * realval
CALL Reallocate(ans, SIZE(test%N, 1))

DO ips = 1, SIZE(realval)
  ans = ans + realval(ips) * test%N(:, ips)
END DO
DEALLOCATE (realval)
END PROCEDURE ForceVector_2

!----------------------------------------------------------------------------
!                                                               ForceVector
!----------------------------------------------------------------------------

MODULE PROCEDURE ForceVector_2b
! Define internal variable
REAL(DFP), ALLOCATABLE :: realval(:)
INTEGER(I4B) :: ips

realval = test%js * test%ws * test%thickness * c
CALL Reallocate(ans, SIZE(test%N, 1))

DO ips = 1, SIZE(realval)
  ans = ans + realval(ips) * test%N(:, ips)
END DO

DEALLOCATE (realval)

END PROCEDURE ForceVector_2b

!----------------------------------------------------------------------------
!                                                               ForceVector
!----------------------------------------------------------------------------

MODULE PROCEDURE ForceVector_3
! Define internal variable
REAL(DFP), ALLOCATABLE :: realval(:)
REAL(DFP), ALLOCATABLE :: cbar(:, :)
INTEGER(I4B) :: ips

! main
CALL GetInterpolation(obj=test, interpol=cbar, val=c)
realval = test%js * test%ws * test%thickness
CALL Reallocate(ans, SIZE(cbar, 1), SIZE(test%N, 1))

DO ips = 1, SIZE(realval)
  ans = ans + realval(ips) * OUTERPROD(cbar(:, ips), test%N(:, ips))
END DO

DEALLOCATE (realval, cbar)
END PROCEDURE ForceVector_3

!----------------------------------------------------------------------------
!                                                               ForceVector
!----------------------------------------------------------------------------

MODULE PROCEDURE ForceVector_4
! Define internal variable
REAL(DFP), ALLOCATABLE :: realval(:)
REAL(DFP), ALLOCATABLE :: cbar(:, :, :)
INTEGER(I4B) :: ips

! main
CALL GetInterpolation(obj=test, interpol=cbar, val=c)
realval = test%js * test%ws * test%thickness
CALL Reallocate(ans, SIZE(cbar, 1), SIZE(cbar, 2), SIZE(test%N, 1))

DO ips = 1, SIZE(realval)
  ans = ans + realval(ips) * OUTERPROD(cbar(:, :, ips), test%N(:, ips))
END DO

DEALLOCATE (realval, cbar)
END PROCEDURE ForceVector_4

!----------------------------------------------------------------------------
!                                                               ForceVector
!----------------------------------------------------------------------------

MODULE PROCEDURE ForceVector_5
! Define internal variable
REAL(DFP), ALLOCATABLE :: realval(:)
REAL(DFP), ALLOCATABLE :: c1bar(:)
REAL(DFP), ALLOCATABLE :: c2bar(:)
INTEGER(I4B) :: ips

! main
CALL GetInterpolation(obj=test, interpol=c1bar, val=c1)
CALL GetInterpolation(obj=test, interpol=c2bar, val=c2)
realval = test%js * test%ws * test%thickness * c1bar * c2bar
CALL Reallocate(ans, SIZE(test%N, 1))

DO ips = 1, SIZE(realval)
  ans = ans + realval(ips) * test%N(:, ips)
END DO

DEALLOCATE (realval, c1bar, c2bar)
END PROCEDURE ForceVector_5

!----------------------------------------------------------------------------
!                                                               ForceVector
!----------------------------------------------------------------------------

MODULE PROCEDURE ForceVector_6
! Define internal variable
REAL(DFP), ALLOCATABLE :: realval(:)
REAL(DFP), ALLOCATABLE :: c1bar(:)
REAL(DFP), ALLOCATABLE :: c2bar(:, :)
INTEGER(I4B) :: ips

! main
CALL GetInterpolation(obj=test, interpol=c1bar, val=c1)
CALL GetInterpolation(obj=test, interpol=c2bar, val=c2)
realval = test%js * test%ws * test%thickness * c1bar
CALL Reallocate(ans, SIZE(c2bar, 1), SIZE(test%N, 1))

DO ips = 1, SIZE(realval)
  ans = ans + realval(ips) * OUTERPROD(c2bar(:, ips), test%N(:, ips))
END DO

DEALLOCATE (realval, c1bar, c2bar)
END PROCEDURE ForceVector_6

!----------------------------------------------------------------------------
!                                                               ForceVector
!----------------------------------------------------------------------------

MODULE PROCEDURE ForceVector_7
! Define internal variable
REAL(DFP), ALLOCATABLE :: realval(:)
REAL(DFP), ALLOCATABLE :: c1bar(:)
REAL(DFP), ALLOCATABLE :: c2bar(:, :, :)
INTEGER(I4B) :: ips

! main
CALL GetInterpolation(obj=test, interpol=c1bar, val=c1)
CALL GetInterpolation(obj=test, interpol=c2bar, val=c2)
realval = test%js * test%ws * test%thickness * c1bar
CALL Reallocate(ans, SIZE(c2bar, 1), SIZE(c2bar, 2), SIZE(test%N, 1))

DO ips = 1, SIZE(realval)
  ans = ans + realval(ips) * OUTERPROD(c2bar(:, :, ips), test%N(:, ips))
END DO

DEALLOCATE (realval, c1bar, c2bar)
END PROCEDURE ForceVector_7

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE Methods
