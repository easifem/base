! This program is a part of EASIFEM library
! Expandable And Scalable Infrastructure for Finite Element Methods
! htttps://www.easifem.com
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

SUBMODULE(ForceVector_Method) Methods
USE ReallocateUtility, ONLY: Reallocate
USE ElemshapeData_Method, ONLY: GetInterpolation, GetInterpolation_
USE ProductUtility, ONLY: OuterProd
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                               ForceVector
!----------------------------------------------------------------------------

MODULE PROCEDURE ForceVector1
INTEGER(I4B) :: tsize
CALL Reallocate(ans, test%nns)
CALL ForceVector_(test=test, ans=ans, tsize=tsize)
END PROCEDURE ForceVector1

!----------------------------------------------------------------------------
!                                                               ForceVector_
!----------------------------------------------------------------------------

MODULE PROCEDURE ForceVector_1
! Define internal variable
REAL(DFP) :: realval
INTEGER(I4B) :: ips

! main
tsize = test%nns

DO ips = 1, test%nips
  realval = test%js(ips) * test%ws(ips) * test%thickness(ips)
  ans(1:tsize) = ans(1:tsize) + realval * test%N(1:tsize, ips)
END DO

END PROCEDURE ForceVector_1

!----------------------------------------------------------------------------
!                                                               ForceVector
!----------------------------------------------------------------------------

MODULE PROCEDURE ForceVector2
INTEGER(I4B) :: tsize
tsize = test%nns
CALL Reallocate(ans, tsize)
CALL ForceVector_(test=test, c=c, crank=crank, ans=ans, tsize=tsize)
END PROCEDURE ForceVector2

!----------------------------------------------------------------------------
!                                                               ForceVector
!----------------------------------------------------------------------------

MODULE PROCEDURE ForceVector_2
REAL(DFP), PARAMETER :: one = 1.0_DFP
LOGICAL(LGT), PARAMETER :: no = .FALSE.

REAL(DFP) :: realval
INTEGER(I4B) :: ips

tsize = test%nns
! CALL Reallocate(ans, SIZE(test%N, 1))

DO ips = 1, test%nips
  CALL GetInterpolation_(obj=test, ans=realval, val=c, scale=one, &
                         addContribution=no, timeIndx=1, spaceIndx=ips)
  realval = test%js(ips) * test%ws(ips) * test%thickness(ips) * realval
  ans(1:tsize) = ans(1:tsize) + realval * test%N(1:tsize, ips)
END DO

END PROCEDURE ForceVector_2

!----------------------------------------------------------------------------
!                                                               ForceVector
!----------------------------------------------------------------------------

MODULE PROCEDURE ForceVector3
! Define internal variable
REAL(DFP), ALLOCATABLE :: realval(:)
REAL(DFP), ALLOCATABLE :: cbar(:, :)
INTEGER(I4B) :: ips

! main
CALL GetInterpolation(obj=test, ans=cbar, val=c)
realval = test%js * test%ws * test%thickness
CALL Reallocate(ans, SIZE(cbar, 1), SIZE(test%N, 1))

DO ips = 1, SIZE(realval)
  ans = ans + realval(ips) * OuterProd(cbar(:, ips), test%N(:, ips))
END DO

DEALLOCATE (realval, cbar)
END PROCEDURE ForceVector3

!----------------------------------------------------------------------------
!                                                               ForceVector
!----------------------------------------------------------------------------

MODULE PROCEDURE ForceVector_3
! ! Define internal variable
! REAL(DFP), ALLOCATABLE :: realval(:)
! REAL(DFP), ALLOCATABLE :: cbar(:, :)
! INTEGER(I4B) :: ips
!
! ! main
! CALL GetInterpolation(obj=test, ans=cbar, val=c)
! realval = test%js * test%ws * test%thickness
! CALL Reallocate(ans, SIZE(cbar, 1), SIZE(test%N, 1))
!
! DO ips = 1, SIZE(realval)
!   ans = ans + realval(ips) * OuterProd(cbar(:, ips), test%N(:, ips))
! END DO
!
! DEALLOCATE (realval, cbar)
END PROCEDURE ForceVector_3

!----------------------------------------------------------------------------
!                                                               ForceVector
!----------------------------------------------------------------------------

MODULE PROCEDURE ForceVector4
! Define internal variable
REAL(DFP), ALLOCATABLE :: realval(:)
REAL(DFP), ALLOCATABLE :: cbar(:, :, :)
INTEGER(I4B) :: ips

! main
CALL GetInterpolation(obj=test, ans=cbar, val=c)
realval = test%js * test%ws * test%thickness
CALL Reallocate(ans, SIZE(cbar, 1), SIZE(cbar, 2), SIZE(test%N, 1))

DO ips = 1, SIZE(realval)
  ans = ans + realval(ips) * OuterProd(cbar(:, :, ips), test%N(:, ips))
END DO

DEALLOCATE (realval, cbar)
END PROCEDURE ForceVector4

!----------------------------------------------------------------------------
!                                                               ForceVector
!----------------------------------------------------------------------------

MODULE PROCEDURE ForceVector5
! Define internal variable
REAL(DFP), ALLOCATABLE :: realval(:)
REAL(DFP), ALLOCATABLE :: c1bar(:)
REAL(DFP), ALLOCATABLE :: c2bar(:)
INTEGER(I4B) :: ips

! main
CALL GetInterpolation(obj=test, ans=c1bar, val=c1)
CALL GetInterpolation(obj=test, ans=c2bar, val=c2)
realval = test%js * test%ws * test%thickness * c1bar * c2bar
CALL Reallocate(ans, SIZE(test%N, 1))

DO ips = 1, SIZE(realval)
  ans = ans + realval(ips) * test%N(:, ips)
END DO

DEALLOCATE (realval, c1bar, c2bar)
END PROCEDURE ForceVector5

!----------------------------------------------------------------------------
!                                                               ForceVector
!----------------------------------------------------------------------------

MODULE PROCEDURE ForceVector6
! Define internal variable
REAL(DFP), ALLOCATABLE :: realval(:)
REAL(DFP), ALLOCATABLE :: c1bar(:)
REAL(DFP), ALLOCATABLE :: c2bar(:, :)
INTEGER(I4B) :: ips

! main
CALL GetInterpolation(obj=test, ans=c1bar, val=c1)
CALL GetInterpolation(obj=test, ans=c2bar, val=c2)
realval = test%js * test%ws * test%thickness * c1bar
CALL Reallocate(ans, SIZE(c2bar, 1), SIZE(test%N, 1))

DO ips = 1, SIZE(realval)
  ans = ans + realval(ips) * OUTERPROD(c2bar(:, ips), test%N(:, ips))
END DO

DEALLOCATE (realval, c1bar, c2bar)
END PROCEDURE ForceVector6

!----------------------------------------------------------------------------
!                                                               ForceVector
!----------------------------------------------------------------------------

MODULE PROCEDURE ForceVector7
! Define internal variable
REAL(DFP), ALLOCATABLE :: realval(:)
REAL(DFP), ALLOCATABLE :: c1bar(:)
REAL(DFP), ALLOCATABLE :: c2bar(:, :, :)
INTEGER(I4B) :: ips

! main
CALL GetInterpolation(obj=test, ans=c1bar, val=c1)
CALL GetInterpolation(obj=test, ans=c2bar, val=c2)
realval = test%js * test%ws * test%thickness * c1bar
CALL Reallocate(ans, SIZE(c2bar, 1), SIZE(c2bar, 2), SIZE(test%N, 1))

DO ips = 1, SIZE(realval)
  ans = ans + realval(ips) * OUTERPROD(c2bar(:, :, ips), test%N(:, ips))
END DO

DEALLOCATE (realval, c1bar, c2bar)
END PROCEDURE ForceVector7

!----------------------------------------------------------------------------
!                                                               ForceVector
!----------------------------------------------------------------------------

MODULE PROCEDURE ForceVector8
! Define internal variable
REAL(DFP), ALLOCATABLE :: realval(:)
INTEGER(I4B) :: ips

realval = test%js * test%ws * test%thickness * c
CALL Reallocate(ans, SIZE(test%N, 1))

DO ips = 1, SIZE(realval)
  ans = ans + realval(ips) * test%N(:, ips)
END DO

DEALLOCATE (realval)

END PROCEDURE ForceVector8

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE Methods
