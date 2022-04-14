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

SUBMODULE(STForceVector_Method) Methods
USE BaseMethod
IMPLICIT NONE
CONTAINS

#include "./STFV_1.inc"
#include "./STFV_2.inc"
#include "./STFV_3.inc"
#include "./STFV_4.inc"
#include "./STFV_5.inc"
#include "./STFV_6.inc"
#include "./STFV_7.inc"

#include "./STFV_8.inc"
#include "./STFV_9.inc"
#include "./STFV_10.inc"
#include "./STFV_11.inc"
#include "./STFV_12.inc"
#include "./STFV_13.inc"
#include "./STFV_14.inc"

#include "./STFV_15.inc"
#include "./STFV_16.inc"
#include "./STFV_17.inc"
#include "./STFV_18.inc"
#include "./STFV_19.inc"
#include "./STFV_20.inc"
#include "./STFV_21.inc"

!----------------------------------------------------------------------------
!                                                               STForceVector
!----------------------------------------------------------------------------

MODULE PROCEDURE STForceVector_1
  !! Define internal variable
  REAL(DFP), ALLOCATABLE :: realval(:)
  INTEGER(I4B) :: ips, ipt
  !!
  !! main
  !!
  CALL reallocate( &
    & ans, &
    & SIZE(test(1)%N, 1), &
    & SIZE(test(1)%T))
  !!
  DO ipt = 1, SIZE( test )
    !!
    realval = test(ipt)%js * test(ipt)%ws * test(ipt)%thickness
    !!
    DO ips = 1, SIZE(realval)
      ans = ans + realval(ips) * OUTERPROD( &
        & a = test(ipt)%N(:, ips), &
        & b = test(ipt)%T )
    END DO
    !!
  END DO
  !!
  DEALLOCATE (realval)
  !!
END PROCEDURE STForceVector_1

!----------------------------------------------------------------------------
!                                                               STForceVector
!----------------------------------------------------------------------------

MODULE PROCEDURE STForceVector_2
  !! Define internal variable
  REAL(DFP), ALLOCATABLE :: realval(:)
  REAL(DFP), ALLOCATABLE :: cbar(:, :)
  INTEGER(I4B) :: ips, ipt
  !!
  !! main
  !!
  CALL getInterpolation(obj=test, interpol=cbar, val=c)
  !!
  CALL reallocate( &
    & ans, &
    & SIZE(test(1)%N, 1), &
    & SIZE(test(1)%T) )
  !!
  DO ipt = 1, SIZE( test )
    !!
    realval = test(ipt)%js*test(ipt)%ws*test(ipt)%thickness*cbar(:,ipt)
    !!
    DO ips = 1, SIZE(realval)
      ans = ans + realval(ips) * OUTERPROD( &
        & a = test(ipt)%N(:, ips), &
        & b = test(ipt)%T )
    END DO
    !!
  END DO
  !!
  DEALLOCATE (realval, cbar)
  !!
END PROCEDURE STForceVector_2

!----------------------------------------------------------------------------
!                                                               STForceVector
!----------------------------------------------------------------------------

MODULE PROCEDURE STForceVector_3
  !! Define internal variable
  REAL(DFP), ALLOCATABLE :: realval(:)
  REAL(DFP), ALLOCATABLE :: cbar(:,:,:)
  INTEGER(I4B) :: ips, ipt
  !!
  !! main
  !!
  CALL getInterpolation(obj=test, interpol=cbar, val=c)
  !!
  CALL reallocate( &
    & ans, &
    & SIZE(cbar, 1), &
    & SIZE(test(1)%N, 1), &
    & SIZE(test(1)%T) )
  !!
  DO ipt = 1, SIZE( test )
    !!
    realval = test(ipt)%js * test(ipt)%ws * test(ipt)%thickness
    !!
    DO ips = 1, SIZE(realval)
      ans = ans &
        & + realval(ips) &
        & * OUTERPROD( &
        & cbar(:, ips, ipt), &
        & test(ipt)%N(:, ips), &
        & test(ipt)%T )
    END DO
    !!
  END DO
  !!
  DEALLOCATE (realval, cbar)
  !!
END PROCEDURE STForceVector_3

!----------------------------------------------------------------------------
!                                                               STForceVector
!----------------------------------------------------------------------------

MODULE PROCEDURE STForceVector_4
  !! Define internal variable
  REAL(DFP), ALLOCATABLE :: realval(:)
  REAL(DFP), ALLOCATABLE :: cbar(:,:,:,:)
  INTEGER(I4B) :: ips, ipt
  !!
  !! main
  !!
  CALL getInterpolation(obj=test, interpol=cbar, val=c)
  !!
  CALL reallocate( &
    & ans, &
    & SIZE(cbar, 1), &
    & SIZE(cbar, 2), &
    & SIZE(test(1)%N, 1), &
    & SIZE(test(1)%T) )
  !!
  DO ipt = 1, SIZE( test )
    !!
    realval = test(ipt)%js * test(ipt)%ws * test(ipt)%thickness
    !!
    DO ips = 1, SIZE(realval)
      ans = ans + realval(ips) &
        & * OUTERPROD( &
        & cbar(:, :, ips, ipt), &
        & test(ipt)%N(:, ips), &
        & test(ipt)%T )
    END DO
    !!
  END DO
  !!
  DEALLOCATE (realval, cbar)
  !!
END PROCEDURE STForceVector_4

!----------------------------------------------------------------------------
!                                                               STForceVector
!----------------------------------------------------------------------------

MODULE PROCEDURE STForceVector_5
  !! Define internal variable
  REAL(DFP), ALLOCATABLE :: realval(:)
  REAL(DFP), ALLOCATABLE :: c1bar(:, :)
  REAL(DFP), ALLOCATABLE :: c2bar(:, :)
  INTEGER(I4B) :: ips, ipt
  !!
  !! main
  !!
  CALL getInterpolation(obj=test, interpol=c1bar, val=c1)
  CALL getInterpolation(obj=test, interpol=c2bar, val=c2)
  !!
  CALL reallocate( &
    & ans, &
    & SIZE(test(1)%N, 1), &
    & SIZE(test(1)%T) )
  !!
  DO ipt = 1, SIZE( test )
    !!
    realval = test(ipt)%js*test(ipt)%ws*test(ipt)%thickness &
      & * c1bar(:,ipt) * c2bar(:, ipt)
    !!
    DO ips = 1, SIZE(realval)
      ans = ans + realval(ips) &
        & * OUTERPROD( &
        & a = test(ipt)%N(:, ips), &
        & b = test(ipt)%T )
    END DO
    !!
  END DO
  !!
  DEALLOCATE (realval, c1bar, c2bar)
  !!
END PROCEDURE STForceVector_5

!----------------------------------------------------------------------------
!                                                               STForceVector
!----------------------------------------------------------------------------

MODULE PROCEDURE STForceVector_6
  !! Define internal variable
  REAL(DFP), ALLOCATABLE :: realval(:)
  REAL(DFP), ALLOCATABLE :: c1bar(:,:)
  REAL(DFP), ALLOCATABLE :: c2bar(:,:,:)
  INTEGER(I4B) :: ips, ipt
  !!
  !! main
  !!
  CALL getInterpolation(obj=test, interpol=c1bar, val=c1)
  CALL getInterpolation(obj=test, interpol=c2bar, val=c2)
  !!
  CALL reallocate( &
    & ans, &
    & SIZE(c2bar, 1), &
    & SIZE(test(1)%N, 1), &
    & SIZE(test(1)%T) )
  !!
  DO ipt = 1, SIZE( test )
    !!
    realval = test(ipt)%js * test(ipt)%ws * test(ipt)%thickness &
      & * c1bar(:, ipt)
    !!
    DO ips = 1, SIZE(realval)
      ans = ans &
        & + realval(ips) &
        & * OUTERPROD( &
        & c2bar(:, ips, ipt), &
        & test(ipt)%N(:, ips), &
        & test(ipt)%T )
    END DO
    !!
  END DO
  !!
  DEALLOCATE (realval, c1bar, c2bar)
  !!
END PROCEDURE STForceVector_6

!----------------------------------------------------------------------------
!                                                               STForceVector
!----------------------------------------------------------------------------

MODULE PROCEDURE STForceVector_7
  !! Define internal variable
  REAL(DFP), ALLOCATABLE :: realval(:)
  REAL(DFP), ALLOCATABLE :: c1bar(:,:)
  REAL(DFP), ALLOCATABLE :: c2bar(:,:,:,:)
  INTEGER(I4B) :: ips, ipt
  !!
  !! main
  !!
  CALL getInterpolation(obj=test, interpol=c1bar, val=c1)
  CALL getInterpolation(obj=test, interpol=c2bar, val=c2)
  !!
  CALL reallocate( &
    & ans, &
    & SIZE(c2bar, 1), &
    & SIZE(c2bar, 2), &
    & SIZE(test(1)%N, 1), &
    & SIZE(test(1)%T) )
  !!
  DO ipt = 1, SIZE( test )
    !!
    realval = test(ipt)%js * test(ipt)%ws * test(ipt)%thickness &
      & * c1bar( :, ipt )
    !!
    DO ips = 1, SIZE(realval)
      ans = ans + realval(ips) &
        & * OUTERPROD( &
        & c2bar(:, :, ips, ipt), &
        & test(ipt)%N(:, ips), &
        & test(ipt)%T )
    END DO
    !!
  END DO
  !!
  DEALLOCATE (realval, c1bar, c2bar)
  !!
END PROCEDURE STForceVector_7

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE STForceVector_8
  !!
  SELECT CASE( term1 )
  !!
  !!
  !!
  CASE( DEL_NONE )
    !!
    CALL STFV_1( ans=ans, test=test, term1=term1 )
  !!
  !!
  !!
  CASE( DEL_t )
    !!
    CALL STFV_8( ans=ans, test=test, term1=term1 )
  !!
  !!
  !!
  CASE( DEL_X, DEL_Y, DEL_Z )
    !!
    CALL STFV_15( ans=ans, test=test, term1=term1 )
  !!
  !!
  !!
  CASE( DEL_X_ALL )
  !!
  !! TODO
  !!
  END SELECT
  !!
END PROCEDURE STForceVector_8

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE STForceVector_9
  !!
  SELECT CASE( term1 )
  !!
  !!
  !!
  CASE( DEL_NONE )
    !!
    CALL STFV_2( ans=ans, test=test, term1=term1, c=c, crank=crank )
  !!
  !!
  !!
  CASE( DEL_t )
    !!
    CALL STFV_9( ans=ans, test=test, term1=term1, c=c, crank=crank )
  !!
  !!
  !!
  CASE( DEL_X, DEL_Y, DEL_Z )
    !!
    CALL STFV_16( ans=ans, test=test, term1=term1, c=c, crank=crank )
  !!
  !!
  !!
  CASE( DEL_X_ALL )
  !!
  !! TODO
  !!
  END SELECT
  !!
END PROCEDURE STForceVector_9

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE STForceVector_10
  !!
  SELECT CASE( term1 )
  !!
  !!
  !!
  CASE( DEL_NONE )
    !!
    CALL STFV_3( ans=ans, test=test, term1=term1, c=c, crank=crank )
  !!
  !!
  !!
  CASE( DEL_t )
    !!
    CALL STFV_10( ans=ans, test=test, term1=term1, c=c, crank=crank )
  !!
  !!
  !!
  CASE( DEL_X, DEL_Y, DEL_Z )
    !!
    CALL STFV_17( ans=ans, test=test, term1=term1, c=c, crank=crank )
  !!
  !!
  !!
  CASE( DEL_X_ALL )
  !!
  !! TODO
  !!
  END SELECT
  !!
END PROCEDURE STForceVector_10

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE STForceVector_11
  !!
  SELECT CASE( term1 )
  !!
  !!
  !!
  CASE( DEL_NONE )
    !!
    CALL STFV_4( ans=ans, test=test, term1=term1, c=c, crank=crank )
  !!
  !!
  !!
  CASE( DEL_t )
    !!
    CALL STFV_11( ans=ans, test=test, term1=term1, c=c, crank=crank )
  !!
  !!
  !!
  CASE( DEL_X, DEL_Y, DEL_Z )
    !!
    CALL STFV_18( ans=ans, test=test, term1=term1, c=c, crank=crank )
  !!
  !!
  !!
  CASE( DEL_X_ALL )
  !!
  !! TODO
  !!
  END SELECT
  !!
END PROCEDURE STForceVector_11

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE STForceVector_12
  !!
  SELECT CASE( term1 )
  !!
  !!
  !!
  CASE( DEL_NONE )
    !!
    CALL STFV_5( ans=ans, test=test, term1=term1, c1=c1, c1rank=c1rank, &
      & c2=c2, c2rank=c2rank )
  !!
  !!
  !!
  CASE( DEL_t )
    !!
    CALL STFV_12( ans=ans, test=test, term1=term1, c1=c1, c1rank=c1rank, &
      & c2=c2, c2rank=c2rank )
  !!
  !!
  !!
  CASE( DEL_X, DEL_Y, DEL_Z )
    !!
    CALL STFV_19( ans=ans, test=test, term1=term1, c1=c1, c1rank=c1rank, &
      & c2=c2, c2rank=c2rank )
  !!
  !!
  !!
  CASE( DEL_X_ALL )
  !!
  !! TODO
  !!
  END SELECT
  !!
END PROCEDURE STForceVector_12

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE STForceVector_13
  !!
  SELECT CASE( term1 )
  !!
  !!
  !!
  CASE( DEL_NONE )
    !!
    CALL STFV_6( ans=ans, test=test, term1=term1, c1=c1, c1rank=c1rank, &
      & c2=c2, c2rank=c2rank )
  !!
  !!
  !!
  CASE( DEL_t )
    !!
    CALL STFV_13( ans=ans, test=test, term1=term1, c1=c1, c1rank=c1rank, &
      & c2=c2, c2rank=c2rank )
  !!
  !!
  !!
  CASE( DEL_X, DEL_Y, DEL_Z )
    !!
    CALL STFV_20( ans=ans, test=test, term1=term1, c1=c1, c1rank=c1rank, &
      & c2=c2, c2rank=c2rank )
  !!
  !!
  !!
  CASE( DEL_X_ALL )
  !!
  !! TODO
  !!
  END SELECT
  !!
END PROCEDURE STForceVector_13

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE STForceVector_14
  !!
  SELECT CASE( term1 )
  !!
  !!
  !!
  CASE( DEL_NONE )
    !!
    CALL STFV_7( ans=ans, test=test, term1=term1, c1=c1, c1rank=c1rank, &
      & c2=c2, c2rank=c2rank )
  !!
  !!
  !!
  CASE( DEL_t )
    !!
    CALL STFV_14( ans=ans, test=test, term1=term1, c1=c1, c1rank=c1rank, &
      & c2=c2, c2rank=c2rank )
  !!
  !!
  !!
  CASE( DEL_X, DEL_Y, DEL_Z )
    !!
    CALL STFV_21( ans=ans, test=test, term1=term1, c1=c1, c1rank=c1rank, &
      & c2=c2, c2rank=c2rank )
  !!
  !!
  !!
  CASE( DEL_X_ALL )
  !!
  !! TODO
  !!
  END SELECT
  !!
END PROCEDURE STForceVector_14

!----------------------------------------------------------------------------
!                                                            STForceVector
!----------------------------------------------------------------------------

MODULE PROCEDURE STForceVector_15
  !!
  !! Define internal variable
  !!
  REAL(DFP), ALLOCATABLE :: realval(:)
  REAL(DFP), ALLOCATABLE :: p1(:,:,:,:)
  INTEGER(I4B) :: ips, ipt
  !!
  !! main
  !!
  CALL GetProjectionOfdNTdXt(obj=test, cdNTdXt=p1, val=c)
  !!
  CALL reallocate( &
    & ans, &
    & SIZE(test(1)%N, 1), &
    & SIZE(test(1)%T))
  !!
  DO ipt = 1, SIZE( test )
    !!
    realval = test(ipt)%js * test(ipt)%ws * test(ipt)%thickness
    !!
    DO ips = 1, SIZE(realval)
      ans = ans + realval(ips) * p1(:, :, ips, ipt )
    END DO
    !!
  END DO
  !!
  DEALLOCATE (realval, p1)
  !!
END PROCEDURE STForceVector_15

!----------------------------------------------------------------------------
!                                                            STForceVector
!----------------------------------------------------------------------------

MODULE PROCEDURE STForceVector_16
  !!
  !! Define internal variable
  !!
  REAL(DFP), ALLOCATABLE :: realval(:)
  REAL(DFP), ALLOCATABLE :: c2bar(:, :)
  REAL(DFP), ALLOCATABLE :: p1(:, :, :, :)
  INTEGER(I4B) :: ips, ipt
  !!
  !! main
  !!
  CALL GetProjectionOfdNTdXt(obj=test, cdNTdXt=p1, val=c1)
  CALL getInterpolation(obj=test, interpol=c2bar, val=c2)
  !!
  CALL reallocate( &
    & ans, &
    & SIZE(test(1)%N, 1), &
    & SIZE(test(1)%T) )
  !!
  DO ipt = 1, SIZE( test )
    !!
    realval = test(ipt)%js*test(ipt)%ws*test(ipt)%thickness*c2bar(:,ipt)
    !!
    DO ips = 1, SIZE(realval)
      ans = ans + realval(ips) * p1(:, :, ips, ipt)
    END DO
    !!
  END DO
  !!
  DEALLOCATE (realval, p1, c2bar)
  !!

END PROCEDURE STForceVector_16

!----------------------------------------------------------------------------
!                                                            STForceVector
!----------------------------------------------------------------------------

MODULE PROCEDURE STForceVector_17
  !!
  REAL(DFP), ALLOCATABLE :: realval(:)
  REAL(DFP), ALLOCATABLE :: c2bar(:,:,:)
  REAL(DFP), ALLOCATABLE :: p1(:,:,:,:)
  INTEGER(I4B) :: ips, ipt
  !!
  !! main
  !!
  CALL GetProjectionOfdNTdXt(obj=test, cdNTdXt=p1, val=c1)
  CALL getInterpolation(obj=test, interpol=c2bar, val=c2)
  !!
  CALL reallocate( &
    & ans, &
    & SIZE(c2bar, 1), &
    & SIZE(test(1)%N, 1), &
    & SIZE(test(1)%T) )
  !!
  DO ipt = 1, SIZE( test )
    !!
    realval = test(ipt)%js * test(ipt)%ws * test(ipt)%thickness
    !!
    DO ips = 1, SIZE(realval)
      ans = ans &
        & + realval(ips) &
        & * OUTERPROD( &
        & c2bar(:, ips, ipt), &
        & p1(:, :, ips, ipt) )
    END DO
    !!
  END DO
  !!
  DEALLOCATE (realval, p1, c2bar)
  !!
END PROCEDURE STForceVector_17

!----------------------------------------------------------------------------
!                                                            STForceVector
!----------------------------------------------------------------------------

MODULE PROCEDURE STForceVector_18
  !!
  !! Define internal variable
  !!
  REAL(DFP), ALLOCATABLE :: realval(:)
  REAL(DFP), ALLOCATABLE :: c2bar(:,:,:,:)
  REAL(DFP), ALLOCATABLE :: p1(:,:,:,:)
  INTEGER(I4B) :: ips, ipt
  !!
  !! main
  !!
  CALL GetProjectionOfdNTdXt(obj=test, cdNTdXt=p1, val=c1)
  CALL getInterpolation(obj=test, interpol=c2bar, val=c2)
  !!
  CALL reallocate( &
    & ans, &
    & SIZE(c2bar, 1), &
    & SIZE(c2bar, 2), &
    & SIZE(test(1)%N, 1), &
    & SIZE(test(1)%T) )
  !!
  DO ipt = 1, SIZE( test )
    !!
    realval = test(ipt)%js * test(ipt)%ws * test(ipt)%thickness
    !!
    DO ips = 1, SIZE(realval)
      ans = ans + realval(ips) &
        & * OUTERPROD( c2bar(:, :, ips, ipt), p1(:,:,ips, ipt) )
    END DO
    !!
  END DO
  !!
  DEALLOCATE (realval, p1, c2bar)
  !!
END PROCEDURE STForceVector_18

!----------------------------------------------------------------------------
!                                                            STForceVector
!----------------------------------------------------------------------------

MODULE PROCEDURE STForceVector_19
  !!
  !! Define internal variable
  !!
  REAL(DFP), ALLOCATABLE :: realval(:)
  REAL(DFP), ALLOCATABLE :: c2bar(:, :)
  REAL(DFP), ALLOCATABLE :: c3bar(:, :)
  REAL(DFP), ALLOCATABLE :: p1(:, :, :, :)
  INTEGER(I4B) :: ips, ipt
  !!
  !! main
  !!
  CALL GetProjectionOfdNTdXt(obj=test, cdNTdXt=p1, val=c1)
  CALL getInterpolation(obj=test, interpol=c2bar, val=c2)
  CALL getInterpolation(obj=test, interpol=c3bar, val=c3)
  !!
  CALL reallocate( &
    & ans, &
    & SIZE(test(1)%N, 1), &
    & SIZE(test(1)%T) )
  !!
  DO ipt = 1, SIZE( test )
    !!
    realval = test(ipt)%js &
      & * test(ipt)%ws &
      & * test(ipt)%thickness &
      & * c2bar(:,ipt) &
      & * c3bar(:,ipt)
    !!
    !!
    DO ips = 1, SIZE(realval)
      ans = ans + realval(ips) * p1(:, :, ips, ipt)
    END DO
    !!
  END DO
  !!
  DEALLOCATE (realval, p1, c2bar, c3bar)
  !!
END PROCEDURE STForceVector_19

!----------------------------------------------------------------------------
!                                                            STForceVector
!----------------------------------------------------------------------------

MODULE PROCEDURE STForceVector_20
  !!
  REAL(DFP), ALLOCATABLE :: realval(:)
  REAL(DFP), ALLOCATABLE :: c2bar(:,:)
  REAL(DFP), ALLOCATABLE :: c3bar(:,:,:)
  REAL(DFP), ALLOCATABLE :: p1(:,:,:,:)
  INTEGER(I4B) :: ips, ipt
  !!
  !! main
  !!
  CALL GetProjectionOfdNTdXt(obj=test, cdNTdXt=p1, val=c1)
  CALL getInterpolation(obj=test, interpol=c2bar, val=c2)
  CALL getInterpolation(obj=test, interpol=c3bar, val=c3)
  !!
  CALL reallocate( &
    & ans, &
    & SIZE(c3bar, 1), &
    & SIZE(test(1)%N, 1), &
    & SIZE(test(1)%T) )
  !!
  DO ipt = 1, SIZE( test )
    !!
    realval = test(ipt)%js * test(ipt)%ws * test(ipt)%thickness &
      & * c2bar(:,ipt)
    !!
    DO ips = 1, SIZE(realval)
      ans = ans &
        & + realval(ips) &
        & * OUTERPROD( c3bar(:, ips, ipt), p1(:, :, ips, ipt) )
    END DO
    !!
  END DO
  !!
  DEALLOCATE (realval, p1, c2bar, c3bar)
  !!
END PROCEDURE STForceVector_20

!----------------------------------------------------------------------------
!                                                            STForceVector
!----------------------------------------------------------------------------

MODULE PROCEDURE STForceVector_21
  !!
  !! Define internal variable
  !!
  REAL(DFP), ALLOCATABLE :: realval(:)
  REAL(DFP), ALLOCATABLE :: c2bar(:,:)
  REAL(DFP), ALLOCATABLE :: c3bar(:,:,:,:)
  REAL(DFP), ALLOCATABLE :: p1(:,:,:,:)
  INTEGER(I4B) :: ips, ipt
  !!
  !! main
  !!
  CALL GetProjectionOfdNTdXt(obj=test, cdNTdXt=p1, val=c1)
  CALL getInterpolation(obj=test, interpol=c2bar, val=c2)
  CALL getInterpolation(obj=test, interpol=c3bar, val=c3)
  !!
  CALL reallocate( &
    & ans, &
    & SIZE(c3bar, 1), &
    & SIZE(c3bar, 2), &
    & SIZE(test(1)%N, 1), &
    & SIZE(test(1)%T) )
  !!
  DO ipt = 1, SIZE( test )
    !!
    realval = test(ipt)%js * test(ipt)%ws * test(ipt)%thickness * &
      & c2bar(:, ipt)
    !!
    DO ips = 1, SIZE(realval)
      !!
      ans = ans + realval(ips) * OUTERPROD( &
        & c3bar(:, :, ips, ipt), &
        & p1(:,:,ips,ipt) )
      !!
    END DO
  END DO
  !!
  DEALLOCATE (realval, p1, c2bar, c3bar)
  !!
END PROCEDURE STForceVector_21

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE Methods