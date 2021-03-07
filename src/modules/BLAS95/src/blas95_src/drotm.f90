!===============================================================================
! Copyright 2005-2020 Intel Corporation.
!
! This software and the related documents are Intel copyrighted  materials,  and
! your !USE of  them is  governed by the  express license  under which  they were
! provided to you (License).  Unless the License provides otherwise, you may not
! !USE, modify, copy, publish, distribute,  disclose or transmit this software or
! the related documents without Intel's prior written permission.
!
! This software and the related documents  are provided as  is,  with no express
! or implied  warranties,  other  than those  that are  expressly stated  in the
! License.
!===============================================================================

!  Content:
!      F95 interface for BLAS routines
!*******************************************************************************
! This file was generated automatically!
!*******************************************************************************

PURE SUBROUTINE DROTM_F95(X,Y,PARAM)
    ! Fortran77 call:
    ! DROTM(N,X,INCX,Y,INCY,PARAM)
    ! <<< !USE statements >>>
    !USE F77_BLAS, ONLY: F77_ROTM
    ! <<< Implicit statement >>>
    !IMPLICIT NONE
    ! <<< Kind parameter >>>
    INTEGER, PARAMETER :: WP = KIND(1.0D0)
    ! <<< Array arguments >>>
    REAL(WP), INTENT(INOUT) :: X(:)
    REAL(WP), INTENT(INOUT) :: Y(:)
    REAL(WP), INTENT(IN) :: PARAM(5)
    ! <<< Local declarations >>>
    ! <<< Parameters >>>
    CHARACTER(LEN=4), PARAMETER :: SRNAME = 'ROTM'
    ! <<< Local scalars >>>
    INTEGER :: INCX
    INTEGER :: INCY
    INTEGER :: N
    ! <<< Intrinsic functions >>>
    INTRINSIC SIZE
    ! <<< Executable statements >>>
    ! <<< Init optional and skipped scalars >>>
    INCX = 1
    INCY = 1
    N = SIZE(X)
    ! <<< Call blas77 routine >>>
    CALL F77_ROTM(N,X,INCX,Y,INCY,PARAM)
END SUBROUTINE DROTM_F95
