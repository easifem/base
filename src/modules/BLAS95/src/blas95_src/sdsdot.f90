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

PURE FUNCTION SDSDOT_F95(SX,SY,SB)
    ! Fortran77 call:
    ! SDSDOT(N,SB,SX,INCX,SY,INCY)
    ! <<< !USE statements >>>
    !USE F77_BLAS, ONLY: F77_SDOT
    ! <<< Implicit statement >>>
    !IMPLICIT NONE
    ! <<< Kind parameter >>>
    INTEGER, PARAMETER :: WP = KIND(1.0E0)
    REAL(WP) :: SDSDOT_F95
    ! <<< Scalar arguments >>>
    REAL(WP), INTENT(IN) :: SB
    ! <<< Array arguments >>>
    REAL(WP), INTENT(IN) :: SX(:)
    REAL(WP), INTENT(IN) :: SY(:)
    ! <<< Local declarations >>>
    ! <<< Parameters >>>
    CHARACTER(LEN=4), PARAMETER :: SRNAME = 'SDOT'
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
    N = SIZE(SX)
    ! <<< Call blas77 routine >>>
    SDSDOT_F95 = F77_SDOT(N,SB,SX,INCX,SY,INCY)
END FUNCTION SDSDOT_F95
