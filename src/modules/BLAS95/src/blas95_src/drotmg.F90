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

PURE SUBROUTINE DROTMG_F95(D1,D2,X1,Y1,PARAM)
    ! Fortran77 call:
    ! DROTMG(D1,D2,X1,Y1,PARAM)
    ! <<< !USE statements >>>
    !USE F77_BLAS, ONLY: F77_ROTMG
    ! <<< Implicit statement >>>
    !IMPLICIT NONE
    ! <<< Kind parameter >>>
    INTEGER, PARAMETER :: WP = KIND(1.0D0)
    ! <<< Scalar arguments >>>
    REAL(WP), INTENT(INOUT ) :: D1
    REAL(WP), INTENT(INOUT ) :: D2
    REAL(WP), INTENT(INOUT ) :: X1
    REAL(WP), INTENT(IN) :: Y1
    ! <<< Array arguments >>>
    REAL(WP), INTENT(OUT) :: PARAM(5)
    ! <<< Local declarations >>>
    ! <<< Parameters >>>
    CHARACTER(LEN=5), PARAMETER :: SRNAME = 'ROTMG'
    ! <<< Local scalars >>>
    ! <<< Executable statements >>>
    ! <<< Init optional and skipped scalars >>>
    ! <<< Call blas77 routine >>>
    CALL F77_ROTMG(D1,D2,X1,Y1,PARAM)
END SUBROUTINE DROTMG_F95
