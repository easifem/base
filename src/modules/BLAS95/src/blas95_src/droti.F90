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

PURE SUBROUTINE DROTI_F95(X,INDX,Y,C,S)
    ! Fortran77 call:
    ! DROTI(NZ,X,INDX,Y,C,S)
    ! Default C=1
    ! Default S=1
    ! <<< !USE statements >>>
    !USE F77_BLAS, ONLY: F77_ROTI
    ! <<< Implicit statement >>>
    !IMPLICIT NONE
    ! <<< Kind parameter >>>
    INTEGER, PARAMETER :: WP = KIND(1.0D0)
    ! <<< Scalar arguments >>>
    REAL(WP), INTENT(IN) :: C
    REAL(WP), INTENT(IN) :: S
    ! <<< Array arguments >>>
    REAL(WP), INTENT(INOUT ) :: X(:)
    INTEGER, INTENT(IN) :: INDX(:)
    REAL(WP), INTENT(IN) :: Y(:)
    ! <<< Local declarations >>>
    ! <<< Parameters >>>
    CHARACTER(LEN=4), PARAMETER :: SRNAME = 'ROTI'
    ! <<< Local scalars >>>
    INTEGER :: NZ
    ! <<< Intrinsic functions >>>
    INTRINSIC SIZE
    ! <<< Executable statements >>>
    ! <<< Init optional and skipped scalars >>>
    NZ = SIZE(X)
    ! <<< Call blas77 routine >>>
    CALL F77_ROTI(NZ,X,INDX,Y,C,S)
END SUBROUTINE DROTI_F95
