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

PURE SUBROUTINE ZGTHR_F95(X,INDX,Y)
    ! Fortran77 call:
    ! ZGTHR(NZ,Y,X,INDX)
    ! <<< !USE statements >>>
    !USE F77_BLAS, ONLY: F77_GTHR
    ! <<< Implicit statement >>>
    !IMPLICIT NONE
    ! <<< Kind parameter >>>
    INTEGER, PARAMETER :: WP = KIND(1.0D0)
    ! <<< Array arguments >>>
    COMPLEX(WP), INTENT(OUT) :: X(:)
    INTEGER, INTENT(IN) :: INDX(:)
    COMPLEX(WP), INTENT(IN) :: Y(:)
    ! <<< Local declarations >>>
    ! <<< Parameters >>>
    CHARACTER(LEN=4), PARAMETER :: SRNAME = 'GTHR'
    ! <<< Local scalars >>>
    INTEGER :: NZ
    ! <<< Intrinsic functions >>>
    INTRINSIC SIZE
    ! <<< Executable statements >>>
    ! <<< Init optional and skipped scalars >>>
    NZ = SIZE(X)
    ! <<< Call blas77 routine >>>
    CALL F77_GTHR(NZ,Y,X,INDX)
END SUBROUTINE ZGTHR_F95
