!===============================================================================
! Copyright 2010-2020 Intel Corporation.
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

PURE SUBROUTINE CAXPBY_F95(X,Y,ALPHA,BETA)
    ! Fortran77 call:
    ! CAXPBY(N,ALPHA,X,INCX,BETA,Y,INCY)
    ! Default ALPHA=1
    ! Default BETA=1
    ! <<< !USE statements >>>
    !USE F77_BLAS, ONLY: F77_AXPBY
    ! <<< Implicit statement >>>
    !IMPLICIT NONE
    ! <<< Kind parameter >>>
    INTEGER, PARAMETER :: WP = KIND(1.0E0)
    ! <<< Scalar arguments >>>
    COMPLEX(WP), INTENT(IN), OPTIONAL :: ALPHA
    COMPLEX(WP), INTENT(IN), OPTIONAL :: BETA
    ! <<< Array arguments >>>
    COMPLEX(WP), INTENT(IN) :: X(:)
    COMPLEX(WP), INTENT(INOUT) :: Y(:)
    ! <<< Local declarations >>>
    ! <<< Parameters >>>
    CHARACTER(LEN=5), PARAMETER :: SRNAME = 'AXPBY'
    ! <<< Local scalars >>>
    COMPLEX(WP) :: O_ALPHA
    COMPLEX(WP) :: O_BETA
    INTEGER :: INCX
    INTEGER :: INCY
    INTEGER :: N
    ! <<< Intrinsic functions >>>
    INTRINSIC PRESENT, SIZE
    ! <<< Executable statements >>>
    ! <<< Init optional and skipped scalars >>>
    IF(PRESENT(ALPHA)) THEN
        O_ALPHA = ALPHA
    ELSE
        O_ALPHA = 1
    ENDIF
    IF(PRESENT(BETA)) THEN
        O_BETA = BETA
    ELSE
        O_BETA = 1
    ENDIF
    INCX = 1
    INCY = 1
    N = SIZE(X)
    ! <<< Call blas77 routine >>>
    CALL F77_AXPBY(N,O_ALPHA,X,INCX,O_BETA,Y,INCY)
END SUBROUTINE CAXPBY_F95
