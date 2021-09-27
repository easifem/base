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

PURE SUBROUTINE CHPR2_F95(AP,X,Y,UPLO,ALPHA)
    ! Fortran77 call:
    ! CHPR2(UPLO,N,ALPHA,X,INCX,Y,INCY,AP)
    ! UPLO='U','L'; default: 'U'
    ! Default ALPHA=1
    ! <<< !USE statements >>>
    !USE F77_BLAS, ONLY: F77_HPR2
    ! <<< Implicit statement >>>
    !IMPLICIT NONE
    ! <<< Kind parameter >>>
    INTEGER, PARAMETER :: WP = KIND(1.0E0)
    ! <<< Scalar arguments >>>
    CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
    COMPLEX(WP), INTENT(IN), OPTIONAL :: ALPHA
    ! <<< Array arguments >>>
    COMPLEX(WP), INTENT(INOUT) :: AP(:)
    COMPLEX(WP), INTENT(IN) :: X(:)
    COMPLEX(WP), INTENT(IN) :: Y(:)
    ! <<< Local declarations >>>
    ! <<< Parameters >>>
    CHARACTER(LEN=4), PARAMETER :: SRNAME = 'HPR2'
    ! <<< Local scalars >>>
    CHARACTER(LEN=1) :: O_UPLO
    COMPLEX(WP) :: O_ALPHA
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
    IF(PRESENT(UPLO)) THEN
        O_UPLO = UPLO
    ELSE
        O_UPLO = 'U'
    ENDIF
    INCX = 1
    INCY = 1
    N = SIZE(X)
    ! <<< Call blas77 routine >>>
    CALL F77_HPR2(O_UPLO,N,O_ALPHA,X,INCX,Y,INCY,AP)
END SUBROUTINE CHPR2_F95
