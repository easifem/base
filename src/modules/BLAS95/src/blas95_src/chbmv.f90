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

PURE SUBROUTINE CHBMV_F95(A,X,Y,UPLO,ALPHA,BETA)
    ! Fortran77 call:
    ! CHBMV(UPLO,N,K,ALPHA,A,LDA,X,INCX,BETA,Y,INCY)
    ! UPLO='U','L'; default: 'U'
    ! Default ALPHA=1
    ! Default BETA=0
    ! <<< !USE statements >>>
    !USE F77_BLAS, ONLY: F77_HBMV
    ! <<< Implicit statement >>>
    !IMPLICIT NONE
    ! <<< Kind parameter >>>
    INTEGER, PARAMETER :: WP = KIND(1.0E0)
    ! <<< Scalar arguments >>>
    CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
    COMPLEX(WP), INTENT(IN), OPTIONAL :: ALPHA
    COMPLEX(WP), INTENT(IN), OPTIONAL :: BETA
    ! <<< Array arguments >>>
    COMPLEX(WP), INTENT(IN) :: A(:,:)
    COMPLEX(WP), INTENT(IN) :: X(:)
    COMPLEX(WP), INTENT(INOUT) :: Y(:)
    ! <<< Local declarations >>>
    ! <<< Parameters >>>
    CHARACTER(LEN=4), PARAMETER :: SRNAME = 'HBMV'
    ! <<< Local scalars >>>
    CHARACTER(LEN=1) :: O_UPLO
    COMPLEX(WP) :: O_ALPHA
    COMPLEX(WP) :: O_BETA
    INTEGER :: INCX
    INTEGER :: INCY
    INTEGER :: N
    INTEGER :: K
    INTEGER :: LDA
    ! <<< Intrinsic functions >>>
    INTRINSIC MAX, PRESENT, SIZE
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
        O_BETA = 0
    ENDIF
    IF(PRESENT(UPLO)) THEN
        O_UPLO = UPLO
    ELSE
        O_UPLO = 'U'
    ENDIF
    INCX = 1
    INCY = 1
    K = SIZE(A,1)-1
    LDA = MAX(1,SIZE(A,1))
    N = SIZE(A,2)
    ! <<< Call blas77 routine >>>
    CALL F77_HBMV(O_UPLO,N,K,O_ALPHA,A,LDA,X,INCX,O_BETA,Y,INCY)
END SUBROUTINE CHBMV_F95
