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

PURE SUBROUTINE CHER_F95(A,X,UPLO,ALPHA)
    ! Fortran77 call:
    ! CHER(UPLO,N,ALPHA,X,INCX,A,LDA)
    ! UPLO='U','L'; default: 'U'
    ! Default ALPHA=1
    ! <<< !USE statements >>>
    !USE F77_BLAS, ONLY: F77_HER
    ! <<< Implicit statement >>>
    !IMPLICIT NONE
    ! <<< Kind parameter >>>
    INTEGER, PARAMETER :: WP = KIND(1.0E0)
    ! <<< Scalar arguments >>>
    CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
    REAL(WP), INTENT(IN), OPTIONAL :: ALPHA
    ! <<< Array arguments >>>
    COMPLEX(WP), INTENT(INOUT ) :: A(:,:)
    COMPLEX(WP), INTENT(IN) :: X(:)
    ! <<< Local declarations >>>
    ! <<< Parameters >>>
    CHARACTER(LEN=3), PARAMETER :: SRNAME = 'HER'
    ! <<< Local scalars >>>
    CHARACTER(LEN=1) :: O_UPLO
    REAL(WP) :: O_ALPHA
    INTEGER :: INCX
    INTEGER :: N
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
    IF(PRESENT(UPLO)) THEN
        O_UPLO = UPLO
    ELSE
        O_UPLO = 'U'
    ENDIF
    INCX = 1
    LDA = MAX(1,SIZE(A,1))
    N = SIZE(A,2)
    ! <<< Call blas77 routine >>>
    CALL F77_HER(O_UPLO,N,O_ALPHA,X,INCX,A,LDA)
END SUBROUTINE CHER_F95
