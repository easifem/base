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

PURE SUBROUTINE DGER_F95(A,X,Y,ALPHA)
    ! Fortran77 call:
    ! DGER(M,N,ALPHA,X,INCX,Y,INCY,A,LDA)
    ! Default ALPHA=1
    ! <<< !USE statements >>>
    !USE F77_BLAS, ONLY: F77_GER
    ! <<< Implicit statement >>>
    !IMPLICIT NONE
    ! <<< Kind parameter >>>
    INTEGER, PARAMETER :: WP = KIND(1.0D0)
    ! <<< Scalar arguments >>>
    REAL(WP), INTENT(IN), OPTIONAL :: ALPHA
    ! <<< Array arguments >>>
    REAL(WP), INTENT(INOUT ) :: A(:,:)
    REAL(WP), INTENT(IN) :: X(:)
    REAL(WP), INTENT(IN) :: Y(:)
    ! <<< Local declarations >>>
    ! <<< Parameters >>>
    CHARACTER(LEN=3), PARAMETER :: SRNAME = 'GER'
    ! <<< Local scalars >>>
    REAL(WP) :: O_ALPHA
    INTEGER :: INCX
    INTEGER :: INCY
    INTEGER :: M
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
    INCX = 1
    INCY = 1
    LDA = MAX(1,SIZE(A,1))
    M = SIZE(A,1)
    N = SIZE(A,2)
    ! <<< Call blas77 routine >>>
    CALL F77_GER(M,N,O_ALPHA,X,INCX,Y,INCY,A,LDA)
END SUBROUTINE DGER_F95
