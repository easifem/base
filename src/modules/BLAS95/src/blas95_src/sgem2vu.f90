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

PURE SUBROUTINE SGEM2VU_F95(A,X1,X2,Y1,Y2,ALPHA,BETA)
    ! Fortran77 call:
    ! SGEM2VU(M,N,ALPHA,A,LDA,X1,INCX1,X2,INCX2,BETA,Y1,INCY1,Y2,INCY2)
    ! Default ALPHA=1
    ! Default BETA=0
    ! <<< !USE statements >>>
    !USE F77_BLAS, ONLY: F77_GEM2V
    ! <<< Implicit statement >>>
    !IMPLICIT NONE
    ! <<< Kind parameter >>>
    INTEGER, PARAMETER :: WP = KIND(1.0E0)
    ! <<< Scalar arguments >>>
    REAL(WP), INTENT(IN), OPTIONAL :: ALPHA
    REAL(WP), INTENT(IN), OPTIONAL :: BETA
    ! <<< Array arguments >>>
    REAL(WP), INTENT(IN) :: A(:,:)
    REAL(WP), INTENT(IN) :: X1(:)
    REAL(WP), INTENT(IN) :: X2(:)
    REAL(WP), INTENT(INOUT) :: Y1(:)
    REAL(WP), INTENT(INOUT) :: Y2(:)
    ! <<< Local declarations >>>
    ! <<< Parameters >>>
    CHARACTER(LEN=5), PARAMETER :: SRNAME = 'GEM2V'
    ! <<< Local scalars >>>
    REAL(WP) :: O_ALPHA
    REAL(WP) :: O_BETA
    INTEGER :: INCX1
    INTEGER :: INCX2
    INTEGER :: INCY1
    INTEGER :: INCY2
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
    IF(PRESENT(BETA)) THEN
        O_BETA = BETA
    ELSE
        O_BETA = 0
    ENDIF
    INCX1 = 1
    INCX2 = 1
    INCY1 = 1
    INCY2 = 1
    LDA = MAX(1,SIZE(A,1))
    M = SIZE(A,1)
    N = SIZE(A,2)
    ! <<< Call blas77 routine >>>
    CALL F77_GEM2V(M,N,O_ALPHA,A,LDA,X1,INCX1,X2,INCX2,O_BETA,Y1,INCY1, &
     &                                                         Y2,INCY2)
END SUBROUTINE SGEM2VU_F95
