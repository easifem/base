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

PURE SUBROUTINE ZHER2K_F95(A,B,C,UPLO,TRANS,ALPHA,BETA)
    ! Fortran77 call:
    ! ZHER2K(UPLO,TRANS,N,K,ALPHA,A,LDA,B,LDB,BETA,C,LDC)
    ! UPLO='U','L'; default: 'U'
    ! TRANS='N','C'; default: 'N'
    ! Default ALPHA=1
    ! Default BETA=0
    ! <<< !USE statements >>>
    !USE F77_BLAS, ONLY: F77_HER2K
    ! <<< Implicit statement >>>
    !IMPLICIT NONE
    ! <<< Kind parameter >>>
    INTEGER, PARAMETER :: WP = KIND(1.0D0)
    ! <<< Scalar arguments >>>
    CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: UPLO
    CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: TRANS
    COMPLEX(WP), INTENT(IN), OPTIONAL :: ALPHA
    REAL(WP), INTENT(IN), OPTIONAL :: BETA
    ! <<< Array arguments >>>
    COMPLEX(WP), INTENT(IN) :: A(:,:)
    COMPLEX(WP), INTENT(IN) :: B(:,:)
    COMPLEX(WP), INTENT(INOUT ) :: C(:,:)
    ! <<< Local declarations >>>
    ! <<< Parameters >>>
    CHARACTER(LEN=5), PARAMETER :: SRNAME = 'HER2K'
    ! <<< Local scalars >>>
    CHARACTER(LEN=1) :: O_UPLO
    CHARACTER(LEN=1) :: O_TRANS
    COMPLEX(WP) :: O_ALPHA
    REAL(WP) :: O_BETA
    INTEGER :: N
    INTEGER :: K
    INTEGER :: LDA
    INTEGER :: LDB
    INTEGER :: LDC
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
    IF(PRESENT(TRANS)) THEN
        O_TRANS = TRANS
    ELSE
        O_TRANS = 'N'
    ENDIF
    IF(PRESENT(UPLO)) THEN
        O_UPLO = UPLO
    ELSE
        O_UPLO = 'U'
    ENDIF
    IF((O_TRANS.EQ.'N'.OR.O_TRANS.EQ.'n')) THEN
        K = SIZE(A,2)
    ELSE
        K = SIZE(A,1)
    ENDIF
    LDA = MAX(1,SIZE(A,1))
    LDB = MAX(1,SIZE(B,1))
    LDC = MAX(1,SIZE(C,1))
    N = SIZE(C,2)
    ! <<< Call blas77 routine >>>
    CALL F77_HER2K(O_UPLO,O_TRANS,N,K,O_ALPHA,A,LDA,B,LDB,O_BETA,C,LDC)
END SUBROUTINE ZHER2K_F95
