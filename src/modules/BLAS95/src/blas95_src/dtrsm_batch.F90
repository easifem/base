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

PURE SUBROUTINE DTRSM_BATCH_F95(A_ARRAY,B_ARRAY,M_ARRAY,N_ARRAY,    &
     &                          GROUP_SIZE,SIDE_ARRAY,UPLO_ARRAY,   &
     &                          TRANSA_ARRAY,DIAG_ARRAY,ALPHA_ARRAY)
    ! Fortran77 call:
    ! DTRSM_BATCH(SIDE_ARRAY,UPLO_ARRAY,TRANSA_ARRAY,DIAG_ARRAY,M_ARRAY,N_ARRAY,
    !             ALPHA_ARRAY,A_ARRAY,LDA_ARRAY,B_ARRAY,LDB_ARRAY,
    !             GROUP_COUNT,GROUP_SIZE)
    ! SIDE_ARRAY=Array where each element is one of 'L' or 'R'; default: 'L'
    ! UPLO_ARRAY=Array where each element is one of 'U' or 'L'; default: 'U'
    ! TRANSA_ARRAY=Array where each element is one of 'N','C','T'; default: 'N'
    ! DIAG_ARRAY=Array where each element is one of 'U' or 'N'; default: 'N'
    ! ALPHA_ARRAY=Array of alpha values; default: array where each element=1
    ! <<< !USE statements >>>
    !USE F77_BLAS, ONLY: F77_TRSM_BATCH, F77_XERBLA
    USE, INTRINSIC :: ISO_C_BINDING
    ! <<< Implicit statement >>>
    !IMPLICIT NONE
    ! <<< Kind parameter >>>
    INTEGER, PARAMETER :: WP = KIND(1.0D0)
    ! <<< Array arguments >>>
    INTEGER, INTENT(IN) :: M_ARRAY(:)
    INTEGER, INTENT(IN) :: N_ARRAY(:)
    INTEGER, INTENT(IN) :: GROUP_SIZE(:)
    ! SIDE_ARRAY
    CHARACTER(LEN=1), INTENT(INOUT ), OPTIONAL, TARGET :: SIDE_ARRAY(:)
    ! UPLO_ARRAY
    CHARACTER(LEN=1), INTENT(INOUT ), OPTIONAL, TARGET :: UPLO_ARRAY(:)
    ! TRANSA_ARRAY: INOUT intent instead of IN beca!USE PURE.
    CHARACTER(LEN=1), INTENT(INOUT ), OPTIONAL, TARGET :: TRANSA_ARRAY(:)
    ! DIAG_ARRAY
    CHARACTER(LEN=1), INTENT(INOUT ), OPTIONAL, TARGET :: DIAG_ARRAY(:)
    ! ALPHA_ARRAY: INOUT intent instead of IN beca!USE PURE.
    REAL(WP), INTENT(INOUT ), OPTIONAL, TARGET :: ALPHA_ARRAY(:)
    INTEGER(KIND=C_SIZE_T), INTENT(IN) :: A_ARRAY(:)
    INTEGER(KIND=C_SIZE_T), INTENT(IN) :: B_ARRAY(:)
    ! <<< Local declarations >>>
    ! <<< Parameters >>>
    CHARACTER(LEN=11), PARAMETER :: SRNAME = 'DTRSM_BATCH'
    ! <<< Local scalars >>>
    INTEGER :: L_STAT_ALLOC, L_STAT_DEALLOC
    INTEGER :: GROUP_COUNT
    INTEGER :: I
    ! <<< Local arrays >>>
    CHARACTER(LEN=1), POINTER :: O_SIDE_ARRAY(:)
    CHARACTER(LEN=1), POINTER :: O_UPLO_ARRAY(:)
    CHARACTER(LEN=1), POINTER :: O_TRANSA_ARRAY(:)
    CHARACTER(LEN=1), POINTER :: O_DIAG_ARRAY(:)
    REAL(WP), POINTER :: O_ALPHA_ARRAY(:)
    INTEGER, POINTER :: LDA_ARRAY(:)
    INTEGER, POINTER :: LDB_ARRAY(:)
    ! <<< Intrinsic functions >>>
    INTRINSIC MAX, PRESENT, SIZE
    ! <<< Executable statements >>>
    ! <<< Init skipped scalars >>>
    GROUP_COUNT = SIZE(GROUP_SIZE)
    ! <<< Init allocate status >>>
    L_STAT_ALLOC = 0
    ! <<< Init optional and skipped arrays >>>
    IF(PRESENT(ALPHA_ARRAY)) THEN
        O_ALPHA_ARRAY => ALPHA_ARRAY
    ELSE
        ALLOCATE(O_ALPHA_ARRAY(GROUP_COUNT), STAT=L_STAT_ALLOC)
        IF(L_STAT_ALLOC==0) THEN
            DO I=1, GROUP_COUNT
                O_ALPHA_ARRAY(I) = 1
            END DO
        ENDIF
    ENDIF
    IF(PRESENT(SIDE_ARRAY)) THEN
        O_SIDE_ARRAY => SIDE_ARRAY
    ELSEIF(L_STAT_ALLOC==0) THEN
        ALLOCATE(O_SIDE_ARRAY(GROUP_COUNT), STAT=L_STAT_ALLOC)
        IF(L_STAT_ALLOC==0) THEN
            DO I=1, GROUP_COUNT
                O_SIDE_ARRAY(I) = 'L'
            END DO
        ENDIF
    ENDIF
    IF(PRESENT(UPLO_ARRAY)) THEN
        O_UPLO_ARRAY => UPLO_ARRAY
    ELSEIF(L_STAT_ALLOC==0) THEN
        ALLOCATE(O_UPLO_ARRAY(GROUP_COUNT), STAT=L_STAT_ALLOC)
        IF(L_STAT_ALLOC==0) THEN
            DO I=1, GROUP_COUNT
                O_UPLO_ARRAY(I) = 'U'
            END DO
        ENDIF
    ENDIF
    IF(PRESENT(TRANSA_ARRAY)) THEN
        O_TRANSA_ARRAY => TRANSA_ARRAY
    ELSEIF(L_STAT_ALLOC==0) THEN
        ALLOCATE(O_TRANSA_ARRAY(GROUP_COUNT), STAT=L_STAT_ALLOC)
        IF(L_STAT_ALLOC==0) THEN
            DO I=1, GROUP_COUNT
                O_TRANSA_ARRAY(I) = 'N'
            END DO
        ENDIF
    ENDIF
    IF(PRESENT(DIAG_ARRAY)) THEN
        O_DIAG_ARRAY => DIAG_ARRAY
    ELSEIF(L_STAT_ALLOC==0) THEN
        ALLOCATE(O_DIAG_ARRAY(GROUP_COUNT), STAT=L_STAT_ALLOC)
        IF(L_STAT_ALLOC==0) THEN
            DO I=1, GROUP_COUNT
                O_DIAG_ARRAY(I) = 'N'
            END DO
        ENDIF
    ENDIF
    IF(L_STAT_ALLOC==0) THEN
        ALLOCATE(LDA_ARRAY(GROUP_COUNT), STAT=L_STAT_ALLOC)
        IF(L_STAT_ALLOC==0) THEN
            DO I=1, GROUP_COUNT
                IF(O_SIDE_ARRAY(I).EQ.'L') THEN
                    LDA_ARRAY(I) = MAX(1,M_ARRAY(I))
                ELSE
                    LDA_ARRAY(I) = MAX(1,N_ARRAY(I))
                ENDIF
            END DO
        ENDIF
    ENDIF
    IF(L_STAT_ALLOC==0) THEN
        ALLOCATE(LDB_ARRAY(GROUP_COUNT), STAT=L_STAT_ALLOC)
        IF(L_STAT_ALLOC==0) THEN
            DO I=1, GROUP_COUNT
                LDB_ARRAY(I) = MAX(1,M_ARRAY(I))
            END DO
        ENDIF
    ENDIF

    ! <<< Call blas77 routine >>>
    IF(L_STAT_ALLOC==0) THEN
        CALL F77_TRSM_BATCH(O_SIDE_ARRAY,O_UPLO_ARRAY,O_TRANSA_ARRAY, &
     &                      O_DIAG_ARRAY,M_ARRAY,N_ARRAY,             &
     &                      O_ALPHA_ARRAY,A_ARRAY,                    &
     &                      LDA_ARRAY,B_ARRAY,LDB_ARRAY,              &
     &                      GROUP_COUNT,GROUP_SIZE)
    ENDIF
    ! <<< Deallocate local arrays >>>
    IF(.NOT. PRESENT(ALPHA_ARRAY)) THEN
        IF (ASSOCIATED(O_ALPHA_ARRAY)) THEN
            DEALLOCATE(O_ALPHA_ARRAY, STAT=L_STAT_DEALLOC)
        ENDIF
    ENDIF
    IF(.NOT. PRESENT(SIDE_ARRAY)) THEN
        IF (ASSOCIATED(O_SIDE_ARRAY)) THEN
            DEALLOCATE(O_SIDE_ARRAY, STAT=L_STAT_DEALLOC)
        ENDIF
    ENDIF
    IF(.NOT. PRESENT(UPLO_ARRAY)) THEN
        IF (ASSOCIATED(O_UPLO_ARRAY)) THEN
            DEALLOCATE(O_UPLO_ARRAY, STAT=L_STAT_DEALLOC)
        ENDIF
    ENDIF
    IF(.NOT. PRESENT(TRANSA_ARRAY)) THEN
        IF (ASSOCIATED(O_TRANSA_ARRAY)) THEN
            DEALLOCATE(O_TRANSA_ARRAY, STAT=L_STAT_DEALLOC)
        ENDIF
    ENDIF
    IF(.NOT. PRESENT(DIAG_ARRAY)) THEN
        IF (ASSOCIATED(O_DIAG_ARRAY)) THEN
            DEALLOCATE(O_DIAG_ARRAY, STAT=L_STAT_DEALLOC)
        ENDIF
    ENDIF
    IF (ASSOCIATED(LDA_ARRAY)) THEN
        DEALLOCATE(LDA_ARRAY, STAT=L_STAT_DEALLOC)
    ENDIF
    IF (ASSOCIATED(LDB_ARRAY)) THEN
        DEALLOCATE(LDB_ARRAY, STAT=L_STAT_DEALLOC)
    ENDIF
    IF(L_STAT_ALLOC .NE. 0) THEN
        CALL F77_XERBLA(SRNAME,1000)
    ENDIF
END SUBROUTINE DTRSM_BATCH_F95
