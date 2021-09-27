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

PURE SUBROUTINE SROTG_F95(A,B,C,S)
    ! Fortran77 call:
    ! SROTG(A,B,C,S)
    ! <<< !USE statements >>>
    !USE F77_BLAS, ONLY: F77_ROTG
    ! <<< Implicit statement >>>
    !IMPLICIT NONE
    ! <<< Kind parameter >>>
    INTEGER, PARAMETER :: WP = KIND(1.0E0)
    ! <<< Scalar arguments >>>
    REAL(WP), INTENT(INOUT) :: A
    REAL(WP), INTENT(INOUT) :: B
    REAL(WP), INTENT(OUT) :: C
    REAL(WP), INTENT(OUT) :: S
    ! <<< Local declarations >>>
    ! <<< Parameters >>>
    CHARACTER(LEN=4), PARAMETER :: SRNAME = 'ROTG'
    ! <<< Local scalars >>>
    ! <<< Executable statements >>>
    ! <<< Call blas77 routine >>>
    CALL F77_ROTG(A,B,C,S)
END SUBROUTINE SROTG_F95
