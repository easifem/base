! This program is a part of EASIFEM library
! Copyright (C) 2020-2021  Vikas Sharma, Ph.D
!
! This program is free software: you can redistribute it and/or modify
! it under the terms of the GNU General Public License as published by
! the Free Software Foundation, either version 3 of the License, or
! (at your option) any later version.
!
! This program is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.
!
! You should have received a copy of the GNU General Public License
! along with this program.  If not, see <https: //www.gnu.org/licenses/>
!

SUBMODULE(Sym_LUMethods) Methods
USE GlobalData, ONLY: DFP, I4B, LGT
USE BaseMethod, ONLY: Display, Input, Arange, Zeros, GetTril, &
& GetTriu, ArgSort, tostring
USE F95_LAPACK, ONLY: SYTRF, LACPY, LAPMR, POTRF, SYTRS, SYTRI
USE F77_LAPACK, ONLY: SYCONV => LA_SYCONV
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                     GetLU
!----------------------------------------------------------------------------

MODULE PROCEDURE SymGetLU_1
CALL LACPY(A=A, B=LU, UPLO=UPLO)
CALL SYTRF(A=LU, UPLO=UPLO, IPIV=IPIV, INFO=INFO)
END PROCEDURE SymGetLU_1

!----------------------------------------------------------------------------
!                                                                     GetLU
!----------------------------------------------------------------------------

MODULE PROCEDURE SymGetLU_2
CALL SYTRF(A=A, UPLO=UPLO, IPIV=IPIV, INFO=INFO)
END PROCEDURE SymGetLU_2

!----------------------------------------------------------------------------
!                                                                     GetLU
!----------------------------------------------------------------------------

MODULE PROCEDURE SymGetLDL_1
CHARACTER(1) :: luplo
INTEGER(I4B) :: linfo, n
INTEGER(I4B), ALLOCATABLE :: work(:, :)
!
! work(1:n, 1) = ipiv(:) from sytrf
! work(1:n, 2) = swap_(:) block diagonal information
! work(1:n, 3) = pivots(:) cleaned version of ipiv
! work(1:n, 4) = perm cleaned version of ipiv
!
n = SIZE(A, 1)
ALLOCATE (work(n, 4))
luplo = INPUT(option=UPLO, default="U")
LU = 0.0_DFP
!
! Copy data in LU from  A
!
CALL LACPY(A=A, B=LU, UPLO=luplo)
!
! Call SYTRF
!
CALL SYTRF(A=LU, UPLO=luplo, IPIV=work(:, 1), INFO=linfo)
!
! Clean the ipiv0 returned by SYTRF
!
CALL LDL_SENITIZE_IPIV(ipiv0=work(:, 1), uplo=luplo, &
  & swap_=work(:, 2), pivots=work(:, 3), info=linfo)
!
CALL LDL_GET_D_and_L(D=D, E=E, ldu=lu, &
  & pivs=work(:, 3), uplo=luplo)
!
CALL LDL_CONSTRUCT_TRI_FACTOR(lu=lu, swap_=work(:, 2), &
  & pivots=work(:, 3), perm=work(:, 4), permTemp=work(:, 1), uplo=luplo)
!
IF (PRESENT(info)) info = linfo
IF (PRESENT(IPIV)) IPIV = work(:, 4)
DEALLOCATE (work)
END PROCEDURE SymGetLDL_1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         2022-12-21
! summary:
!
!# Introduction
!
! This helper function takes the rather strangely encoded permutation array
!     returned by the LAPACK routines ?(HE/SY)TRF and converts it into
!     regularized permutation and diagonal pivot size format.
!     Since FORTRAN uses 1-indexing and LAPACK uses different start points for
!     upper and lower formats there are certain offsets in the indices used
!     below.
!     Let's assume a result where the matrix is 6x6 and there are two 2x2
!     and two 1x1 blocks reported by the routine. To ease the coding efforts,
!     we still populate a 6-sized array and fill zeros as the following ::
!         pivots = [2, 0, 2, 0, 1, 1]
!     This denotes a diagonal matrix of the form ::
!         [x x        ]
!         [x x        ]
!         [    x x    ]
!         [    x x    ]
!         [        x  ]
!         [          x]
!     In other words, we write 2 when the 2x2 block is first encountered and
!     automatically write 0 to the next entry and skip the next spin of the
!     loop. Thus, a separate counter or array appends to keep track of block
!     sizes are avoided. If needed, zeros can be filtered out later without
!     losing the block structure.
!     Parameters
!     ----------
!     a : ndarray
!         The permutation array ipiv returned by LAPACK
!     lower : bool, optional
!         The switch to select whether upper or lower triangle is chosen in
!         the LAPACK call.
!     Returns
!     -------
!     swap_ : ndarray
!         The array that defines the row/column swap operations. For example,
!         if row two is swapped with row four, the result is [0, 3, 2, 3].
!     pivots : ndarray
!         The array that defines the block diagonal structure as given above.

SUBROUTINE LDL_SENITIZE_IPIV(ipiv0, uplo, swap_, pivots, info)
  INTEGER(I4B), INTENT(IN) :: ipiv0(:)
  CHARACTER(1), INTENT(IN) :: uplo
  INTEGER(I4B), INTENT(INOUT) :: swap_(:)
  INTEGER(I4B), INTENT(INOUT) :: pivots(:)
  INTEGER(I4B), INTENT(OUT) :: info
  !
  ! internal variables
  !
  INTEGER(I4B) :: n, ind, x, y, rs, re, ri, cur_val
  LOGICAL(LGT) :: skip2x2
  !
  info = 0
  n = SIZE(ipiv0)
  !
  IF (uplo .EQ. "L") THEN
    x = 1
    y = 0
    rs = 1
    re = n
    ri = 1
  ELSE
    x = -1
    y = -1
    rs = n
    re = 1
    ri = -1
  END IF
  !
  skip2x2 = .FALSE.
  swap_ = arange(1_I4B, n)
  pivots = zeros(n, 1_I4B)
  !
  DO ind = rs, re, ri
    IF (skip2x2) skip2x2 = .FALSE.
    cur_val = ipiv0(ind)
    !
    IF (cur_val .GT. 0) THEN
      IF (cur_val .NE. ind) THEN
        swap_(ind) = swap_(cur_val)
      END IF
      pivots(ind) = 1
      !
    ELSEIF (cur_val < 0 .AND. cur_val == ipiv0(ind + x)) THEN
      !
      IF (-cur_val .NE. ind + 1) THEN
        swap_(ind + x) = swap_(-cur_val)
      END IF
      !
      pivots(ind + y) = 2
      skip2x2 = .TRUE.
      !
    END IF
  END DO

END SUBROUTINE LDL_SENITIZE_IPIV

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-12-21
! summary:
!
!# Introduction
!
! Helper function to extract the diagonal and triangular matrices for
! LDL.T factorization.
!
!## Parameters
!
! ldu : ndarray
!     The compact output returned by the LAPACK routing
!
! pivs : ndarray
!     The sanitized array of {0, 1, 2} denoting the sizes of the pivots. For
!     every 2 there is a succeeding 0.
!
! lower : bool, optional
!     If set to False, upper triangular part is considered.
!
! hermitian : bool, optional
!     If set to False a symmetric complex array is assumed.
!
!## Returns
!
! d : ndarray
!     The block diagonal matrix.
!
! lu : ndarray
!     The upper/lower triangular matrix

SUBROUTINE LDL_GET_D_and_L(D, E, ldu, pivs, uplo)
  REAL(DFP), INTENT(INOUT) :: D(:)
  REAL(DFP), INTENT(INOUT) :: E(:)
  REAL(DFP), INTENT(INOUT) :: ldu(:, :)
  INTEGER(I4B), INTENT(IN) :: pivs(:)
  CHARACTER(1), INTENT(IN) :: uplo
  !
  ! internal variables
  !
  INTEGER(I4B) :: x, y, ii, n, blk_i, inc
  !
  ! extract D from LDU
  !
  n = SIZE(ldu, 1)
  !
  DO CONCURRENT(ii=1:n)
    D(ii) = ldu(ii, ii)
    E(ii) = 0.0_DFP
    ldu(ii, ii) = 1.0_DFP
  END DO
  !
  blk_i = 1
  !
  IF (uplo .EQ. "L") THEN
    x = 1
    y = 0
    !!
    DO ii = 1, n
      IF (pivs(ii) .EQ. 0) CYCLE
      ! increment the block index and check for 2s
      ! if 2 then copy the off diagonals depending on uplo
      inc = blk_i + pivs(ii)
      !
      IF (pivs(ii) .EQ. 2) THEN
        E(blk_i) = ldu(blk_i + x, blk_i + y)
        ldu(blk_i + x, blk_i + y) = 0.0
      END IF
      !
      blk_i = inc
      !
    END DO
    !!
  ELSE
    y = 1
    x = 0
    !!
    DO ii = 1, n
      IF (pivs(ii) .EQ. 0) CYCLE
      ! increment the block index and check for 2s
      ! if 2 then copy the off diagonals depending on uplo
      inc = blk_i + pivs(ii)
      !
      IF (pivs(ii) .EQ. 2) THEN
        E(blk_i) = ldu(blk_i + x, blk_i + y)
        ldu(blk_i + x, blk_i + y) = 0.0
      END IF
      !
      blk_i = inc
      !
    END DO
  END IF
  !
END SUBROUTINE LDL_GET_D_and_L

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         2022-12-22
! summary:         Helper
!
!# Introduction
!
! Helper function to construct explicit outer factors of LDL factorization.
! If lower is True the permuted factors are multiplied as L(1)*L(2)*...*L(k).
! Otherwise, the permuted factors are multiplied as L(k)*...*L(2)*L(1). See
! LAPACK documentation for more details.
!
!### Parameters
! lu : ndarray
! The triangular array that is extracted from LAPACK routine call with
! ones on the diagonals.
! swap_ : ndarray
! The array that defines the row swapping indices. If the kth entry is m
! then rows k,m are swapped. Notice that the mth entry is not necessarily
! k to avoid undoing the swapping.
! pivots : ndarray
! The array that defines the block diagonal structure returned by
! _ldl_sanitize_ipiv().
! lower : bool, optional
! The boolean to switch between lower and upper triangular structure.
!
!### Returns
!lu : ndarray
!The square outer factor which satisfies the L * D * L.T = A
!perm : ndarray
!The permutation vector that brings the lu to the triangular form
!
!@note
!Note that the original argument "lu" is overwritten.
!@endnote

SUBROUTINE LDL_CONSTRUCT_TRI_FACTOR(lu, swap_, pivots, perm, permTemp, uplo)
  REAL(DFP), INTENT(INOUT) :: lu(:, :)
  INTEGER(I4B), INTENT(IN) :: swap_(:)
  INTEGER(I4B), INTENT(IN) :: pivots(:)
  INTEGER(I4B), INTENT(INOUT) :: perm(:)
  INTEGER(I4B), INTENT(INOUT) :: permTemp(:)
  CHARACTER(1), INTENT(IN) :: uplo
  !
  ! internal variables
  !
  INTEGER(I4B) :: n, rs, re, ri, ind, s_ind, col_s, col_e, zero_or_two, &
    & incr_col_s, incr_col_e
  LOGICAL(LGT) :: islower
  !
  ! main program
  !
  n = SIZE(lu, 1)
  permTemp = arange(1_I4B, n, 1_I4B)
  !
  IF (uplo .EQ. "L") THEN
    rs = n
    re = 1
    ri = -1
    islower = .TRUE.
    zero_or_two = 0
    incr_col_s = -1
    incr_col_e = 0
  ELSE
    rs = 1
    re = n
    ri = 1
    islower = .FALSE.
    zero_or_two = 2
    incr_col_s = 0
    incr_col_e = 1
  END IF
  !
  DO ind = rs, re, ri
    s_ind = swap_(ind)
    !
    IF (s_ind .NE. ind) THEN
      ! Column start and end positions
      IF (islower) THEN
        col_s = ind
        col_e = n
      ELSE
        col_s = 1
        col_e = ind
      END IF
      !
      ! If we stumble upon a 2x2 block include both cols in the permTemp.
      !
      IF (pivots(ind) .EQ. zero_or_two) THEN
        col_s = col_s + incr_col_s
        col_e = col_e + incr_col_e
      END IF
      !
      lu([s_ind, ind], col_s:col_e) = lu([ind, s_ind], col_s:col_e)
      permTemp([s_ind, ind]) = permTemp([ind, s_ind])
      !
    END IF
  END DO
  !!
  perm = ArgSort(permTemp)
  !!
END SUBROUTINE LDL_CONSTRUCT_TRI_FACTOR

!----------------------------------------------------------------------------
!                                                                     getLU
!----------------------------------------------------------------------------

MODULE PROCEDURE SymGetLDL_2
CHARACTER(1) :: luplo
INTEGER(I4B) :: linfo, n
INTEGER(I4B), ALLOCATABLE :: work(:, :)
!
! work(1:n, 1) = ipiv(:) from sytrf
! work(1:n, 2) = swap_(:) block diagonal information
! work(1:n, 3) = pivots(:) cleaned version of ipiv
! work(1:n, 4) = perm cleaned version of ipiv
!
n = SIZE(A, 1)
ALLOCATE (work(n, 4))
luplo = INPUT(option=UPLO, default="U")
!
! Call SYTRF
!
CALL SYTRF(A=A, UPLO=luplo, IPIV=work(:, 1), INFO=info)
!
! Clean the ipiv0 returned by SYTRF
!
CALL LDL_SENITIZE_IPIV(ipiv0=work(:, 1), uplo=luplo, &
  & swap_=work(:, 2), pivots=work(:, 3), info=linfo)
!
CALL LDL_GET_D_and_L(D=D, E=E, ldu=A, &
  & pivs=work(:, 3), uplo=luplo)
!
CALL LDL_CONSTRUCT_TRI_FACTOR(lu=A, swap_=work(:, 2), &
  & pivots=work(:, 3), perm=work(:, 4), permTemp=work(:, 1), uplo=luplo)
!
IF (PRESENT(info)) info = linfo
IF (PRESENT(IPIV)) IPIV = work(:, 4)
DEALLOCATE (work)
END PROCEDURE SymGetLDL_2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE SymGetCholesky_1
!
! Make a copy of LU
!
CALL LACPY(A=A, B=LU, UPLO=uplo)
CALL POTRF(A=LU, uplo=uplo, info=info)
!
END PROCEDURE SymGetCholesky_1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE SymGetCholesky_2
CALL POTRF(A=A, uplo=uplo, info=info)
END PROCEDURE SymGetCholesky_2

!----------------------------------------------------------------------------
!                                                                SymLUSolve
!----------------------------------------------------------------------------

MODULE PROCEDURE SymLUSolve_1
CALL SYTRS(A=A, B=B, IPIV=IPIV, UPLO=UPLO, INFO=INFO)
END PROCEDURE SymLUSolve_1

!----------------------------------------------------------------------------
!                                                                 SymLUSolve
!----------------------------------------------------------------------------

MODULE PROCEDURE SymLUSolve_2
CALL SYTRS(A=A, B=B, IPIV=IPIV, UPLO=UPLO, INFO=INFO)
END PROCEDURE SymLUSolve_2

!----------------------------------------------------------------------------
!                                                                 SymLUSolve
!----------------------------------------------------------------------------

MODULE PROCEDURE SymLUSolve_3
X = B
CALL SYTRS(A=A, B=X, IPIV=IPIV, UPLO=UPLO, INFO=INFO)
END PROCEDURE SymLUSolve_3

!----------------------------------------------------------------------------
!                                                                 SymLUSolve
!----------------------------------------------------------------------------

MODULE PROCEDURE SymLUSolve_4
X = B
CALL SYTRS(A=A, B=X, IPIV=IPIV, UPLO=UPLO, INFO=INFO)
END PROCEDURE SymLUSolve_4

!----------------------------------------------------------------------------
!                                                                     Inv
!----------------------------------------------------------------------------

MODULE PROCEDURE SymGetInv_1
CALL LACPY(A=A, B=invA, UPLO=UPLO)
CALL SYTRI(A=invA, IPIV=IPIV, UPLO=UPLO, info=INFO)
END PROCEDURE SymGetInv_1

!----------------------------------------------------------------------------
!                                                                     Inv
!----------------------------------------------------------------------------

MODULE PROCEDURE SymGetInv_2
CALL SYTRI(A=A, IPIV=IPIV, UPLO=UPLO, INFO=INFO)
END PROCEDURE SymGetInv_2

END SUBMODULE Methods
