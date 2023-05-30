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

MODULE SuperLU_Enums
USE ISO_C_BINDING, ONLY: C_INT, C_DOUBLE, C_CHAR, C_FLOAT, C_PTR
IMPLICIT NONE
PRIVATE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

ENUM, BIND(C)
! typedef enum {NO, YES}                                          yes_no_t;
  ENUMERATOR :: NO, YES
END ENUM

PUBLIC :: NO, YES

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

ENUM, BIND(c)
  ENUMERATOR :: DOFACT, SamePattern, SamePattern_SameRowPerm, FACTORED
! typedef enum {DOFACT, SamePattern, SamePattern_SameRowPerm, FACTORED} fact_t;
END ENUM

PUBLIC :: DOFACT, SamePattern, SamePattern_SameRowPerm, FACTORED

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! typedef ENUM{NOROWPERM, LargeDiag_MC64, LargeDiag_HWPM, MY_PERMR}rowperm_t;
ENUM, BIND(C)
  ENUMERATOR :: NOROWPERM, LargeDiag_MC64, LargeDiag_HWPM, MY_PERMR
END ENUM

PUBLIC :: NOROWPERM, LargeDiag_MC64, LargeDiag_HWPM, MY_PERMR

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! typedef enum {NATURAL, MMD_ATA, MMD_AT_PLUS_A, COLAMD,
!               METIS_AT_PLUS_A, PARMETIS, ZOLTAN, MY_PERMC}      colperm_t;
ENUM, BIND(c)
  ENUMERATOR :: NATURAL, MMD_ATA, MMD_AT_PLUS_A, COLAMD, &
 & METIS_AT_PLUS_A, PARMETIS, ZOLTAN, MY_PERMC
END ENUM

PUBLIC :: NATURAL, MMD_ATA, MMD_AT_PLUS_A, COLAMD, &
& METIS_AT_PLUS_A, PARMETIS, ZOLTAN, MY_PERMC

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

ENUM, BIND(c)
  ENUMERATOR :: NOTRANS, TRANS, CONJ
! typedef enum {NOTRANS, TRANS, CONJ}                             trans_t;
END ENUM

PUBLIC :: NOTRANS, TRANS, CONJ

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

ENUM, BIND(C)
  ENUMERATOR :: NOEQUIL, ROW, COL, BOTH
! typedef enum {NOEQUIL, ROW, COL, BOTH}                          DiagScale_t;
END ENUM

PUBLIC :: NOEQUIL, ROW, COL, BOTH

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! typedef enum {NOREFINE, SLU_SINGLE=1, SLU_DOUBLE, SLU_EXTRA}    IterRefine_t;

ENUM, BIND(c)
  ENUMERATOR :: NOREFINE, SLU_SINGLE = 1, SLU_DOUBLE, SLU_EXTRA
! typedef enum {NOREFINE, SLU_SINGLE=1, SLU_DOUBLE, SLU_EXTRA}    IterRefine_t;
END ENUM

PUBLIC :: NOREFINE, SLU_SINGLE, SLU_DOUBLE, SLU_EXTRA

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

ENUM, BIND(c)
  ENUMERATOR :: USUB, LSUB, UCOL, LUSUP, LLVL, ULVL, NO_MEMTYPE
! typedef enum {USUB, LSUB, UCOL, LUSUP, LLVL, ULVL, NO_MEMTYPE}  MemType;
END ENUM

PUBLIC :: USUB, LSUB, UCOL, LUSUP, LLVL, ULVL, NO_MEMTYPE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

ENUM, BIND(C)
  ENUMERATOR :: HEAD, TAIL
! typedef enum {HEAD, TAIL}                                       stack_end_t;
END ENUM

PUBLIC :: HEAD, TAIL

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! typedef enum {SYSTEM, USER}                                     LU_space_t;

ENUM, BIND(c)
  ENUMERATOR :: SYSTEM, USER
END ENUM

PUBLIC :: SYSTEM, USER

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

ENUM, BIND(c)
  ENUMERATOR :: ONE_NORM, TWO_NORM, INF_NORM
! typedef enum {ONE_NORM, TWO_NORM, INF_NORM}                        norm_t;
END ENUM

PUBLIC :: ONE_NORM, TWO_NORM, INF_NORM

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

ENUM, BIND(C)
  ENUMERATOR :: SILU, SMILU_1, SMILU_2, SMILU_3
! typedef enum {SILU, SMILU_1, SMILU_2, SMILU_3}                        milu_t;
END ENUM

PUBLIC :: SILU, SMILU_1, SMILU_2, SMILU_3

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 23-01-21
! summary: Stype_t enums
!
!# Introduction
!
! typedef enum {
!   SLU_NC,    /* column-wise, no supernode */
!   SLU_NCP,   /* column-wise, column-permuted, no supernode
!                 (The consecutive columns of nonzeros, after permutation,
!                  may not be stored  contiguously.) */
!   SLU_NR,    /* row-wize, no supernode */
!   SLU_SC,    /* column-wise, supernode */
!   SLU_SCP,   /* supernode, column-wise, permuted */
!   SLU_SR,    /* row-wise, supernode */
!   SLU_DN,    /* Fortran style column-wise storage for dense matrix */
!   SLU_NR_loc /* distributed compressed row format  */
! } Stype_t;
!
ENUM, BIND(C)
  ENUMERATOR :: SLU_NC
  ENUMERATOR :: SLU_NCP
  ENUMERATOR :: SLU_NR
  ENUMERATOR :: SLU_SC
  ENUMERATOR :: SLU_SCP
  ENUMERATOR :: SLU_SR
  ENUMERATOR :: SLU_DN
  ENUMERATOR :: SLU_NR_loc
END ENUM

PUBLIC :: SLU_NC, SLU_NCP, SLU_NR, SLU_SC, SLU_SCP, SLU_SR, &
  & SLU_DN, SLU_NR_loc

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  23-01-21
! summary: Dtype_t
!
!# Introduction
!
!```c
! typedef enum {
!   SLU_S, /* single */
!   SLU_D, /* double */
!   SLU_C, /* single complex */
!   SLU_Z  /* double complex */
! } Dtype_t;
!```

ENUM, BIND(c)
  ENUMERATOR :: SLU_S
  ENUMERATOR :: SLU_D
  ENUMERATOR :: SLU_C
  ENUMERATOR :: SLU_Z
END ENUM

PUBLIC :: SLU_S, SLU_D, SLU_C, SLU_Z

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  23-01-21
! summary: MType_t
!
!# Introduction
!
!```c
! typedef enum {
!   SLU_GE,   /* general */
!   SLU_TRLU, /* lower triangular, unit diagonal */
!   SLU_TRUU, /* upper triangular, unit diagonal */
!   SLU_TRL,  /* lower triangular */
!   SLU_TRU,  /* upper triangular */
!   SLU_SYL,  /* symmetric, store lower half */
!   SLU_SYU,  /* symmetric, store upper half */
!   SLU_HEL,  /* Hermitian, store lower half */
!   SLU_HEU   /* Hermitian, store upper half */
! } Mtype_t;
!```

ENUM, BIND(c)
  ENUMERATOR :: SLU_GE
  ENUMERATOR :: SLU_TRLU
  ENUMERATOR :: SLU_TRUU
  ENUMERATOR :: SLU_TRL
  ENUMERATOR :: SLU_TRU
  ENUMERATOR :: SLU_SYL
  ENUMERATOR :: SLU_SYU
  ENUMERATOR :: SLU_HEL
  ENUMERATOR :: SLU_HEU
END ENUM

PUBLIC :: SLU_GE, SLU_TRLU, SLU_TRUU, SLU_TRL, SLU_TRU, &
  & SLU_SYL, SLU_SYU, SLU_HEL, SLU_HEU

END MODULE SuperLU_Enums
