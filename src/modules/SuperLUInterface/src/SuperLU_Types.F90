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

MODULE SuperLU_Types
USE ISO_C_BINDING, ONLY: C_INT, C_DOUBLE, C_CHAR, C_FLOAT, C_PTR
USE SuperLU_Enums
IMPLICIT NONE
PRIVATE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

TYPE :: yes_no_
  INTEGER(C_INT) :: no, yes
END TYPE

TYPE(yes_no_), PUBLIC, PARAMETER :: yes_no_t = yes_no_(no=no, yes=yes)

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

TYPE :: fact_
  INTEGER(C_INT) :: DOFACT, SamePattern, SamePattern_SameRowPerm, FACTORED
END TYPE fact_

TYPE(fact_), PARAMETER, PUBLIC :: fact_t = fact_(&
  & DOFACT=DOFACT, &
  & SamePattern=SamePattern, &
  & SamePattern_SameRowPerm=SamePattern_SameRowPerm, &
  & FACTORED=FACTORED &
  &)

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

TYPE :: rowperm_
  INTEGER(C_INT) :: NOROWPERM
  INTEGER(C_INT) :: LargeDiag_MC64
  INTEGER(C_INT) :: LargeDiag_HWPM
  INTEGER(C_INT) :: MY_PERMR
END TYPE rowperm_

TYPE(rowperm_), PUBLIC, PARAMETER :: rowperm_t = rowperm_(&
  & NOROWPERM=NOROWPERM, &
  & LargeDiag_MC64=LargeDiag_MC64, &
  & LargeDiag_HWPM=LargeDiag_HWPM, &
  & MY_PERMR=MY_PERMR &
  & )

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

TYPE :: colperm_
  INTEGER(C_INT) :: NATURAL, MMD_ATA, MMD_AT_PLUS_A, &
    & COLAMD, METIS_AT_PLUS_A, PARMETIS, ZOLTAN, MY_PERMC
END TYPE colperm_

TYPE(colperm_), PUBLIC, PARAMETER :: colperm_t = colperm_(&
  & NATURAL=NATURAL, &
  & MMD_ATA=MMD_ATA, &
  & MMD_AT_PLUS_A=MMD_AT_PLUS_A, &
  & COLAMD=COLAMD, &
  & METIS_AT_PLUS_A=METIS_AT_PLUS_A, &
  & PARMETIS=PARMETIS, &
  & ZOLTAN=ZOLTAN, &
  & MY_PERMC=MY_PERMC &
  &)

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

TYPE :: trans_
  INTEGER(C_INT) :: NOTRANS, TRANS, CONJ
END TYPE trans_

TYPE(trans_), PARAMETER, PUBLIC :: trans_t = trans_(&
  & NOTRANS=NOTRANS, trans=trans, conj=conj)

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

TYPE :: DiagScale_
  INTEGER(C_INT) :: NOEQUIL, ROW, COL, BOTH
END TYPE DiagScale_

TYPE(DiagScale_), PARAMETER, PUBLIC :: DiagScale_t = DiagScale_(&
  & NOEQUIL=NOEQUIL, ROW=ROW, COL=COL, BOTH=BOTH)

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! typedef enum {NOREFINE, SLU_SINGLE=1, SLU_DOUBLE, SLU_EXTRA}    IterRefine_t;

TYPE :: IterRefine_
  INTEGER(C_INT) :: NOREFINE, SLU_SINGLE = 1, SLU_DOUBLE, SLU_EXTRA
END TYPE

TYPE(IterRefine_), PARAMETER, PUBLIC :: IterRefine_t = IterRefine_(&
& NOREFINE, SLU_SINGLE, SLU_DOUBLE, SLU_EXTRA)

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

TYPE :: MemType_
  INTEGER(C_INT) :: USUB, LSUB, UCOL, LUSUP, LLVL, ULVL, NO_MEMTYPE
END TYPE

TYPE(MemType_), PUBLIC, PARAMETER :: MemType_t = MemType_( &
& USUB, LSUB, UCOL, LUSUP, LLVL, ULVL, NO_MEMTYPE)

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! typedef enum {HEAD, TAIL}                                       stack_end_t;

TYPE :: stack_end_
  INTEGER(C_INT) :: HEAD, TAIL
END TYPE

TYPE(stack_end_), PUBLIC, PARAMETER :: stack_end_t = stack_end_(&
& HEAD, TAIL)

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

TYPE :: LU_space_
  INTEGER(C_INT) :: SYSTEM
  INTEGER(C_INT) :: USER
END TYPE LU_space_

TYPE(LU_space_), PARAMETER, PUBLIC :: LU_space_t = LU_space_(&
  & SYSTEM=SYSTEM, &
  & USER=USER &
  & )

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

TYPE :: Stype_
  INTEGER(C_INT) :: SLU_NC
  INTEGER(C_INT) :: SLU_NCP
  INTEGER(C_INT) :: SLU_NR
  INTEGER(C_INT) :: SLU_SC
  INTEGER(C_INT) :: SLU_SCP
  INTEGER(C_INT) :: SLU_SR
  INTEGER(C_INT) :: SLU_DN
  INTEGER(C_INT) :: SLU_NR_LOC
END TYPE Stype_

TYPE(Stype_), PARAMETER, PUBLIC :: Stype_t = Stype_(&
  & SLU_NC=SLU_NC, &
  & SLU_NCP=SLU_NCP, &
  & SLU_NR=SLU_NR, &
  & SLU_SC=SLU_SC, &
  & SLU_SCP=SLU_SCP, &
  & SLU_SR=SLU_SR, &
  & SLU_DN=SLU_DN, &
  & SLU_NR_LOC=SLU_NR_LOC &
  & )

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

TYPE :: Dtype_
  INTEGER(C_INT) :: SLU_S
  INTEGER(C_INT) :: SLU_D
  INTEGER(C_INT) :: SLU_C
  INTEGER(C_INT) :: SLU_Z
END TYPE Dtype_

TYPE(Dtype_), PARAMETER, PUBLIC :: Dtype_t = Dtype_(&
  & SLU_S=SLU_S, &
  & SLU_D=SLU_D, &
  & SLU_C=SLU_C, &
  & SLU_Z=SLU_Z)

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

TYPE :: Mtype_
  INTEGER(C_INT) :: SLU_GE
  INTEGER(C_INT) :: SLU_TRLU
  INTEGER(C_INT) :: SLU_TRUU
  INTEGER(C_INT) :: SLU_TRL
  INTEGER(C_INT) :: SLU_TRU
  INTEGER(C_INT) :: SLU_SYL
  INTEGER(C_INT) :: SLU_SYU
  INTEGER(C_INT) :: SLU_HEL
  INTEGER(C_INT) :: SLU_HEU
END TYPE Mtype_

TYPE(Mtype_), PUBLIC, PARAMETER :: Mtype_t = Mtype_(&
  & SLU_GE=SLU_GE, &
  & SLU_TRLU=SLU_TRLU, &
  & SLU_TRUU=SLU_TRUU, &
  & SLU_TRL=SLU_TRL, &
  & SLU_TRU=SLU_TRU, &
  & SLU_SYL=SLU_SYL, &
  & SLU_SYU=SLU_SYU, &
  & SLU_HEL=SLU_HEL, &
  & SLU_HEU=SLU_HEU)

!----------------------------------------------------------------------------
!                                                          superlu_options_t
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 23-01-21
! summary: SuperLU options
!
!# Introduction
!

! typedef struct {
!     fact_t        Fact;
!     yes_no_t      Equil;
!     colperm_t     ColPerm;
!     trans_t       Trans;
!     IterRefine_t  IterRefine;
!     double        DiagPivotThresh;
!     yes_no_t      SymmetricMode;
!     yes_no_t      PivotGrowth;
!     yes_no_t      ConditionNumber;
!     rowperm_t     RowPerm;
!     int           ILU_DropRule;
!     double          ILU_DropTol;    /* threshold for dropping */
!     double          ILU_FillFactor; /* gamma in the secondary dropping */
!     norm_t          ILU_Norm;       /* infinity-norm, 1-norm, or 2-norm */
!     double          ILU_FillTol;    /* threshold for zero pivot perturbation */
!     milu_t          ILU_MILU;
!     double          ILU_MILU_Dim;   /* Dimension of PDE (if available) */
!     yes_no_t      ParSymbFact;
!     yes_no_t      ReplaceTinyPivot; /* used in SuperLU_DIST */
!     yes_no_t      SolveInitialized;
!     yes_no_t      RefineInitialized;
!     yes_no_t      PrintStat;
!     int           nnzL, nnzU;      /* used to store nnzs for now       */
!     int           num_lookaheads;  /* num of levels in look-ahead      */
!     yes_no_t      lookahead_etree; /* use etree computed from the
!                                       serial symbolic factorization */
!     yes_no_t      SymPattern;      /* symmetric factorization          */
! } superlu_options_t;
!

TYPE, BIND(C) :: superlu_options_t
  INTEGER(C_INT) :: Fact
  INTEGER(C_INT) :: Equil
  INTEGER(C_INT) :: ColPerm
  INTEGER(C_INT) :: Trans
  INTEGER(C_INT) :: IterRefine
  REAL(C_DOUBLE) :: DiagPivotThresh
  INTEGER(C_INT) :: SymmetricMode
  INTEGER(C_INT) :: PivotGrowth
  INTEGER(C_INT) :: ConditionNumber
  INTEGER(C_INT) :: RowPerm
  INTEGER(C_INT) :: ILU_DropRule
  REAL(C_DOUBLE) :: ILU_DropTol
  REAL(C_DOUBLE) :: ILU_FillFactor
  INTEGER(C_INT) :: ILU_Norm
  REAL(C_DOUBLE) :: ILU_FillTol
  INTEGER(C_INT) :: ILU_MILU; 
  REAL(C_DOUBLE) :: ILU_MILU_Dim
  INTEGER(C_INT) :: ParSymbFact
  INTEGER(C_INT) :: ReplaceTinyPivot
  INTEGER(C_INT) :: SolveInitialized
  INTEGER(C_INT) :: RefineInitialized
  INTEGER(C_INT) :: PrintStat = 0
  INTEGER(C_INT) :: nnzL
  INTEGER(C_INT) :: nnzU
  INTEGER(C_INT) :: num_lookaheads
  INTEGER(C_INT) :: lookahead_etree
  INTEGER(C_INT) :: SymPattern
END TYPE superlu_options_t

PUBLIC :: superlu_options_t

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! typedef struct e_node {
!   int size;  /* length of the memory that has been used */
!   void *mem; /* pointer to the new malloc'd store */
! } ExpHeader;

TYPE, BIND(c) :: ExpHeader
  INTEGER(C_INT) :: size
  TYPE(C_PTR) :: mem
END TYPE ExpHeader

PUBLIC :: ExpHeader

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! typedef struct {
!   int size;
!   int used;
!   int top1; /* grow upward, relative to &array[0] */
!   int top2; /* grow downward */
!   void *array;
! } LU_stack_t;

TYPE, BIND(c) :: LU_stack_t
  INTEGER(C_INT) :: size
  INTEGER(C_INT) :: used
  INTEGER(C_INT) :: top1
  INTEGER(C_INT) :: top2
  TYPE(C_PTR) :: array
END TYPE LU_stack_t

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! typedef struct {
!     int     *panel_histo; /* histogram of panel size distribution */
!     double  *utime;       /* running time at various phases */
!     flops_t *ops;         /* operation count at various phases */
!     int     TinyPivots;   /* number of tiny pivots */
!     int     RefineSteps;  /* number of iterative refinement steps */
!     int     expansions;   /* number of memory expansions */
! } SuperLUStat_t;

TYPE, BIND(C) :: SuperLUStat_t
  ! INTEGER(C_INT), POINTER :: panel_histo(:)
  ! REAL(C_DOUBLE), POINTER :: utime(:)
  ! REAL(C_FLOAT), POINTER :: ops(:)
  TYPE(C_PTR) :: panel_histo
  TYPE(C_PTR) :: utime
  TYPE(C_PTR) :: ops
  INTEGER(C_INT) :: TinyPivots
  INTEGER(C_INT) :: RefineSteps
  INTEGER(C_INT) :: expansions
END TYPE SuperLUStat_t

PUBLIC :: SuperLUStat_t

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! typedef struct {
!     float for_lu;
!     float total_needed;
! } mem_usage_t;

TYPE, BIND(C) :: mem_usage_t
  REAL(C_FLOAT) :: for_lu
  REAL(C_FLOAT) :: total_needed
END TYPE mem_usage_t

PUBLIC :: mem_usage_t

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! typedef struct{
! int * xsup; /*supernode and column mapping*/
! int * supno;
! int * lsub; /*compressed L subscripts*/
! int * xlsub;
! void * lusup; /*L supernodes*/
! int * xlusup;
! void * ucol; /*U columns*/
! int * usub;
! int * xusub;
! int nzlmax; /*current max size of lsub*/
! int nzumax; int nzlumax; int n; /*number of columns in the matrix*/
! LU_space_t MemModel; int num_expansions;
! ExpHeader * expanders; /*Array of pointers to 4 types of memory*/
! LU_stack_t stack; /*USE user supplied memory*/
! }GlobalLU_t;

TYPE, BIND(c) :: GlobalLU_t
  TYPE(C_PTR) :: xsup, supno, lsub, xlsub, lusup, xlusup, ucol, usub
  TYPE(C_PTR) :: xusub
  INTEGER(C_INT) :: nzlmax, nzumax, nzlumax, n, num_expansions
  INTEGER(C_INT) :: MemModel
  TYPE(ExpHeader) :: expanders
  TYPE(LU_stack_t) :: stack
END TYPE GlobalLU_t

PUBLIC :: GlobalLU_t

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! typedef struct {
!   Stype_t Stype; /* Storage type: interprets the storage structure
!                     pointed to by *Store. */
!   Dtype_t Dtype; /* Data type. */
!   Mtype_t Mtype; /* Matrix type: describes the mathematical property of
!                     the matrix. */
!   int_t nrow;    /* number of rows */
!   int_t ncol;    /* number of columns */
!   void *Store;   /* pointer to the actual storage of the matrix */
! } SuperMatrix;

TYPE, BIND(C) :: SuperMatrix
  INTEGER(C_INT) :: Stype; 
  INTEGER(C_INT) :: Dtype; 
  INTEGER(C_INT) :: Mtype; 
  INTEGER(C_INT) :: nrow
  INTEGER(C_INT) :: ncol
  TYPE(C_PTR) :: Store
END TYPE SuperMatrix

PUBLIC :: SuperMatrix

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! typedef struct {
!   int_t nnz;     /* number of nonzeros in the matrix */
!   void *nzval;   /* pointer to array of nonzero values, packed by column */
!   int_t *rowind; /* pointer to array of row indices of the nonzeros */
!   int_t *colptr; /* pointer to array of beginning of columns in nzval[]
!                     and rowind[]  */
!                  /* Note:
!                     Zero-based indexing is used;
!                     colptr[] has ncol+1 entries, the last one pointing
!                     beyond the last column, so that colptr[ncol] = nnz. */
! } NCformat;

TYPE, BIND(c) :: NCformat
  INTEGER(C_INT) :: nnz
  TYPE(C_PTR) :: nzval
  TYPE(C_PTR) :: rowind
  TYPE(C_PTR) :: colptr
END TYPE NCformat

PUBLIC :: NCformat

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! typedef struct {
!   int_t nnz;     /* number of nonzeros in the matrix */
!   void *nzval;   /* pointer to array of nonzero values, packed by raw */
!   int_t *colind; /* pointer to array of columns indices of the nonzeros */
!   int_t *rowptr; /* pointer to array of beginning of rows in nzval[]
!                     and colind[]  */
!                  /* Note:
!                     Zero-based indexing is used;
!                     rowptr[] has nrow+1 entries, the last one pointing
!                     beyond the last row, so that rowptr[nrow] = nnz. */
! } NRformat;

TYPE, BIND(c) :: NRformat
  INTEGER(C_INT) :: nnz
  TYPE(C_PTR) :: nzval
  TYPE(C_PTR) :: colind
  TYPE(C_PTR) :: rowptr
END TYPE NRformat

PUBLIC :: NRformat

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! typedef struct {
!   int_t nnz;    /* number of nonzeros in the matrix */
!   int_t nsuper; /* number of supernodes, minus 1 */
!   void *nzval;  /* pointer to array of nonzero values, packed by column */
!   int_t *nzval_colptr; /* pointer to array of beginning of columns in nzval[] */
!   int_t *rowind;       /* pointer to array of compressed row indices of
!                           rectangular supernodes */
!   int_t
!       *rowind_colptr; /* pointer to array of beginning of columns in rowind[] */
!   int_t *col_to_sup;  /* col_to_sup[j] is the supernode number to which column
!                        j belongs; mapping from column to supernode number. */
!   int_t *sup_to_col;  /* sup_to_col[s] points to the start of the s-th
!                        supernode; mapping from supernode number to column.
!                        e.g.: col_to_sup: 0 1 2 2 3 3 3 4 4 4 4 4 4 (ncol=12)
!                              sup_to_col: 0 1 2 4 7 12           (nsuper=4) */
!                       /* Note:
!                          Zero-based indexing is used;
!                          nzval_colptr[], rowind_colptr[], col_to_sup and
!                          sup_to_col[] have ncol+1 entries, the last one
!                          pointing beyond the last column.
!                          For col_to_sup[], only the first ncol entries are
!                          defined. For sup_to_col[], only the first nsuper+2
!                          entries are defined. */
! } SCformat;
!

TYPE, BIND(c) :: SCformat
  INTEGER(C_INT) :: nnz
  INTEGER(C_INT) :: nsuper
  TYPE(C_PTR) :: nzval
  TYPE(C_PTR) :: nzval_colptr
  TYPE(C_PTR) :: rowind
  TYPE(C_PTR) :: rowind_colptr
  TYPE(C_PTR) :: col_to_sup
  TYPE(C_PTR) :: sup_to_col
END TYPE SCformat

PUBLIC :: SCformat

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! typedef struct {
!   int_t nnz;    /* number of nonzeros in the matrix */
!   int_t nsuper; /* number of supernodes */
!   void *nzval;  /* pointer to array of nonzero values, packed by column */
!   int_t *nzval_colbeg;  /* nzval_colbeg[j] points to beginning of column j
!                            in nzval[] */
!   int_t *nzval_colend;  /* nzval_colend[j] points to one past the last element
!                            of column j in nzval[] */
!   int_t *rowind;        /* pointer to array of compressed row indices of
!                            rectangular supernodes */
!   int_t *rowind_colbeg; /* rowind_colbeg[j] points to beginning of column j
!                            in rowind[] */
!   int_t *rowind_colend; /* rowind_colend[j] points to one past the last element
!                            of column j in rowind[] */
!   int_t *col_to_sup;    /* col_to_sup[j] is the supernode number to which column
!                            j belongs; mapping from column to supernode. */
!   int_t *sup_to_colbeg; /* sup_to_colbeg[s] points to the start of the s-th
!                            supernode; mapping from supernode to column.*/
!   int_t *sup_to_colend; /* sup_to_colend[s] points to one past the end of the
!                            s-th supernode; mapping from supernode number to
!                            column.
!                         e.g.: col_to_sup: 0 1 2 2 3 3 3 4 4 4 4 4 4 (ncol=12)
!                               sup_to_colbeg: 0 1 2 4 7              (nsuper=4)
!                               sup_to_colend: 1 2 4 7 12                    */
!                         /* Note:
!                            Zero-based indexing is used;
!                            nzval_colptr[], rowind_colptr[], col_to_sup and
!                            sup_to_col[] have ncol+1 entries, the last one
!                            pointing beyond the last column.         */
! } SCPformat;

TYPE, BIND(c) :: SCPformat
  INTEGER(C_INT) :: nnz, nsuper
  TYPE(C_PTR) :: nzval, nzval_colbeg, nzval_colend, rowind, &
    & rowind_colbeg, rowindx_colend, col_to_sup, sup_to_colbeg, &
    & sup_to_colend
END TYPE SCPformat

PUBLIC :: SCPformat

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! typedef struct {
!   int_t nnz;     /* number of nonzeros in the matrix */
!   void *nzval;   /* pointer to array of nonzero values, packed by column */
!   int_t *rowind; /* pointer to array of row indices of the nonzeros */
!   /* Note: nzval[]/rowind[] always have the same length */
!   int_t *colbeg; /* colbeg[j] points to the beginning of column j in nzval[]
!                     and rowind[]  */
!   int_t *colend; /* colend[j] points to one past the last element of column
!                     j in nzval[] and rowind[]  */
!                  /* Note:
!                     Zero-based indexing is used;
!                     The consecutive columns of the nonzeros may not be
!                     contiguous in storage, because the matrix has been
!                     postmultiplied by a column permutation matrix. */
! } NCPformat;

TYPE, BIND(c) :: NCPformat
  INTEGER(C_INT) :: nnz
  TYPE(C_PTR) :: nzval, rowindx, colbeg, colend
END TYPE NCPformat

PUBLIC :: NCPformat

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! typedef struct {
!   int_t lda;   /* leading dimension */
!   void *nzval; /* array of size lda*ncol to represent a dense matrix */
! } DNformat;

TYPE, BIND(c) :: DNformat
  INTEGER(C_INT) :: lda
  TYPE(C_PTR) :: nzval
END TYPE DNformat

PUBLIC :: DNformat

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! typedef struct {
!   int_t nnz_loc; /* number of nonzeros in the local submatrix */
!   int_t m_loc;   /* number of rows local to this processor */
!   int_t fst_row; /* global index of the first row */
!   void *nzval;   /* pointer to array of nonzero values, packed by row */
!   int_t *rowptr; /* pointer to array of beginning of rows in nzval[]
!                     and colind[]  */
!   int_t *colind; /* pointer to array of column indices of the nonzeros */
!                  /* Note:
!                     Zero-based indexing is used;
!                     rowptr[] has n_loc + 1 entries, the last one pointing
!                     beyond the last row, so that rowptr[n_loc] = nnz_loc.*/
! } NRformat_loc;

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! typedef struct NRformat_loc3d {
!   NRformat_loc *A_nfmt; // Gathered A matrix on 2D grid-0
!   void *B3d;            // on the entire 3D process grid
!   int ldb;              // relative to 3D process grid
!   int nrhs;
!   int m_loc; // relative to 3D process grid
!   void *B2d; // on 2D process layer grid-0
!
!   int *row_counts_int; // these counts are stored on 2D layer grid-0,
!   int *row_disp;       // but count the number of {A, B} rows along Z-dimension
!   int *nnz_counts_int;
!   int *nnz_disp;
!   int *b_counts_int;
!   int *b_disp;
!
!   /* The following 4 structures are used for scattering
!      solution X from 2D grid-0 back to 3D processes */
!   int num_procs_to_send;
!   int *procs_to_send_list;
!   int *send_count_list;
!   int num_procs_to_recv;
!   int *procs_recv_from_list;
!   int *recv_count_list;
! } NRformat_loc3d;

END MODULE SuperLU_Types
