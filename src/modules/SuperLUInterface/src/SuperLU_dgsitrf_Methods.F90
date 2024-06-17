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

MODULE SuperLU_dgsitrf_Methods
USE SuperLU_Types
USE ISO_C_BINDING, ONLY: C_CHAR, C_INT, C_FLOAT, C_DOUBLE, C_PTR
IMPLICIT NONE
PRIVATE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------
!
!
!  DGSITRF computes an ILU factorization of a general sparse m-by-n
!  matrix A using partial pivoting with row interchanges.
!  The factorization has the form
!      Pr * A = L * U
!  where Pr is a row permutation matrix, L is lower triangular with unit
!  diagonal elements (lower trapezoidal if A->nrow > A->ncol), and U is upper
!  triangular (upper trapezoidal if A->nrow < A->ncol).
!
!  See supermatrix.h for the definition of 'SuperMatrix' structure.
!
!  ======================================================================
!
!  Local Working Arrays:
!  ======================
!    m = number of rows in the matrix
!    n = number of columns in the matrix
!
!    marker[0:3*m-1]: marker[i] = j means that node i has been
!         reached when working on column j.
!         Storage: relative to original row subscripts
!         NOTE: There are 4 of them:
!               marker/marker1 are used for panel dfs, see (ilu_)dpanel_dfs.c;
!               marker2 is used for inner-factorization, see (ilu)_dcolumn_dfs.c;
!               marker_relax(has its own space) is used for relaxed supernodes.
!
!    parent[0:m-1]: parent vector used during dfs
!         Storage: relative to new row subscripts
!
!    xplore[0:m-1]: xplore[i] gives the location of the next (dfs)
!         unexplored neighbor of i in lsub[*]
!
!    segrep[0:nseg-1]: contains the list of supernodal representatives
!         in topological order of the dfs. A supernode representative is the
!         last column of a supernode.
!         The maximum size of segrep[] is n.
!
!    repfnz[0:W*m-1]: for a nonzero segment U[*,j] that ends at a
!         supernodal representative r, repfnz[r] is the location of the first
!         nonzero in this segment.  It is also used during the dfs: repfnz[r]>0
!         indicates the supernode r has been explored.
!         NOTE: There are W of them, each used for one column of a panel.
!
!    panel_lsub[0:W*m-1]: temporary for the nonzeros row indices below
!         the panel diagonal. These are filled in during dpanel_dfs(), and are
!         used later in the inner LU factorization within the panel.
!         panel_lsub[]/dense[] pair forms the SPA data structure.
!         NOTE: There are W of them.
!
!    dense[0:W*m-1]: sparse accumulating (SPA) vector for intermediate values;
!                    NOTE: there are W of them.
!
!    tempv[0:*]: real temporary used for dense numeric kernels;
!         The size of this array is defined by NUM_TEMPV() in slu_util.h.
!         It is also used by the dropping routine ilu_ddrop_row().
!
! void
! dgsitrf(superlu_options_t *options, SuperMatrix *A, int relax, int panel_size,
!         int *etree, void *work, int lwork, int *perm_c, int *perm_r,
!         SuperMatrix *L, SuperMatrix *U,
!             GlobalLU_t *Glu, /* persistent to facilitate multiple factorizations */
!         SuperLUStat_t *stat, int *info)

INTERFACE
  SUBROUTINE dgsitrf(options, A, relax, panel_size, etree, &
    & work, lwork, perm_c, perm_r, &
    & L, U, &
    & Glu, stat, info) &
    & BIND(C, name="dgsitrf")
    IMPORT :: superlu_options_t, SuperLUStat_t, C_INT, C_PTR, &
      & SuperMatrix, GlobalLU_t, C_CHAR, C_DOUBLE
    !
    TYPE(superlu_options_t), INTENT(IN) :: options
    !  options (input) superlu_options_t*
    !            The structure defines the input parameters to control
    !            how the ILU decomposition will be performed.
    TYPE(SuperMatrix), INTENT(INOUT) :: A
    !  A            (input) SuperMatrix*
    !             Original matrix A, permuted by columns, of dimension
    !             (A->nrow, A->ncol). The type of A can be:
    !             Stype = SLU_NCP; Dtype = SLU_D; Mtype = SLU_GE.
    INTEGER(C_INT), VALUE, INTENT(IN) :: relax
    !  relax    (input) int
    !             To control degree of relaxing supernodes. If the number
    !             of nodes (columns) in a subtree of the elimination tree is less
    !             than relax, this subtree is considered as one supernode,
    !             regardless of the row structures of those columns.
    INTEGER(C_INT), VALUE, INTENT(IN) :: panel_size
    !  panel_size (input) int
    !             A panel consists of at most panel_size consecutive columns.
    INTEGER(C_INT), INTENT(INOUT) :: etree(*)
    !  etree    (input) int*, dimension (A->ncol)
    !             Elimination tree of A'*A.
    !             Note: etree is a vector of parent pointers for a forest whose
    !             vertices are the integers 0 to A->ncol-1; etree[root]==A->ncol.
    !             On input, the columns of A should be permuted so that the
    !             etree is in a certain postorder.
    TYPE(C_PTR), INTENT(inout) :: work
    !  work     (input/output) void*, size (lwork) (in bytes)
    !             User-supplied work space and space for the output data structures.
    !             Not referenced if lwork = 0;
    INTEGER(C_INT), VALUE, INTENT(IN) :: lwork
    !  lwork   (input) int
    !            Specifies the size of work array in bytes.
    !            = 0:  allocate space internally by system malloc;
    !            > 0:  use user-supplied work array of length lwork in bytes,
    !                  returns error if space runs out.
    !            = -1: the routine guesses the amount of space needed without
    !                  performing the factorization, and returns it in
    !                  *info; no other side effects.
    INTEGER(C_INT), INTENT(INOUT) :: perm_c(*)
    !  perm_c   (input) int*, dimension (A->ncol)
    !             Column permutation vector, which defines the
    !             permutation matrix Pc; perm_c[i] = j means column i of A is
    !             in position j in A*Pc.
    !             When searching for diagonal, perm_c[*] is applied to the
    !             row subscripts of A, so that diagonal threshold pivoting
    !             can find the diagonal of A, rather than that of A*Pc.
    INTEGER(C_INT), INTENT(INOUT) :: perm_r(*)
    !  perm_r   (input/output) int*, dimension (A->nrow)
    !             Row permutation vector which defines the permutation matrix Pr,
    !             perm_r[i] = j means row i of A is in position j in Pr*A.
    !             If options->Fact = SamePattern_SameRowPerm, the pivoting routine
    !                will try to use the input perm_r, unless a certain threshold
    !                criterion is violated. In that case, perm_r is overwritten by
    !                a new permutation determined by partial pivoting or diagonal
    !                threshold pivoting.
    !             Otherwise, perm_r is output argument;
    TYPE(SuperMatrix), INTENT(INOUT) :: L
    !  L            (output) SuperMatrix*
    !             The factor L from the factorization Pr*A=L*U; use compressed row
    !             subscripts storage for supernodes, i.e., L has type:
    !             Stype = SLU_SC, Dtype = SLU_D, Mtype = SLU_TRLU.
    TYPE(SuperMatrix), INTENT(INOUT) :: U
    !  U            (output) SuperMatrix*
    !             The factor U from the factorization Pr*A*Pc=L*U. Use column-wise
    !             storage scheme, i.e., U has types: Stype = SLU_NC,
    !             Dtype = SLU_D, Mtype = SLU_TRU.
    TYPE(GlobalLU_t), INTENT(inout) :: Glu
    !  Glu      (input/output) GlobalLU_t *
    !           If options->Fact == SamePattern_SameRowPerm, it is an input;
    !               The matrix A will be factorized assuming that a
    !               factorization of a matrix with the same sparsity pattern
    !               and similar numerical values was performed prior to this one.
    !               Therefore, this factorization will reuse both row and column
    !                 scaling factors R and C, both row and column permutation
    !                 vectors perm_r and perm_c, and the L & U data structures
    !                 set up from the previous factorization.
    !           Otherwise, it is an output.
    TYPE(SuperLUStat_t), INTENT(INOUT) :: stat
    !  stat     (output) SuperLUStat_t*
    !             Record the statistics on runtime and floating-point operation count.
    !             See slu_util.h for the definition of 'SuperLUStat_t'.
    INTEGER(C_INT), INTENT(INOUT) :: info
    !  info     (output) int*
    !             = 0: successful exit
    !             < 0: if info = -i, the i-th argument had an illegal value
    !             > 0: if info = i, and i is
    !                <= A->ncol: number of zero pivots. They are replaced by small
    !                   entries according to options->ILU_FillTol.
    !                > A->ncol: number of bytes allocated when memory allocation
    !                   failure occurred, plus A->ncol. If lwork = -1, it is
    !                   the estimated amount of space needed, plus A->ncol.
  END SUBROUTINE dgsitrf
END INTERFACE

PUBLIC :: dgsitrf

END MODULE SuperLU_dgsitrf_Methods
