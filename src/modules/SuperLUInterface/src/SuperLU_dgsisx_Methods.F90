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

MODULE SuperLU_dgsisx_Methods
USE SuperLU_Types
USE ISO_C_BINDING, ONLY: C_CHAR, C_INT, C_FLOAT, C_DOUBLE, C_PTR
IMPLICIT NONE
PRIVATE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------
!
!
!  * Purpose
!  * =======
!  *
!  * DGSISX computes an approximate solutions of linear equations
!  * A*X=B or A'*X=B, using the ILU factorization from dgsitrf().
!  * An estimation of the condition number is provided.
!  * The routine performs the following steps:
!  *
!  *   1. If A is stored column-wise (A->Stype = SLU_NC):
!  *
!  *        1.1. If options->Equil = YES or options->RowPerm = LargeDiag_MC64, scaling
!  *             factors are computed to equilibrate the system:
!  *             options->Trans = NOTRANS:
!  *                 diag(R)*A*diag(C) *inv(diag(C))*X = diag(R)*B
!  *             options->Trans = TRANS:
!  *                 (diag(R)*A*diag(C))**T *inv(diag(R))*X = diag(C)*B
!  *             options->Trans = CONJ:
!  *                 (diag(R)*A*diag(C))**H *inv(diag(R))*X = diag(C)*B
!  *             Whether or not the system will be equilibrated depends on the
!  *             scaling of the matrix A, but if equilibration is used, A is
!  *             overwritten by diag(R)*A*diag(C) and B by diag(R)*B
!  *             (if options->Trans=NOTRANS) or diag(C)*B (if options->Trans
!  *             = TRANS or CONJ).
!  *
!  *        1.2. Permute columns of A, forming A*Pc, where Pc is a permutation
!  *             matrix that usually preserves sparsity.
!  *             For more details of this step, see sp_preorder.c.
!  *
!  *        1.3. If options->Fact != FACTORED, the LU decomposition is used to
!  *             factor the matrix A (after equilibration if options->Equil = YES)
!  *             as Pr*A*Pc = L*U, with Pr determined by partial pivoting.
!  *
!  *        1.4. Compute the reciprocal pivot growth factor.
!  *
!  *        1.5. If some U(i,i) = 0, so that U is exactly singular, then the
!  *             routine fills a small number on the diagonal entry, that is
!  *                U(i,i) = ||A(:,i)||_oo * options->ILU_FillTol ** (1 - i / n),
!  *             and info will be increased by 1. The factored form of A is used
!  *             to estimate the condition number of the preconditioner. If the
!  *             reciprocal of the condition number is less than machine precision,
!  *             info = A->ncol+1 is returned as a warning, but the routine still
!  *             goes on to solve for X.
!  *
!  *        1.6. The system of equations is solved for X using the factored form
!  *             of A.
!  *
!  *        1.7. options->IterRefine is not used
!  *
!  *        1.8. If equilibration was used, the matrix X is premultiplied by
!  *             diag(C) (if options->Trans = NOTRANS) or diag(R)
!  *             (if options->Trans = TRANS or CONJ) so that it solves the
!  *             original system before equilibration.
!  *
!  *        1.9. options for ILU only
!  *             1) If options->RowPerm = LargeDiag_MC64, MC64 is used to scale and
!  *                permute the matrix to an I-matrix, that is Pr*Dr*A*Dc has
!  *                entries of modulus 1 on the diagonal and off-diagonal entries
!  *                of modulus at most 1. If MC64 fails, dgsequ() is used to
!  *                equilibrate the system.
!  *              ( Default: LargeDiag_MC64 )
!  *             2) options->ILU_DropTol = tau is the threshold for dropping.
!  *                For L, it is used directly (for the whole row in a supernode);
!  *                For U, ||A(:,i)||_oo * tau is used as the threshold
!  *                for the        i-th column.
!  *                If a secondary dropping rule is required, tau will
!  *                also be used to compute the second threshold.
!  *              ( Default: 1e-4 )
!  *             3) options->ILU_FillFactor = gamma, used as the initial guess
!  *                of memory growth.
!  *                If a secondary dropping rule is required, it will also
!  *              be used as an upper bound of the memory.
!  *              ( Default: 10 )
!  *             4) options->ILU_DropRule specifies the dropping rule.
!  *                Option              Meaning
!  *                ======              ===========
!  *                DROP_BASIC:   Basic dropping rule, supernodal based ILUTP(tau).
!  *                DROP_PROWS:   Supernodal based ILUTP(p,tau), p = gamma*nnz(A)/n.
!  *                DROP_COLUMN:  Variant of ILUTP(p,tau), for j-th column,
!  *                              p = gamma * nnz(A(:,j)).
!  *                DROP_AREA:    Variation of ILUTP, for j-th column, use
!  *                              nnz(F(:,1:j)) / nnz(A(:,1:j)) to control memory.
!  *                DROP_DYNAMIC: Modify the threshold tau during factorizaion:
!  *                              If nnz(L(:,1:j)) / nnz(A(:,1:j)) > gamma
!  *                                  tau_L(j) := MIN(tau_0, tau_L(j-1) * 2);
!  *                              Otherwise
!  *                                  tau_L(j) := MAX(tau_0, tau_L(j-1) / 2);
!  *                              tau_U(j) uses the similar rule.
!  *                              NOTE: the thresholds used by L and U are separate.
!  *                DROP_INTERP:  Compute the second dropping threshold by
!  *                              interpolation instead of sorting (default).
!  *                              In this case, the actual fill ratio is not
!  *                              guaranteed smaller than gamma.
!  *                DROP_PROWS, DROP_COLUMN and DROP_AREA are mutually exclusive.
!  *                ( Default: DROP_BASIC | DROP_AREA )
!  *             5) options->ILU_Norm is the criterion of measuring the magnitude
!  *                of a row in a supernode of L. ( Default is INF_NORM )
!  *                options->ILU_Norm        RowSize(x[1:n])
!  *                =================        ===============
!  *                ONE_NORM                ||x||_1 / n
!  *                TWO_NORM                ||x||_2 / sqrt(n)
!  *                INF_NORM                max{|x[i]|}
!  *             6) options->ILU_MILU specifies the type of MILU's variation.
!  *                = SILU: do not perform Modified ILU;
!  *                = SMILU_1 (not recommended):
!  *                    U(i,i) := U(i,i) + sum(dropped entries);
!  *                = SMILU_2:
!  *                    U(i,i) := U(i,i) + SGN(U(i,i)) * sum(dropped entries);
!  *                = SMILU_3:
!  *                    U(i,i) := U(i,i) + SGN(U(i,i)) * sum(|dropped entries|);
!  *                NOTE: Even SMILU_1 does not preserve the column sum because of
!  *                late dropping.
!  *              ( Default: SILU )
!  *             7) options->ILU_FillTol is used as the perturbation when
!  *                encountering zero pivots. If some U(i,i) = 0, so that U is
!  *                exactly singular, then
!  *                   U(i,i) := ||A(:,i)|| * options->ILU_FillTol ** (1 - i / n).
!  *              ( Default: 1e-2 )
!  *
!  *   2. If A is stored row-wise (A->Stype = SLU_NR), apply the above algorithm
!  *        to the transpose of A:
!  *
!  *        2.1. If options->Equil = YES or options->RowPerm = LargeDiag_MC64, scaling
!  *             factors are computed to equilibrate the system:
!  *             options->Trans = NOTRANS:
!  *                 diag(R)*A*diag(C) *inv(diag(C))*X = diag(R)*B
!  *             options->Trans = TRANS:
!  *                 (diag(R)*A*diag(C))**T *inv(diag(R))*X = diag(C)*B
!  *             options->Trans = CONJ:
!  *                 (diag(R)*A*diag(C))**H *inv(diag(R))*X = diag(C)*B
!  *             Whether or not the system will be equilibrated depends on the
!  *             scaling of the matrix A, but if equilibration is used, A' is
!  *             overwritten by diag(R)*A'*diag(C) and B by diag(R)*B
!  *             (if trans='N') or diag(C)*B (if trans = 'T' or 'C').
!  *
!  *        2.2. Permute columns of transpose(A) (rows of A),
!  *             forming transpose(A)*Pc, where Pc is a permutation matrix that
!  *             usually preserves sparsity.
!  *             For more details of this step, see sp_preorder.c.
!  *
!  *        2.3. If options->Fact != FACTORED, the LU decomposition is used to
!  *             factor the transpose(A) (after equilibration if
!  *             options->Fact = YES) as Pr*transpose(A)*Pc = L*U with the
!  *             permutation Pr determined by partial pivoting.
!  *
!  *        2.4. Compute the reciprocal pivot growth factor.
!  *
!  *        2.5. If some U(i,i) = 0, so that U is exactly singular, then the
!  *             routine fills a small number on the diagonal entry, that is
!  *                 U(i,i) = ||A(:,i)||_oo * options->ILU_FillTol ** (1 - i / n).
!  *             And info will be increased by 1. The factored form of A is used
!  *             to estimate the condition number of the preconditioner. If the
!  *             reciprocal of the condition number is less than machine precision,
!  *             info = A->ncol+1 is returned as a warning, but the routine still
!  *             goes on to solve for X.
!  *
!  *        2.6. The system of equations is solved for X using the factored form
!  *             of transpose(A).
!  *
!  *        2.7. If options->IterRefine is not used.
!  *
!  *        2.8. If equilibration was used, the matrix X is premultiplied by
!  *             diag(C) (if options->Trans = NOTRANS) or diag(R)
!  *             (if options->Trans = TRANS or CONJ) so that it solves the
!  *             original system before equilibration.
!  *
!  *   See supermatrix.h for the definition of 'SuperMatrix' structure.
!  *
!  * Arguments
!  * =========
!  *
!  * options (input) superlu_options_t*
!  *           The structure defines the input parameters to control
!  *           how the LU decomposition will be performed and how the
!  *           system will be solved.
!  *
!  * A           (input/output) SuperMatrix*
!  *           Matrix A in A*X=B, of dimension (A->nrow, A->ncol). The number
!  *           of the linear equations is A->nrow. Currently, the type of A can be:
!  *           Stype = SLU_NC or SLU_NR, Dtype = SLU_D, Mtype = SLU_GE.
!  *           In the future, more general A may be handled.
!  *
!  *           On entry, If options->Fact = FACTORED and equed is not 'N',
!  *           then A must have been equilibrated by the scaling factors in
!  *           R and/or C.
!  *           On exit, A is not modified
!  *         if options->Equil = NO, or
!  *         if options->Equil = YES but equed = 'N' on exit, or
!  *         if options->RowPerm = NO.
!  *
!  *           Otherwise, if options->Equil = YES and equed is not 'N',
!  *           A is scaled as follows:
!  *           If A->Stype = SLU_NC:
!  *             equed = 'R':  A := diag(R) * A
!  *             equed = 'C':  A := A * diag(C)
!  *             equed = 'B':  A := diag(R) * A * diag(C).
!  *           If A->Stype = SLU_NR:
!  *             equed = 'R':  transpose(A) := diag(R) * transpose(A)
!  *             equed = 'C':  transpose(A) := transpose(A) * diag(C)
!  *             equed = 'B':  transpose(A) := diag(R) * transpose(A) * diag(C).
!  *
!  *         If options->RowPerm = LargeDiag_MC64, MC64 is used to scale and permute
!  *            the matrix to an I-matrix, that is A is modified as follows:
!  *            P*Dr*A*Dc has entries of modulus 1 on the diagonal and
!  *            off-diagonal entries of modulus at most 1. P is a permutation
!  *            obtained from MC64.
!  *            If MC64 fails, dgsequ() is used to equilibrate the system,
!  *            and A is scaled as above, but no permutation is involved.
!  *            On exit, A is restored to the orginal row numbering, so
!  *            Dr*A*Dc is returned.
!  *
!  * perm_c  (input/output) int*
!  *           If A->Stype = SLU_NC, Column permutation vector of size A->ncol,
!  *           which defines the permutation matrix Pc; perm_c[i] = j means
!  *           column i of A is in position j in A*Pc.
!  *           On exit, perm_c may be overwritten by the product of the input
!  *           perm_c and a permutation that postorders the elimination tree
!  *           of Pc'*A'*A*Pc; perm_c is not changed if the elimination tree
!  *           is already in postorder.
!  *
!  *           If A->Stype = SLU_NR, column permutation vector of size A->nrow,
!  *           which describes permutation of columns of transpose(A)
!  *           (rows of A) as described above.
!  *
!  * perm_r  (input/output) int*
!  *           If A->Stype = SLU_NC, row permutation vector of size A->nrow,
!  *           which defines the permutation matrix Pr, and is determined
!  *           by MC64 first then followed by partial pivoting.
!  *         perm_r[i] = j means row i of A is in position j in Pr*A.
!  *
!  *           If A->Stype = SLU_NR, permutation vector of size A->ncol, which
!  *           determines permutation of rows of transpose(A)
!  *           (columns of A) as described above.
!  *
!  *           If options->Fact = SamePattern_SameRowPerm, the pivoting routine
!  *           will try to use the input perm_r, unless a certain threshold
!  *           criterion is violated. In that case, perm_r is overwritten by a
!  *           new permutation determined by partial pivoting or diagonal
!  *           threshold pivoting.
!  *           Otherwise, perm_r is output argument.
!  *
!  * etree   (input/output) int*,  dimension (A->ncol)
!  *           Elimination tree of Pc'*A'*A*Pc.
!  *           If options->Fact != FACTORED and options->Fact != DOFACT,
!  *           etree is an input argument, otherwise it is an output argument.
!  *           Note: etree is a vector of parent pointers for a forest whose
!  *           vertices are the integers 0 to A->ncol-1; etree[root]==A->ncol.
!  *
!  * equed   (input/output) char*
!  *           Specifies the form of equilibration that was done.
!  *           = 'N': No equilibration.
!  *           = 'R': Row equilibration, i.e., A was premultiplied by diag(R).
!  *           = 'C': Column equilibration, i.e., A was postmultiplied by diag(C).
!  *           = 'B': Both row and column equilibration, i.e., A was replaced
!  *                  by diag(R)*A*diag(C).
!  *           If options->Fact = FACTORED, equed is an input argument,
!  *           otherwise it is an output argument.
!  *
!  * R           (input/output) double*, dimension (A->nrow)
!  *           The row scale factors for A or transpose(A).
!  *           If equed = 'R' or 'B', A (if A->Stype = SLU_NC) or transpose(A)
!  *               (if A->Stype = SLU_NR) is multiplied on the left by diag(R).
!  *           If equed = 'N' or 'C', R is not accessed.
!  *           If options->Fact = FACTORED, R is an input argument,
!  *               otherwise, R is output.
!  *           If options->Fact = FACTORED and equed = 'R' or 'B', each element
!  *               of R must be positive.
!  *
!  * C           (input/output) double*, dimension (A->ncol)
!  *           The column scale factors for A or transpose(A).
!  *           If equed = 'C' or 'B', A (if A->Stype = SLU_NC) or transpose(A)
!  *               (if A->Stype = SLU_NR) is multiplied on the right by diag(C).
!  *           If equed = 'N' or 'R', C is not accessed.
!  *           If options->Fact = FACTORED, C is an input argument,
!  *               otherwise, C is output.
!  *           If options->Fact = FACTORED and equed = 'C' or 'B', each element
!  *               of C must be positive.
!  *
!  * L           (output) SuperMatrix*
!  *           The factor L from the factorization
!  *               Pr*A*Pc=L*U                (if A->Stype SLU_= NC) or
!  *               Pr*transpose(A)*Pc=L*U        (if A->Stype = SLU_NR).
!  *           Uses compressed row subscripts storage for supernodes, i.e.,
!  *           L has types: Stype = SLU_SC, Dtype = SLU_D, Mtype = SLU_TRLU.
!  *
!  * U           (output) SuperMatrix*
!  *           The factor U from the factorization
!  *               Pr*A*Pc=L*U                (if A->Stype = SLU_NC) or
!  *               Pr*transpose(A)*Pc=L*U        (if A->Stype = SLU_NR).
!  *           Uses column-wise storage scheme, i.e., U has types:
!  *           Stype = SLU_NC, Dtype = SLU_D, Mtype = SLU_TRU.
!  *
!  * work    (workspace/output) void*, size (lwork) (in bytes)
!  *           User supplied workspace, should be large enough
!  *           to hold data structures for factors L and U.
!  *           On exit, if fact is not 'F', L and U point to this array.
!  *
!  * lwork   (input) int
!  *           Specifies the size of work array in bytes.
!  *           = 0:  allocate space internally by system malloc;
!  *           > 0:  use user-supplied work array of length lwork in bytes,
!  *                 returns error if space runs out.
!  *           = -1: the routine guesses the amount of space needed without
!  *                 performing the factorization, and returns it in
!  *                 mem_usage->total_needed; no other side effects.
!  *
!  *           See argument 'mem_usage' for memory usage statistics.
!  *
!  * B           (input/output) SuperMatrix*
!  *           B has types: Stype = SLU_DN, Dtype = SLU_D, Mtype = SLU_GE.
!  *           On entry, the right hand side matrix.
!  *           If B->ncol = 0, only LU decomposition is performed, the triangular
!  *                           solve is skipped.
!  *           On exit,
!  *              if equed = 'N', B is not modified; otherwise
!  *              if A->Stype = SLU_NC:
!  *                 if options->Trans = NOTRANS and equed = 'R' or 'B',
!  *                    B is overwritten by diag(R)*B;
!  *                 if options->Trans = TRANS or CONJ and equed = 'C' of 'B',
!  *                    B is overwritten by diag(C)*B;
!  *              if A->Stype = SLU_NR:
!  *                 if options->Trans = NOTRANS and equed = 'C' or 'B',
!  *                    B is overwritten by diag(C)*B;
!  *                 if options->Trans = TRANS or CONJ and equed = 'R' of 'B',
!  *                    B is overwritten by diag(R)*B.
!  *
!  * X           (output) SuperMatrix*
!  *           X has types: Stype = SLU_DN, Dtype = SLU_D, Mtype = SLU_GE.
!  *           If info = 0 or info = A->ncol+1, X contains the solution matrix
!  *           to the original system of equations. Note that A and B are modified
!  *           on exit if equed is not 'N', and the solution to the equilibrated
!  *           system is inv(diag(C))*X if options->Trans = NOTRANS and
!  *           equed = 'C' or 'B', or inv(diag(R))*X if options->Trans = 'T' or 'C'
!  *           and equed = 'R' or 'B'.
!  *
!  * recip_pivot_growth (output) double*
!  *           The reciprocal pivot growth factor max_j( norm(A_j)/norm(U_j) ).
!  *           The infinity norm is used. If recip_pivot_growth is much less
!  *           than 1, the stability of the LU factorization could be poor.
!  *
!  * rcond   (output) double*
!  *           The estimate of the reciprocal condition number of the matrix A
!  *           after equilibration (if done). If rcond is less than the machine
!  *           precision (in particular, if rcond = 0), the matrix is singular
!  *           to working precision. This condition is indicated by a return
!  *           code of info > 0.
!  *
!  * mem_usage (output) mem_usage_t*
!  *           Record the memory usage statistics, consisting of following fields:
!  *           - for_lu (float)
!  *             The amount of space used in bytes for L\U data structures.
!  *           - total_needed (float)
!  *             The amount of space needed in bytes to perform factorization.
!  *           - expansions (int)
!  *             The number of memory expansions during the LU factorization.
!  *
!  * stat   (output) SuperLUStat_t*
!  *          Record the statistics on runtime and floating-point operation count.
!  *          See slu_util.h for the definition of 'SuperLUStat_t'.
!  *
!  * info    (output) int*
!  *           = 0: successful exit
!  *           < 0: if info = -i, the i-th argument had an illegal value
!  *           > 0: if info = i, and i is
!  *                <= A->ncol: number of zero pivots. They are replaced by small
!  *                      entries due to options->ILU_FillTol.
!  *                = A->ncol+1: U is nonsingular, but RCOND is less than machine
!  *                      precision, meaning that the matrix is singular to
!  *                      working precision. Nevertheless, the solution and
!  *                      error bounds are computed because there are a number
!  *                      of situations where the computed solution can be more
!  *                      accurate than the value of RCOND would suggest.
!  *                > A->ncol+1: number of bytes allocated when memory allocation
!  *                      failure occurred, plus A->ncol.
!  * </pre>
!  */
!
! void
! dgsisx(superlu_options_t *options, SuperMatrix *A, int *perm_c, int *perm_r,
!        int *etree, char *equed, double *R, double *C,
!        SuperMatrix *L, SuperMatrix *U, void *work, int lwork,
!        SuperMatrix *B, SuperMatrix *X,
!        double *recip_pivot_growth, double *rcond,
!        GlobalLU_t *Glu, mem_usage_t *mem_usage, SuperLUStat_t *stat, int *info)

INTERFACE
  SUBROUTINE dgsisx(options, A, perm_c, perm_r, etree, &
    & equed, R, C, L, U, work, lwork, B, X, recip_pivot_growth, &
    & rcond, Glu, mem_usage, stat, info) &
    & BIND(C, name="dgsisx")
    IMPORT :: superlu_options_t, SuperLUStat_t, C_INT, C_PTR, &
      & SuperMatrix, GlobalLU_t, C_CHAR, C_DOUBLE, mem_usage_t
    !
    TYPE(superlu_options_t), INTENT(IN) :: options
    TYPE(SuperMatrix), INTENT(INOUT) :: A
    INTEGER(C_INT), INTENT(INOUT) :: perm_c(*)
    INTEGER(C_INT), INTENT(INOUT) :: perm_r(*)
    INTEGER(C_INT), INTENT(INOUT) :: etree(*)
    CHARACTER(1, kind=C_CHAR), INTENT(IN) :: equed
    REAL(C_DOUBLE), INTENT(INOUT) :: R(*)
    REAL(C_DOUBLE), INTENT(INOUT) :: C(*)
    TYPE(SuperMatrix), INTENT(INOUT) :: L
    TYPE(SuperMatrix), INTENT(INOUT) :: U
    TYPE(C_PTR), INTENT(inout) :: work
    INTEGER(C_INT), VALUE, INTENT(IN) :: lwork
    TYPE(SuperMatrix), INTENT(INOUT) :: B
    TYPE(SuperMatrix), INTENT(INOUT) :: X
    REAL(C_DOUBLE), INTENT(INOUT) :: recip_pivot_growth
    REAL(C_DOUBLE), INTENT(INOUT) :: rcond
    TYPE(GlobalLU_t), INTENT(inout) :: Glu
    TYPE(mem_usage_t), INTENT(INOUT) :: mem_usage
    TYPE(SuperLUStat_t), INTENT(INOUT) :: stat
    INTEGER(C_INT), INTENT(INOUT) :: info
  END SUBROUTINE dgsisx
END INTERFACE

PUBLIC :: dgsisx

END MODULE SuperLU_dgsisx_Methods
