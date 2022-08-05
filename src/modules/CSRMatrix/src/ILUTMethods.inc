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

PUBLIC :: getILUT
PUBLIC :: getILUTP
PUBLIC :: getILUD
PUBLIC :: getILUDP
PUBLIC :: getILUK

!----------------------------------------------------------------------------
!                                                       getILUT@ILUTMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 18 July 2021
! summary: Returns the ILUT precondition
!
!# Introduction
!
! This routine builds the ILUT precondition. Incomplete LU factorization with
! dual truncation mechanism.
!
! - `obj` matrix stored in Compressed Sparse Row format.
! - `lfil` = integer. The fill-in parameter. Each row of L and each row of U
! will have a maximum of lfil elements (excluding the diagonal element). lfil
! must be .ge. 0.
! - `droptol` = real*8. Sets the threshold for dropping small terms in the
! factorization. See below for details on dropping strategy.
!
! - `ALU,JLU`, matrix stored in Modified Sparse Row (MSR) Format containing
! the L and U factors together. The diagonal (stored in ALU(1:n) ) is
! inverted. Each ith row of the ALU,JLU matrix contains the ith row of L
! (excluding the diagonal entry=1) followed by the ith row of U.
! - JU = integer array of length n containing the pointers to the beginning
! of each row of U in the matrix ALU,JLU.
!
! The diagonal elements of the input matrix must be nonzero (at least
! 'structurally'). Dual drop strategy works as follows:
!
! - Theresholding in L and U as set by `droptol`. Any element whose
! MAGNITUDE is less than some tolerance (relative to the abs value of
! diagonal element in U) is dropped.
! - Keeping only the largest `lfil` elements in the ith row of L and the
! largest `lfil` elements in the ith row of `U` (excluding diagonal elements).
! - Flexibility: one  can use  `droptol=0` to get  a strategy  based on
! keeping the largest elements in each row  of `L` and `U`.
! - Taking `droptol .ne. 0` but `lfil=n` will give the usual threshold
! strategy (however, fill-in is then mpredictible).

INTERFACE
MODULE SUBROUTINE csrMat_getILUT1( obj, ALU, JLU, JU, lfil, droptol )
  TYPE( CSRMatrix_ ), INTENT( INOUT ) :: obj
  REAL( DFP ), ALLOCATABLE, INTENT( INOUT ) :: ALU( : )
  INTEGER( I4B ), ALLOCATABLE, INTENT( INOUT ) :: JLU( : )
  INTEGER( I4B ), ALLOCATABLE, INTENT( INOUT ) :: JU( : )
  INTEGER( I4B ), INTENT( IN ) :: lfil
  REAL( DFP ), INTENT( IN ) :: droptol
END SUBROUTINE csrMat_getILUT1
END INTERFACE

INTERFACE getILUT
  MODULE PROCEDURE csrMat_getILUT1
END INTERFACE getILUT


!----------------------------------------------------------------------------
!                                                       getILUT@ILUTMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 18 July 2021
! summary: Returns the ILUT precondition
!
!# Introduction
! This routine builds the ILUT precondition. Incomplete LU factorization with
! dual truncation mechanism.
!
! - `obj` matrix stored in Compressed Sparse Row format.
! - `lfil` = integer. The fill-in parameter. Each row of L and each row of U
! will have a maximum of lfil elements (excluding the diagonal element). lfil
! must be .ge. 0.
! - `droptol` = real*8. Sets the threshold for dropping small terms in the
! factorization. See below for details on dropping strategy.
!
! - `ALU`,`JLU`, matrix stored in Modified Sparse Row (MSR) Format containing
! the L and U factors together. The diagonal (stored in ALU(1:n) ) is
! inverted. Each ith row of the ALU,JLU matrix contains the ith row of L
! (excluding the diagonal entry=1) followed by the ith row of U.
! - `JU` = integer array of length n containing the pointers to the beginning
! of each row of U in the matrix ALU,JLU.
!
! The diagonal elements of the input matrix must be nonzero (at least
! 'structurally'). Dual drop strategy works as follows:
!
! - Theresholding in L and U as set by `droptol`. Any element whose
! MAGNITUDE is less than some tolerance (relative to the abs value of
! diagonal element in U) is dropped.
! - Keeping only the largest `lfil` elements in the ith row of L and the
! largest `lfil` elements in the ith row of `U` (excluding diagonal elements).
! - Flexibility: one  can use  `droptol=0` to get  a strategy  based on
! keeping the largest elements in each row  of `L` and `U`.
! - Taking `droptol .ne. 0` but `lfil=n` will give the usual threshold
! strategy (however, fill-in is then mpredictible).

INTERFACE
MODULE SUBROUTINE csrMat_getILUT2( obj, Pmat, lfil, droptol )
  TYPE( CSRMatrix_ ), INTENT( INOUT ) :: obj
  TYPE( CSRMatrix_ ), INTENT( INOUT ) :: Pmat
  INTEGER( I4B ), INTENT( IN ) :: lfil
  REAL( DFP ), INTENT( IN ) :: droptol
END SUBROUTINE csrMat_getILUT2
END INTERFACE

INTERFACE getILUT
  MODULE PROCEDURE csrMat_getILUT2
END INTERFACE getILUT

!----------------------------------------------------------------------------
!                                                      getILUTP@ILUTMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 18 July 2021
! summary: Returns the ILUT precondition
!
!# Introduction
! This routine builds the ILUTP precondition. ILUT with pivoting, incomplete
! LU factorization with dual truncation mechanism
!
! - `obj` matrix stored in Compressed Sparse Row format.
! - `lfil` = integer. The fill-in parameter. Each row of L and each row of U
! will have a maximum of lfil elements (excluding the diagonal element). lfil
! must be .ge. 0.
! - `droptol` = real*8. Sets the threshold for dropping small terms in the
! factorization. See below for details on dropping strategy.
! - `permtol` = tolerance ratio used to determine whether or not to permute
! two columns.  At step i columns i and j are permuted when
!
! `abs(a(i,j))*permtol .gt. abs(a(i,i))`.
!
! - permtol=0 implies never permute; good values 0.1 to 0.01
!
! - `mbloc` = if desired, permuting can be done only within the diagonal
! blocks of size mbloc. Useful for PDE problems with several degrees of
! freedom.. If feature not wanted take mbloc=n.
!
! `iperm` = contains the permutation arrays. iperm(1:n) = old numbers of
! unknowns iperm(n+1:2*n) = reverse permutation = new unknowns.
!
! TO AVOID PERMUTING THE SOLUTION VECTORS ARRAYS FOR EACH LU-SOLVE, THE
! MATRIX A IS PERMUTED ON RETURN. All column indices are changed. SIMILARLY
! FOR THE U MATRIX. To permute the matrix back to its original state use the
! loop:
!
!```fortran
!  do k=ia(1), ia(n+1)-1
!    ja(k) = iperm(ja(k))
!  enddo
!```
!
! - `ALU,JLU`, matrix stored in Modified Sparse Row (MSR) Format containing
! the L and U factors together. The diagonal (stored in ALU(1:n) ) is
! inverted. Each ith row of the ALU,JLU matrix contains the ith row of L
! (excluding the diagonal entry=1) followed by the ith row of U.
! - JU = integer array of length n containing the pointers to the beginning
! of each row of U in the matrix ALU,JLU.
!
! - Theresholding in L and U as set by `droptol`. Any element whose
! MAGNITUDE is less than some tolerance (relative to the abs value of
! diagonal element in U) is dropped.
! - Keeping only the largest `lfil` elements in the ith row of L and the
! largest `lfil` elements in the ith row of `U` (excluding diagonal elements).
! - Flexibility: one  can use  `droptol=0` to get  a strategy  based on
! keeping the largest elements in each row  of `L` and `U`.
! - Taking `droptol .ne. 0` but `lfil=n` will give the usual threshold
! strategy (however, fill-in is then mpredictible).

INTERFACE
MODULE SUBROUTINE csrMat_getILUTP1( obj, ALU, JLU, JU, lfil, droptol, &
  & permtol, mbloc, IPERM )
  TYPE( CSRMatrix_ ), INTENT( INOUT ) :: obj
  REAL( DFP ), ALLOCATABLE, INTENT( INOUT ) :: ALU( : )
  INTEGER( I4B ), ALLOCATABLE, INTENT( INOUT ) :: JLU( : )
  INTEGER( I4B ), ALLOCATABLE, INTENT( INOUT ) :: JU( : )
  INTEGER( I4B ), INTENT( IN ) :: lfil
  REAL( DFP ), INTENT( IN ) :: droptol
  REAL( DFP ), INTENT( IN ) :: permtol
  INTEGER( I4B ), INTENT( IN ) :: mbloc
  INTEGER( I4B ), ALLOCATABLE, INTENT( INOUT ) :: IPERM( : )
END SUBROUTINE csrMat_getILUTP1
END INTERFACE

INTERFACE getILUTP
  MODULE PROCEDURE csrMat_getILUTP1
END INTERFACE getILUTP

!----------------------------------------------------------------------------
!                                                      getILUTP@ILUTMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 18 July 2021
! summary: Returns the ILUT precondition
!
!# Introduction
! This routine builds the ILUTP precondition. ILUT with pivoting, incomplete
! LU factorization with dual truncation mechanism
!
! - `obj` matrix stored in Compressed Sparse Row format.
! - `lfil` = integer. The fill-in parameter. Each row of L and each row of U
! will have a maximum of lfil elements (excluding the diagonal element). lfil
! must be .ge. 0.
! - `droptol` = real*8. Sets the threshold for dropping small terms in the
! factorization. See below for details on dropping strategy.
! - `permtol` = tolerance ratio used to determine whether or not to permute
! two columns.  At step i columns i and j are permuted when
!
! `abs(a(i,j))*permtol .gt. abs(a(i,i))`.
!
! - permtol=0 implies never permute; good values 0.1 to 0.01
!
! - `mbloc` = if desired, permuting can be done only within the diagonal
! blocks of size mbloc. Useful for PDE problems with several degrees of
! freedom.. If feature not wanted take mbloc=n.
!
! `iperm` = contains the permutation arrays. iperm(1:n) = old numbers of
! unknowns iperm(n+1:2*n) = reverse permutation = new unknowns.
!
! TO AVOID PERMUTING THE SOLUTION VECTORS ARRAYS FOR EACH LU-SOLVE, THE
! MATRIX A IS PERMUTED ON RETURN. All column indices are changed. SIMILARLY
! FOR THE U MATRIX. To permute the matrix back to its original state use the
! loop:
!
!```fortran
!  do k=ia(1), ia(n+1)-1
!     ja(k) = iperm(ja(k))
!  enddo
!```
!
! - `ALU,JLU`, matrix stored in Modified Sparse Row (MSR) Format containing
! the L and U factors together. The diagonal (stored in ALU(1:n) ) is
! inverted. Each ith row of the ALU,JLU matrix contains the ith row of L
! (excluding the diagonal entry=1) followed by the ith row of U.
! - JU = integer array of length n containing the pointers to the beginning
! of each row of U in the matrix ALU,JLU.
!
! - Theresholding in L and U as set by `droptol`. Any element whose
! MAGNITUDE is less than some tolerance (relative to the abs value of
! diagonal element in U) is dropped.
! - Keeping only the largest `lfil` elements in the ith row of L and the
! largest `lfil` elements in the ith row of `U` (excluding diagonal elements).
! - Flexibility: one  can use  `droptol=0` to get  a strategy  based on
! keeping the largest elements in each row  of `L` and `U`.
! - Taking `droptol .ne. 0` but `lfil=n` will give the usual threshold
! strategy (however, fill-in is then mpredictible).

INTERFACE
MODULE SUBROUTINE csrMat_getILUTP2( obj, Pmat, lfil, droptol, permtol, &
  & mbloc, IPERM )
  TYPE( CSRMatrix_ ), INTENT( INOUT ) :: obj
  TYPE( CSRMatrix_ ), INTENT( INOUT ) :: Pmat
  INTEGER( I4B ), INTENT( IN ) :: lfil
  REAL( DFP ), INTENT( IN ) :: droptol
  REAL( DFP ), INTENT( IN ) :: permtol
  INTEGER( I4B ), INTENT( IN ) :: mbloc
  INTEGER( I4B ), ALLOCATABLE, INTENT( INOUT ) :: IPERM( : )
END SUBROUTINE csrMat_getILUTP2
END INTERFACE

INTERFACE getILUTP
  MODULE PROCEDURE csrMat_getILUTP2
END INTERFACE getILUTP

!----------------------------------------------------------------------------
!                                                       getILUTD@ILUTMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 18 July 2021
! summary: Returns the ILUT precondition
!
!# Introduction
! This routine computes the ILU factorization with standard threshold
! dropping: at ith step of elimination, an element a(i,j) in row i is dropped
! if it satisfies the criterion:
!
! - abs(a(i,j)) < tol, that is, average magnitude of elements in row i of A
! - There is no control on memory size required for the factors as is done in
! ILUT.
! - This routines computes also various diagonal compensation ILU's such
! MILU. These are defined through the parameter `alph`
!
! - alph = diagonal compensation parameter, alph*(sum of all dropped out
! elements in a given row) is added to the diagonal element of U of the
! factorization
!   - alph = 0 means the scheme is ILU with threshold,
!   - alph = 1 means the scheme is MILU with threshold.
! - droptol = Threshold parameter for dropping small terms in the
! factorization. During the elimination, a term a(i,j) is dropped whenever abs
! (a(i,j)) .lt. tol * [weighted norm of row i]. Here weighted norm = 1-norm /
! number of nnz elements in the row.
! - `obj` matrix stored in Compressed Sparse Row format.
!
! - `ALU,JLU`, matrix stored in Modified Sparse Row (MSR) Format containing
! the L and U factors together. The diagonal (stored in ALU(1:n) ) is
! inverted. Each ith row of the ALU,JLU matrix contains the ith row of L
! (excluding the diagonal entry=1) followed by the ith row of U.
! - JU = integer array of length n containing the pointers to the beginning
! of each row of U in the matrix ALU,JLU.
!
! - Theresholding in L and U as set by `droptol`. Any element whose
! MAGNITUDE is less than some tolerance (relative to the abs value of
! diagonal element in U) is dropped.

INTERFACE
MODULE SUBROUTINE csrMat_getILUD1( obj, ALU, JLU, JU, alpha, droptol )
  TYPE( CSRMatrix_ ), INTENT( INOUT ) :: obj
  REAL( DFP ), ALLOCATABLE, INTENT( INOUT ) :: ALU( : )
  INTEGER( I4B ), ALLOCATABLE, INTENT( INOUT ) :: JLU( : )
  INTEGER( I4B ), ALLOCATABLE, INTENT( INOUT ) :: JU( : )
  REAL( DFP ), INTENT( IN ) :: alpha
  REAL( DFP ), INTENT( IN ) :: droptol
END SUBROUTINE csrMat_getILUD1
END INTERFACE

INTERFACE getILUD
  MODULE PROCEDURE csrMat_getILUD1
END INTERFACE getILUD

!----------------------------------------------------------------------------
!                                                          ILUD@ILUTMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 18 July 2021
! summary: Returns the ILUT precondition
!
!# Introduction
!
! This routine computes the ILU factorization with standard threshold
! dropping: at ith step of elimination, an element a(i,j) in row i is dropped
! if it satisfies the criterion:
!
! - abs(a(i,j)) < tol, that is, average magnitude of elements in row i of A
! - There is no control on memory size required for the factors as is done in
! ILUT.
! - This routines computes also various diagonal compensation ILU's such
! MILU. These are defined through the parameter `alph`
!
! - alph = diagonal compensation parameter, alph*(sum of all dropped out
! elements in a given row) is added to the diagonal element of U of the
! factorization
!   - alph = 0 means the scheme is ILU with threshold,
!   - alph = 1 means the scheme is MILU with threshold.
! - droptol = Threshold parameter for dropping small terms in the
! factorization. During the elimination, a term a(i,j) is dropped whenever abs
! (a(i,j)) .lt. tol * [weighted norm of row i]. Here weighted norm = 1-norm /
! number of nnz elements in the row.
! - `obj` matrix stored in Compressed Sparse Row format.
!
! - `ALU,JLU`, matrix stored in Modified Sparse Row (MSR) Format containing
! the L and U factors together. The diagonal (stored in ALU(1:n) ) is
! inverted. Each ith row of the ALU,JLU matrix contains the ith row of L
! (excluding the diagonal entry=1) followed by the ith row of U.
! - JU = integer array of length n containing the pointers to the beginning
! of each row of U in the matrix ALU,JLU.
!
! - Theresholding in L and U as set by `droptol`. Any element whose
! MAGNITUDE is less than some tolerance (relative to the abs value of
! diagonal element in U) is dropped.

INTERFACE
MODULE SUBROUTINE csrMat_getILUD2( obj, Pmat, alpha, droptol )
  TYPE( CSRMatrix_ ), INTENT( INOUT ) :: obj
  TYPE( CSRMatrix_ ), INTENT( INOUT ) :: Pmat
  REAL( DFP ), INTENT( IN ) :: alpha
  REAL( DFP ), INTENT( IN ) :: droptol
END SUBROUTINE csrMat_getILUD2
END INTERFACE

INTERFACE getILUD
  MODULE PROCEDURE csrMat_getILUD2
END INTERFACE getILUD

!----------------------------------------------------------------------------
!                                                       getILUDP@ILUTMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 18 July 2021
! summary: Returns the ILUDP precondition
!
!# Introduction
!
! This routine computes ILUDP preconditioner, incomplete LU factorization
! with standard droppoing strategy.
!
! - `droptol` = tolerance used for dropping elements in L and U. elements are
! dropped if they are .lt. norm(row) x droptol row = row being eliminated
! - `permtol` = tolerance ratio used for determning whether to permute two
! columns.  Two columns are permuted only when abs(a(i,j))*permtol .gt. abs(a
! (i,i)) [0 --> never permute; good values 0.1 to 0.01]
! - `mbloc` = if desired, permuting can be done only within the diagonal
! blocks of size mbloc. Useful for PDE problems with several degrees of
! freedom.. If feature not wanted take mbloc=n.
!
! - iperm   = contains the permutation arrays, iperm(1:n) = old numbers of
! unknowns, iperm(n+1:2*n) = reverse permutation = new unknowns.
!
! - abs(a(i,j)) < droptol, that is, average magnitude of elements in row i
! of A
! - alph = diagonal compensation parameter, alph*(sum of all dropped out
! elements in a given row) is added to the diagonal element of U of the
! factorization
!   - alph = 0 means the scheme is ILU with threshold,
!   - alph = 1 means the scheme is MILU with threshold.
! - droptol = Threshold parameter for dropping small terms in the
! factorization. During the elimination, a term a(i,j) is dropped whenever abs
! (a(i,j)) .lt. droptol * [weighted norm of row i]. Here weighted norm =
! 1-norm / number of nnz elements in the row.
! - `obj` matrix stored in Compressed Sparse Row format.
!
! - `ALU,JLU`, matrix stored in Modified Sparse Row (MSR) Format containing
! the L and U factors together. The diagonal (stored in ALU(1:n) ) is
! inverted. Each ith row of the ALU,JLU matrix contains the ith row of L
! (excluding the diagonal entry=1) followed by the ith row of U.
! - JU = integer array of length n containing the pointers to the beginning
! of each row of U in the matrix ALU,JLU.
!
! - Theresholding in L and U as set by `droptol`. Any element whose
! MAGNITUDE is less than some tolerance (relative to the abs value of
! diagonal element in U) is dropped.

INTERFACE
MODULE SUBROUTINE csrMat_getILUDP1( obj, ALU, JLU, JU, alpha, droptol, &
  & permtol, mbloc, IPERM )
  TYPE( CSRMatrix_ ), INTENT( INOUT ) :: obj
  REAL( DFP ), ALLOCATABLE, INTENT( INOUT ) :: ALU( : )
  INTEGER( I4B ), ALLOCATABLE, INTENT( INOUT ) :: JLU( : )
  INTEGER( I4B ), ALLOCATABLE, INTENT( INOUT ) :: JU( : )
  REAL( DFP ), INTENT( IN ) :: alpha
  REAL( DFP ), INTENT( IN ) :: droptol
  REAL( DFP ), INTENT( IN ) :: permtol
  INTEGER( I4B ), INTENT( IN ) :: mbloc
  INTEGER( I4B ), ALLOCATABLE, INTENT( INOUT ) :: IPERM( : )
END SUBROUTINE csrMat_getILUDP1
END INTERFACE

INTERFACE getILUDP
  MODULE PROCEDURE csrMat_getILUDP1
END INTERFACE getILUDP

!----------------------------------------------------------------------------
!                                                     getILUTDP@ILUTMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 18 July 2021
! summary: Returns the ILUDP precondition
!
!# Introduction
!
! This routine computes ILUDP preconditioner, incomplete LU factorization
! with standard droppoing strategy.
!
! - `droptol` = tolerance used for dropping elements in L and U. elements are
! dropped if they are .lt. norm(row) x droptol row = row being eliminated
! - `permtol` = tolerance ratio used for determning whether to permute two
! columns.  Two columns are permuted only when abs(a(i,j))*permtol .gt. abs(a
! (i,i)) [0 --> never permute; good values 0.1 to 0.01]
! - `mbloc` = if desired, permuting can be done only within the diagonal
! blocks of size mbloc. Useful for PDE problems with several degrees of
! freedom.. If feature not wanted take mbloc=n.
!
! - iperm   = contains the permutation arrays, iperm(1:n) = old numbers of
! unknowns, iperm(n+1:2*n) = reverse permutation = new unknowns.
!
! - abs(a(i,j)) < droptol, that is, average magnitude of elements in row i of
! A
! - alph = diagonal compensation parameter, alph*(sum of all dropped out
! elements in a given row) is added to the diagonal element of U of the
! factorization
!   - alph = 0 means the scheme is ILU with threshold,
!   - alph = 1 means the scheme is MILU with threshold.
! - droptol = Threshold parameter for dropping small terms in the
! factorization. During the elimination, a term a(i,j) is dropped whenever abs
! (a(i,j)) .lt. droptol * [weighted norm of row i]. Here weighted norm =
! 1-norm / number of nnz elements in the row.
! - `obj` matrix stored in Compressed Sparse Row format.
!
! - `ALU,JLU`, matrix stored in Modified Sparse Row (MSR) Format containing
! the L and U factors together. The diagonal (stored in ALU(1:n) ) is
! inverted. Each ith row of the ALU,JLU matrix contains the ith row of L
! (excluding the diagonal entry=1) followed by the ith row of U.
! - JU = integer array of length n containing the pointers to the beginning
! of each row of U in the matrix ALU,JLU.
!
! - Theresholding in L and U as set by `droptol`. Any element whose
! MAGNITUDE is less than some tolerance (relative to the abs value of
! diagonal element in U) is dropped.

INTERFACE
MODULE SUBROUTINE csrMat_getILUDP2( obj, Pmat, alpha, droptol, &
  & permtol, mbloc, IPERM )
  TYPE( CSRMatrix_ ), INTENT( INOUT ) :: obj
  TYPE( CSRMatrix_ ), INTENT( INOUT ) :: Pmat
  REAL( DFP ), INTENT( IN ) :: alpha
  REAL( DFP ), INTENT( IN ) :: droptol
  REAL( DFP ), INTENT( IN ) :: permtol
  INTEGER( I4B ), INTENT( IN ) :: mbloc
  INTEGER( I4B ), ALLOCATABLE, INTENT( INOUT ) :: IPERM( : )
END SUBROUTINE csrMat_getILUDP2
END INTERFACE

INTERFACE getILUDP
  MODULE PROCEDURE csrMat_getILUDP2
END INTERFACE getILUDP

!----------------------------------------------------------------------------
!                                                       getILUK@ILUTMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 18 July 2021
! summary: Returns the ILUK precondition

INTERFACE
MODULE SUBROUTINE csrMat_getILUK1( obj, ALU, JLU, JU, lfil, LEVS )
  TYPE( CSRMatrix_ ), INTENT( INOUT ) :: obj
  REAL( DFP ), ALLOCATABLE, INTENT( INOUT ) :: ALU( : )
  INTEGER( I4B ), ALLOCATABLE, INTENT( INOUT ) :: JLU( : )
  INTEGER( I4B ), ALLOCATABLE, INTENT( INOUT ) :: JU( : )
  INTEGER( I4B ), INTENT( IN ) :: lfil
  INTEGER( I4B ), ALLOCATABLE, INTENT( INOUT ) :: LEVS( : )
END SUBROUTINE csrMat_getILUK1
END INTERFACE

INTERFACE getILUK
  MODULE PROCEDURE csrMat_getILUK1
END INTERFACE getILUK

!----------------------------------------------------------------------------
!                                                       getILUK@ILUTMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 18 July 2021
! summary: Returns the ILUK precondition
!
!# Introduction
!
! This routine returns the ILU WITH LEVEL OF FILL-IN OF K (ILU(k))
!
! - `lfil` = integer. The fill-in parameter. Each element whose leve-of-fill
! exceeds lfil during the ILU process is dropped. lfil must be .ge. 0
! - droptol = real*8. Sets the threshold for dropping small terms in the
! factorization. See below for details on dropping strategy.
! - `ALU,JLU` = matrix stored in Modified Sparse Row (MSR) format containing
! the L and U factors together. The diagonal (stored in alu(1:n) ) is
! inverted. Each i-th row of the `ALU,JLU` matrix contains the i-th row of L
! (excluding the diagonal entry=1) followed by the i-th row of `U`.
! - `JU` = integer array of length n containing the pointers to the beginning
! of each row of `U` in the matrix `ALU,JLU`.
! - `LEVS` = integer (work) array of size `IWK`, which contains the levels of
! each element in `ALU, JLU`.
!
!@note
! This is not implemented efficiently storage-wise. For example: Only the
! part of the array levs(*) associated with the U-matrix is needed in the
! routine.. So some storage can be saved if needed. The levels of fills in
! the LU matrix are output for information only -- they are not needed by
! LU-solve.
!@endnote

INTERFACE
MODULE SUBROUTINE csrMat_getILUK2( obj, Pmat, lfil, LEVS )
  TYPE( CSRMatrix_ ), INTENT( INOUT ) :: obj
  TYPE( CSRMatrix_ ), INTENT( INOUT ) :: Pmat
  INTEGER( I4B ), INTENT( IN ) :: lfil
  INTEGER( I4B ), ALLOCATABLE, INTENT( INOUT ) :: LEVS( : )
END SUBROUTINE csrMat_getILUK2
END INTERFACE

INTERFACE getILUK
  MODULE PROCEDURE csrMat_getILUK2
END INTERFACE getILUK
