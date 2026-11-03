! This program is a part of EASIFEM librarycsrsparsity
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

!> author: Vikas Sharma, Ph. D.
! date:         22 March 2021
! summary:         This module contains methods for [[CSRSparsity_]]

MODULE CSRSparsity_Method
USE Basetype
USE GlobalData
IMPLICIT NONE
PRIVATE

PUBLIC :: GetSym
PUBLIC :: Initiate
PUBLIC :: ASSIGNMENT(=)
PUBLIC :: SetSparsity
PUBLIC :: CSRSparsity
PUBLIC :: CSRSparsityPointer
PUBLIC :: DEALLOCATE
PUBLIC :: GetDiagonal
PUBLIC :: Display
PUBLIC :: Shape
PUBLIC :: Size
PUBLIC :: GetNNZ
PUBLIC :: GetColIndex
PUBLIC :: GetColNumber
PUBLIC :: OPERATOR(.startColumn.)
PUBLIC :: OPERATOR(.endColumn.)
PUBLIC :: SetIA
PUBLIC :: SetJA
PUBLIC :: GetIA
PUBLIC :: GetJA

!----------------------------------------------------------------------------
!                                               Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         22 March 2021
! summary: This subroutine constructs the CSR sparsity object
!
!# Introduction
!
! This subroutine initiate the instance of [[CSRSparsity_]] object
!
! - ncol is the number of columns
! - nrow is the number of rows
! - dof is the degrees of freedom object, if it is present then it used
! to initiate [[DOF_:dof]].
!
!@note
! If dof object is not present, then this routine initiates
! [[CSRSparsity_:dof]] internally with following options.
!
! - tNodes = [nrow]
! - names= ["K"]
! - spacecompo= [1]
! - timecompo = [1]
! - storageFMT = FMT_NODES
!@endnote
!

INTERFACE Initiate
  MODULE SUBROUTINE obj_initiate1(obj, ncol, nrow, idof, jdof, nnz)
    TYPE(CSRSparsity_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: ncol, nrow
    TYPE(DOF_), OPTIONAL, INTENT(IN) :: idof
    !! DOF for row
    TYPE(DOF_), OPTIONAL, INTENT(IN) :: jdof
    !! DOF for column
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: nnz
    !! number of nonzeros
  END SUBROUTINE obj_initiate1
END INTERFACE Initiate

!----------------------------------------------------------------------------
!                                               Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         22 March 2021
! summary: This subroutine construct `CSRSparsity_` object from copying
!
!# Introduction
!
! This subroutine copies `obj2` into `obj`, and initiates the latter one.
! This routine is used to define the assignment operator.

INTERFACE Initiate
  MODULE SUBROUTINE obj_initiate2(obj, obj2)
    TYPE(CSRSparsity_), INTENT(INOUT) :: obj
    TYPE(CSRSparsity_), INTENT(IN) :: obj2
  END SUBROUTINE obj_initiate2
END INTERFACE Initiate

INTERFACE ASSIGNMENT(=)
  MODULE PROCEDURE obj_initiate2
END INTERFACE ASSIGNMENT(=)

!----------------------------------------------------------------------------
!                                                 Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         22 March 2021
! summary: This subroutine constructs `CSRSparsity_` object from IA, JA
!
!# Introduction
!
! - This routine constructs [[CSRSparsity_]] instance by using the
! indices `IA` and `JA`
! - This routine is helpful in reading data from files.
! - This routine calls [[CSRSparsity_Method:obj_initiate1]] method
! without `dof` argument. So this type of initiation does not contain
! useful information about the degree of freedoms.
!

INTERFACE Initiate
  MODULE SUBROUTINE obj_initiate3(obj, IA, JA, ncol)
    TYPE(CSRSparsity_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: IA(:), JA(:)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: ncol
    !! number of columns, default is number of rows
  END SUBROUTINE obj_initiate3
END INTERFACE Initiate

!----------------------------------------------------------------------------
!                                            CSRSparsity@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 10 Oct, 2021
! summary: Returns an instance of [[CSRSparsity_]]
!
!# Introduction
!
! This function returns an instance of [[CSRSparsity_]]

INTERFACE CSRSparsity
  MODULE FUNCTION obj_constructor1(nrow, ncol, idof, jdof) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: nrow
    INTEGER(I4B), INTENT(IN) :: ncol
    TYPE(DOF_), OPTIONAL, INTENT(IN) :: idof
    !! dof for row
    TYPE(DOF_), OPTIONAL, INTENT(IN) :: jdof
    !! dof for column
    TYPE(CSRSparsity_) :: ans
  END FUNCTION obj_constructor1
END INTERFACE CSRSparsity

!----------------------------------------------------------------------------
!                                            CSRSparsity@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 10 Oct, 2021
! summary: Returns an instance of [[CSRSparsity_]]
!
!# Introduction
!
! This function returns an instance of [[CSRSparsity_]]

INTERFACE CSRSparsity
  MODULE FUNCTION obj_constructor2(IA, JA) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: IA(:)
    INTEGER(I4B), INTENT(IN) :: JA(:)
    TYPE(CSRSparsity_) :: ans
  END FUNCTION obj_constructor2
END INTERFACE CSRSparsity

!----------------------------------------------------------------------------
!                                       CSRSparsityPointer@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 10 Oct, 2021
! summary: Returns an instance of [[CSRSparsity_]]
!
!# Introduction
!
! This function returns an instance of [[CSRSparsity_]]

INTERFACE CSRSparsityPointer
  MODULE FUNCTION obj_constructor_1(nrow, ncol, idof, jdof) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: nrow
    INTEGER(I4B), INTENT(IN) :: ncol
    TYPE(DOF_), OPTIONAL, INTENT(IN) :: idof
    !! dof for row
    TYPE(DOF_), OPTIONAL, INTENT(IN) :: jdof
    !! dof for column
    TYPE(CSRSparsity_), POINTER :: ans
  END FUNCTION obj_constructor_1
END INTERFACE CSRSparsityPointer

!----------------------------------------------------------------------------
!                                      CSRSparsityPointer@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 10 Oct, 2021
! summary: Returns an instance of [[CSRSparsity_]]
!
!# Introduction
!
! This function returns an instance of [[CSRSparsity_]]

INTERFACE CSRSparsityPointer
  MODULE FUNCTION obj_constructor_2(IA, JA) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: IA(:)
    INTEGER(I4B), INTENT(IN) :: JA(:)
    TYPE(CSRSparsity_), POINTER :: ans
  END FUNCTION obj_constructor_2
END INTERFACE CSRSparsityPointer

!----------------------------------------------------------------------------
!                                             Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         22 March 2021
! summary:          This subroutine deallocates the data

INTERFACE DEALLOCATE
  MODULE PURE SUBROUTINE obj_Deallocate(obj)
    TYPE(CSRSparsity_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_Deallocate
END INTERFACE DEALLOCATE

!----------------------------------------------------------------------------
!                                                         Display@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 22 March 2021
! summary: This subroutine display the content of sparsity

INTERFACE Display
  MODULE SUBROUTINE obj_Display(obj, Msg, UnitNo)
    TYPE(CSRSparsity_), INTENT(IN) :: obj
    CHARACTER(*), INTENT(IN) :: Msg
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: UnitNo
  END SUBROUTINE obj_Display
END INTERFACE Display

!----------------------------------------------------------------------------
!                                                          Shape@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         22 March 2021
! summary:         This function returns the shape of the sparse matrix
!
!# Introduction
!
! This function returns the shape of sparse matrix

INTERFACE Shape
  MODULE PURE FUNCTION obj_shape(obj) RESULT(ans)
    TYPE(CSRSparsity_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans(2)
  END FUNCTION obj_shape
END INTERFACE Shape

!----------------------------------------------------------------------------
!                                                           Size@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         22 March 2021
! summary: This function returns the size of sparse matrix
!
!# Introduction
!
! This function returns the size of sparse matrix
! If Dims equal to 1 then total number of rows are returned
! If Dims is equal to 2 then total number of columns are return
! If Dims is absent then nrow*ncol are returned

INTERFACE Size
  MODULE PURE FUNCTION obj_size(obj, Dims) RESULT(ans)
    TYPE(CSRSparsity_), INTENT(IN) :: obj
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: Dims
    INTEGER(I4B) :: ans
  END FUNCTION obj_size
END INTERFACE Size

!----------------------------------------------------------------------------
!                                                          GetNNZ@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         22 March 2021
! summary:         Return the total number of non zero entry

INTERFACE GetNNZ
  MODULE PURE FUNCTION obj_GetNNZ(obj) RESULT(ans)
    TYPE(CSRSparsity_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION obj_GetNNZ
END INTERFACE GetNNZ

!----------------------------------------------------------------------------
!                                                          GetNNZ@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         22 March 2021
! summary:         Return the total number of non zero entry

INTERFACE GetNNZ
  MODULE PURE FUNCTION obj_GetNNZ_from_operation(obj1, obj2, op, isSorted)  &
    & RESULT(ans)
    TYPE(CSRSparsity_), INTENT(IN) :: obj1
    !! CSRSparsity object
    TYPE(CSRSparsity_), INTENT(IN) :: obj2
    !! CSRSparsity object
    CHARACTER(1), INTENT(IN) :: op
    !! "*", "+", "-"
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isSorted
    !! Set it to true if the columns are sorted in obj1 and obj2
    !! Default is .false.
    INTEGER(I4B) :: ans
    !! total number of non zero entries
  END FUNCTION obj_GetNNZ_from_operation
END INTERFACE GetNNZ

!----------------------------------------------------------------------------
!                                                           Initiate@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-12-16
! summary:  Initiate an object by adding two csrmatrix

INTERFACE
  MODULE PURE FUNCTION GetNNZ_Add_Subtract(nrow, ncol, ja, ia, jb, ib)  &
    & RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: nrow, ncol
    !! number of rows in a and b matrix
    INTEGER(I4B), INTENT(IN) :: ja(:)
    !! sparsity of ja
    INTEGER(I4B), INTENT(IN) :: ia(:)
    !! nrow + 1
    INTEGER(I4B), INTENT(IN) :: jb(:)
    !! sparsity of jb
    INTEGER(I4B), INTENT(IN) :: ib(:)
    !! nrow + 1
    INTEGER(I4B) :: ans
  END FUNCTION GetNNZ_Add_Subtract
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Initiate@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-12-16
! summary:  Initiate an object by adding two csrmatrix

INTERFACE
  MODULE PURE FUNCTION GetNNZ_Add_Subtract_sorted(nrow, ncol, ja, ia, jb,  &
    & ib) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: nrow, ncol
    !! number of rows in a and b matrix
    INTEGER(I4B), INTENT(IN) :: ja(:)
    !! sparsity of ja
    INTEGER(I4B), INTENT(IN) :: ia(:)
    !! nrow + 1
    INTEGER(I4B), INTENT(IN) :: jb(:)
    !! sparsity of jb
    INTEGER(I4B), INTENT(IN) :: ib(:)
    !! nrow + 1
    INTEGER(I4B) :: ans
  END FUNCTION GetNNZ_Add_Subtract_sorted
END INTERFACE

!----------------------------------------------------------------------------
!                                                          GetNNZ@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-01-28
! summary:         Return the total number of non zero entry

INTERFACE GetNNZ
  MODULE PURE FUNCTION obj_GetNNZ1(obj, from) RESULT(ans)
    TYPE(CSRSparsity_), INTENT(IN) :: obj
    CHARACTER(1), INTENT(IN) :: from
    !! "U" nnz in upper triangular part, j > i
    !! "L" nnz in lower triangular part, i > j
    !! "D" nnz in diagonal part, i=j
    !! "A" nnz in whole matrix, L+U+D
    INTEGER(I4B) :: ans
  END FUNCTION obj_GetNNZ1
END INTERFACE GetNNZ

!----------------------------------------------------------------------------
!                                                          GetNNZ@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-01-28
! summary: Return the total number of non zero in U, L and D

INTERFACE GetNNZ
  MODULE PURE FUNCTION obj_GetNNZ2(obj, from) RESULT(ans)
    TYPE(CSRSparsity_), INTENT(IN) :: obj
    CHARACTER(1), INTENT(IN) :: from(1)
    !! this argument is not referred, it is here
    !! to create a unique interface only
    INTEGER(I4B) :: ans(3)
    !! [nnzU, nnzL, nnzD]
  END FUNCTION obj_GetNNZ2
END INTERFACE GetNNZ

!----------------------------------------------------------------------------
!                                                      GetDiagonal@GeMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 15 July 2021
! summary: Returns the diagonal of sparse matrix
!
!# Introduction
!
! This subroutine returns the diagonal entries of sparse matrix. This
! Routine calls the Saad's sparse library.
!
! `offSet`: containing the `offset` of the wanted diagonal. The diagonal
! extracted is the one corresponding to the entries `a(i,j)` with `j-i =
! offSet`. Therefore, `offset = 0` means the main diagonal
!
! `diag` : real array of length `nrow` containing the wanted diagonal. `diag`
! contains the diagonal (`a(i,j),j-i = offSet`) as defined above.
!
! `idiag` = integer array. It contains the poisitions of diagonal in the
! original arrays `A`. If `idiag(i)=0` then it means that there was no
! diagonal found in row=i.

INTERFACE GetDiagonal
  MODULE SUBROUTINE obj_GetDiagonal1(obj, A, diag, idiag, offSet)
    TYPE(CSRSparsity_), INTENT(IN) :: obj
    REAL(DFP), INTENT(IN) :: A(:)
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: diag(:)
    !! Diagonal entries
    INTEGER(I4B), ALLOCATABLE, INTENT(INOUT) :: idiag(:)
    !! Position of diagonal entries in `A(:)`
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: offSet
    !! offSet of the wanted diagonal
  END SUBROUTINE obj_GetDiagonal1
END INTERFACE GetDiagonal

!----------------------------------------------------------------------------
!                                                    GetDiagonal@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 15 July 2021
! summary: Returns the diagonal of sparse matrix
!
!# Introduction
!
! This subroutine returns the diagonal entries of sparse matrix.
!
! This routine is similar to [[CSRSparsity_Method:obj_GetDiagonal1]].
! However, this routine does not return the position of diagonal in `A`

INTERFACE GetDiagonal
  MODULE SUBROUTINE obj_GetDiagonal2(obj, A, diag, offSet)
    TYPE(CSRSparsity_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: A(:)
    !! Sparse matrix values
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: diag(:)
    !! Diagonal entries
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: offSet
    !! offSet of diagonal
  END SUBROUTINE obj_GetDiagonal2
END INTERFACE GetDiagonal

!----------------------------------------------------------------------------
!                                                   GetColNumber@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-12-14
! summary:  Get the column number from JA.

INTERFACE GetColNumber
  MODULE PURE FUNCTION obj_GetColNumber1(obj, indx) RESULT(ans)
    TYPE(CSRSparsity_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: indx
    INTEGER(I4B) :: ans
  END FUNCTION obj_GetColNumber1
END INTERFACE GetColNumber

!----------------------------------------------------------------------------
!                                                     GetColIndex@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-12-14
! summary:  Get the starting  and ending column index of irow

INTERFACE GetColIndex
  MODULE PURE FUNCTION obj_GetColIndex1(obj, irow) RESULT(ans)
    TYPE(CSRSparsity_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: irow
    INTEGER(I4B) :: ans(2)
  END FUNCTION obj_GetColIndex1
END INTERFACE GetColIndex

!----------------------------------------------------------------------------
!                                                     startColumn@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-12-14
! summary:  Get the starting column index of irow

INTERFACE OPERATOR(.startColumn.)
  MODULE PURE FUNCTION obj_startColumn1(obj, irow) RESULT(ans)
    TYPE(CSRSparsity_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: irow
    INTEGER(I4B) :: ans
  END FUNCTION obj_startColumn1
END INTERFACE OPERATOR(.startColumn.)

!----------------------------------------------------------------------------
!                                                        endColumn@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-12-14
! summary:  Get the ending column index of irow

INTERFACE OPERATOR(.endColumn.)
  MODULE PURE FUNCTION obj_endColumn1(obj, irow) RESULT(ans)
    TYPE(CSRSparsity_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: irow
    INTEGER(I4B) :: ans
  END FUNCTION obj_endColumn1
END INTERFACE OPERATOR(.endColumn.)

!----------------------------------------------------------------------------
!                                                     SetSparsity@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 22 March 2021
! summary: Add the column nodes of one row node to the sparsity pattern
!
! This routine adds the coupling between the row node `row` and the
! column nodes `col` to the sparsity pattern that is being built.
!
! The node numbers are first converted to matrix indices. Every degree of
! freedom of `row` in `obj%idof` gives one matrix row, and every degree of
! freedom of each entry of `col` in `obj%jdof` gives one matrix column.
! Both conversions follow the storage format (`FMT_NODES` or `FMT_DOF`)
! of the DOF objects. The list of column indices is then appended to
! `obj%row(i)` for each of these matrix rows.
!
! The appended indices are neither sorted nor made unique, and repeated
! calls for the same row only append more entries. `obj%nnz` grows by
! the number of appended entries, so it is an upper bound at this
! stage. Call `SetSparsity(obj)` after the last call. It sorts each row,
! removes the duplicates, and builds `obj%IA`, `obj%JA` and the exact
! `obj%nnz`.
!
! If `col` is empty, only the row storage is allocated. In debug builds
! the routine stops when `obj` is not initiated or the sparsity pattern
! is locked.
!
! Changes:
!   - 2026-11-03: Row and column indices are no longer sorted here, since
!     SetSparsity(obj) sorts each row anyway.
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: CSRSparsity_
! USE BaseType, ONLY: DOF_
! USE CSRSparsity_Method, ONLY: Initiate
! USE CSRSparsity_Method, ONLY: SetSparsity
! USE CSRSparsity_Method, ONLY: DEALLOCATE
! USE DOF_Method, ONLY: Initiate
! USE DOF_Method, ONLY: DEALLOCATE
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: FMT_NODES
! USE GlobalData, ONLY: FMT_DOF
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! INTEGER(I4B) :: empty(0)
!
! ! Test 1: scalar DOF, unsorted and repeated col, one empty row
! BLOCK
!   TYPE(CSRSparsity_) :: csr
!   CALL Initiate(obj=csr, ncol=4_I4B, nrow=4_I4B)
!   CALL SetSparsity(obj=csr, row=1_I4B, col=[3_I4B, 1_I4B, 1_I4B])
!   CALL SetSparsity(obj=csr, row=2_I4B, col=[2_I4B, 4_I4B])
!   CALL SetSparsity(obj=csr, row=1_I4B, col=[2_I4B, 3_I4B])
!   CALL SetSparsity(obj=csr, row=3_I4B, col=empty)
!   CALL SetSparsity(obj=csr, row=4_I4B, col=[4_I4B, 1_I4B, 4_I4B])
!   CALL SetSparsity(csr)
!   CALL OK(csr%nnz .EQ. 7_I4B, "scalar: nnz")
!   CALL OK(ALL(csr%IA .EQ. [1, 4, 6, 6, 8]), "scalar: IA")
!   CALL OK(ALL(csr%JA .EQ. [1, 2, 3, 2, 4, 1, 4]), "scalar: JA")
!   CALL DEALLOCATE (csr)
! END BLOCK
!
! ! Test 2: 3 nodes, 2 components, FMT_NODES
! BLOCK
!   TYPE(CSRSparsity_) :: csr
!   TYPE(DOF_) :: dof
!   CALL Initiate(obj=dof, tNodes=[3_I4B], names=["U"], &
!                 spacecompo=[2_I4B], timecompo=[1_I4B], &
!                 storagefmt=FMT_NODES)
!   CALL Initiate(obj=csr, ncol=6_I4B, nrow=6_I4B, idof=dof, jdof=dof)
!   CALL SetSparsity(obj=csr, row=1_I4B, col=[2_I4B, 1_I4B])
!   CALL SetSparsity(obj=csr, row=3_I4B, col=[3_I4B])
!   CALL SetSparsity(obj=csr, row=1_I4B, col=[1_I4B, 3_I4B])
!   CALL SetSparsity(csr)
!   CALL OK(csr%nnz .EQ. 16_I4B, "FMT_NODES: nnz")
!   CALL OK(ALL(csr%IA .EQ. [1, 7, 13, 13, 13, 15, 17]), "FMT_NODES: IA")
!   CALL OK(ALL(csr%JA .EQ. [1, 2, 3, 4, 5, 6, 1, 2, 3, 4, 5, 6, &
!                            5, 6, 5, 6]), "FMT_NODES: JA")
!   CALL DEALLOCATE (csr)
!   CALL DEALLOCATE (dof)
! END BLOCK
!
! ! Test 3: 3 nodes, 2 components, FMT_DOF
! BLOCK
!   TYPE(CSRSparsity_) :: csr
!   TYPE(DOF_) :: dof
!   CALL Initiate(obj=dof, tNodes=[3_I4B], names=["U"], &
!                 spacecompo=[2_I4B], timecompo=[1_I4B], &
!                 storagefmt=FMT_DOF)
!   CALL Initiate(obj=csr, ncol=6_I4B, nrow=6_I4B, idof=dof, jdof=dof)
!   CALL SetSparsity(obj=csr, row=1_I4B, col=[2_I4B, 1_I4B])
!   CALL SetSparsity(obj=csr, row=3_I4B, col=[3_I4B])
!   CALL SetSparsity(obj=csr, row=1_I4B, col=[1_I4B, 3_I4B])
!   CALL SetSparsity(csr)
!   CALL OK(csr%nnz .EQ. 16_I4B, "FMT_DOF: nnz")
!   CALL OK(ALL(csr%IA .EQ. [1, 7, 7, 9, 15, 15, 17]), "FMT_DOF: IA")
!   CALL OK(ALL(csr%JA .EQ. [1, 2, 3, 4, 5, 6, 3, 6, &
!                            1, 2, 3, 4, 5, 6, 3, 6]), "FMT_DOF: JA")
!   CALL DEALLOCATE (csr)
!   CALL DEALLOCATE (dof)
! END BLOCK
!
! END PROGRAM main
!```
!
! See also: [[CSRSparsity_Method:obj_SetSparsity2]],
! [[CSRSparsity_Method:obj_SetSparsity_final]]

INTERFACE SetSparsity
  MODULE SUBROUTINE obj_SetSparsity1(obj, row, col)
    TYPE(CSRSparsity_), INTENT(INOUT) :: obj
    !! sparsity pattern, initiated and not locked
    INTEGER(I4B), INTENT(IN) :: row
    !! row node number, in the node numbering of obj%idof
    INTEGER(I4B), INTENT(IN) :: col(:)
    !! column node numbers connected to row, in the node numbering of
    !! obj%jdof; may be unsorted, contain repeats, or be empty
  END SUBROUTINE obj_SetSparsity1
END INTERFACE SetSparsity

!----------------------------------------------------------------------------
!                                                     SetSparsity@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 22 March 2021
! summary: Add the column nodes of several row nodes to the sparsity
!   pattern
!
! This routine adds the coupling between each row node `row(i)` and the
! column nodes `col(i)%val` to the sparsity pattern that is being built.
! It calls [[CSRSparsity_Method:obj_SetSparsity1]] once for every `i`, so
! the conversion of node numbers to matrix indices through `obj%idof` and
! `obj%jdof` is the same as in that routine.
!
! Rows may appear more than once in `row` and in any order, and each
! `col(i)%val` may be unsorted, contain repeats, or have size zero. Call
! `SetSparsity(obj)` after the last call to sort the rows, remove the
! duplicates, and build `obj%IA`, `obj%JA` and `obj%nnz`.
!
! The sizes of `row` and `col` are not checked against each other, and
! every `col(i)%val` must be allocated.
!
! Examples
!
!```fortran
! TYPE(CSRSparsity_) :: csr
! TYPE(IntVector_) :: col(3)
!
! col(1)%val = [2_I4B, 1_I4B]
! col(2)%val = [3_I4B]
! col(3)%val = [1_I4B, 3_I4B]
! CALL Initiate(obj=csr, ncol=3_I4B, nrow=3_I4B)
! CALL SetSparsity(obj=csr, row=[1_I4B, 3_I4B, 1_I4B], col=col)
! CALL SetSparsity(csr)
! ! csr%IA = [1, 4, 4, 5], csr%JA = [1, 2, 3, 3]
!```
!
! Full example: src/modules/CSRSparsity/examples/CSRSparsity_test_3.F90
!
! See also: [[CSRSparsity_Method:obj_SetSparsity1]],
! [[CSRSparsity_Method:obj_SetSparsity_final]]

INTERFACE SetSparsity
  MODULE SUBROUTINE obj_SetSparsity2(obj, row, col)
    TYPE(CSRSparsity_), INTENT(INOUT) :: obj
    !! sparsity pattern, initiated and not locked
    INTEGER(I4B), INTENT(IN) :: row(:)
    !! row node numbers, in the node numbering of obj%idof
    TYPE(IntVector_), INTENT(IN) :: col(:)
    !! col(i)%val are the column node numbers connected to row(i), in
    !! the node numbering of obj%jdof; SIZE(col) = SIZE(row)
  END SUBROUTINE obj_SetSparsity2
END INTERFACE SetSparsity

!----------------------------------------------------------------------------
!                                                     SetSparsity@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 10 Oct 2021
! summary: This subroutine Sets sparsity pattern for block `CSRSparsity_`

INTERFACE SetSparsity
  MODULE SUBROUTINE obj_SetSparsity3(obj, row, col, ivar, jvar)
    TYPE(CSRSparsity_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: row
    !! row number
    INTEGER(I4B), INTENT(IN) :: col(:)
    !! sparsity of row, column numbers
    INTEGER(I4B), INTENT(IN) :: ivar
    !! block address (row index)
    INTEGER(I4B), INTENT(IN) :: jvar
    !! block address (col index)
  END SUBROUTINE obj_SetSparsity3
END INTERFACE SetSparsity

!----------------------------------------------------------------------------
!                                                     SetSparsity@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 22 March 2021
! summary: This subroutine Sets the sparsity pattern of a given row
!
!# Introduction
! This routine is similar to the [[CSRSparsity_Method:obj_SetSparsity3]],
! however, in this routine we can specify several rows and their
! column indices.

INTERFACE SetSparsity
  MODULE SUBROUTINE obj_SetSparsity4(obj, row, col, ivar, jvar)
    TYPE(CSRSparsity_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: row(:)
    !! several row numbers
    TYPE(IntVector_), INTENT(IN) :: col(:)
    !! column index for each row number
    INTEGER(I4B), INTENT(IN) :: ivar
    !! block address (row index)
    INTEGER(I4B), INTENT(IN) :: jvar
    !! block address (col index)
  END SUBROUTINE obj_SetSparsity4
END INTERFACE SetSparsity

!----------------------------------------------------------------------------
!                                                     SetSparsity@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         22 March 2021
! summary:          This subroutine Set the sparsity pattern of a given row
!
!# Introduction
!
! This subroutine Sets the sparsity pattern by using the graph.
! graph( i, j ) is either 0 or 1, if zero then there is not connection
! between row-i and row-j

INTERFACE SetSparsity
  MODULE SUBROUTINE obj_SetSparsity5(obj, graph)
    TYPE(CSRSparsity_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: graph(:, :)
  !! graph of sparsity
  !! If graph( i, j ) .EQ. 0, then i and j are not connected
  !! else they are connected.
  END SUBROUTINE obj_SetSparsity5
END INTERFACE SetSparsity

!----------------------------------------------------------------------------
!                                                     SetSparsity@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         22 March 2021
! summary:          This subroutine Set the sparsity pattern of a given row
!
!# Introduction
!
! This subroutine Sets the sparsity pattern by using the graph.
! graph( i, j ) is either FALSE or TRUE, if FALSE then there is not connection
! between row-i and row-j

INTERFACE SetSparsity
  MODULE SUBROUTINE obj_SetSparsity6(obj, graph)
    TYPE(CSRSparsity_), INTENT(INOUT) :: obj
    LOGICAL(LGT), INTENT(IN) :: graph(:, :)
  !! graph of sparsity
  !! If graph( i, j ) .EQ. FALSE, then i and j are not connected
  !! else they are connected.
  END SUBROUTINE obj_SetSparsity6
END INTERFACE SetSparsity

!----------------------------------------------------------------------------
!                                                     SetSparsity@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         22 March 2021
! summary:          This subroutine Set sparsity pattern of `CSRSparsity_`
!
!# Introduction
! This subroutine Set sparsity pattern of `CSRSparsity_`
! This will finally Set the data into
! - `obj%IA(:)`,
! - `obj%JA(:)`
! in CSR format. This routine also Set data inside `obj%ColSize(:)` and
! `obj%RowSize(:) `, and `obj%DiagIndx(:)`

INTERFACE SetSparsity
  MODULE SUBROUTINE obj_SetSparsity_final(obj)
    TYPE(CSRSparsity_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_SetSparsity_final
END INTERFACE SetSparsity

!----------------------------------------------------------------------------
!                                                        SetIA@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-12-14
! summary:  Set entry in IA

INTERFACE SetIA
  MODULE PURE SUBROUTINE obj_SetIA(obj, irow, VALUE)
    TYPE(CSRSparsity_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: irow
    INTEGER(I4B), INTENT(IN) :: VALUE
  END SUBROUTINE obj_SetIA
END INTERFACE SetIA

!----------------------------------------------------------------------------
!                                                        SetJA@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-12-14
! summary:  Set entry in JA

INTERFACE SetJA
  MODULE PURE SUBROUTINE obj_SetJA(obj, indx, VALUE)
    TYPE(CSRSparsity_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: indx
    INTEGER(I4B), INTENT(IN) :: VALUE
  END SUBROUTINE obj_SetJA
END INTERFACE SetJA

!----------------------------------------------------------------------------
!                                                          GetIA@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-12-23
! summary:  Get entry from IA

INTERFACE GetIA
  MODULE PURE FUNCTION obj_GetIA(obj, irow) RESULT(ans)
    TYPE(CSRSparsity_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: irow
    INTEGER(I4B) :: ans
  END FUNCTION obj_GetIA
END INTERFACE GetIA

!----------------------------------------------------------------------------
!                                                        GetJA@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-12-14
! summary:  Get entry from JA

INTERFACE GetJA
  MODULE PURE FUNCTION obj_GetJA(obj, indx) RESULT(ans)
    TYPE(CSRSparsity_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: indx
    INTEGER(I4B) :: ans
  END FUNCTION obj_GetJA
END INTERFACE GetJA

!----------------------------------------------------------------------------
!                                                         GetSym@SymMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-01-28
! summary: Get symmetric part

INTERFACE GetSym
  MODULE SUBROUTINE obj_GetSym1(obj, symObj, from)
    TYPE(CSRSparsity_), INTENT(IN) :: obj
    TYPE(CSRSparsity_), INTENT(INOUT) :: symObj
    CHARACTER(1), INTENT(IN) :: from
  END SUBROUTINE obj_GetSym1
END INTERFACE GetSym

!----------------------------------------------------------------------------
!                                                         GetSym@SymMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-01-28
! summary: Get symmetric part

INTERFACE GetSym
  MODULE SUBROUTINE obj_GetSym2(obj, from)
    TYPE(CSRSparsity_), INTENT(INOUT) :: obj
    CHARACTER(1), INTENT(IN) :: from
  END SUBROUTINE obj_GetSym2
END INTERFACE GetSym

END MODULE CSRSparsity_Method
