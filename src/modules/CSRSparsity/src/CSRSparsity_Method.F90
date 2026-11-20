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

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-21
! summary: This module contains methods for [[CSRSparsity_]]

MODULE CSRSparsity_Method
USE BaseType, ONLY: CSRSparsity_
USE BaseType, ONLY: DOF_
USE BaseType, ONLY: IntVector_
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: LGT
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
! date: 2021-03-22
! summary: Initiate an empty sparsity pattern of a given shape
!
! This routine deallocates `obj` and prepares it for the `SetSparsity`
! calls. `obj%IA` and `obj%idiag` are allocated, with sizes `nrow + 1`
! and `nrow`, and `obj%nnz` is set to `nnz` (default 0). `obj%JA` is
! allocated only when `nnz` is positive. The object is not locked.
!
! `idof` and `jdof` are copied into the object, and they must be given
! together. In debug builds the routine stops when only one of them is
! present, or when `.tNodes. idof` differs from `nrow` or `.tNodes. jdof`
! differs from `ncol`.
!
! @note
! If `idof` and `jdof` are not present, then this routine initiates
! `obj%idof` and `obj%jdof` internally with the following options.
!
! - `tNodes` = [nrow] for `idof`, [ncol] for `jdof`
! - names = ["K"]
! - spacecompo = [1]
! - timecompo = [1]
! - `storageFMT` = `NODES_FMT`
! @endnote
!
! Examples
!
! - [test 1](|base|CSRSparsity/examples/Initiate_test_1.F90)
!
! See also: [[CSRSparsity_Method:obj_Initiate3]],
! [[CSRSparsity_Method:obj_SetSparsity1]]

INTERFACE Initiate
  MODULE SUBROUTINE obj_Initiate1(obj, ncol, nrow, idof, jdof, nnz)
    TYPE(CSRSparsity_), INTENT(INOUT) :: obj
    !! sparsity pattern, its old content is deallocated
    INTEGER(I4B), INTENT(IN) :: ncol, nrow
    !! number of columns and rows of the matrix
    TYPE(DOF_), OPTIONAL, INTENT(IN) :: idof
    !! DOF for row
    TYPE(DOF_), OPTIONAL, INTENT(IN) :: jdof
    !! DOF for column
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: nnz
    !! number of nonzeros
  END SUBROUTINE obj_Initiate1
END INTERFACE Initiate

!----------------------------------------------------------------------------
!                                               Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-22
! summary: Copy a `CSRSparsity_` object
!
! This routine deallocates `obj` and copies `obj2` into it: the shape,
! the flags, `IA`, `JA`, `idiag`, the row storage `row`, `idof` and
! `jdof`. The copy does not share data with `obj2`. It also defines the
! assignment operator.
!
! An object can be copied at any time, also while the pattern is being
! built. The final `SetSparsity(obj)` call on the copy then gives the
! same pattern as on `obj2`.
!
! Changes:
!   - 2026-11-05: The row storage of obj2 is now copied.
!
! Examples
!
! - [test 2](|base|CSRSparsity/examples/Initiate_test_2.F90)
!
! See also: [[CSRSparsity_Method:obj_Deallocate]]

INTERFACE Initiate
  MODULE SUBROUTINE obj_Initiate2(obj, obj2)
    TYPE(CSRSparsity_), INTENT(INOUT) :: obj
    !! the copy, its old content is deallocated
    TYPE(CSRSparsity_), INTENT(IN) :: obj2
    !! object to copy
  END SUBROUTINE obj_Initiate2
END INTERFACE Initiate

INTERFACE ASSIGNMENT(=)
  MODULE PROCEDURE obj_Initiate2
END INTERFACE ASSIGNMENT(=)

!----------------------------------------------------------------------------
!                                                 Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-22
! summary: Initiate a `CSRSparsity_` object from IA and JA
!
! This routine constructs the object from existing CSR indices, which is
! helpful in reading data from files. The number of rows is
! `SIZE(IA) - 1` and `obj%nnz` is `SIZE(JA)`. The indices are copied
! without any check.
!
! The object gets the default scalar `idof` and `jdof`, as described for
! [[CSRSparsity_Method:obj_Initiate1]]. It holds no other information
! about the degrees of freedom.
!
! The object is not locked. `SetSparsity(obj)` locks it and keeps `IA`
! and `JA` unchanged.
!
! Examples
!
! - [test 3](|base|CSRSparsity/examples/Initiate_test_3.F90)
!
! See also: [[CSRSparsity_Method:obj_Initiate1]]

INTERFACE Initiate
  MODULE SUBROUTINE obj_Initiate3(obj, IA, JA, ncol)
    TYPE(CSRSparsity_), INTENT(INOUT) :: obj
    !! sparsity pattern, its old content is deallocated
    INTEGER(I4B), INTENT(IN) :: IA(:), JA(:)
    !! CSR indices, SIZE(IA) = nrow + 1, SIZE(JA) = nnz
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: ncol
    !! number of columns, default is number of rows
  END SUBROUTINE obj_Initiate3
END INTERFACE Initiate

!----------------------------------------------------------------------------
!                                            CSRSparsity@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-10-10
! summary: Returns an empty sparsity pattern of a given shape
!
! This function returns an object set up like
! [[CSRSparsity_Method:obj_Initiate1]] does, with `nnz` equal to 0.
! `idof` and `jdof` must be given together.
!
! Returns:
!   ans : TYPE(CSRSparsity_) - initiated object
!
! Examples
!
! - [test 1](|base|CSRSparsity/examples/Initiate_test_1.F90)
!
! See also: [[CSRSparsity_Method:obj_Initiate1]]

INTERFACE CSRSparsity
  MODULE FUNCTION obj_Constructor1(nrow, ncol, idof, jdof) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: nrow
    !! number of rows
    INTEGER(I4B), INTENT(IN) :: ncol
    !! number of columns
    TYPE(DOF_), OPTIONAL, INTENT(IN) :: idof
    !! dof for row
    TYPE(DOF_), OPTIONAL, INTENT(IN) :: jdof
    !! dof for column
    TYPE(CSRSparsity_) :: ans
    !! initiated object
  END FUNCTION obj_Constructor1
END INTERFACE CSRSparsity

!----------------------------------------------------------------------------
!                                            CSRSparsity@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-10-10
! summary: Returns a sparsity pattern made from IA and JA
!
! This function returns an object set up like
! [[CSRSparsity_Method:obj_Initiate3]] does. The matrix is square, since
! `ncol` cannot be given here.
!
! Returns:
!   ans : TYPE(CSRSparsity_) - initiated object
!
! Examples
!
! - [test 3](|base|CSRSparsity/examples/Initiate_test_3.F90)
!
! See also: [[CSRSparsity_Method:obj_Initiate3]]

INTERFACE CSRSparsity
  MODULE FUNCTION obj_Constructor2(IA, JA) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: IA(:)
    !! row pointers, SIZE(IA) = nrow + 1
    INTEGER(I4B), INTENT(IN) :: JA(:)
    !! column numbers, SIZE(JA) = nnz
    TYPE(CSRSparsity_) :: ans
    !! initiated object
  END FUNCTION obj_Constructor2
END INTERFACE CSRSparsity

!----------------------------------------------------------------------------
!                                       CSRSparsityPointer@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-10-10
! summary: Returns a pointer to an empty sparsity pattern
!
! This function allocates a new object and sets it up like
! [[CSRSparsity_Method:obj_Initiate1]] does. The caller owns the target.
! To free it, first call `DEALLOCATE(ptr)` of this module for the
! content. Then use the Fortran `DEALLOCATE` statement for the pointer.
!
! Returns:
!   ans : TYPE(CSRSparsity_), POINTER - new object
!
! Examples
!
! - [test 1](|base|CSRSparsity/examples/Initiate_test_1.F90)
!
! See also: [[CSRSparsity_Method:obj_Constructor1]]

INTERFACE CSRSparsityPointer
  MODULE FUNCTION obj_Constructor_1(nrow, ncol, idof, jdof) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: nrow
    !! number of rows
    INTEGER(I4B), INTENT(IN) :: ncol
    !! number of columns
    TYPE(DOF_), OPTIONAL, INTENT(IN) :: idof
    !! dof for row
    TYPE(DOF_), OPTIONAL, INTENT(IN) :: jdof
    !! dof for column
    TYPE(CSRSparsity_), POINTER :: ans
    !! newly allocated and initiated object
  END FUNCTION obj_Constructor_1
END INTERFACE CSRSparsityPointer

!----------------------------------------------------------------------------
!                                      CSRSparsityPointer@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-10-10
! summary: Returns a pointer to a sparsity pattern made from IA and JA
!
! This function allocates a new object and sets it up like
! [[CSRSparsity_Method:obj_Initiate3]] does. The matrix is square. The
! caller owns the target, as described for
! [[CSRSparsity_Method:obj_Constructor_1]].
!
! Returns:
!   ans : TYPE(CSRSparsity_), POINTER - new object
!
! Examples
!
! - [test 3](|base|CSRSparsity/examples/Initiate_test_3.F90)
!
! See also: [[CSRSparsity_Method:obj_Constructor2]]

INTERFACE CSRSparsityPointer
  MODULE FUNCTION obj_Constructor_2(IA, JA) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: IA(:)
    !! row pointers, SIZE(IA) = nrow + 1
    INTEGER(I4B), INTENT(IN) :: JA(:)
    !! column numbers, SIZE(JA) = nnz
    TYPE(CSRSparsity_), POINTER :: ans
    !! newly allocated and initiated object
  END FUNCTION obj_Constructor_2
END INTERFACE CSRSparsityPointer

!----------------------------------------------------------------------------
!                                             Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-22
! summary: Deallocate the data of a `CSRSparsity_` object
!
! This routine frees the data of `obj`, resets `nnz`, `nrow` and `ncol`
! to 0 and all the flags to `.FALSE.`. The object can be initiated again
! with any shape. Calling it on an object that was never initiated is
! allowed.
!
! Changes:
!   - 2026-11-05: The row storage `obj%row` is now deallocated.
!
! Examples
!
! - [test 2](|base|CSRSparsity/examples/Initiate_test_2.F90)
!
! See also: [[CSRSparsity_Method:obj_Initiate1]]

INTERFACE DEALLOCATE
  MODULE PURE SUBROUTINE obj_Deallocate(obj)
    TYPE(CSRSparsity_), INTENT(INOUT) :: obj
    !! object to deallocate
  END SUBROUTINE obj_Deallocate
END INTERFACE DEALLOCATE

!----------------------------------------------------------------------------
!                                                         Display@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-22
! summary: Display the content of a `CSRSparsity_` object
!
! This routine writes `Msg`, `nnz`, `ncol`, `nrow`, `idof`, `jdof`, `IA`,
! `JA` and `idiag`, and `row` while it is allocated. For `IA`, `JA` and
! `idiag`, a line with the text "is not allocated" replaces an array that
! is not allocated.
!
! Examples
!
! - [test 1](|base|CSRSparsity/examples/Display_test_1.F90)

INTERFACE Display
  MODULE SUBROUTINE obj_Display(obj, Msg, UnitNo)
    TYPE(CSRSparsity_), INTENT(IN) :: obj
    !! object to display
    CHARACTER(*), INTENT(IN) :: Msg
    !! message written first
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: UnitNo
    !! output unit, default is stdout
  END SUBROUTINE obj_Display
END INTERFACE Display

!----------------------------------------------------------------------------
!                                                          Shape@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-22
! summary: Return the shape of the sparse matrix
!
! This function returns the number of rows and the number of columns.
!
! Returns:
!   ans : INTEGER(I4B)(2) - [nrow, ncol]
!
! Examples
!
! - [test 1](|base|CSRSparsity/examples/GetColIndex_test_1.F90) shows
!   the shape of a pattern
!
! See also: [[CSRSparsity_Method:obj_Size]]

INTERFACE Shape
  MODULE PURE FUNCTION obj_Shape(obj) RESULT(ans)
    TYPE(CSRSparsity_), INTENT(IN) :: obj
    !! sparsity pattern
    INTEGER(I4B) :: ans(2)
    !! [nrow, ncol]
  END FUNCTION obj_Shape
END INTERFACE Shape

!----------------------------------------------------------------------------
!                                                           Size@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-22
! summary: Return the size of the sparse matrix
!
! This function returns the number of rows, the number of columns, or
! the number of entries of the full matrix.
!
! - If `Dims` is 1, it returns the number of rows.
! - If `Dims` has any other value, it returns the number of columns.
! - If `Dims` is absent, it returns `nrow * ncol`.
!
! Returns:
!   ans : INTEGER(I4B) - requested size
!
! Examples
!
! - [test 1](|base|CSRSparsity/examples/GetColIndex_test_1.F90)
!
! See also: [[CSRSparsity_Method:obj_Shape]]

INTERFACE Size
  MODULE PURE FUNCTION obj_Size(obj, Dims) RESULT(ans)
    TYPE(CSRSparsity_), INTENT(IN) :: obj
    !! sparsity pattern
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: Dims
    !! 1 for rows, otherwise columns
    INTEGER(I4B) :: ans
    !! requested size
  END FUNCTION obj_Size
END INTERFACE Size

!----------------------------------------------------------------------------
!                                                          GetNNZ@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-22
! summary: Return the total number of nonzero entries
!
! This function returns the number of nonzero entries stored in `obj`.
! This is the value of `obj%nnz`.
!
! @note
! The value is exact only after the final call of `SetSparsity(obj)`.
! Before that call it is an upper bound. See
! [[CSRSparsity_Method:obj_SetSparsity1]].
! @endnote
!
! Returns:
!   ans : INTEGER(I4B) - `obj%nnz`
!
! Examples
!
! - [test 1](|base|CSRSparsity/examples/GetNNZ_test_1.F90)
!
! See also: [[CSRSparsity_Method:obj_GetNNZ1]],
! [[CSRSparsity_Method:obj_GetNNZ2]]

INTERFACE GetNNZ
  MODULE PURE FUNCTION obj_GetNNZ(obj) RESULT(ans)
    TYPE(CSRSparsity_), INTENT(IN) :: obj
    !! sparsity pattern
    INTEGER(I4B) :: ans
    !! obj%nnz
  END FUNCTION obj_GetNNZ
END INTERFACE GetNNZ

!----------------------------------------------------------------------------
!                                                          GetNNZ@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-22
! summary: Return the number of nonzeros of the sum of two patterns
!
! This function counts the nonzero entries of the sum or difference of
! two patterns. It counts the union of the patterns of `obj1` and `obj2`,
! row by row. An entry present in both patterns counts once.
!
! The count ignores numerical cancellation.
!
! If `isSorted` is true, the columns of each row must be sorted. A wrong
! count is returned for unsorted columns.
!
! @note
! Only "+" and "-" are implemented. For any other `op`, including "*",
! the result is undefined.
! @endnote
!
! @warning
! `obj1` and `obj2` must have the same shape. The routine does not check
! this.
! @endwarning
!
! Returns:
!   ans : INTEGER(I4B) - nonzeros of the sum
!
! Examples
!
! - [test 2](|base|CSRSparsity/examples/GetNNZ_test_2.F90)
!
! See also: [[CSRSparsity_Method:GetNNZ_Add_Subtract]],
! [[CSRSparsity_Method:GetNNZ_Add_Subtract_sorted]]

INTERFACE GetNNZ
  MODULE PURE FUNCTION obj_GetNNZ_from_operation(obj1, obj2, op, isSorted) &
    RESULT(ans)
    TYPE(CSRSparsity_), INTENT(IN) :: obj1
    !! first sparsity pattern
    TYPE(CSRSparsity_), INTENT(IN) :: obj2
    !! second sparsity pattern, same shape as `obj1`
    CHARACTER(1), INTENT(IN) :: op
    !! operation, "+" or "-"
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isSorted
    !! set it to true if the columns are sorted in each row of
    !! `obj1` and `obj2`, default is `.FALSE.`
    INTEGER(I4B) :: ans
    !! number of nonzero entries of the sum
  END FUNCTION obj_GetNNZ_from_operation
END INTERFACE GetNNZ

!----------------------------------------------------------------------------
!                                                          GetNNZ@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-12-16
! summary: Number of nonzeros in the sum of two CSR patterns
!
! This function counts the nonzero entries of the union of two CSR
! patterns, row by row. An entry present in both patterns counts once.
!
! The columns of each row may be unsorted.
!
! This function is not public.
! [[CSRSparsity_Method:obj_GetNNZ_from_operation]] calls it when
! `isSorted` is false.
!
! Returns:
!   ans : INTEGER(I4B) - nonzeros of the sum
!
! See also: [[CSRSparsity_Method:obj_GetNNZ_from_operation]]

INTERFACE
  MODULE PURE FUNCTION GetNNZ_Add_Subtract(nrow, ncol, ja, ia, jb, ib) &
    RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: nrow, ncol
    !! number of rows and columns of both patterns
    INTEGER(I4B), INTENT(IN) :: ja(:)
    !! column numbers (JA) of the first pattern
    INTEGER(I4B), INTENT(IN) :: ia(:)
    !! row pointers (IA) of the first pattern, size `nrow + 1`
    INTEGER(I4B), INTENT(IN) :: jb(:)
    !! column numbers (JA) of the second pattern
    INTEGER(I4B), INTENT(IN) :: ib(:)
    !! row pointers (IA) of the second pattern, size `nrow + 1`
    INTEGER(I4B) :: ans
    !! number of nonzeros of the sum
  END FUNCTION GetNNZ_Add_Subtract
END INTERFACE

!----------------------------------------------------------------------------
!                                                          GetNNZ@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-12-16
! summary: Number of nonzeros in the sum of two sorted CSR patterns
!
! This function counts the nonzero entries of the union of two CSR
! patterns, row by row. An entry present in both patterns counts once.
!
! @warning
! The columns of each row must be sorted. Unsorted columns give a wrong
! count.
! @endwarning
!
! This function is not public.
! [[CSRSparsity_Method:obj_GetNNZ_from_operation]] calls it when
! `isSorted` is true.
!
! Returns:
!   ans : INTEGER(I4B) - nonzeros of the sum
!
! See also: [[CSRSparsity_Method:obj_GetNNZ_from_operation]]

INTERFACE
  MODULE PURE FUNCTION GetNNZ_Add_Subtract_sorted(nrow, ncol, ja, ia, jb, &
                                                  ib) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: nrow, ncol
    !! number of rows and columns of both patterns
    INTEGER(I4B), INTENT(IN) :: ja(:)
    !! column numbers (JA) of the first pattern
    INTEGER(I4B), INTENT(IN) :: ia(:)
    !! row pointers (IA) of the first pattern, size `nrow + 1`
    INTEGER(I4B), INTENT(IN) :: jb(:)
    !! column numbers (JA) of the second pattern
    INTEGER(I4B), INTENT(IN) :: ib(:)
    !! row pointers (IA) of the second pattern, size `nrow + 1`
    INTEGER(I4B) :: ans
    !! number of nonzeros of the sum
  END FUNCTION GetNNZ_Add_Subtract_sorted
END INTERFACE

!----------------------------------------------------------------------------
!                                                          GetNNZ@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-01-28
! summary: Return the number of nonzeros in one part of the matrix
!
! This function counts the nonzero entries in the upper triangle, the
! lower triangle, or the diagonal of the pattern. The letter in `from`
! selects the part, and its case does not matter.
!
! Any other letter returns `obj%nnz`, the count for the whole matrix.
! If `obj` is not initiated, the result is 0.
!
! Returns:
!   ans : INTEGER(I4B) - entries in the requested part
!
! Examples
!
! - [test 1](|base|CSRSparsity/examples/GetNNZ_test_1.F90)
!
! See also: [[CSRSparsity_Method:obj_GetNNZ2]]

INTERFACE GetNNZ
  MODULE PURE FUNCTION obj_GetNNZ1(obj, from) RESULT(ans)
    TYPE(CSRSparsity_), INTENT(IN) :: obj
    !! sparsity pattern
    CHARACTER(1), INTENT(IN) :: from
    !! "U": upper triangular part, j > i.
    !! "L": lower triangular part, i > j.
    !! "D": diagonal part, i = j.
    !! "A" or any other letter: whole matrix.
    INTEGER(I4B) :: ans
    !! number of entries in the requested part
  END FUNCTION obj_GetNNZ1
END INTERFACE GetNNZ

!----------------------------------------------------------------------------
!                                                          GetNNZ@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-01-28
! summary: Return the number of nonzeros in U, L and D
!
! This function counts the nonzero entries in the upper triangle, the
! lower triangle and the diagonal in one pass. The result holds the three
! counts.
!
! If `obj` is not initiated, all three counts are 0.
!
! Call it with `from=[" "]`. The array only selects this specific
! function, and its value is ignored.
!
! Returns:
!   ans : INTEGER(I4B)(3) - `[nnzU, nnzL, nnzD]`
!
! Examples
!
! - [test 1](|base|CSRSparsity/examples/GetNNZ_test_1.F90)
!
! See also: [[CSRSparsity_Method:obj_GetNNZ1]]

INTERFACE GetNNZ
  MODULE PURE FUNCTION obj_GetNNZ2(obj, from) RESULT(ans)
    TYPE(CSRSparsity_), INTENT(IN) :: obj
    !! sparsity pattern
    CHARACTER(1), INTENT(IN) :: from(1)
    !! this argument is not referred, it is here
    !! to create a unique interface only
    INTEGER(I4B) :: ans(3)
    !! `[nnzU, nnzL, nnzD]`
  END FUNCTION obj_GetNNZ2
END INTERFACE GetNNZ

!----------------------------------------------------------------------------
!                                                      GetDiagonal@GeMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-07-15
! summary: Returns a diagonal of a sparse matrix and its positions
!
! This subroutine returns the entries `a(i, j)` with `j - i = offSet`.
! It also returns the position of each entry in `A`. The value
! `offSet = 0` selects the main diagonal. The default is 0.
!
! `diag` and `idiag` are reallocated to size `nrow`. If `idiag(i)` is 0,
! row `i` has no entry on this diagonal, and `diag(i)` is zero.
!
! The object `obj` is not changed.
!
! Examples
!
! - [test 1](|base|CSRSparsity/examples/GetDiagonal_test_1.F90)
!
! See also: [[CSRSparsity_Method:obj_GetDiagonal2]]

INTERFACE GetDiagonal
  MODULE SUBROUTINE obj_GetDiagonal1(obj, A, diag, idiag, offSet)
    TYPE(CSRSparsity_), INTENT(IN) :: obj
    !! sparsity pattern of the matrix
    REAL(DFP), INTENT(IN) :: A(:)
    !! sparse matrix values, SIZE(A) = obj%nnz
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
! date: 2021-07-15
! summary: Returns a diagonal of a sparse matrix, storing its positions
!
! This subroutine returns the entries `a(i, j)` with `j - i = offSet`.
! It does not return their positions. The default `offSet` is 0, the
! main diagonal.
!
! For the main diagonal, `obj` keeps the positions in `obj%idiag`.
!
! 1. The first call finds the positions and sets `obj%isDiagStored`.
! 2. Later calls read `A(obj%idiag(i))`, which is faster.
!
! Any other `offSet` is searched on every call. It leaves `obj%idiag` and
! `obj%isDiagStored` unchanged.
!
! In every case, `diag(i)` is zero when row `i` has no entry on the
! diagonal.
!
! @note
! The stored positions belong to the pattern, not to the values `A`. Use
! the same pattern for all matrices that share `obj`.
! @endnote
!
! Changes:
!   - 2026-11-05: Only the main diagonal is stored, so offSet is honored
!     on every call. Rows with no entry give zero on later calls.
!
! Examples
!
! - [test 1](|base|CSRSparsity/examples/GetDiagonal_test_1.F90)
!
! See also: [[CSRSparsity_Method:obj_GetDiagonal1]]

INTERFACE GetDiagonal
  MODULE SUBROUTINE obj_GetDiagonal2(obj, A, diag, offSet)
    TYPE(CSRSparsity_), INTENT(INOUT) :: obj
    !! sparsity pattern, the main diagonal is stored in obj%idiag
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
! summary: Get the column number from JA
!
! This function returns the column number stored at position `indx` of
! `JA`. This is `obj%JA(indx)`, the same value as
! [[CSRSparsity_Method:obj_GetJA]] returns.
!
! Returns:
!   ans : INTEGER(I4B) - column number
!
! Examples
!
! - [test 1](|base|CSRSparsity/examples/GetColIndex_test_1.F90)
!
! See also: [[CSRSparsity_Method:obj_GetColIndex1]]

INTERFACE GetColNumber
  MODULE PURE FUNCTION obj_GetColNumber1(obj, indx) RESULT(ans)
    TYPE(CSRSparsity_), INTENT(IN) :: obj
    !! sparsity pattern
    INTEGER(I4B), INTENT(IN) :: indx
    !! position in JA, from 1 to obj%nnz
    INTEGER(I4B) :: ans
    !! column number
  END FUNCTION obj_GetColNumber1
END INTERFACE GetColNumber

!----------------------------------------------------------------------------
!                                                     GetColIndex@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-12-14
! summary: Get the first and last position of a row in JA
!
! This function returns the range of positions that row `irow` uses in
! `obj%JA`. The range is `[obj%IA(irow), obj%IA(irow + 1) - 1]`.
!
! If the row is empty, the second entry is smaller than the first one.
! A `DO` loop over this range then runs zero times.
!
! Returns:
!   ans : INTEGER(I4B)(2) - first and last position in JA
!
! Examples
!
! - [test 1](|base|CSRSparsity/examples/GetColIndex_test_1.F90)
!
! See also: [[CSRSparsity_Method:obj_startColumn1]],
! [[CSRSparsity_Method:obj_endColumn1]]

INTERFACE GetColIndex
  MODULE PURE FUNCTION obj_GetColIndex1(obj, irow) RESULT(ans)
    TYPE(CSRSparsity_), INTENT(IN) :: obj
    !! sparsity pattern
    INTEGER(I4B), INTENT(IN) :: irow
    !! row number, from 1 to the number of rows
    INTEGER(I4B) :: ans(2)
    !! first and last position in JA
  END FUNCTION obj_GetColIndex1
END INTERFACE GetColIndex

!----------------------------------------------------------------------------
!                                                     startColumn@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-12-14
! summary: Get the first position of a row in JA
!
! This operator returns the first position that row `irow` uses in
! `obj%JA`. This is `obj%IA(irow)`.
!
! Returns:
!   ans : INTEGER(I4B) - first position in JA
!
! Examples
!
! - [test 1](|base|CSRSparsity/examples/GetColIndex_test_1.F90)
!
! See also: [[CSRSparsity_Method:obj_GetColIndex1]]

INTERFACE OPERATOR(.startColumn.)
  MODULE PURE FUNCTION obj_startColumn1(obj, irow) RESULT(ans)
    TYPE(CSRSparsity_), INTENT(IN) :: obj
    !! sparsity pattern
    INTEGER(I4B), INTENT(IN) :: irow
    !! row number, from 1 to the number of rows
    INTEGER(I4B) :: ans
    !! first position in JA
  END FUNCTION obj_startColumn1
END INTERFACE OPERATOR(.startColumn.)

!----------------------------------------------------------------------------
!                                                        endColumn@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-12-14
! summary: Get the last position of a row in JA
!
! This operator returns the last position that row `irow` uses in
! `obj%JA`. This is `obj%IA(irow + 1) - 1`.
!
! For an empty row, the result is one less than `.startColumn.` of
! that row.
!
! Returns:
!   ans : INTEGER(I4B) - last position in JA
!
! Examples
!
! - [test 1](|base|CSRSparsity/examples/GetColIndex_test_1.F90)
!
! See also: [[CSRSparsity_Method:obj_GetColIndex1]]

INTERFACE OPERATOR(.endColumn.)
  MODULE PURE FUNCTION obj_endColumn1(obj, irow) RESULT(ans)
    TYPE(CSRSparsity_), INTENT(IN) :: obj
    !! sparsity pattern
    INTEGER(I4B), INTENT(IN) :: irow
    !! row number, from 1 to the number of rows
    INTEGER(I4B) :: ans
    !! last position in JA
  END FUNCTION obj_endColumn1
END INTERFACE OPERATOR(.endColumn.)

!----------------------------------------------------------------------------
!                                                     SetSparsity@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-22
! summary: Add the column nodes of one row node to the sparsity pattern
!
! This routine adds the coupling between the row node `row` and the
! column nodes `col`. The coupling goes into the pattern under
! construction.
!
! The routine works in these steps.
!
! 1. It converts `row` to matrix rows. Each degree of freedom of `row`
!    in `obj%idof` gives one matrix row.
! 2. It converts `col` to matrix columns. Each degree of freedom of each
!    entry of `col` in `obj%jdof` gives one matrix column.
! 3. It appends the matrix columns to `obj%row(i)` for each matrix row.
!
! Both conversions follow the storage format of the DOF objects. This
! format is `FMT_NODES` or `FMT_DOF`.
!
! The appended indices are not sorted and not made unique. Repeated calls
! for the same row only append more entries. The value `obj%nnz` grows by
! the number of appended entries. At this stage it is an upper bound.
!
! Call `SetSparsity(obj)` after the last call. It sorts each row and
! removes the duplicates. It also builds `obj%IA`, `obj%JA` and the exact
! `obj%nnz`.
!
! If `col` is empty, the routine only allocates the row storage. In debug
! builds, it stops when `obj` is not initiated or is locked.
!
! Changes:
!   - 2026-11-03: Row and column indices are no longer sorted here, since
!     SetSparsity(obj) sorts each row anyway.
!
! Examples
!
! - [test 1](|base|CSRSparsity/examples/SetSparsity_test_1.F90)
! - [test 4](|base|CSRSparsity/examples/SetSparsity_test_4.F90)
! - [test 5](|base|CSRSparsity/examples/SetSparsity_test_5.F90)
!
! See also: [[CSRSparsity_Method:obj_SetSparsity2]],
! [[CSRSparsity_Method:obj_SetSparsity_final]]

INTERFACE SetSparsity
  MODULE SUBROUTINE obj_SetSparsity1(obj, row, col)
    TYPE(CSRSparsity_), INTENT(INOUT) :: obj
    !! sparsity pattern, initiated and not locked
    INTEGER(I4B), INTENT(IN) :: row
    !! row node number, as numbered in the row DOF object
    INTEGER(I4B), INTENT(IN) :: col(:)
    !! column node numbers connected to row, as numbered in the
    !! column DOF object. May be unsorted, repeat or be empty.
  END SUBROUTINE obj_SetSparsity1
END INTERFACE SetSparsity

!----------------------------------------------------------------------------
!                                                     SetSparsity@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-22
! summary: Add the column nodes of several row nodes to the sparsity
!   pattern
!
! This routine adds the coupling between each row node `row(i)` and the
! column nodes `col(i)%val`. The coupling goes into the pattern under
! construction.
!
! Node numbers become matrix indices as in
! [[CSRSparsity_Method:obj_SetSparsity1]].
!
! A row may appear more than once in `row`, in any order. Each `col(i)`
! may be unsorted, contain repeats, or be empty.
!
! Call `SetSparsity(obj)` after the last call. It sorts the rows and
! removes the duplicates. It also builds `obj%IA`, `obj%JA` and
! `obj%nnz`.
!
! @note
! The routine reads only `col(i)%val(1:col(i)%tsize)`. Set each `col(i)`
! with the `IntVector_` assignment. If you fill `col(i)%val` directly,
! `tsize` stays 0 and the entry adds nothing.
! @endnote
!
! In debug builds, the routine stops when `row` and `col` differ in size.
!
! Examples
!
! - [test 2](|base|CSRSparsity/examples/SetSparsity_test_2.F90)
! - [test 6](|base|CSRSparsity/examples/SetSparsity_test_6.F90)
! - [test 7](|base|CSRSparsity/examples/SetSparsity_test_7.F90)
!
! See also: [[CSRSparsity_Method:obj_SetSparsity1]],
! [[CSRSparsity_Method:obj_SetSparsity_final]]

INTERFACE SetSparsity
  MODULE SUBROUTINE obj_SetSparsity2(obj, row, col)
    TYPE(CSRSparsity_), INTENT(INOUT) :: obj
    !! sparsity pattern, initiated and not locked
    INTEGER(I4B), INTENT(IN) :: row(:)
    !! row node numbers, as numbered in the row DOF object
    TYPE(IntVector_), INTENT(IN) :: col(:)
    !! column nodes of row(i) are col(i)%val(1:col(i)%tsize).
    !! They use the numbering of the column DOF object.
    !! SIZE(col) = SIZE(row)
  END SUBROUTINE obj_SetSparsity2
END INTERFACE SetSparsity

!----------------------------------------------------------------------------
!                                                     SetSparsity@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-10-10
! summary: Add the column nodes of one row node to a block of the sparsity
!   pattern
!
! This routine works like [[CSRSparsity_Method:obj_SetSparsity1]], but
! only for one block of a DOF with several physical variables. The row
! node `row` belongs to variable `ivar` of `obj%idof`, and the column
! nodes `col` belong to variable `jvar` of `obj%jdof`.
!
! Both DOF objects must use the storage format `FMT_DOF`.
!
! Call `SetSparsity(obj)` after the last call. In debug builds, the
! routine stops when a DOF object uses another format. It also stops when
! `obj` is not initiated or is locked.
!
! Examples
!
! - [test 8](|base|CSRSparsity/examples/SetSparsity_test_8.F90)
!
! See also: [[CSRSparsity_Method:obj_SetSparsity4]],
! [[CSRSparsity_Method:obj_SetSparsity_final]]

INTERFACE SetSparsity
  MODULE SUBROUTINE obj_SetSparsity3(obj, row, col, ivar, jvar)
    TYPE(CSRSparsity_), INTENT(INOUT) :: obj
    !! sparsity pattern, initiated and not locked
    INTEGER(I4B), INTENT(IN) :: row
    !! row node number of variable ivar
    INTEGER(I4B), INTENT(IN) :: col(:)
    !! column node numbers of variable jvar connected to row.
    !! May be unsorted, repeat or be empty.
    INTEGER(I4B), INTENT(IN) :: ivar
    !! physical variable of the row block, in the row DOF object
    INTEGER(I4B), INTENT(IN) :: jvar
    !! physical variable of the column block, in the column DOF object
  END SUBROUTINE obj_SetSparsity3
END INTERFACE SetSparsity

!----------------------------------------------------------------------------
!                                                     SetSparsity@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-22
! summary: Add the column nodes of several row nodes to a block of the
!   sparsity pattern
!
! This routine works like [[CSRSparsity_Method:obj_SetSparsity2]], but
! only for one block of a DOF with several physical variables, see
! [[CSRSparsity_Method:obj_SetSparsity3]]. Both DOF objects must use the
! storage format `FMT_DOF`.
!
! A row may appear more than once in `row`, in any order. Each `col(i)`
! may be unsorted or contain repeats. In debug builds, the routine stops
! when `row` and `col` differ in size.
!
! @note
! The routine reads only `col(i)%val(1:col(i)%tsize)`. Set each `col(i)`
! with the `IntVector_` assignment.
! @endnote
!
! Examples
!
! - [test 9](|base|CSRSparsity/examples/SetSparsity_test_9.F90)
!
! See also: [[CSRSparsity_Method:obj_SetSparsity3]]

INTERFACE SetSparsity
  MODULE SUBROUTINE obj_SetSparsity4(obj, row, col, ivar, jvar)
    TYPE(CSRSparsity_), INTENT(INOUT) :: obj
    !! sparsity pattern, initiated and not locked
    INTEGER(I4B), INTENT(IN) :: row(:)
    !! row node numbers of variable ivar
    TYPE(IntVector_), INTENT(IN) :: col(:)
    !! col(i)%val(1:col(i)%tsize) are the column node numbers of
    !! variable jvar connected to row(i). SIZE(col) = SIZE(row)
    INTEGER(I4B), INTENT(IN) :: ivar
    !! physical variable of the row block, in the row DOF object
    INTEGER(I4B), INTENT(IN) :: jvar
    !! physical variable of the column block, in the column DOF object
  END SUBROUTINE obj_SetSparsity4
END INTERFACE SetSparsity

!----------------------------------------------------------------------------
!                                                     SetSparsity@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-22
! summary: Set the sparsity pattern from an integer graph
!
! This routine adds the coupling between row node `i` and column node `j`
! for every nonzero `graph(i, j)`.
!
! The graph is in terms of nodes. The DOF objects of `obj` expand it to
! matrix indices, as in [[CSRSparsity_Method:obj_SetSparsity2]].
!
! Call `SetSparsity(obj)` after the last call.
!
! Examples
!
! - [test 10](|base|CSRSparsity/examples/SetSparsity_test_10.F90)
!
! See also: [[CSRSparsity_Method:obj_SetSparsity6]]

INTERFACE SetSparsity
  MODULE SUBROUTINE obj_SetSparsity5(obj, graph)
    TYPE(CSRSparsity_), INTENT(INOUT) :: obj
    !! sparsity pattern, initiated and not locked
    INTEGER(I4B), INTENT(IN) :: graph(:, :)
    !! node graph. Rows of graph are row nodes.
    !! Columns of graph are column nodes.
    !! graph(i, j) = 0 means no connection. Any other value connects
    !! row node i and column node j.
  END SUBROUTINE obj_SetSparsity5
END INTERFACE SetSparsity

!----------------------------------------------------------------------------
!                                                     SetSparsity@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-22
! summary: Set the sparsity pattern from a logical graph
!
! This routine works like [[CSRSparsity_Method:obj_SetSparsity5]]. Row
! node `i` and column node `j` are connected when `graph(i, j)` is
! `.TRUE.`.
!
! Examples
!
! - [test 11](|base|CSRSparsity/examples/SetSparsity_test_11.F90)
!
! See also: [[CSRSparsity_Method:obj_SetSparsity5]]

INTERFACE SetSparsity
  MODULE SUBROUTINE obj_SetSparsity6(obj, graph)
    TYPE(CSRSparsity_), INTENT(INOUT) :: obj
    !! sparsity pattern, initiated and not locked
    LOGICAL(LGT), INTENT(IN) :: graph(:, :)
    !! node graph. Rows of graph are row nodes.
    !! Columns of graph are column nodes.
    !! graph(i, j) true connects row node i and column node j.
  END SUBROUTINE obj_SetSparsity6
END INTERFACE SetSparsity

!----------------------------------------------------------------------------
!                                                     SetSparsity@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-03-22
! summary: Finish the sparsity pattern and build IA and JA
!
! Call this routine once, after the last `SetSparsity` call that adds
! entries. It turns the rows collected so far into the final pattern.
!
! The routine works in these steps.
!
! 1. It sorts each row and removes the duplicates.
! 2. It sets the exact `obj%nnz`.
! 3. It builds `obj%IA` and `obj%JA`.
! 4. It frees the row storage `obj%row`.
! 5. It locks the pattern, so `obj%isSparsityLock` becomes `.TRUE.`.
!
! The routine does not set `obj%idiag` or `obj%isSorted`.
!
! If `obj` is already locked, the routine changes nothing. Debug builds
! print a warning.
!
! If `obj` has no row storage, the routine keeps `obj%IA` and `obj%JA`
! and only locks the pattern. This is the case for an object that was
! initiated from `IA` and `JA`.
!
! In debug builds, the routine stops when `obj` is not initiated.
!
! Examples
!
! - [test 3](|base|CSRSparsity/examples/SetSparsity_test_3.F90)
! - [test 12](|base|CSRSparsity/examples/SetSparsity_test_12.F90)
!
! See also: [[CSRSparsity_Method:obj_SetSparsity1]]

INTERFACE SetSparsity
  MODULE SUBROUTINE obj_SetSparsity_final(obj)
    TYPE(CSRSparsity_), INTENT(INOUT) :: obj
    !! sparsity pattern, Initiated
  END SUBROUTINE obj_SetSparsity_final
END INTERFACE SetSparsity

!----------------------------------------------------------------------------
!                                                        SetIA@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-12-14
! summary: Set an entry of IA
!
! Sets the entry `obj%IA(irow)` to `VALUE`.
!
! @warning
! The routine does not check `irow` or `VALUE`. The caller must keep `IA`
! and `JA` consistent with each other.
! @endwarning
!
! Examples
!
! - [test 1](|base|CSRSparsity/examples/GetColIndex_test_1.F90)
!
! See also: [[CSRSparsity_Method:obj_GetIA]]

INTERFACE SetIA
  MODULE PURE SUBROUTINE obj_SetIA(obj, irow, VALUE)
    TYPE(CSRSparsity_), INTENT(INOUT) :: obj
    !! sparsity pattern
    INTEGER(I4B), INTENT(IN) :: irow
    !! position in IA, from 1 to obj%nrow + 1
    INTEGER(I4B), INTENT(IN) :: VALUE
    !! new value
  END SUBROUTINE obj_SetIA
END INTERFACE SetIA

!----------------------------------------------------------------------------
!                                                        SetJA@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-12-14
! summary: Set an entry of JA
!
! Sets the entry `obj%JA(indx)` to `VALUE`.
!
! @warning
! The routine does not check `indx` or `VALUE`. The caller must keep `IA`
! and `JA` consistent with each other.
! @endwarning
!
! Examples
!
! - [test 1](|base|CSRSparsity/examples/GetColIndex_test_1.F90)
!
! See also: [[CSRSparsity_Method:obj_GetJA]]

INTERFACE SetJA
  MODULE PURE SUBROUTINE obj_SetJA(obj, indx, VALUE)
    TYPE(CSRSparsity_), INTENT(INOUT) :: obj
    !! sparsity pattern
    INTEGER(I4B), INTENT(IN) :: indx
    !! position in JA, from 1 to obj%nnz
    INTEGER(I4B), INTENT(IN) :: VALUE
    !! new value
  END SUBROUTINE obj_SetJA
END INTERFACE SetJA

!----------------------------------------------------------------------------
!                                                          GetIA@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-12-23
! summary: Get an entry of IA
!
! Returns the entry `obj%IA(irow)`.
!
! @warning
! The routine does not check `irow`. An index outside `IA` gives
! undefined behaviour.
! @endwarning
!
! Returns:
!   ans : INTEGER(I4B) - entry of IA
!
! Examples
!
! - [test 1](|base|CSRSparsity/examples/GetColIndex_test_1.F90)
!
! See also: [[CSRSparsity_Method:obj_SetIA]]

INTERFACE GetIA
  MODULE PURE FUNCTION obj_GetIA(obj, irow) RESULT(ans)
    TYPE(CSRSparsity_), INTENT(IN) :: obj
    !! sparsity pattern
    INTEGER(I4B), INTENT(IN) :: irow
    !! position in IA, from 1 to obj%nrow + 1
    INTEGER(I4B) :: ans
    !! entry of IA
  END FUNCTION obj_GetIA
END INTERFACE GetIA

!----------------------------------------------------------------------------
!                                                        GetJA@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-12-14
! summary: Get an entry of JA
!
! Returns the entry `obj%JA(indx)`.
!
! @warning
! The routine does not check `indx`. An index outside `JA` gives
! undefined behaviour.
! @endwarning
!
! Returns:
!   ans : INTEGER(I4B) - entry of JA
!
! Examples
!
! - [test 1](|base|CSRSparsity/examples/GetColIndex_test_1.F90)
!
! See also: [[CSRSparsity_Method:obj_SetJA]]

INTERFACE GetJA
  MODULE PURE FUNCTION obj_GetJA(obj, indx) RESULT(ans)
    TYPE(CSRSparsity_), INTENT(IN) :: obj
    !! sparsity pattern
    INTEGER(I4B), INTENT(IN) :: indx
    !! position in JA, from 1 to obj%nnz
    INTEGER(I4B) :: ans
    !! entry of JA
  END FUNCTION obj_GetJA
END INTERFACE GetJA

!----------------------------------------------------------------------------
!                                                         GetSym@SymMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-01-28
! summary: Get the symmetric pattern of one triangle
!
! Builds in `symObj` the pattern of a symmetric matrix.
! The matrix is defined by one triangle stored in `obj`.
! Use `from = "U"` for the upper triangle and `from = "L"` for the lower
! one. The letter is not case sensitive. The other triangle of `obj` is
! ignored.
!
! Each row of `symObj` lists its lower part, its diagonal entry and its
! upper part, in this order. The row is sorted when `obj` is sorted.
!
! `symObj%nnz` is twice the number of strict triangle entries plus the
! number of diagonal entries. `symObj%idiag(i)` is the position of the
! diagonal of row `i` in `symObj%JA`. It is 0 when `obj` has no diagonal
! entry in that row. `symObj%isDiagStored` is set to `.TRUE.`. The other
! flags, `idof` and `jdof` are copied from `obj`.
!
! With any other value of `from`, `symObj` stays unchanged. Debug builds
! also print an error message.
!
! @note
! The pattern in `obj` must be square.
! @endnote
!
! Changes:
!   - 2026-11-05: Fixed the last entry of symObj%IA for from = "U".
!
! Examples
!
! - [test 1](|base|CSRSparsity/examples/GetSym_test_1.F90)
!
! See also: [[CSRSparsity_Method:obj_GetNNZ2]]

INTERFACE GetSym
  MODULE SUBROUTINE obj_GetSym1(obj, symObj, from)
    TYPE(CSRSparsity_), INTENT(IN) :: obj
    !! sparsity pattern with the triangle
    TYPE(CSRSparsity_), INTENT(INOUT) :: symObj
    !! symmetric pattern
    CHARACTER(1), INTENT(IN) :: from
    !! "U" or "L", the triangle of obj to use
  END SUBROUTINE obj_GetSym1
END INTERFACE GetSym

!----------------------------------------------------------------------------
!                                                         GetSym@SymMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-01-28
! summary: Replace a pattern by its symmetric pattern (not implemented)
!
! This routine is meant to be the in-place form of
! [[CSRSparsity_Method:obj_GetSym1]]. It is not implemented yet.
!
! @note
! The routine does nothing. It leaves `obj` unchanged.
! @endnote
!
! See also: [[CSRSparsity_Method:obj_GetSym1]]

INTERFACE GetSym
  MODULE SUBROUTINE obj_GetSym2(obj, from)
    TYPE(CSRSparsity_), INTENT(INOUT) :: obj
    !! sparsity pattern
    CHARACTER(1), INTENT(IN) :: from
    !! "U" or "L", the triangle of obj to use
  END SUBROUTINE obj_GetSym2
END INTERFACE GetSym

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE CSRSparsity_Method
