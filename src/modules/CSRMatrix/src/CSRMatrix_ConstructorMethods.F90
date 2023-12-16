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

MODULE CSRMatrix_ConstructorMethods
USE GlobalData, ONLY: I4B, DFP, LGT
USE Basetype, ONLY: CSRMatrix_, DOF_, CSRSparsity_
IMPLICIT NONE
PRIVATE

PUBLIC :: Initiate
PUBLIC :: Shape
PUBLIC :: Size
PUBLIC :: TotalDimension
PUBLIC :: SetTotalDimension
PUBLIC :: GetNNZ
PUBLIC :: ALLOCATE
PUBLIC :: DEALLOCATE
PUBLIC :: ASSIGNMENT(=)
PUBLIC :: CSRMatrixAPLSB
PUBLIC :: CSRMatrixAPLSBSorted

!----------------------------------------------------------------------------
!                                                  Shape@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 22 March 2021
! summary: This function returns the shape of sparse matrix

INTERFACE Shape
  MODULE PURE FUNCTION obj_Shape(obj) RESULT(ans)
    TYPE(CSRMatrix_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans(2)
  END FUNCTION obj_Shape
END INTERFACE Shape

!----------------------------------------------------------------------------
!                                                    Size@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         22 March 2021
! summary: This function returns the size of sparse matrix
!
!# Introduction
!
! This function returns the size of sparse matrix
! If dims equal to 1 then total number of rows are returned
! If dims is equal to 2 then total number of columns are return
! If dims is absent then nrow*ncol are returned

INTERFACE Size
  MODULE PURE FUNCTION obj_Size(obj, dims) RESULT(ans)
    TYPE(CSRMatrix_), INTENT(IN) :: obj
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: dims
    INTEGER(I4B) :: ans
  END FUNCTION obj_Size
END INTERFACE Size

!----------------------------------------------------------------------------
!                                          TotalDimension@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         23 Feb 2021
! summary:         Returns the total dimension of an array

INTERFACE TotalDimension
  MODULE PURE FUNCTION obj_TotalDimension(obj) RESULT(ans)
    CLASS(CSRMatrix_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION obj_TotalDimension
END INTERFACE TotalDimension

!----------------------------------------------------------------------------
!                                       SetTotalDimension@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         23 Feb 2021
! summary:         This subroutine Set the total dimension (rank) of an array

INTERFACE SetTotalDimension
  MODULE PURE SUBROUTINE obj_SetTotalDimension(obj, tDimension)
    CLASS(CSRMatrix_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: tDimension
  END SUBROUTINE obj_SetTotalDimension
END INTERFACE SetTotalDimension

!----------------------------------------------------------------------------
!                                                 GetNNZ@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         22 March 2021
! summary:         Return the total number of non zero entry in the matrix

INTERFACE GetNNZ
  MODULE PURE FUNCTION obj_GetNNZ(obj) RESULT(ans)
    TYPE(CSRMatrix_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION obj_GetNNZ
END INTERFACE GetNNZ

!----------------------------------------------------------------------------
!                                                Allocate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 22 March 2021
! summary: This subroutine creates memeory space for the sparse matrix object
!
!# Introduction
!
! This subroutine creates memory space for the sparse matrix
!
! dims(1) denotes total number of rows
! dims(2) denotes total number of columns
! tDOF is Set to 1
! tNodes is Set to dims(1)
! nnz is Set to to 0

INTERFACE ALLOCATE
  MODULE SUBROUTINE obj_Allocate(obj, dims, matrixProp)
    TYPE(CSRMatrix_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: dims(2)
    CHARACTER(*), OPTIONAL, INTENT(IN) :: matrixProp
  END SUBROUTINE obj_Allocate
END INTERFACE ALLOCATE

!----------------------------------------------------------------------------
!                                              Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         22 March 2021
! summary:          This subroutine deallocates the data

INTERFACE DEALLOCATE
  MODULE SUBROUTINE obj_Deallocate(obj)
    TYPE(CSRMatrix_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_Deallocate
END INTERFACE DEALLOCATE

!----------------------------------------------------------------------------
!                                                Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         14 July 2021
! summary: This subroutine construct the `CSRMatrix_` object

INTERFACE Initiate
 MODULE SUBROUTINE obj_Initiate1(obj, ncol, nrow, idof, jdof, matrixProp, nnz)
    TYPE(CSRMatrix_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: ncol
    !! number of columns in sparse matrix
    INTEGER(I4B), INTENT(IN) :: nrow
    !! number of rows in sparse matrix
    TYPE(DOF_), OPTIONAL, INTENT(IN) :: idof, jdof
    !! degree of freedom object; It contains information like
    !! storage format (NODES_FMT, DOF_FMT), and names of physical variable
    !! space-time component in each physical variables
    !! Total number of nodes used for these physical variables
    CHARACTER(*), OPTIONAL, INTENT(IN) :: matrixProp
    !! Matrix is `SYM`, `UNSYM`
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: nnz
    !! number of non zeros
  END SUBROUTINE obj_Initiate1
END INTERFACE Initiate

!----------------------------------------------------------------------------
!                                                Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         14 July 2021
! summary: This subroutine construct the `CSRMatrix_` object
!
!# Introduction
! This subroutine Initiates an instance of [[CSRMatrix_]]. The object so
! created does not own the ownership of `obj%csr`. Instead it points to a
! [[CSRSparsity_]] object which is supplied by the user.
!
!@note
! The object `csr` should be Initiated by the user before sending it to
! CSR matrix via this routine. This is because this routine uses information
! such as ncol, nrow, nnz from the csr.
!@endnote

INTERFACE Initiate
  MODULE SUBROUTINE obj_Initiate2(obj, csr, matrixProp)
    TYPE(CSRMatrix_), INTENT(INOUT) :: obj
    TYPE(CSRSparsity_), INTENT(IN) :: csr
    !! number of columns in sparse matrix
    !! number of rows in sparse matrix
    !! degree of freedom object; It contains information like
    !! storage format (NODES_FMT, DOF_FMT), and names of physical variable
    !! space-time component in each physical variables
    !! Total number of nodes used for these physical variables
    CHARACTER(*), OPTIONAL, INTENT(IN) :: matrixProp
    !! Matrix is `SYM`, `UNSYM`
  END SUBROUTINE obj_Initiate2
END INTERFACE Initiate

!----------------------------------------------------------------------------
!                                                Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         22 March 2021
! summary: This subroutine constructs `sparsematrix_` object from IA, JA, A

INTERFACE Initiate
  MODULE SUBROUTINE obj_Initiate3(obj, A, IA, JA, matrixProp, ncol)
    TYPE(CSRMatrix_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: A(:)
    INTEGER(I4B), INTENT(IN) :: IA(:), JA(:)
    CHARACTER(*), OPTIONAL, INTENT(IN) :: matrixProp
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: ncol
    !! Number of columns in obj, default is number of rows
  END SUBROUTINE obj_Initiate3
END INTERFACE Initiate

!----------------------------------------------------------------------------
!                                                Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 16 July 2021
! summary: Initiate by copying
!
!# Introduction
! This routine Initiates obj by copying contents from obj2
! This routine is used in defining the assignment operator.

INTERFACE Initiate
  MODULE SUBROUTINE obj_Initiate4(obj, obj2)
    TYPE(CSRMatrix_), INTENT(INOUT) :: obj
    TYPE(CSRMatrix_), INTENT(IN) :: obj2
  END SUBROUTINE obj_Initiate4
END INTERFACE Initiate

INTERFACE ASSIGNMENT(=)
  MODULE PROCEDURE obj_Initiate4
END INTERFACE ASSIGNMENT(=)

!----------------------------------------------------------------------------
!                                                Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 14 July 2021
! summary: Initiates a submatrix

INTERFACE Initiate
  MODULE SUBROUTINE obj_Initiate5(obj, obj2, i1, i2, j1, j2)
    TYPE(CSRMatrix_), INTENT(INOUT) :: obj
    !! submatrix to be returned
    TYPE(CSRMatrix_), INTENT(IN) :: obj2
    !! csr matrix
    INTEGER(I4B), INTENT(IN) :: i1, i2
    !! start and end row indices
    INTEGER(I4B), INTENT(IN) :: j1, j2
    !! start and end col indices
  END SUBROUTINE obj_Initiate5
END INTERFACE Initiate

!----------------------------------------------------------------------------
!                                                Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 14 July 2021
! summary: This routine Initiates an instance of sparse matrix by copying
! the content of another object obj2
!
!# Introduction
!
! This method has been deprecated as it is same as `Initiate4`

INTERFACE Initiate
  MODULE SUBROUTINE obj_Initiate6(obj, obj2, hardCopy)
    TYPE(CSRMatrix_), INTENT(INOUT) :: obj
    TYPE(CSRMatrix_), INTENT(IN) :: obj2
    LOGICAL(LGT), INTENT(IN) :: hardCopy
  END SUBROUTINE obj_Initiate6
END INTERFACE Initiate

!----------------------------------------------------------------------------
!                                                           Initiate@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-12-16
! summary:  Initiate an object by adding two csrmatrix

INTERFACE Initiate
  MODULE SUBROUTINE obj_Initiate7(obj, obj1, obj2, scale, isSorted)
    TYPE(CSRMatrix_), INTENT(INOUT) :: obj
    TYPE(CSRMatrix_), INTENT(IN) :: obj1
    TYPE(CSRMatrix_), INTENT(IN) :: obj2
    REAL(DFP), INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isSorted
  END SUBROUTINE obj_Initiate7
END INTERFACE Initiate

!----------------------------------------------------------------------------
!                                                           Initiate@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-12-16
! summary:  Initiate an object by adding two csrmatrix

INTERFACE CSRMatrixAPLSB
  MODULE SUBROUTINE obj_aplsb(nrow, ncol, a, ja, ia, s, b, jb, ib, c,  &
    & jc, ic, nzmax, ierr)
    INTEGER(I4B), INTENT(IN) :: nrow
    INTEGER(I4B), INTENT(IN) :: ncol
    REAL(DFP), INTENT(IN) :: a(:)
    INTEGER(I4B), INTENT(IN) :: ja(:)
    INTEGER(I4B), INTENT(IN) :: ia(:)
    !! nrow + 1
    REAL(DFP), INTENT(IN) :: s
    REAL(DFP), INTENT(IN) :: b(:)
    INTEGER(I4B), INTENT(IN) :: jb(:)
    INTEGER(I4B), INTENT(IN) :: ib(:)
    !! nrow + 1
    REAL(DFP), INTENT(INOUT) :: c(:)
    !! The size of c should be less than or equalto nzmax
    INTEGER(I4B), INTENT(INOUT) :: jc(:)
    !! The size of jc should be less than or equalto nzmax
    INTEGER(I4B), INTENT(INOUT) :: ic(:)
    !! nrow + 1
    INTEGER(I4B), INTENT(IN) :: nzmax
    !! max number of nonzero in c
    INTEGER(I4B), INTENT(OUT) :: ierr
  END SUBROUTINE obj_aplsb
END INTERFACE CSRMatrixAPLSB

!----------------------------------------------------------------------------
!                                                           Initiate@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-12-16
! summary:  Initiate an object by adding two csrmatrix

INTERFACE CSRMatrixAPLSBSorted
  MODULE SUBROUTINE obj_aplsb_sorted(nrow, ncol, a, ja, ia, s, b, jb, ib,  &
    & c, jc, ic, nzmax, ierr)
    INTEGER(I4B), INTENT(IN) :: nrow
    INTEGER(I4B), INTENT(IN) :: ncol
    REAL(DFP), INTENT(IN) :: a(:)
    INTEGER(I4B), INTENT(IN) :: ja(:)
    INTEGER(I4B), INTENT(IN) :: ia(:)
    !! nrow + 1
    REAL(DFP), INTENT(IN) :: s
    !! scale
    REAL(DFP), INTENT(IN) :: b(:)
    INTEGER(I4B), INTENT(IN) :: jb(:)
    INTEGER(I4B), INTENT(IN) :: ib(:)
    !! nrow + 1
    REAL(DFP), INTENT(INOUT) :: c(:)
    !! The size of c should be less than or equalto nzmax
    INTEGER(I4B), INTENT(INOUT) :: jc(:)
    !! The size of jc should be less than or equalto nzmax
    INTEGER(I4B), INTENT(INOUT) :: ic(:)
    !! nrow + 1
    INTEGER(I4B), INTENT(IN) :: nzmax
    !! max number of nonzero in c
    INTEGER(I4B), INTENT(OUT) :: ierr
  END SUBROUTINE obj_aplsb_sorted
END INTERFACE CSRMatrixAPLSBSorted

END MODULE CSRMatrix_ConstructorMethods
