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
USE BaseType, ONLY: CSRMatrix_, DOF_, CSRSparsity_

PUBLIC :: Initiate
PUBLIC :: Shape
PUBLIC :: Size
PUBLIC :: TotalDimension
PUBLIC :: setTotalDimension
PUBLIC :: getNNZ
PUBLIC :: ALLOCATE
PUBLIC :: DEALLOCATE
PUBLIC :: ASSIGNMENT(=)

!----------------------------------------------------------------------------
!                                                  Shape@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         22 March 2021
! summary:         This function returns the shape of sparse matrix

INTERFACE
  MODULE PURE FUNCTION csrMat_Shape(obj) RESULT(Ans)
    TYPE(CSRMatrix_), INTENT(IN) :: obj
    INTEGER(I4B) :: Ans(2)
  END FUNCTION csrMat_Shape
END INTERFACE

INTERFACE Shape
  MODULE PROCEDURE csrMat_Shape
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
! If Dims equal to 1 then total number of rows are returned
! If Dims is equal to 2 then total number of columns are return
! If Dims is absent then nrow*ncol are returned

INTERFACE
  MODULE PURE FUNCTION csrMat_Size(obj, Dims) RESULT(Ans)
    TYPE(CSRMatrix_), INTENT(IN) :: obj
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: Dims
    INTEGER(I4B) :: Ans
  END FUNCTION csrMat_Size
END INTERFACE

INTERFACE Size
  MODULE PROCEDURE csrMat_Size
END INTERFACE Size

!----------------------------------------------------------------------------
!                                          TotalDimension@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         23 Feb 2021
! summary:         Returns the total dimension of an array

INTERFACE
  MODULE PURE FUNCTION csrMat_TotalDimension(obj) RESULT(Ans)
    CLASS(CSRMatrix_), INTENT(IN) :: obj
    INTEGER(I4B) :: Ans
  END FUNCTION csrMat_TotalDimension
END INTERFACE

INTERFACE TotalDimension
  MODULE PROCEDURE csrMat_TotalDimension
END INTERFACE TotalDimension

!----------------------------------------------------------------------------
!                                       setTotalDimension@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         23 Feb 2021
! summary:         This subroutine set the total dimension (rank) of an array

INTERFACE
  MODULE PURE SUBROUTINE csrMat_setTotalDimension(obj, tDimension)
    CLASS(CSRMatrix_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: tDimension
  END SUBROUTINE csrMat_setTotalDimension
END INTERFACE

INTERFACE setTotalDimension
  MODULE PROCEDURE csrMat_setTotalDimension
END INTERFACE setTotalDimension

!----------------------------------------------------------------------------
!                                                 getNNZ@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         22 March 2021
! summary:         Return the total number of non zero entry in the matrix

INTERFACE
  MODULE PURE FUNCTION csrMat_getNNZ(obj) RESULT(Ans)
    TYPE(CSRMatrix_), INTENT(IN) :: obj
    INTEGER(I4B) :: Ans
  END FUNCTION csrMat_getNNZ
END INTERFACE

INTERFACE getNNZ
  MODULE PROCEDURE csrMat_getNNZ
END INTERFACE getNNZ

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
! Dims(1) denotes total number of rows
! Dims(2) denotes total number of columns
! tDOF is set to 1
! tNodes is set to Dims(1)
! nnz is set to to 0

INTERFACE
  MODULE SUBROUTINE csrMat_Allocate(obj, Dims, MatrixProp)
    TYPE(CSRMatrix_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: Dims(2)
    CHARACTER(*), OPTIONAL, INTENT(IN) :: MatrixProp
  END SUBROUTINE csrMat_Allocate
END INTERFACE

INTERFACE ALLOCATE
  MODULE PROCEDURE csrMat_Allocate
END INTERFACE ALLOCATE

!----------------------------------------------------------------------------
!                                              Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         22 March 2021
! summary:          This subroutine deallocates the data

INTERFACE
  MODULE SUBROUTINE csrMat_Deallocate(obj)
    TYPE(CSRMatrix_), INTENT(INOUT) :: obj
  END SUBROUTINE csrMat_Deallocate
END INTERFACE

INTERFACE DEALLOCATE
  MODULE PROCEDURE csrMat_Deallocate
END INTERFACE DEALLOCATE

!----------------------------------------------------------------------------
!                                                Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         14 July 2021
! summary: This subroutine construct the `CSRMatrix_` object

INTERFACE
  MODULE SUBROUTINE csrMat_initiate1(obj, ncol, nrow, idof, jdof, matrixProp)
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
    CHARACTER(*), OPTIONAL, INTENT(IN) :: MatrixProp
    !! Matrix is `SYM`, `UNSYM`
  END SUBROUTINE csrMat_initiate1
END INTERFACE

INTERFACE Initiate
  MODULE PROCEDURE csrMat_initiate1
END INTERFACE Initiate

!----------------------------------------------------------------------------
!                                                Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         14 July 2021
! summary: This subroutine construct the `CSRMatrix_` object
!
!# Introduction
! This subroutine initiates an instance of [[CSRMatrix_]]. The object so
! created does not own the ownership of `obj%csr`. Instead it points to a
! [[CSRSparsity_]] object which is supplied by the user.
!
!@note
! The object `csr` should be initiated by the user before sending it to
! CSR matrix via this routine. This is because this routine uses information
! such as ncol, nrow, nnz from the csr.
!@endnote

INTERFACE
  MODULE SUBROUTINE csrMat_initiate2(obj, csr, matrixProp)
    TYPE(CSRMatrix_), INTENT(INOUT) :: obj
    TYPE(CSRSparsity_), INTENT(IN) :: csr
    !! number of columns in sparse matrix
    !! number of rows in sparse matrix
    !! degree of freedom object; It contains information like
    !! storage format (NODES_FMT, DOF_FMT), and names of physical variable
    !! space-time component in each physical variables
    !! Total number of nodes used for these physical variables
    CHARACTER(*), OPTIONAL, INTENT(IN) :: MatrixProp
    !! Matrix is `SYM`, `UNSYM`
  END SUBROUTINE csrMat_initiate2
END INTERFACE

INTERFACE Initiate
  MODULE PROCEDURE csrMat_initiate2
END INTERFACE Initiate

!----------------------------------------------------------------------------
!                                                Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         22 March 2021
! summary: This subroutine constructs `sparsematrix_` object from IA, JA, A

INTERFACE
  MODULE SUBROUTINE csrMat_initiate3(obj, A, IA, JA, MatrixProp)
    TYPE(CSRMatrix_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: A(:)
    INTEGER(I4B), INTENT(IN) :: IA(:), JA(:)
    CHARACTER(*), OPTIONAL, INTENT(IN) :: MatrixProp
  END SUBROUTINE csrMat_initiate3
END INTERFACE

INTERFACE Initiate
  MODULE PROCEDURE csrMat_initiate3
END INTERFACE Initiate

!----------------------------------------------------------------------------
!                                                Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 16 July 2021
! summary: Initiate by copying
!
!# Introduction
! This routine initiates obj by copying contents from obj2
! This routine is used in defining the assignment operator.

INTERFACE
  MODULE SUBROUTINE csrMat_initiate4(obj, obj2)
    TYPE(CSRMatrix_), INTENT(INOUT) :: obj
    TYPE(CSRMatrix_), INTENT(IN) :: obj2
  END SUBROUTINE csrMat_initiate4
END INTERFACE

INTERFACE Initiate
  MODULE PROCEDURE csrMat_initiate4
END INTERFACE Initiate

INTERFACE ASSIGNMENT(=)
  MODULE PROCEDURE csrMat_initiate4
END INTERFACE ASSIGNMENT(=)

!----------------------------------------------------------------------------
!                                                Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 14 July 2021
! summary: Initiates a submatrix

INTERFACE
  MODULE SUBROUTINE csrMat_initiate5(obj, obj2, i1, i2, j1, j2)
    TYPE(CSRMatrix_), INTENT(INOUT) :: obj
    !! submatrix to be returned
    TYPE(CSRMatrix_), INTENT(IN) :: obj2
    !! csr matrix
    INTEGER(I4B), INTENT(IN) :: i1, i2
    !! start and end row indices
    INTEGER(I4B), INTENT(IN) :: j1, j2
    !! start and end col indices
  END SUBROUTINE csrMat_initiate5
END INTERFACE

INTERFACE Initiate
  MODULE PROCEDURE csrMat_initiate5
END INTERFACE Initiate

!----------------------------------------------------------------------------
!                                                Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 14 July 2021
! summary: This routine initiates an instance of sparse matrix by copying
! the content of another object obj2
!
!# Introduction
!
! This method has been deprecated as it is same as `Initiate4`

INTERFACE
  MODULE SUBROUTINE csrMat_initiate6(obj, obj2, hardCopy)
    TYPE(CSRMatrix_), INTENT(INOUT) :: obj
    TYPE(CSRMatrix_), INTENT(IN) :: obj2
    LOGICAL(LGT), INTENT(IN) :: hardCopy
  END SUBROUTINE csrMat_initiate6
END INTERFACE

INTERFACE Initiate
  MODULE PROCEDURE csrMat_initiate6
END INTERFACE Initiate

END MODULE CSRMatrix_ConstructorMethods
