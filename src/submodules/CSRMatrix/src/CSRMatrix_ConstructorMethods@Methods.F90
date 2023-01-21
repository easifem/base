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
! date:         22 March 2021
! summary: This submodule contains method for constructing [[CSRMatrix_]]

SUBMODULE(CSRMatrix_ConstructorMethods) Methods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                     Shape
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_Shape
Ans = [obj%csr%nrow, obj%csr%ncol]
END PROCEDURE csrMat_Shape

!----------------------------------------------------------------------------
!                                                                       Size
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_Size
IF (PRESENT(Dims)) THEN
  IF (Dims .EQ. 1) THEN
    Ans = obj%csr%nrow
  ELSE
    Ans = obj%csr%ncol
  END IF
ELSE
  Ans = obj%csr%nrow * obj%csr%ncol
END IF
END PROCEDURE csrMat_Size

!----------------------------------------------------------------------------
!                                                            TotalDimension
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_TotalDimension
ans = obj%tDimension
END PROCEDURE csrMat_TotalDimension

!----------------------------------------------------------------------------
!                                                         setTotalDimension
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_setTotalDimension
obj%tDimension = tDimension
END PROCEDURE csrMat_setTotalDimension

!----------------------------------------------------------------------------
!                                                                    getNNZ
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_getNNZ
Ans = obj%csr%nnz
END PROCEDURE csrMat_getNNZ

!----------------------------------------------------------------------------
!                                                               Allocate
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_Allocate
CALL Initiate(obj=obj, ncol=dims(2), nrow=dims(1), matrixProp=matrixProp)
END PROCEDURE csrMat_Allocate

!----------------------------------------------------------------------------
!                                                            Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_Deallocate
CALL Deallocate (obj%csr)
obj%csrOwnership = .FALSE.
obj%tDimension = 2
obj%MatrixProp = 'UNSYM'
IF (ALLOCATED(obj%A)) DEALLOCATE (obj%A)
END PROCEDURE csrMat_Deallocate

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_initiate1
CALL Deallocate (obj)
obj%csrOwnership = .TRUE.
IF (PRESENT(matrixProp)) obj%matrixProp = TRIM(matrixProp)
CALL Initiate(obj=obj%csr, ncol=ncol, nrow=nrow, idof=idof, jdof=jdof)
CALL Reallocate(obj%A, obj%csr%nnz)
CALL setTotalDimension(obj, 2_I4B)
END PROCEDURE csrMat_initiate1

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_initiate2
!!
IF (.NOT. csr%isInitiated) THEN
  CALL ErrorMSG( &
    & "Instance of CSRSparsity is not initiated!", &
    & "CSRMatrix_Method@ConstructorMethods.F90", &
    & "csrMat_initiate2()", &
    & __LINE__, stderr)
  STOP
END IF
!!
CALL Deallocate (obj)
obj%csrOwnership = .TRUE.
IF (PRESENT(matrixProp)) obj%matrixProp = TRIM(matrixProp)
obj%csr = csr
CALL Reallocate(obj%A, obj%csr%nnz)
CALL setTotalDimension(obj, 2_I4B)
!!
END PROCEDURE csrMat_initiate2

!----------------------------------------------------------------------------
!                                                                  Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_initiate3
obj%csrOwnership = .TRUE.
IF (PRESENT(matrixProp)) obj%matrixProp = TRIM(matrixProp)
CALL Initiate(obj=obj%csr, IA=IA, JA=JA)
obj%A = A
CALL setTotalDimension(obj, 2_I4B)
CALL setSparsity(obj)
END PROCEDURE csrMat_initiate3

!----------------------------------------------------------------------------
!                                                                 Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_Initiate4
obj%csr = obj2%csr
obj%tDimension = obj2%tDimension
obj%csrOwnership = obj2%csrOwnership
obj%matrixProp = obj2%matrixProp
IF (ALLOCATED(obj2%A)) obj%A = obj2%A
END PROCEDURE csrMat_Initiate4

!----------------------------------------------------------------------------
!                                                                 Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_Initiate5
INTEGER(I4B) :: nrow, ncol, nnz, job
INTEGER(I4B), ALLOCATABLE :: IA(:), JA(:)
REAL(DFP), ALLOCATABLE :: A(:)
job = 1
nrow = i2 - i1 + 1
ncol = j2 - j1 + 1
nnz = obj2%csr%nnz
ALLOCATE (A(nnz), IA(nrow + 1), JA(nnz))
A = 0.0; IA = 0; JA = 0
!! calling from Sparsekit
CALL SUBMAT(job, i1, i2, j1, j2, obj2%A, obj2%csr%JA, obj2%csr%IA,&
  & nrow, ncol, A, JA, IA)
!!
nnz = IA(nrow + 1) - 1
CALL initiate(obj=obj, A=A(1:nnz), IA=IA, JA=JA(1:nnz))
obj%csr%ncol = ncol
DEALLOCATE (IA, JA, A)
END PROCEDURE csrMat_Initiate5

!----------------------------------------------------------------------------
!                                                                 Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_Initiate6
CALL csrMat_Initiate4(obj=obj, obj2=obj2)
END PROCEDURE csrMat_Initiate6

END SUBMODULE Methods