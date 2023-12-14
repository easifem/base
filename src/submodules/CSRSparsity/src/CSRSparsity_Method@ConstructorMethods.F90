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
! date: 13 July 2021
! summary:         Methods related to CSR sparsity

SUBMODULE(CSRSparsity_Method) ConstructorMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                              Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE csr_initiate1
#ifdef DEBUG_VER
INTEGER(I4B) :: tNodes1, tNodes2
LOGICAL(LGT) :: isok, problem
#endif

#ifdef DEBUG_VER

problem = PRESENT(idof) .AND. (.NOT. PRESENT(jdof))
IF (problem) THEN
  CALL ErrorMSG( &
    & "When idof is present, jdof should be present too.", &
    & "CSRSparsity_Method@Constructor.F90", &
    & "csr_initiate1()", &
    & __LINE__, stderr)
  STOP
END IF

problem = PRESENT(jdof) .AND. (.NOT. PRESENT(idof))
IF (problem) THEN
  CALL ErrorMSG( &
    & "When jdof is present, idof should be present too.", &
    & "CSRSparsity_Method@Constructor.F90", &
    & "csr_initiate1()", &
    & __LINE__, stderr)
  STOP
END IF

isok = PRESENT(idof)
IF (isok) THEN
  tnodes1 = .tNodes.idof
  tnodes2 = .tNodes.jdof
  problem = tnodes1 .NE. nrow .OR. tnodes2 .NE. ncol
  IF (problem) THEN
    CALL ErrorMSG( &
    & "Size of the matrix does not conform with the dof data!", &
    & "CSRSparsity_Method@Constructor.F90", &
    & "csr_initiate1()", &
    & __LINE__, stderr)
    STOP
  END IF
END IF
#endif

CALL DEALLOCATE (obj)

obj%nnz = Input(default=0_I4B, option=nnz)
obj%ncol = ncol
obj%nrow = nrow

IF (PRESENT(idof)) THEN
  obj%idof = idof
ELSE
  CALL Initiate(obj=obj%idof, tNodes=[nrow], names=['K'], &
    & spacecompo=[1], timecompo=[1], storageFMT=NODES_FMT)
END IF

IF (PRESENT(jdof)) THEN
  obj%jdof = jdof
ELSE
  CALL Initiate(obj=obj%jdof, tNodes=[ncol], names=['K'], &
    & spacecompo=[1], timecompo=[1], storageFMT=NODES_FMT)
END IF

CALL Reallocate(obj%IA, nrow + 1)
CALL Reallocate(obj%idiag, nrow)

IF (obj%nnz .GT. 0) THEN
  ALLOCATE (obj%row(obj%nnz), obj%JA(obj%nnz))
END IF

END PROCEDURE csr_initiate1

!----------------------------------------------------------------------------
!                                                                 Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE csr_initiate2
INTEGER(I4B) :: ii, n
obj%nnz = obj2%nnz
obj%ncol = obj2%ncol
obj%nrow = obj2%nrow
obj%isSorted = obj2%isSorted
obj%isInitiated = obj2%isInitiated
obj%isSparsityLock = obj2%isSparsityLock
obj%isDiagStored = obj2%isDiagStored
IF (ALLOCATED(obj2%IA)) obj%IA = obj2%IA
IF (ALLOCATED(obj2%JA)) obj%JA = obj2%JA
IF (ALLOCATED(obj2%idiag)) obj%idiag = obj2%idiag
IF (ALLOCATED(obj%row)) THEN
  n = SIZE(obj%row)
  DO ii = 1, n
    obj%row(ii) = obj2%row(ii)
  END DO
END IF
obj%idof = obj2%idof
obj%jdof = obj2%jdof
END PROCEDURE csr_initiate2

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE csr_Initiate3
INTEGER(I4B) :: nrow, ncol

nrow = SIZE(IA) - 1; ncol = MAXVAL(JA)
CALL Initiate(obj=obj, nrow=nrow, ncol=ncol)
obj%nnz = SIZE(JA)
obj%IA = IA
obj%JA = JA

END PROCEDURE csr_Initiate3

!----------------------------------------------------------------------------
!                                                                CSRSparsity
!----------------------------------------------------------------------------

MODULE PROCEDURE csr_constructor1
CALL Initiate(obj=ans, ncol=ncol, nrow=nrow, idof=idof, jdof=jdof)
END PROCEDURE csr_constructor1

!----------------------------------------------------------------------------
!                                                                CSRSparsity
!----------------------------------------------------------------------------

MODULE PROCEDURE csr_constructor2
CALL Initiate(obj=ans, IA=IA, JA=JA)
END PROCEDURE csr_constructor2

!----------------------------------------------------------------------------
!                                                                CSRSparsity
!----------------------------------------------------------------------------

MODULE PROCEDURE csr_constructor_1
ALLOCATE (CSRSparsity_ :: ans)
CALL Initiate(obj=ans, ncol=ncol, nrow=nrow, idof=idof, jdof=jdof)
END PROCEDURE csr_constructor_1

!----------------------------------------------------------------------------
!                                                                CSRSparsity
!----------------------------------------------------------------------------

MODULE PROCEDURE csr_constructor_2
ALLOCATE (CSRSparsity_ :: ans)
CALL Initiate(obj=ans, IA=IA, JA=JA)
END PROCEDURE csr_constructor_2

!----------------------------------------------------------------------------
!                                                            Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE csr_Deallocate
IF (ALLOCATED(obj%IA)) DEALLOCATE (obj%IA)
IF (ALLOCATED(obj%JA)) DEALLOCATE (obj%JA)
IF (ALLOCATED(obj%idiag)) DEALLOCATE (obj%idiag)
IF (ALLOCATED(obj%Row)) DEALLOCATE (obj%Row)
CALL DEALLOCATE (obj%idof)
CALL DEALLOCATE (obj%jdof)
obj%nnz = 0
obj%nrow = 0
obj%ncol = 0
obj%isSorted = .FALSE.
obj%isInitiated = .FALSE.
obj%isSparsityLock = .FALSE.
obj%isDiagStored = .FALSE.
END PROCEDURE csr_Deallocate

END SUBMODULE ConstructorMethods
