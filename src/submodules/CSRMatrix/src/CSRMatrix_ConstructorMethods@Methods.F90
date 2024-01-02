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

MODULE PROCEDURE obj_Shape
Ans = [obj%csr%nrow, obj%csr%ncol]
END PROCEDURE obj_Shape

!----------------------------------------------------------------------------
!                                                                       Size
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Size
IF (PRESENT(Dims)) THEN
  IF (Dims .EQ. 1) THEN
    Ans = obj%csr%nrow
  ELSE
    Ans = obj%csr%ncol
  END IF
ELSE
  Ans = obj%csr%nrow * obj%csr%ncol
END IF
END PROCEDURE obj_Size

!----------------------------------------------------------------------------
!                                                            TotalDimension
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_TotalDimension
ans = obj%tDimension
END PROCEDURE obj_TotalDimension

!----------------------------------------------------------------------------
!                                                         SetTotalDimension
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetTotalDimension
obj%tDimension = tDimension
END PROCEDURE obj_SetTotalDimension

!----------------------------------------------------------------------------
!                                                                    getNNZ
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_getNNZ
Ans = obj%csr%nnz
END PROCEDURE obj_getNNZ

!----------------------------------------------------------------------------
!                                                               Allocate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Allocate
CALL Initiate(obj=obj, ncol=dims(2), nrow=dims(1), matrixProp=matrixProp)
END PROCEDURE obj_Allocate

!----------------------------------------------------------------------------
!                                                            Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Deallocate
CALL DEALLOCATE (obj%csr)
obj%csrOwnership = .FALSE.
obj%tDimension = 2
obj%MatrixProp = 'UNSYM'
IF (ALLOCATED(obj%A)) DEALLOCATE (obj%A)
#ifdef USE_SuperLU
CALL SuperluDeallocate(obj)
#endif
END PROCEDURE obj_Deallocate

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate1
CALL DEALLOCATE (obj)
obj%csrOwnership = .TRUE.
IF (PRESENT(matrixProp)) obj%matrixProp = TRIM(matrixProp)
CALL Initiate(obj=obj%csr, ncol=ncol, nrow=nrow, idof=idof, jdof=jdof,  &
& nnz=nnz)
CALL Reallocate(obj%A, obj%csr%nnz)
CALL SetTotalDimension(obj, 2_I4B)
END PROCEDURE obj_Initiate1

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate2
IF (.NOT. csr%isInitiated) THEN
  CALL ErrorMSG( &
    & "Instance of CSRSparsity is not Initiated!", &
    & __FILE__, &
    & "obj_Initiate2()", &
    & __LINE__, stderr)
  STOP
END IF

CALL DEALLOCATE (obj)
obj%csrOwnership = .TRUE.
IF (PRESENT(matrixProp)) obj%matrixProp = TRIM(matrixProp)
obj%csr = csr
CALL Reallocate(obj%A, obj%csr%nnz)
CALL SetTotalDimension(obj, 2_I4B)

END PROCEDURE obj_Initiate2

!----------------------------------------------------------------------------
!                                                                  Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate3
CALL Initiate(obj=obj%csr, IA=IA, JA=JA, ncol=ncol)
obj%csrOwnership = .TRUE.
IF (PRESENT(matrixProp)) obj%matrixProp = TRIM(matrixProp)
CALL Reallocate(obj%A, SIZE(A))
#ifdef USE_BLAS95
CALL Copy(y=obj%A, x=A)
#else
obj%A = A
#endif
CALL SetTotalDimension(obj, 2_I4B)
CALL SetSparsity(obj)
END PROCEDURE obj_Initiate3

!----------------------------------------------------------------------------
!                                                                 Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate4
CALL DEALLOCATE (obj)
obj%csr = obj2%csr
obj%tDimension = obj2%tDimension
obj%csrOwnership = obj2%csrOwnership
obj%matrixProp = obj2%matrixProp
IF (ALLOCATED(obj2%A)) obj%A = obj2%A
END PROCEDURE obj_Initiate4

!----------------------------------------------------------------------------
!                                                                 Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate5
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
CALL Initiate(obj=obj, A=A(1:nnz), IA=IA, JA=JA(1:nnz))
obj%csr%ncol = ncol
DEALLOCATE (IA, JA, A)
END PROCEDURE obj_Initiate5

!----------------------------------------------------------------------------
!                                                                 Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate6
CALL obj_Initiate4(obj=obj, obj2=obj2)
END PROCEDURE obj_Initiate6

!----------------------------------------------------------------------------
!                                                                 Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate7
INTEGER(I4B) :: nzmax, nrow, ncol, ierr
TYPE(DOF_), POINTER :: idof, jdof
LOGICAL(LGT) :: case1, case2, isSorted0

nzmax = GetNNZ(obj1=obj1%csr, obj2=obj2%csr, isSorted=isSorted, op="+")

nrow = SIZE(obj1, 1)
ncol = SIZE(obj2, 2)

NULLIFY (idof, jdof)
idof => GetDOFPointer(obj1, 1)
jdof => GetDOFPointer(obj1, 2)

case1 = ASSOCIATED(idof)
case2 = ASSOCIATED(jdof)

IF (case1 .AND. case2) THEN
  CALL Initiate(obj=obj, ncol=ncol, nrow=nrow, idof=idof,  &
    & jdof=jdof, nnz=nzmax)
ELSEIF (case1 .AND. .NOT. case2) THEN
  CALL Initiate(obj=obj, ncol=ncol, nrow=nrow, idof=idof, nnz=nzmax)
ELSEIF (.NOT. case1 .AND. case2) THEN
  CALL Initiate(obj=obj, ncol=ncol, nrow=nrow, jdof=jdof, nnz=nzmax)
ELSE
  CALL Errormsg( &
    & "Some error occured while getting idof and jdof",  &
    & __FILE__,  &
    & "obj_Initiate7()",  &
    & __LINE__,  &
    & unitno=stderr)
  RETURN
END IF

isSorted0 = Input(default=.FALSE., option=isSorted)

IF (isSorted0) THEN
  CALL obj_aplsb_sorted(nrow=nrow, ncol=ncol,  &
    & a=obj1%A, ja=obj1%csr%JA, ia=obj1%csr%IA, s=scale,  &
    & b=obj2%A, jb=obj2%csr%JA, ib=obj2%csr%IA,  &
    & c=obj%A, jc=obj%csr%JA, ic=obj%csr%IA, nzmax=nzmax, ierr=ierr)
ELSE
  CALL obj_aplsb(nrow=nrow, ncol=ncol,  &
    & a=obj1%A, ja=obj1%csr%JA, ia=obj1%csr%IA, s=scale,  &
    & b=obj2%A, jb=obj2%csr%JA, ib=obj2%csr%IA,  &
    & c=obj%A, jc=obj%csr%JA, ic=obj%csr%IA, nzmax=nzmax, ierr=ierr)
END IF

IF (ierr .NE. 0) THEN
  CALL Errormsg( &
    & "Some error occured while calling obj_aplsb(_sorted) method",  &
    & __FILE__,  &
    & "obj_Initiate7()",  &
    & __LINE__,  &
    & unitno=stderr)
  RETURN
END IF

END PROCEDURE obj_Initiate7

!----------------------------------------------------------------------------
!                                                                 Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_aplsb
! internal variables
INTEGER(I4B) :: tsize, j, ii, ka, jcol, kb, jpos, iw(ncol), k

ierr = 0
tsize = 0
ic(1) = 1
iw = 0

DO ii = 1, nrow
  ! copy row ii  to C
  DO ka = ia(ii), ia(ii + 1) - 1
    tsize = tsize + 1
    jcol = ja(ka)
    IF (tsize .GT. nzmax) THEN
      ierr = ii
      RETURN
    END IF
    jc(tsize) = jcol
    c(tsize) = a(ka)
    iw(jcol) = tsize
  END DO

  DO kb = ib(ii), ib(ii + 1) - 1
    jcol = jb(kb)
    jpos = iw(jcol)
    IF (jpos .EQ. 0) THEN
      tsize = tsize + 1
      IF (tsize .GT. nzmax) THEN
        ierr = ii
        RETURN
      END IF
      jc(tsize) = jcol
      c(tsize) = s * b(kb)
      iw(jcol) = tsize
    ELSE
      c(jpos) = c(jpos) + s * b(kb)
    END IF
  END DO
  DO k = ic(ii), tsize
    iw(jc(k)) = 0
  END DO
  ic(ii + 1) = tsize + 1
END DO

END PROCEDURE obj_aplsb

!----------------------------------------------------------------------------
!                                                       CSRMatrixAPLSBSorted
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_aplsb_sorted
! internal variables
INTEGER(I4B) :: len, i, ka, kb, kc, &
  & kamax, kbmax, j1, jj2
LOGICAL(LGT) :: isok

ierr = 0
kc = 1
ic(1) = kc

DO i = 1, nrow
  ka = ia(i)
  kb = ib(i)
  kamax = ia(i + 1) - 1
  kbmax = ib(i + 1) - 1

  DO
    isok = ka .LE. kamax .OR. kb .LE. kbmax
    IF (.NOT. isok) THEN
      ic(i + 1) = kc
      EXIT
    END IF

    IF (ka .LE. kamax) THEN
      j1 = ja(ka)
    ELSE
      ! take j1 large enough  that always jj2 .lt. j1
      j1 = ncol + 1
    END IF

    IF (kb .LE. kbmax) THEN
      jj2 = jb(kb)
    ELSE
      ! similarly take jj2 large enough  that always j1 .lt. jj2
      jj2 = ncol + 1
    END IF

    IF (j1 .EQ. jj2) THEN
      c(kc) = a(ka) + s * b(kb)
      jc(kc) = j1
      ka = ka + 1
      kb = kb + 1
      kc = kc + 1
    ELSE IF (j1 .LT. jj2) THEN
      jc(kc) = j1
      c(kc) = a(ka)
      ka = ka + 1
      kc = kc + 1
    ELSE IF (j1 .GT. jj2) THEN
      jc(kc) = jj2
      c(kc) = s * b(kb)
      kb = kb + 1
      kc = kc + 1
    END IF

    IF (kc .GT. nzmax) THEN
      ierr = i
      RETURN
    END IF
  END DO
END DO

END PROCEDURE obj_aplsb_sorted

END SUBMODULE Methods
