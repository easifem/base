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

SUBMODULE(RealMatrix_Method) ConstructorMethods
USE ReallocateUtility, ONLY: Reallocate
USE ConvertUtility, ONLY: UtilConvert => Convert

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                      Shape
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Shape
LOGICAL(LGT) :: isok

isok = ALLOCATED(obj%val)
IF (isok) THEN
  ans = SHAPE(obj%val)
ELSE
  ans = 0
END IF
END PROCEDURE obj_Shape

!----------------------------------------------------------------------------
!                                                                       Size
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Size
INTEGER(I4B) :: s(2)
LOGICAL(LGT) :: isok

isok = ALLOCATED(obj%val)

IF (isok) THEN
  s = SHAPE(obj%val)

  isok = PRESENT(dims)
  IF (isok) THEN
    ans = s(dims)
  ELSE
    ans = s(1) * s(2)
  END IF

ELSE
  ans = 0
END IF
END PROCEDURE obj_Size

!----------------------------------------------------------------------------
!                                                         getTotalDimension
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_TotalDimension
ans = obj%tDimension
END PROCEDURE obj_TotalDimension

!----------------------------------------------------------------------------
!                                                         setTotalDimension
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetTotalDimension
obj%tDimension = tDimension
END PROCEDURE obj_SetTotalDimension

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Allocate
CALL Reallocate(obj%val, dims(1), dims(2))
CALL SetTotalDimension(obj, 2_I4B)
END PROCEDURE obj_Allocate

!----------------------------------------------------------------------------
!                                                             Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Deallocate
LOGICAL(LGT) :: isok
isok = ALLOCATED(obj%val)
IF (isok) DEALLOCATE (obj%val)
CALL SetTotalDimension(obj, 0)
END PROCEDURE obj_Deallocate

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate1
CALL ALLOCATE (obj, dims)
END PROCEDURE obj_Initiate1

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate2
CALL ALLOCATE (obj, [nrow, ncol])
END PROCEDURE obj_Initiate2

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate3
INTEGER(I4B) :: j, tsize
tsize = SIZE(obj)
DO j = 1, tsize
  CALL ALLOCATE (obj(j), Dims)
END DO
END PROCEDURE obj_Initiate3

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate4
INTEGER(I4B) :: j, tsize

tsize = SIZE(obj)
DO j = 1, tsize
  CALL ALLOCATE (obj(j), dims(j, :))
END DO
END PROCEDURE obj_Initiate4

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate5
obj%val = val
CALL SetTotalDimension(obj, 2_I4B)
END PROCEDURE obj_Initiate5

!----------------------------------------------------------------------------
!                                                                     Matrix
!----------------------------------------------------------------------------

MODULE PROCEDURE Constructor1
CALL Initiate(obj, dims)
END PROCEDURE Constructor1

!----------------------------------------------------------------------------
!                                                                        Eye
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Eye1
INTEGER(I4B) :: i

CALL Initiate(ans, [m, m])
DO i = 1, m
  ans%val(i, i) = 1.0
END DO

END PROCEDURE obj_Eye1

!----------------------------------------------------------------------------
!                                                                    Convert
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Convert1
CALL UtilConvert(from=from%val, to=to%val, conversion=conversion, nns=nns, &
                 tdof=tdof)
END PROCEDURE obj_Convert1

!----------------------------------------------------------------------------
!                                                                        Sym
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Sym1
ans%val = 0.5_DFP * (obj%val + TRANSPOSE(obj%val))
END PROCEDURE obj_Sym1

!----------------------------------------------------------------------------
!                                                                        Sym
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Sym2
ans = 0.5_DFP * (obj + TRANSPOSE(obj))
END PROCEDURE obj_Sym2

!----------------------------------------------------------------------------
!                                                                    SkewSym
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SkewSym1
ans%val = 0.5_DFP * (obj%val - TRANSPOSE(obj%val))
END PROCEDURE obj_SkewSym1

!----------------------------------------------------------------------------
!                                                                    SkewSym
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SkewSym2
ans = 0.5_DFP * (obj - TRANSPOSE(obj))
END PROCEDURE obj_SkewSym2

!----------------------------------------------------------------------------
!                                                         MakeDiagonalCopies
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_MakeDiagonalCopies1
INTEGER(I4B) :: I, s(2)
REAL(DFP), ALLOCATABLE :: DummyMat2(:, :)

IF (ALLOCATED(mat)) THEN
  s = SHAPE(mat)
  DummyMat2 = mat
  CALL Reallocate(mat, s(1) * nCopy, s(2) * nCopy)
  DO I = 1, nCopy
    mat((I - 1) * s(1) + 1:I * s(1), &
    & (I - 1) * s(2) + 1:I * s(2)) &
    & = DummyMat2(:, :)
  END DO
  DEALLOCATE (DummyMat2)
END IF
END PROCEDURE obj_MakeDiagonalCopies1

!----------------------------------------------------------------------------
!                                                       MakeDiaginalCopies
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_MakeDiagonalCopies1_
INTEGER(I4B) :: ii, jj, kk

DO ii = 2, ncopy
  DO CONCURRENT(jj=1:nrow, kk=1:ncol)
    mat((ii - 1) * nrow + jj, (ii - 1) * ncol + kk) = mat(jj, kk)
  END DO
END DO

END PROCEDURE obj_MakeDiagonalCopies1_

!----------------------------------------------------------------------------
!                                                         MakeDiagonalCopies
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_MakeDiagonalCopies2
INTEGER(I4B) :: I, S(2)

S = SHAPE(From)
CALL Reallocate(To, S(1) * nCopy, S(2) * nCopy)
To = 0.0_DFP
DO I = 1, nCopy
  To((I - 1) * S(1) + 1:I * S(1), &
  & (I - 1) * S(2) + 1:I * S(2)) &
  & = From(:, :)
END DO
END PROCEDURE obj_MakeDiagonalCopies2

!----------------------------------------------------------------------------
!                                                         MakeDiagonalCopies
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_MakeDiagonalCopies2_
INTEGER(I4B) :: ii, jj, kk, nrow, ncol

nrow = SIZE(from, 1)
ncol = SIZE(from, 2)

DO ii = 1, ncopy
  DO CONCURRENT(jj=1:nrow, kk=1:ncol)
    to((ii - 1) * nrow + jj, (ii - 1) * ncol + kk) = from(jj, kk)
  END DO
END DO
END PROCEDURE obj_MakeDiagonalCopies2_

!----------------------------------------------------------------------------
!                                                         MakeDiagonalCopies
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_MakeDiagonalCopies3
CALL MakeDiagonalCopies(mat=mat%val, nCopy=nCopy)
END PROCEDURE obj_MakeDiagonalCopies3

!----------------------------------------------------------------------------
!                                                         MakeDiagonalCopies
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_MakeDiagonalCopies4
CALL MakeDiagonalCopies(from=from%val, to=to%val, &
                        nCopy=nCopy)
END PROCEDURE obj_MakeDiagonalCopies4

!----------------------------------------------------------------------------
!                                                              Random_Number
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Random_Number1
LOGICAL(LGT) :: isok

isok = PRESENT(m) .AND. PRESENT(n)
IF (isok) THEN
  CALL Reallocate(obj%val, m, n)
  CALL RANDOM_NUMBER(obj%val)
  RETURN
END IF

isok = PRESENT(m)
IF (isok) THEN
  CALL Reallocate(obj%val, m, m)
  CALL RANDOM_NUMBER(obj%val)
  RETURN
END IF

isok = PRESENT(n)
IF (isok) THEN
  CALL Reallocate(obj%val, n, n)
  CALL RANDOM_NUMBER(obj%val)
  RETURN
END IF

CALL RANDOM_NUMBER(obj%val)
END PROCEDURE obj_Random_Number1

!----------------------------------------------------------------------------
!                                                                 testMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE TestMatrix
SELECT CASE (matNo)
CASE (1)
  ALLOCATE (ans(4, 4))
  ans(:, 1) = [3.0, -3.0, 6.0, -9.0]
  ans(:, 2) = [-7.0, 5.0, -4.0, 5.0]
  ans(:, 3) = [-2.0, 1.0, 0.0, -5.0]
  ans(:, 4) = [2.0, 0.0, -5.0, 12.0]
END SELECT
END PROCEDURE TestMatrix

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------
END SUBMODULE ConstructorMethods
