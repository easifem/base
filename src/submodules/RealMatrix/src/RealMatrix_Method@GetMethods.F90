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
! You should have received a Copy of the GNU General Public License
! along with this program.  If not, see <https: //www.gnu.org/licenses/>
!

SUBMODULE(RealMatrix_Method) GetMethods
USE ReallocateUtility, ONLY: Reallocate
USE BaseType, ONLY: math => TypeMathOpt

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                        Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get1
LOGICAL(LGT) :: isok

isok = ALLOCATED(obj%val)

IF (isok) THEN
  CALL Reallocate(ans, SIZE(obj, 1), SIZE(obj, 2))
  ans = obj%val
ELSE
  CALL Reallocate(ans, 0, 0)
END IF
END PROCEDURE obj_Get1

!----------------------------------------------------------------------------
!                                                                       Get_
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get_1
LOGICAL(LGT) :: isok

isok = ALLOCATED(obj%val)
nrow = 0
ncol = 0

IF (isok) THEN
  nrow = SIZE(obj, 1)
  ncol = SIZE(obj, 2)
  ans(1:nrow, 1:ncol) = obj%val(1:nrow, 1:ncol)
END IF
END PROCEDURE obj_Get_1

!----------------------------------------------------------------------------
!                                                                       Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get1b
ans = Get(obj=obj, datatype=math%one)
END PROCEDURE obj_Get1b

!----------------------------------------------------------------------------
!                                                                        Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get2
ans = obj%val(rindx, cindx)
END PROCEDURE obj_Get2

!----------------------------------------------------------------------------
!                                                                       Get_
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get_2
LOGICAL(LGT) :: isok

isok = ALLOCATED(obj%val)
nrow = 0
ncol = 0
IF (isok) THEN
  nrow = SIZE(rindx)
  ncol = SIZE(cindx)
  ans(1:nrow, 1:ncol) = obj%val(rindx, cindx)
END IF
END PROCEDURE obj_Get_2

!----------------------------------------------------------------------------
!                                                                        Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get3
ans = obj%val(istart:iend:stride, istart:iend:stride)
END PROCEDURE obj_Get3

!----------------------------------------------------------------------------
!                                                                        Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get_3
LOGICAL(LGT) :: isok
isok = ALLOCATED(obj%val)
nrow = 0
ncol = 0
IF (isok) THEN
  nrow = (iend - istart) / stride
  ncol = nrow
  ans(1:nrow, 1:ncol) = obj%val(istart:iend:stride, istart:iend:stride)
END IF
END PROCEDURE obj_Get_3

!----------------------------------------------------------------------------
!                                                                       Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get4
ans%val = obj%val
CALL SetTotalDimension(ans, math%two_i)
END PROCEDURE obj_Get4

!----------------------------------------------------------------------------
!                                                                        Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get5
ans%val = obj%val(RIndx, CIndx)
CALL SetTotalDimension(ans, math%two_i)
END PROCEDURE obj_Get5

!----------------------------------------------------------------------------
!                                                                        Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get6
#define Indx iStart:iEnd:Stride
ans%val = obj%val(Indx, Indx)
#undef Indx
CALL SetTotalDimension(ans, math%two_i)
END PROCEDURE obj_Get6

!----------------------------------------------------------------------------
!                                                                       Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get7
INTEGER(I4B) :: s(2), i, j, r1, r2, c1, c2
INTEGER(I4B), ALLOCATABLE :: rc(:, :)

s = SHAPE(obj)
ALLOCATE (rc(0:2, 0:(s(1) * s(2))))
rc = 0

DO j = 1, s(2)
  DO i = 1, s(1)
    rc(1:2, i + (j - 1) * s(1)) = SHAPE(obj(i, j))
  END DO
END DO

i = MAXVAL(SUM(RESHAPE(rc(1, 1:), SHAPE(obj)), 1))
j = MAXVAL(SUM(RESHAPE(rc(2, 1:), SHAPE(obj)), 2))

ALLOCATE (ans(i, j)); ans = 0.0_DFP

c1 = 0
c2 = 0

DO j = 1, s(2)
  c1 = 1 + c2
  c2 = c1 + rc(2, j) - 1
  r1 = 0; r2 = 0
  DO i = 1, s(1)
    r1 = 1 + r2
    r2 = r1 + rc(1, i) - 1
    ans(r1:r2, c1:c2) = obj(i, j)%val
  END DO
END DO

END PROCEDURE obj_Get7

!----------------------------------------------------------------------------
!                                                                        Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get8
ans%val = Get(obj, math%one)
CALL SetTotalDimension(ans, math%two_i)
END PROCEDURE obj_Get8

!----------------------------------------------------------------------------
!                                                                       Copy
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Copy1
to = from%val
END PROCEDURE obj_Copy1

!----------------------------------------------------------------------------
!                                                                       Copy
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Copy2
to%val = from%val
CALL SetTotalDimension(to, math%two_i)
END PROCEDURE obj_Copy2

!----------------------------------------------------------------------------
!                                                                       Copy
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Copy3
to%val = from
CALL SetTotalDimension(to, math%two_i)
END PROCEDURE obj_Copy3

!----------------------------------------------------------------------------
!                                                                 GetPointer
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetPointer
ans => obj%val
END PROCEDURE obj_GetPointer

!----------------------------------------------------------------------------
!                                                                  GetColumn
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetColumn_1
LOGICAL(LGT) :: isok
isok = ALLOCATED(obj%val)
tsize = 0
IF (isok) THEN
  tsize = SIZE(obj%val, 1)
  ans(1:tsize) = obj%val(1:tsize, col)
END IF
END PROCEDURE obj_GetColumn_1

END SUBMODULE GetMethods
