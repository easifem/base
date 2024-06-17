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
! date: 25 Feb 2021
! summary: This submodule contains Get methods of [[RealVector_]]

SUBMODULE(RealVector_GetMethods) Methods
USE DOF_Method, ONLY: GetNodeLoc, DOF_GetIndex => GetIndex

USE InputUtility, ONLY: INPUT

USE ReallocateUtility, ONLY: Reallocate

USE F95_BLAS, ONLY: COPY

USE RealVector_AssignMethods, ONLY: ASSIGNMENT(=)

USE RealVector_ConstructorMethods, ONLY: RealVector_Size => Size

USE SafeSizeUtility, ONLY: SafeSize

IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                               GetPointer
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetPointer1
val => obj%val
END PROCEDURE obj_GetPointer1

!----------------------------------------------------------------------------
!                                                               GetPointer
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetPointer2
INTEGER(I4B) :: s(3)
s = GetNodeLoc(obj=dofobj, idof=idof)
val => obj%val(s(1):s(2):s(3))
END PROCEDURE obj_GetPointer2

!----------------------------------------------------------------------------
!                                                                     IndexOf
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetIndex1
Ans = MINLOC(ABS(obj%val - VALUE), 1)
END PROCEDURE obj_GetIndex1

!----------------------------------------------------------------------------
!                                                                     IndexOf
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetIndex2
! Ans = MINLOC( ABS( obj%val - Value ), 1 )
INTEGER(I4B) :: i, j, m
LOGICAL(LGT), ALLOCATABLE :: Search(:)
REAL(DFP) :: tol0

tol0 = INPUT(default=REAL(1.0E-10, DFP), option=tol)
m = SIZE(VALUE)
ALLOCATE (Search(m), Ans(m))
Search = .TRUE.
Ans = 0
DO i = 1, SIZE(obj%val)
  DO j = 1, m
    IF (Search(j)) THEN
      IF (ABS(VALUE(j) - obj%val(i)) .LE. tol0) THEN
        Search(j) = .FALSE.
        Ans(j) = i
      END IF
    END IF
  END DO
END DO
END PROCEDURE obj_GetIndex2

!----------------------------------------------------------------------------
!                                                                  isPresent
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_isPresent1
INTEGER(I4B) :: i
REAL(DFP) :: tol0
Ans = .FALSE.
tol0 = INPUT(default=REAL(1.0E-10, DFP), option=tol)
DO i = 1, SIZE(obj%val)
  IF (ABS(obj%val(i) - VALUE) .LE. tol0) THEN
    Ans = .TRUE.
    EXIT
  END IF
END DO
END PROCEDURE obj_isPresent1

!----------------------------------------------------------------------------
!                                                                  isPresent
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_isPresent2
INTEGER(I4B) :: i, m, j
REAL(DFP) :: tol0
LOGICAL(LGT), ALLOCATABLE :: Search(:)

tol0 = INPUT(default=REAL(1.0E-10, DFP), option=tol)
m = SIZE(VALUE)
ALLOCATE (Ans(m), Search(m))
Search = .TRUE.
Ans = .FALSE.
DO i = 1, SIZE(obj%val)
  DO j = 1, m
    IF (Search(j)) THEN
      IF (ABS(VALUE(j) - obj%val(i)) .LE. tol0) THEN
        Search(j) = .FALSE.
        Ans(j) = .TRUE.
      END IF
    END IF
  END DO
END DO
END PROCEDURE obj_isPresent2

!----------------------------------------------------------------------------
!                                                                        Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get1
INTEGER(I4B) :: tsize, ii
tsize = SafeSize(obj%val)
ALLOCATE (ans(tsize))

DO CONCURRENT(ii=1:tsize)
  ans(ii) = INT(obj%val(ii), kind=I4B)
END DO
END PROCEDURE obj_Get1

!----------------------------------------------------------------------------
!                                                                        Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get2
INTEGER(I4B) :: tsize, ii

tsize = SIZE(nodenum)
ALLOCATE (ans(tsize))

DO CONCURRENT(ii=1:tsize)
  ans(ii) = INT(obj%val(nodenum(ii)), kind=I4B)
END DO
END PROCEDURE obj_Get2

!----------------------------------------------------------------------------
!                                                                        Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get3
INTEGER(I4B) :: tsize, ii, jj

tsize = 1_I4B + (iend - istart) / stride
ALLOCATE (ans(tsize))

jj = 0

DO ii = istart, iend, stride
  jj = jj + 1
  ans(jj) = INT(obj%val(ii), kind=I4B)
END DO
END PROCEDURE obj_Get3

!----------------------------------------------------------------------------
!                                                                        Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get4a
INTEGER(I4B) :: tsize, ii
tsize = SafeSize(obj%val)
ALLOCATE (ans(tsize))

DO CONCURRENT(ii=1:tsize)
  ans(ii) = REAL(obj%val(ii), kind=REAL32)
END DO

END PROCEDURE obj_Get4a

!----------------------------------------------------------------------------
!                                                                       Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get4b
INTEGER(I4B) :: tsize, ii
tsize = SafeSize(obj%val)
ALLOCATE (ans(tsize))

DO CONCURRENT(ii=1:tsize)
  ans(ii) = REAL(obj%val(ii), kind=REAL64)
END DO
END PROCEDURE obj_Get4b

!----------------------------------------------------------------------------
!                                                                        Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get5a
INTEGER(I4B) :: tsize, ii

tsize = SIZE(nodenum)
ALLOCATE (ans(tsize))

DO ii = 1, tsize
  ans(ii) = REAL(obj%val(nodenum(ii)), kind=REAL32)
END DO

END PROCEDURE obj_Get5a

!----------------------------------------------------------------------------
!                                                                       Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get5b
INTEGER(I4B) :: tsize, ii

tsize = SIZE(nodenum)
ALLOCATE (ans(tsize))

DO ii = 1, tsize
  ans(ii) = REAL(obj%val(nodenum(ii)), kind=REAL64)
END DO
END PROCEDURE obj_Get5b

!----------------------------------------------------------------------------
!                                                                       Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get6
INTEGER(I4B) :: tsize, ii, jj

tsize = 1_I4B + (iend - istart) / stride
ALLOCATE (ans(tsize))

jj = 0

DO ii = istart, iend, stride
  jj = jj + 1
  ans(jj) = obj%val(ii)
END DO

END PROCEDURE obj_Get6

!----------------------------------------------------------------------------
!                                                                       Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get7
INTEGER(I4B) :: N, i, tNodes, r1, r2

N = SIZE(obj)
tNodes = 0
DO i = 1, N
  tNodes = tNodes + RealVector_SIZE(obj(i))
END DO

ALLOCATE (val(tNodes))
tNodes = 0
r1 = 0
r2 = 0

DO i = 1, N
  r1 = r2 + 1
  r2 = r2 + RealVector_SIZE(obj(i))
  val(r1:r2) = obj(i)%val
END DO

END PROCEDURE obj_Get7

!----------------------------------------------------------------------------
!                                                                       Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get8
INTEGER(I4B) :: N, i, M
N = SIZE(obj)
M = SIZE(nodenum)
ALLOCATE (val(N * M))
DO i = 1, N
  val((i - 1) * M + 1:i * M) = obj(i)%val(nodenum)
END DO
END PROCEDURE obj_Get8

!----------------------------------------------------------------------------
!                                                                       Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get9
INTEGER(I4B) :: N, i, M
N = SIZE(obj)
M = 1 + (iend - istart) / stride
ALLOCATE (val(M * N))
DO i = 1, N
  val((i - 1) * M + 1:i * M) = obj(i)%val(istart:iend:stride)
END DO
END PROCEDURE obj_Get9

!----------------------------------------------------------------------------
!                                                                        Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get10a
INTEGER(I4B) :: N, i, tNodes, r1, r2
N = SIZE(obj)
tNodes = 0
DO i = 1, N
  tNodes = tNodes + SIZE(obj(i)%val)
END DO
ALLOCATE (val(tNodes))
tNodes = 0; r1 = 0; r2 = 0
DO i = 1, N
  r1 = r2 + 1; r2 = r2 + SIZE(obj(i)%val)
  val(r1:r2) = obj(i)%val
END DO
END PROCEDURE obj_Get10a

!----------------------------------------------------------------------------
!                                                                      Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get10b
INTEGER(I4B) :: N, i, tNodes, r1, r2
N = SIZE(obj)
tNodes = 0
DO i = 1, N
  tNodes = tNodes + SIZE(obj(i)%val)
END DO
ALLOCATE (val(tNodes))
tNodes = 0; r1 = 0; r2 = 0
DO i = 1, N
  r1 = r2 + 1; r2 = r2 + SIZE(obj(i)%val)
  val(r1:r2) = obj(i)%val
END DO
END PROCEDURE obj_Get10b

!----------------------------------------------------------------------------
!                                                                        Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get11a
INTEGER(I4B) :: N, i, M
N = SIZE(obj)
M = SIZE(nodenum)
ALLOCATE (val(M * N))
DO i = 1, N
  val((i - 1) * M + 1:i * M) = obj(i)%val(nodenum)
END DO
END PROCEDURE obj_Get11a

!----------------------------------------------------------------------------
!                                                                       Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get11b
INTEGER(I4B) :: N, i, M
N = SIZE(obj)
M = SIZE(nodenum)
ALLOCATE (val(M * N))
DO i = 1, N
  val((i - 1) * M + 1:i * M) = obj(i)%val(nodenum)
END DO
END PROCEDURE obj_Get11b

!----------------------------------------------------------------------------
!                                                                        Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get12a
INTEGER(I4B) :: N, i, M
N = SIZE(obj)
M = 1 + (iend - istart) / stride
ALLOCATE (val(M * N))
DO i = 1, N
  val((i - 1) * M + 1:i * M) = obj(i)%val(istart:iend:stride)
END DO
END PROCEDURE obj_Get12a

!----------------------------------------------------------------------------
!                                                                       Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get12b
INTEGER(I4B) :: N, i, M
N = SIZE(obj)
M = 1 + (iend - istart) / stride
ALLOCATE (val(M * N))
DO i = 1, N
  val((i - 1) * M + 1:i * M) = obj(i)%val(istart:iend:stride)
END DO
END PROCEDURE obj_Get12b

!----------------------------------------------------------------------------
!                                                                       Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get13
val = Get(obj=obj, dataType=1.0_DFP)
END PROCEDURE obj_Get13

!----------------------------------------------------------------------------
!                                                                        Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get14
val = Get(obj=obj, nodenum=nodenum, dataType=1.0_DFP)
END PROCEDURE obj_Get14

!----------------------------------------------------------------------------
!                                                                        Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get15
val = Get(obj=obj, istart=istart, iend=iend, stride=stride,  &
  & dataType=1.0_DFP)
END PROCEDURE obj_Get15

!----------------------------------------------------------------------------
!                                                                        Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get16
val = Get(obj=obj, nodenum=nodenum, dataType=1.0_DFP)
END PROCEDURE obj_Get16

!----------------------------------------------------------------------------
!                                                                       Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get17
val = Get(obj=obj, istart=istart, iend=iend, stride=stride,  &
  & dataType=1.0_DFP)
END PROCEDURE obj_Get17

!----------------------------------------------------------------------------
!                                                                       Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get18a
val = obj%val(nodenum)
END PROCEDURE obj_Get18a

MODULE PROCEDURE obj_Get18b
val = obj%val(nodenum)
END PROCEDURE obj_Get18b

!----------------------------------------------------------------------------
!                                                                        Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get19
IF (ALLOCATED(obj%val)) THEN
  ans = obj
ELSE
  ALLOCATE (ans(0))
END IF
END PROCEDURE obj_Get19

!----------------------------------------------------------------------------
!                                                                        Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get20
IF (ALLOCATED(obj%val)) THEN
  CALL Reallocate(ans, SIZE(nodenum))
  CALL COPY(Y=ans, X=obj%val(nodenum))
ELSE
  ALLOCATE (ans(0))
END IF
END PROCEDURE obj_Get20

!----------------------------------------------------------------------------
!                                                                       Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get21
IF (ALLOCATED(obj%val)) THEN
  ans = obj%val(istart:iend:stride)
ELSE
  ALLOCATE (ans(0))
END IF
END PROCEDURE obj_Get21

!----------------------------------------------------------------------------
!                                                                        Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get22
INTEGER(I4B) :: N, i, tNodes, r1, r2
N = SIZE(obj)
tNodes = 0
DO i = 1, N
  tNodes = tNodes + SIZE(obj(i)%val)
END DO
ALLOCATE (val(tNodes))
tNodes = 0; r1 = 0; r2 = 0
DO i = 1, N
  r1 = r2 + 1; r2 = r2 + SIZE(obj(i)%val)
  val(r1:r2) = obj(i)%val
END DO
END PROCEDURE obj_Get22

!----------------------------------------------------------------------------
!                                                                        Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get23
INTEGER(I4B) :: N, i, M
N = SIZE(obj)
M = SIZE(nodenum)
ALLOCATE (val(M * N))
DO i = 1, N
  val((i - 1) * M + 1:i * M) = obj(i)%val(nodenum)
END DO
END PROCEDURE obj_Get23

!----------------------------------------------------------------------------
!                                                                        Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get24
INTEGER(I4B) :: N, i, M
N = SIZE(obj)
M = 1 + (iend - istart) / stride
ALLOCATE (val(M * N))
DO i = 1, N
  val((i - 1) * M + 1:i * M) = obj(i)%val(istart:iend:stride)
END DO
END PROCEDURE obj_Get24

!----------------------------------------------------------------------------
!                                                                      Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get25
ans = obj%val(DOF_GetIndex(obj=dofobj, nodenum=nodenum, &
                           ivar=ivar, idof=idof))
END PROCEDURE obj_Get25

!----------------------------------------------------------------------------
!                                                                      Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get26
ans = obj%val(DOF_GetIndex(obj=dofobj, nodenum=nodenum, &
                           ivar=ivar, idof=idof))
END PROCEDURE obj_Get26

!----------------------------------------------------------------------------
!                                                                      Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get27
ans = obj%val(DOF_GetIndex(obj=dofobj, nodenum=nodenum, ivar=ivar))
END PROCEDURE obj_Get27

!----------------------------------------------------------------------------
!                                                                      Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get28
ans = obj%val(DOF_GetIndex(obj=dofobj, nodenum=nodenum, ivar=ivar, &
                           spacecompo=spacecompo, timecompo=timecompo))
END PROCEDURE obj_Get28

!----------------------------------------------------------------------------
!                                                                      Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get29
INTEGER(I4B) :: s(3)
s = GetNodeLoc(obj=dofobj, idof=idof)
ans = Get(obj=obj, istart=s(1), iend=s(2), stride=s(3), dataType=1.0_DFP)
END PROCEDURE obj_Get29

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE Methods
