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
! summary:         This submodule contains method for swaping

SUBMODULE(ConvertUtility) Methods
USE ReallocateUtility, ONLY: Reallocate
USE EyeUtility, ONLY: eye
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                    Convert
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Convert1
CALL Reallocate(to, nns * tdof, nns * tdof)
CALL ConvertSafe(from=from, to=to, conversion=conversion, &
                 nns=nns, tdof=tdof)
END PROCEDURE obj_Convert1

!----------------------------------------------------------------------------
!                                                             ConvertSafe
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Convert_1
nrow = nns * tdof
ncol = nns * tdof
CALL ConvertSafe(from=from, to=to(1:nrow, 1:ncol), conversion=conversion, &
                 nns=nns, tdof=tdof)
END PROCEDURE obj_Convert_1

!----------------------------------------------------------------------------
!                                                             ConvertSafe
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ConvertSafe1
INTEGER(I4B) :: m, inode, idof, i, j
INTEGER(I4B) :: T(nns * tdof, nns * tdof)
!> main
m = nns * tdof
T = eye(m, TypeInt)

SELECT CASE (conversion)
CASE (DofToNodes)

  DO inode = 1, nns
    DO idof = 1, tdof
      j = (inode - 1) * tdof + idof
      T(j, j) = 0
      i = (idof - 1) * nns + inode
      T(i, j) = 1
    END DO
  END DO

CASE (NodesToDOF)

  DO idof = 1, tdof
    DO inode = 1, nns
      j = (idof - 1) * nns + inode
      T(j, j) = 0
      i = (inode - 1) * tdof + idof
      T(i, j) = 1
    END DO
  END DO

END SELECT

to = MATMUL(TRANSPOSE(T), MATMUL(from, T))
END PROCEDURE obj_ConvertSafe1

!----------------------------------------------------------------------------
!                                                                   Convert
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Convert2
!   Define internal variables
INTEGER(I4B) :: a, b, I(4), r1, r2, c1, c2
I = SHAPE(From)
CALL Reallocate(To, I(1) * I(3), I(2) * I(4))
c1 = 0; c2 = 0
DO b = 1, I(4)
  c1 = c2 + 1
  c2 = b * I(2)
  r1 = 0; r2 = 0
  DO a = 1, I(3)
    r1 = r2 + 1; 
    r2 = a * I(1)
    To(r1:r2, c1:c2) = From(:, :, a, b)
  END DO
END DO
END PROCEDURE obj_Convert2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Convert_2
INTEGER(I4B) :: a, b, r1, r2, c1, c2
INTEGER(I4B) :: dim1, dim2, dim3, dim4

dim1 = SIZE(From, 1)
dim2 = SIZE(From, 2)
dim3 = SIZE(From, 3)
dim4 = SIZE(From, 4)
nrow = dim1 * dim3
ncol = dim2 * dim4
c1 = 0; c2 = 0

DO b = 1, dim4
  c1 = c2 + 1
  c2 = b * dim2
  r1 = 0; r2 = 0
  DO a = 1, dim3
    r1 = r2 + 1; 
    r2 = a * dim1
    To(r1:r2, c1:c2) = From(1:dim1, 1:dim2, a, b)
  END DO
END DO

END PROCEDURE obj_Convert_2

!----------------------------------------------------------------------------
!                                                                 Convert
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Convert3
INTEGER(I4B) :: a, b, s(6)
REAL(DFP), ALLOCATABLE :: m2(:, :)
  !!
s = SHAPE(from)
CALL Reallocate(to, s(1) * s(3), s(2) * s(4), s(5), s(6))
  !!
DO b = 1, s(6)
  DO a = 1, s(5)
    CALL Convert(from=from(:, :, :, :, a, b), to=m2)
    to(:, :, a, b) = m2
  END DO
END DO
DEALLOCATE (m2)
END PROCEDURE obj_Convert3

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Convert_3
INTEGER(I4B) :: a, b
INTEGER(I4B) :: n1, n2, n3, n4, n5, n6

n1 = SIZE(from, 1)
n2 = SIZE(from, 2)
n3 = SIZE(from, 3)
n4 = SIZE(from, 4)
n5 = SIZE(from, 5)
n6 = SIZE(from, 6)

dim3 = n5
dim4 = n6

DO b = 1, n6
  DO a = 1, n5
    CALL Convert_(from=from(1:n1, 1:n2, 1:n3, 1:n4, a, b), &
                  to=to(1:n1 * n3, 1:n2 * n4, a, b), &
                  nrow=dim1, ncol=dim2)
  END DO
END DO

END PROCEDURE obj_Convert_3

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE Methods
