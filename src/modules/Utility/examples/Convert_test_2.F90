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
! along with this program.  If not, see <https://www.gnu.org/licenses/>

!> Test: Convert (rank-2) with NodesToDOF
! summary: to(j1,j2) = from(p(j1),p(j2)), where
! j = (idof-1)*nns + inode and p(j) = (inode-1)*tdof + idof.
! The hand-computed permutation for nns=3, tdof=2 is p=[1,3,5,2,4,6].

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: NodesToDOF
USE ConvertUtility, ONLY: Convert
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP

! hand-computed case
INTEGER(I4B), PARAMETER :: nns1 = 3, tdof1 = 2, m1 = nns1 * tdof1
INTEGER(I4B), PARAMETER :: p(m1) = [1, 3, 5, 2, 4, 6]
REAL(DFP) :: from1(m1, m1), expected1(m1, m1)
REAL(DFP), ALLOCATABLE :: to1(:, :)

! general case, reference built with explicit loops
INTEGER(I4B), PARAMETER :: nns2 = 4, tdof2 = 3, m2 = nns2 * tdof2
REAL(DFP) :: from2(m2, m2), expected2(m2, m2)
REAL(DFP), ALLOCATABLE :: to2(:, :)

INTEGER(I4B) :: ii, jj, i1, j1, i2, j2, inode, idof, jnode, jdof

DO jj = 1, m1
  DO ii = 1, m1
    from1(ii, jj) = REAL(10 * ii + jj, DFP)
    expected1(ii, jj) = REAL(10 * p(ii) + p(jj), DFP)
  END DO
END DO

CALL Convert(from=from1, to=to1, conversion=NodesToDOF, &
             nns=nns1, tdof=tdof1)

CALL OK(ALL(SHAPE(to1) .EQ. [m1, m1]), "Convert_NodesToDOF_hand shape")
CALL OK(MAXVAL(ABS(to1 - expected1)) .LT. tol, "Convert_NodesToDOF_hand")

from2 = RESHAPE([(REAL(MOD(7 * ii, 13) - 6, DFP) / 4.0_DFP, &
                  ii=1, SIZE(from2))], SHAPE(from2))

DO jdof = 1, tdof2
  DO jnode = 1, nns2
    j1 = (jdof - 1) * nns2 + jnode
    j2 = (jnode - 1) * tdof2 + jdof
    DO idof = 1, tdof2
      DO inode = 1, nns2
        i1 = (idof - 1) * nns2 + inode
        i2 = (inode - 1) * tdof2 + idof
        expected2(i1, j1) = from2(i2, j2)
      END DO
    END DO
  END DO
END DO

CALL Convert(from=from2, to=to2, conversion=NodesToDOF, &
             nns=nns2, tdof=tdof2)

CALL OK(ALL(SHAPE(to2) .EQ. [m2, m2]), "Convert_NodesToDOF_loop shape")
CALL OK(MAXVAL(ABS(to2 - expected2)) .LT. tol, "Convert_NodesToDOF_loop")

END PROGRAM main
