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

SUBMODULE(LinearAlgebraUtility) Methods
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                           IntHilbertMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE InvHilbertMatrix
REAL(DFP) :: p
REAL(DFP) :: r
INTEGER(I4B) :: i
INTEGER(I4B) :: j
INTEGER(I4B) :: ip1

p = REAL(n, kind=DFP)

DO i = 1, n
IF (i .NE. 1) p = (REAL(n - i + 1, DFP) * p * REAL(n + i - 1, DFP)) / REAL(i - 1, DFP)**2
  r = p * p
  ans(i, i) = r / REAL(2 * i - 1, DFP)
  IF (i .EQ. n) CYCLE
  ip1 = i + 1
  DO j = ip1, n
    r = (-1) * (REAL(n - j + 1, DFP) * r * (n + j - 1)) / REAL(j - 1, DFP)**2
    ans(i, j) = r / REAL(i + j - 1, DFP)
    ans(j, i) = ans(i, j)
  END DO
END DO
END PROCEDURE InvHilbertMatrix

!----------------------------------------------------------------------------
!                                                           HilbertMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE HilbertMatrix
INTEGER(I4B) :: ii, jj
REAL(DFP) :: avar

DO jj = 1, n
  DO ii = 1, n
    avar = REAL(ii + jj - 1, KIND=DFP)
    ans(ii, jj) = 1.0_DFP / avar
  END DO
END DO
END PROCEDURE HilbertMatrix

END SUBMODULE Methods
