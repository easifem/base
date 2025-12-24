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

SUBMODULE(ReverseUtility) Methods
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                    Reverse
!----------------------------------------------------------------------------

MODULE PROCEDURE Reverse_Int8_R1
INTEGER(INT8) :: temp
#include "./Reverse/ReverseVector.F90"
END PROCEDURE Reverse_Int8_R1

!----------------------------------------------------------------------------
!                                                                    Reverse
!----------------------------------------------------------------------------

MODULE PROCEDURE Reverse_Int16_R1
INTEGER(INT16) :: temp
#include "./Reverse/ReverseVector.F90"
END PROCEDURE Reverse_Int16_R1

!----------------------------------------------------------------------------
!                                                                    Reverse
!----------------------------------------------------------------------------

MODULE PROCEDURE Reverse_Int32_R1
INTEGER(INT32) :: temp
#include "./Reverse/ReverseVector.F90"
END PROCEDURE Reverse_Int32_R1

!----------------------------------------------------------------------------
!                                                                    Reverse
!----------------------------------------------------------------------------

MODULE PROCEDURE Reverse_Int64_R1
INTEGER(INT64) :: temp
#include "./Reverse/ReverseVector.F90"
END PROCEDURE Reverse_Int64_R1

!----------------------------------------------------------------------------
!                                                                     Reverse
!----------------------------------------------------------------------------

MODULE PROCEDURE Reverse_Real32_R1
REAL(REAL32) :: temp
#include "./Reverse/ReverseVector.F90"
END PROCEDURE Reverse_Real32_R1

!----------------------------------------------------------------------------
!                                                                     Reverse
!----------------------------------------------------------------------------

MODULE PROCEDURE Reverse_Real64_R1
REAL(REAL64) :: temp
#include "./Reverse/ReverseVector.F90"
END PROCEDURE Reverse_Real64_R1

!----------------------------------------------------------------------------
!                                                                     Reverse
!----------------------------------------------------------------------------

MODULE PROCEDURE Reverse_Int8_R2
INTEGER(INT8) :: temp
#include "./Reverse/ReverseMatrix.F90"
END PROCEDURE Reverse_Int8_R2

!----------------------------------------------------------------------------
!                                                                     Reverse
!----------------------------------------------------------------------------

MODULE PROCEDURE Reverse_Int16_R2
INTEGER(INT16) :: temp
#include "./Reverse/ReverseMatrix.F90"
END PROCEDURE Reverse_Int16_R2

!----------------------------------------------------------------------------
!                                                                     Reverse
!----------------------------------------------------------------------------

MODULE PROCEDURE Reverse_Int32_R2
INTEGER(INT32) :: temp
#include "./Reverse/ReverseMatrix.F90"
END PROCEDURE Reverse_Int32_R2

!----------------------------------------------------------------------------
!                                                                     Reverse
!----------------------------------------------------------------------------

MODULE PROCEDURE Reverse_Int64_R2
INTEGER(INT64) :: temp
#include "./Reverse/ReverseMatrix.F90"
END PROCEDURE Reverse_Int64_R2

!----------------------------------------------------------------------------
!                                                                     Reverse
!----------------------------------------------------------------------------

MODULE PROCEDURE Reverse_Real32_R2
REAL(REAL32) :: temp
#include "./Reverse/ReverseMatrix.F90"
END PROCEDURE Reverse_Real32_R2

!----------------------------------------------------------------------------
!                                                                     Reverse
!----------------------------------------------------------------------------

MODULE PROCEDURE Reverse_Real64_R2
REAL(REAL64) :: temp
#include "./Reverse/ReverseMatrix.F90"
END PROCEDURE Reverse_Real64_R2

!----------------------------------------------------------------------------
!                                                                     Reverse
!----------------------------------------------------------------------------

MODULE PROCEDURE Reverse_Real64_R3
REAL(REAL64) :: temp
INTEGER(I4B) :: tsize, halfSize, indx, indx1, indx2, ii, jj

SELECT CASE (dim)
CASE (1)
  !! dim = 1
  tsize = r2 - r1 + 1
  halfSize = tsize / 2

  DO jj = d1, d2
    DO ii = c1, c2
      DO indx = 1, halfSize
        indx1 = r1 + indx - 1
        indx2 = r2 - indx + 1
        temp = ans(indx2, ii, jj)
        ans(indx2, ii, jj) = ans(indx1, ii, jj)
        ans(indx1, ii, jj) = temp
      END DO
    END DO
  END DO

CASE (2)
  !! dim = 2
  tsize = c2 - c1 + 1
  halfSize = tsize / 2

  DO jj = d1, d2
    DO indx = 1, halfSize
      indx1 = c1 + indx - 1
      indx2 = c2 - indx + 1
      DO ii = r1, r2
        temp = ans(ii, indx2, jj)
        ans(ii, indx2, jj) = ans(ii, indx1, jj)
        ans(ii, indx1, jj) = temp
      END DO
    END DO
  END DO

CASE (3)
  !! dim = 3
  tsize = d2 - d1 + 1
  halfSize = tsize / 2

  DO indx = 1, halfSize
    indx1 = d1 + indx - 1
    indx2 = d2 - indx + 1
    DO jj = c1, c2
      DO ii = r1, r2
        temp = ans(ii, jj, indx2)
        ans(ii, jj, indx2) = ans(ii, jj, indx1)
        ans(ii, jj, indx1) = temp
      END DO
    END DO
  END DO
END SELECT
END PROCEDURE Reverse_Real64_R3

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE Methods
