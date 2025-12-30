! This program is a part of EASIFEM library
! Expandable And Scalable Infrastructure for Finite Element Methods
! htttps://www.easifem.com
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

MODULE IndexUtility
USE GlobalData, ONLY: I4B
IMPLICIT NONE

PRIVATE
PUBLIC :: FortranIndex

!----------------------------------------------------------------------------
!                                                                FortranIndex
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-12-30
! summary: Get the index of 2D fortran array

INTERFACE
  MODULE PURE FUNCTION FortranIndex1(ii, jj, nrow, ncol) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: ii, jj, nrow, ncol
    INTEGER(I4B) :: ans
  END FUNCTION FortranIndex1
END INTERFACE

INTERFACE FortranIndex
  MODULE PROCEDURE FortranIndex1
END INTERFACE FortranIndex

!----------------------------------------------------------------------------
!                                                                FortranIndex
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-12-30
! summary: Get the index of 3D fortran array

INTERFACE
  MODULE PURE FUNCTION FortranIndex2(ii, jj, kk, dim1, dim2, dim3) &
    RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: ii, jj, kk, dim1, dim2, dim3
    INTEGER(I4B) :: ans
  END FUNCTION FortranIndex2
END INTERFACE

INTERFACE FortranIndex
  MODULE PROCEDURE FortranIndex2
END INTERFACE FortranIndex

!----------------------------------------------------------------------------
!                                                               FortranIndex
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-12-30
! summary: Get the index of 3D fortran array

INTERFACE
  MODULE PURE FUNCTION FortranIndex3(ii, jj, kk, ll, dim1, dim2, dim3, &
                                     dim4) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: ii, jj, kk, ll, dim1, dim2, dim3, dim4
    INTEGER(I4B) :: ans
  END FUNCTION FortranIndex3
END INTERFACE

INTERFACE FortranIndex
  MODULE PROCEDURE FortranIndex3
END INTERFACE FortranIndex

END MODULE IndexUtility
