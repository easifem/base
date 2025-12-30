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

SUBMODULE(IndexUtility) Methods
IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                              FortranIndex1
!----------------------------------------------------------------------------

MODULE PROCEDURE FortranIndex1
ans = ii + (jj - 1) * nrow
END PROCEDURE FortranIndex1

!----------------------------------------------------------------------------
!                                                              FortranIndex2
!----------------------------------------------------------------------------

MODULE PROCEDURE FortranIndex2
ans = ii + (jj - 1) * dim1 + (kk - 1) * dim1 * dim2
END PROCEDURE FortranIndex2

!----------------------------------------------------------------------------
!                                                              FortranIndex3
!----------------------------------------------------------------------------

MODULE PROCEDURE FortranIndex3
ans = ii+(jj-1)*dim1+(kk-1)*dim1*dim2+(ll-1)*dim1*dim2*dim3
END PROCEDURE FortranIndex3

END SUBMODULE Methods
