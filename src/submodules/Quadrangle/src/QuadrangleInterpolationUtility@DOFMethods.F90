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

SUBMODULE(QuadrangleInterpolationUtility) DOFMethods
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                     GetTotalDOF_Quadrangle
!----------------------------------------------------------------------------

MODULE PROCEDURE GetTotalDOF_Quadrangle
ans = (order + 1)**2
END PROCEDURE GetTotalDOF_Quadrangle

!----------------------------------------------------------------------------
!                                                   GetTotalInDOF_Quadrangle
!----------------------------------------------------------------------------

MODULE PROCEDURE GetTotalInDOF_Quadrangle1
ans = (order - 1)**2
END PROCEDURE GetTotalInDOF_Quadrangle1

!----------------------------------------------------------------------------
!                                                   GetTotalInDOF_Quadrangle
!----------------------------------------------------------------------------

MODULE PROCEDURE GetTotalInDOF_Quadrangle2
ans = (p - 1) * (q - 1)
END PROCEDURE GetTotalInDOF_Quadrangle2

!----------------------------------------------------------------------------
!                                               QuadraturePoint_Quadrangle3
!----------------------------------------------------------------------------

END SUBMODULE DOFMethods
