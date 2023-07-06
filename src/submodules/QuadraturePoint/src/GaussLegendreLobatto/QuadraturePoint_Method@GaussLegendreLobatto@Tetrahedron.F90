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

SUBMODULE(QuadraturePoint_Method:GaussLegendreLobatto) GLL_Tetrahedron
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                 Tetrahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE getGaussLegendreLobattoQPTetrahedron1
CALL ErrorMsg(&
  & msg="This routine is under development",  &
  & file=__FILE__,  &
  & routine="getGaussLegendreLobattoQPTetrahedron1()", &
  & line=__LINE__, &
  & unitno=stderr)
END PROCEDURE getGaussLegendreLobattoQPTetrahedron1

!----------------------------------------------------------------------------
!                                                                 Tetrahedron
!----------------------------------------------------------------------------

MODULE PROCEDURE getGaussLegendreLobattoQPTetrahedron2
CALL ErrorMsg(&
  & msg="This routine is under development",  &
  & file=__FILE__,  &
  & routine="getGaussLegendreLobattoQPTetrahedron2()", &
  & line=__LINE__, &
  & unitno=stderr)
END PROCEDURE getGaussLegendreLobattoQPTetrahedron2

END SUBMODULE GLL_Tetrahedron
