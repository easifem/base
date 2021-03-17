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

!> authors: Vikas Sharma, Ph. D.
! date: 	10 March 2021
! summary: 	VTK_Fortran, pure Fortran (2003+) library to parse and emitt VTK files.

MODULE VTK_FORTRAN
USE VTK_FORTRAN_PVTK_FILE, ONLY : PVTK_FILE
USE VTK_FORTRAN_VTK_FILE, ONLY : VTK_FILE
USE VTK_FORTRAN_VTM_FILE, ONLY : VTM_FILE
PRIVATE
PUBLIC :: PVTK_FILE
PUBLIC :: VTK_FILE
PUBLIC :: VTM_FILE
ENDMODULE VTK_FORTRAN
