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


! !----------------------------------------------------------------------------
! !                                                           Matmul@MatVec
! !----------------------------------------------------------------------------

! !> authors: Vikas Sharma, Ph. D.
! ! date: 14 July 2021
! ! summary: Matrix vector multiplication

! INTERFACE
! MODULE FUNCTION csrMat_Matmul( obj, x, matvectype ) RESULT( Ans )
!   TYPE( CSRMatrix_ ), INTENT( IN ) :: obj
!   REAL( DFP ), INTENT( IN ) :: x( : )
!   CHARACTER( LEN = * ), INTENT( IN ) :: matvectype
!   REAL( DFP ) :: Ans( SIZE( x ) )
! END FUNCTION csrMat_Matmul
! END INTERFACE

! INTERFACE Matmul
!   MODULE PROCEDURE csrMat_Matmul
! END INTERFACE Matmul

! PUBLIC :: Matmul