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

PUBLIC :: Display
PUBLIC :: SPY
PUBLIC :: Import

!----------------------------------------------------------------------------
!                                                        Display@IOMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	22 March 2021
! summary: 	 This subroutine display the content of sparse matrix
!
!# Introduction
!
! This subroutine display the content of sparse matrix
! - In this subroutine `dump` routine from sparsekit lib is called

INTERFACE
MODULE SUBROUTINE csrMat_Display( obj, Msg, UnitNo )
  TYPE( CSRMatrix_ ), INTENT( IN ) :: obj
  CHARACTER( LEN = * ), INTENT( IN ) :: Msg
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: UnitNo
END SUBROUTINE csrMat_Display
END INTERFACE

INTERFACE Display
  MODULE PROCEDURE csrMat_Display
END INTERFACE Display

!----------------------------------------------------------------------------
!                                                              Spy@IOMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	22 March 2021
! summary: 	This subroutine prints the structure of sparse matrix in pdf format.

INTERFACE
MODULE SUBROUTINE csrMat_SPY( obj, filename, ext )
  TYPE( CSRMatrix_ ), INTENT( IN ) :: obj
  CHARACTER( LEN = * ), INTENT( IN ) :: filename
  CHARACTER( LEN = * ), INTENT( IN ) :: ext
END SUBROUTINE csrMat_SPY
END INTERFACE

INTERFACE SPY
  MODULE PROCEDURE csrMat_SPY
END INTERFACE SPY

!----------------------------------------------------------------------------
!                                                           IMPORT@IOMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 28 Jul 2021
! summary: Import sparse matrix from a file

INTERFACE
MODULE SUBROUTINE csrMat_Import( obj, fileName, matFormat )
  TYPE( CSRMatrix_ ), INTENT( INOUT ) :: obj
  CHARACTER( LEN = * ), INTENT( IN ) :: fileName
  INTEGER( I4B ), INTENT( IN ) :: matFormat
END SUBROUTINE csrMat_Import
END INTERFACE

INTERFACE Import
  MODULE PROCEDURE csrMat_Import
END INTERFACE Import