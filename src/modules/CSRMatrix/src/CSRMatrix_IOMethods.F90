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

MODULE CSRMatrix_IOMethods
USE GlobalData, ONLY: I4B, DFP, LGT
USE BaseType, ONLY: CSRMatrix_

PUBLIC :: Display
PUBLIC :: SPY
PUBLIC :: IMPORT

!----------------------------------------------------------------------------
!                                                        Display@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         22 March 2021
! summary:          This subroutine display the content of sparse matrix
!
!# Introduction
!
! This subroutine display the content of sparse matrix
! - In this subroutine `dump` routine from sparsekit lib is called

INTERFACE
  MODULE SUBROUTINE csrMat_Display(obj, Msg, UnitNo)
    TYPE(CSRMatrix_), INTENT(IN) :: obj
    CHARACTER(*), INTENT(IN) :: Msg
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: UnitNo
  END SUBROUTINE csrMat_Display
END INTERFACE

INTERFACE Display
  MODULE PROCEDURE csrMat_Display
END INTERFACE Display

!----------------------------------------------------------------------------
!                                                              Spy@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         22 March 2021
! summary: Prints the structure of sparse matrix in pdf/svg/png format.

INTERFACE
  MODULE SUBROUTINE csrMat_SPY(obj, filename, ext)
    TYPE(CSRMatrix_), INTENT(IN) :: obj
    CHARACTER(*), INTENT(IN) :: filename
    CHARACTER(*), INTENT(IN) :: ext
  END SUBROUTINE csrMat_SPY
END INTERFACE

INTERFACE SPY
  MODULE PROCEDURE csrMat_SPY
END INTERFACE SPY

!----------------------------------------------------------------------------
!                                                           IMPORT@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 28 Jul 2021
! summary: Import sparse matrix from a file
!
!# Introduction
!
! this routine will open the file and read the data and close the file
! Currently only matFormat="SPARSE_FMT_COO" is supported.
!

INTERFACE
  MODULE SUBROUTINE csrMat_Import(obj, fileName, matFormat)
    TYPE(CSRMatrix_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: fileName
    !! File from which data should be read. This file will
    !! be opened by the this routine on entry. This file
    !! will be closed on return.
    INTEGER(I4B), INTENT(IN) :: matFormat
    !! Currently only `SPARSE_FMT_COO` is supported
  END SUBROUTINE csrMat_Import
END INTERFACE

INTERFACE IMPORT
  MODULE PROCEDURE csrMat_Import
END INTERFACE IMPORT

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  23-01-19
! summary: Deprecated version of csrMat_Import

INTERFACE
  MODULE SUBROUTINE deprecated_csrMat_Import(obj, fileName, matFormat)
    TYPE(CSRMatrix_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: fileName
    !! File from which data should be read. This file will
    !! be opened by the this routine on entry. This file
    !! will be closed on return.
    INTEGER(I4B), INTENT(IN) :: matFormat
    !! Currently only `SPARSE_FMT_COO` is supported
  END SUBROUTINE deprecated_csrMat_Import
END INTERFACE

END MODULE CSRMatrix_IOMethods
