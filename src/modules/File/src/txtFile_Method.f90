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

MODULE txtFile_Method
USE GlobalData
USE BaseType
IMPLICIT NONE

!----------------------------------------------------------------------------
!                                                       Write@WriteMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 5 March 2021
! summary: This routine writes data into a file
!
!### Introduction
!
! * This routine writes data into a file
! * If transpose is present then data is printed after taking transpose
!
!### Usage
!
!```fortran
!
!```

INTERFACE
MODULE SUBROUTINE save_to_file_r0( Obj, Val )
  CLASS( txtFile_ ), INTENT( INOUT) :: Obj
  CLASS( * ), INTENT( IN ) :: Val
END SUBROUTINE save_to_file_r0
END INTERFACE

!----------------------------------------------------------------------------
!                                                       Write@WriteMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 5 March 2021
! summary: This routine writes data into a file
!
!### Introduction
!
! This routine writes data into a file
! If row is present then data is printed as row
! If col is present then data is printed as column
! If both row or col are absent then data is printed as row
!
!
!### Usage
!
!```fortran
!
!```

INTERFACE
MODULE SUBROUTINE save_to_file_r1( Obj, Val, row, col )
  CLASS( txtFile_), INTENT( INOUT) :: Obj
    !! File object
  CLASS(*), INTENT( IN ) :: Val(:)
    !! One D array
  LOGICAL( LGT ), INTENT( IN ), OPTIONAL :: row
    !! If present then vector will be printed as rowwise
  LOGICAL( LGT ), INTENT( IN ), OPTIONAL :: col
    !! If present then vector will be printed as column wise
END SUBROUTINE save_to_file_r1
END INTERFACE

!----------------------------------------------------------------------------
!                                                       Write@WriteMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 5 March 2021
! summary: This routine writes data into a file
!
!### Introduction
!
! This routine writes data into a file
! If transpose is true then data is printed after taking transpose
!
!### Usage
!
!```fortran
!
!```

INTERFACE
MODULE SUBROUTINE save_to_file_r2( Obj, Val, transpose )
  CLASS( txtFile_ ), INTENT( INOUT) :: Obj
  CLASS( * ), INTENT( IN ) :: Val( :, : )
  LOGICAL( LGT ), INTENT( IN ) :: transpose
END SUBROUTINE save_to_file_r2
END INTERFACE


INTERFACE SaveFile
  MODULE PROCEDURE save_to_file_r0, save_to_file_r1, save_to_file_r2
END INTERFACE SaveFile

PUBLIC :: SaveFile

!----------------------------------------------------------------------------
!                                                      SkipLines@ReadMethods
!----------------------------------------------------------------------------

INTERFACE
!! This subroutine skips nLines

!> authors: Dr. Vikas Sharma
!
! This subroutine skips nLines

MODULE SUBROUTINE SkipLines( Obj, nLines )
  CLASS( File_ ), INTENT( INOUT) :: Obj
  INTEGER( I4B ), INTENT( IN ) :: nLines
END SUBROUTINE SkipLines
END INTERFACE

PUBLIC :: SkipLines

END MODULE txtFile_Method