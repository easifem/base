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

PUBLIC :: DiagonalScaling

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE csrmat_DiagonalScaling_1( obj, side, operator )
  CLASS( CSRMatrix_ ), INTENT( INOUT ) :: obj
  CHARACTER( LEN = * ), INTENT( IN ) :: side
  !! LEFT
  !! RIGHT
  !! BOTH
  CHARACTER( LEN=* ), OPTIONAL, INTENT( IN ) :: operator
  !!
  !! SQRT <-- default
  !! NONE
  !!
END SUBROUTINE csrmat_DiagonalScaling_1
END INTERFACE

INTERFACE DiagonalScaling
  MODULE PROCEDURE csrmat_DiagonalScaling_1
END INTERFACE DiagonalScaling

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE csrmat_DiagonalScaling_2( obj, side, diag, operator )
  CLASS( CSRMatrix_ ), INTENT( INOUT ) :: obj
  CHARACTER( LEN = * ), INTENT( IN ) :: side
  !! LEFT
  !! RIGHT
  !! BOTH
  REAL( DFP ), INTENT( IN ) :: diag( : )
  !! Use this diagonal if present
  !!
  CHARACTER( LEN=* ), OPTIONAL, INTENT( IN ) :: operator
  !!
  !! SQRT <-- default
  !! NONE
END SUBROUTINE csrmat_DiagonalScaling_2
END INTERFACE

INTERFACE DiagonalScaling
  MODULE PROCEDURE csrmat_DiagonalScaling_2
END INTERFACE DiagonalScaling
