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

MODULE CSRMatrix_DiagonalScalingMethods
USE GlobalData, ONLY: I4B, DFP, LGT
USE BaseType, ONLY: CSRMatrix_
IMPLICIT NONE
PRIVATE

PUBLIC :: DiagonalScaling

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE csrmat_DiagonalScaling_1(obj, side, OPERATOR)
    CLASS(CSRMatrix_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: side
    !! LEFT
    !! RIGHT
    !! BOTH
    CHARACTER(*), OPTIONAL, INTENT(IN) :: OPERATOR
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
  MODULE SUBROUTINE csrmat_DiagonalScaling_2(obj, side, diag, OPERATOR)
    CLASS(CSRMatrix_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: side
    !! LEFT
    !! RIGHT
    !! BOTH
    REAL(DFP), INTENT(IN) :: diag(:)
    !! Use this diagonal if present
    !!
    CHARACTER(*), OPTIONAL, INTENT(IN) :: OPERATOR
    !!
    !! SQRT <-- default
    !! NONE
    !!
  END SUBROUTINE csrmat_DiagonalScaling_2
END INTERFACE

INTERFACE DiagonalScaling
  MODULE PROCEDURE csrmat_DiagonalScaling_2
END INTERFACE DiagonalScaling

END MODULE CSRMatrix_DiagonalScalingMethods
