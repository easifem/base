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

MODULE InvUtility
USE GlobalData
IMPLICIT NONE
PRIVATE

!----------------------------------------------------------------------------
!                                                         Det@InverseMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION det_2D(A) RESULT(Ans)
    REAL(DFP), INTENT(IN) :: A(:, :)
    REAL(DFP) :: Ans
  END FUNCTION det_2D
END INTERFACE

INTERFACE Det
  MODULE PROCEDURE det_2D
END INTERFACE Det

PUBLIC :: DET

!----------------------------------------------------------------------------
!                                                        Det@InverseMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION det_3D(A) RESULT(Ans)
    REAL(DFP), INTENT(IN) :: A(:, :, :)
    REAL(DFP), ALLOCATABLE :: Ans(:)
  END FUNCTION det_3D
END INTERFACE

INTERFACE Det
  MODULE PROCEDURE det_3D
END INTERFACE Det

!----------------------------------------------------------------------------
!                                                        INV@InverseMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE SUBROUTINE Inv_2D(invA, A)
    REAL(DFP), INTENT(INOUT) :: invA(:, :)
    REAL(DFP), INTENT(IN) :: A(:, :)
  END SUBROUTINE
END INTERFACE

INTERFACE Inv
  MODULE PROCEDURE Inv_2D
END INTERFACE Inv

PUBLIC :: INV

!----------------------------------------------------------------------------
!                                                         INV@InverseMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE SUBROUTINE Inv_3D(invA, A)
    REAL(DFP), INTENT(INOUT) :: invA(:, :, :)
    REAL(DFP), INTENT(IN) :: A(:, :, :)
  END SUBROUTINE
END INTERFACE

INTERFACE Inv
  MODULE PROCEDURE Inv_3D
END INTERFACE Inv

END MODULE InvUtility
