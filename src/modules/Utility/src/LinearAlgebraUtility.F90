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

MODULE LinearAlgebraUtility
USE GlobalData
IMPLICIT NONE
PRIVATE

PUBLIC :: InvHilbertMatrix
PUBLIC :: HilbertMatrix

!----------------------------------------------------------------------------
!                                                   InvHilbertMatrix@Methods
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION InvHilbertMatrix(n) RESULT(Ans)
    INTEGER(I4B), INTENT(IN) :: n
    REAL(DFP) :: Ans(n, n)
  END FUNCTION InvHilbertMatrix
END INTERFACE

!----------------------------------------------------------------------------
!                                                      HilbertMatrix@Methods
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION HilbertMatrix(n) RESULT(Ans)
    INTEGER(I4B), INTENT(IN) :: n
    REAL(DFP) :: Ans(n, n)
  END FUNCTION HilbertMatrix
END INTERFACE

END MODULE LinearAlgebraUtility
