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

SUBMODULE(GE_Lapack_Method) CompRoutineMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                      ConditionNo
!----------------------------------------------------------------------------

MODULE PROCEDURE ge_ConditionNo_1
REAL(DFP), DIMENSION(SIZE(A, 1), SIZE(A, 2)) :: tempA
tempA = A
CALL getLU(A=tempA, RCOND=ans, NORM=NORM)
ans = 1.0_DFP / ans
END PROCEDURE ge_ConditionNo_1

END SUBMODULE CompRoutineMethods
