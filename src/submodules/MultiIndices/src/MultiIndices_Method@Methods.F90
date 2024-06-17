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

SUBMODULE(MultiIndices_Method) Methods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                 Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate1
obj%n = n
obj%d = d
END PROCEDURE obj_Initiate1

!----------------------------------------------------------------------------
!                                                               MultiIndices
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_MultiIndices
ans%n = n
ans%d = d
END PROCEDURE obj_MultiIndices

!----------------------------------------------------------------------------
!                                                                 Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Deallocate
obj%n = 0
obj%d = 0
END PROCEDURE obj_Deallocate

!----------------------------------------------------------------------------
!                                                                 Display
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Display
CALL Display(msg, unitno=unitno)
CALL Display(obj%n, "n = ", unitno=unitno)
CALL Display(obj%d, "d = ", unitno=unitno)
END PROCEDURE obj_Display

!----------------------------------------------------------------------------
!                                                                 Size
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Size1
ans = INT(Binom(obj%n + obj%d, obj%d, 1.0_DFP), KIND=I4B)
END PROCEDURE obj_Size1

!----------------------------------------------------------------------------
!                                                                 Size
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Size2
INTEGER(I4B) :: ii
ans = 0_I4B
DO ii = 0, obj%n
  ans = ans + Size(n=ii, d=obj%d)
END DO
END PROCEDURE obj_Size2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetMultiIndices1
ans = GetMultiIndices(n=obj%n, d=obj%d)
END PROCEDURE obj_GetMultiIndices1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetMultiIndices2
ans = GetMultiIndices(n=obj%n, d=obj%d, upto=.true.)
END PROCEDURE obj_GetMultiIndices2

END SUBMODULE Methods
