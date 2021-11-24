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


!----------------------------------------------------------------------------
!                                                StringToUID@HashingMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date:         22 Aug 2021
! summary: This function returns a unique number for a given string
!
!# Introduction
! This function returns a unique number for a given string
!
! Reference
!  https://cp-algorithms.com/string/string-hashing.html

INTERFACE
  MODULE FUNCTION StringToUID_PolyRoll(charVar) RESULT(Ans)
    CHARACTER(LEN=*), INTENT(IN) :: charVar
    INTEGER(I4B) :: ans
  END FUNCTION StringToUID_PolyRoll
END INTERFACE

INTERFACE StringToUID
  MODULE PROCEDURE StringToUID_PolyRoll
END INTERFACE StringToUID

PUBLIC :: StringToUID
