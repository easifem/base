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

SUBMODULE(HeadUtility) Methods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                     Head
!----------------------------------------------------------------------------

MODULE PROCEDURE head_Int8
ans = x(1)
END PROCEDURE head_Int8

MODULE PROCEDURE head_Int16
ans = x(1)
END PROCEDURE head_Int16

MODULE PROCEDURE head_Int32
ans = x(1)
END PROCEDURE head_Int32

MODULE PROCEDURE head_Int64
ans = x(1)
END PROCEDURE head_Int64

!----------------------------------------------------------------------------
!                                                                     Head
!----------------------------------------------------------------------------

MODULE PROCEDURE head_Real32
ans = x(1)
END PROCEDURE head_Real32

MODULE PROCEDURE head_Real64
ans = x(1)
END PROCEDURE head_Real64

!----------------------------------------------------------------------------
!                                                                     Head
!----------------------------------------------------------------------------

MODULE PROCEDURE head_char
ans(1:1) = x(1:1)
END PROCEDURE head_char

!----------------------------------------------------------------------------
!                                                                 Tail
!----------------------------------------------------------------------------

END SUBMODULE Methods
