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

SUBMODULE(IndexValue_Method) Constructor
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                IndexValue
!----------------------------------------------------------------------------

MODULE PROCEDURE Constructor1
Obj%Indx = Indx
  Obj%Val = Val
END PROCEDURE Constructor1

!----------------------------------------------------------------------------
!                                                                IndexValue
!----------------------------------------------------------------------------

MODULE PROCEDURE Constructor2
  INTEGER( I4B ) :: n, i
  n = SIZE( Indx )
  ALLOCATE( Obj( n ) )
  DO i = 1, n
    Obj( i )%Indx = Indx( i )
    Obj( i )%Val = Val( i )
  END DO
END PROCEDURE Constructor2

!----------------------------------------------------------------------------
!                                                                IndexValue
!----------------------------------------------------------------------------

MODULE PROCEDURE Constructor3
  INTEGER( I4B ) :: n, i
  n = SIZE( Indx )
  ALLOCATE( Obj( n ) )
  DO i = 1, n
    Obj( i )%Indx = Indx( i )
    Obj( i )%Val = Val
  END DO
END PROCEDURE Constructor3

END SUBMODULE Constructor