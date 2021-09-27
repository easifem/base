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

SUBMODULE( Utility ) Append
USE BaseMethod
IMPLICIT NONE
CONTAINS


!----------------------------------------------------------------------------
!                                                                     Append
!----------------------------------------------------------------------------

MODULE PROCEDURE Append_I1
  INTEGER(I4B),  ALLOCATABLE :: Dummy( : )
  INTEGER(I4B) :: tSize
  IF( .NOT. ALLOCATED( A ) ) THEN
    A = [Entry]
  ELSE
    tSize = SIZE( A ); ALLOCATE( Dummy( tSize + 1 ) )
    Dummy( 1 : tSize ) = A; Dummy( tSize + 1 ) = Entry
    CALL MOVE_ALLOC( From = Dummy, To = A )
  END IF
END PROCEDURE Append_I1

!----------------------------------------------------------------------------
!                                                                     Append
!----------------------------------------------------------------------------

MODULE PROCEDURE Append_I2
  INTEGER(I4B),  ALLOCATABLE :: Dummy( : )
  INTEGER(I4B) :: n, m
  !
  IF( .NOT. ALLOCATED( A ) ) THEN
    A = Entry
  ELSE
    m = SIZE( Entry ); n = SIZE( A )
    ALLOCATE( Dummy( n + m ) ); Dummy( 1 : n ) = A; Dummy( n+1 : ) = Entry
    CALL MOVE_ALLOC( From = Dummy, To = A )
  END IF
END PROCEDURE Append_I2

!----------------------------------------------------------------------------
!                                                                    Append
!----------------------------------------------------------------------------

MODULE PROCEDURE Append_R1
  REAL(DFP),  ALLOCATABLE :: Dummy( : )
  INTEGER(I4B) :: n
  IF( .NOT. ALLOCATED( A ) ) THEN
    A = [Entry]
  ELSE
    n = SIZE( A ); ALLOCATE( Dummy( n + 1 ) )
    Dummy( 1 : n ) = A; Dummy( 1 + n ) = Entry
    CALL MOVE_ALLOC( From = Dummy, TO = A )
  END IF
END PROCEDURE Append_R1

!----------------------------------------------------------------------------
!                                                                     Append
!----------------------------------------------------------------------------

MODULE PROCEDURE Append_R2
  REAL(DFP),  ALLOCATABLE :: Dummy( : )
  INTEGER(I4B) :: n, m
  IF( .NOT. ALLOCATED( A ) ) THEN
    A = Entry
  ELSE
    m = SIZE( Entry ); n = SIZE( A ); ALLOCATE( Dummy( n + m ) )
    Dummy( 1 : n ) = A; Dummy( 1 + n : ) = Entry
    CALL MOVE_ALLOC( FROM = Dummy, TO = A )
  END IF
END PROCEDURE Append_R2

END SUBMODULE Append