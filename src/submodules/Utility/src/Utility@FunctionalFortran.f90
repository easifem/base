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

SUBMODULE( Utility ) FunctionalFortran
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                    arange
!----------------------------------------------------------------------------

MODULE PROCEDURE arange_int
  ! Internal var
  integer(i4b) :: incr
  integer(i4b) :: i
  integer(i4b) :: n
  incr = INPUT( default = 1, option=increment )
  n = ( iend - istart ) / incr+1
  ALLOCATE( Ans(n) )
  DO CONCURRENT( i = 1:n )
    Ans(i) = istart + ( i - 1 ) * incr
  enddo
END PROCEDURE arange_int

!----------------------------------------------------------------------------
!                                                                     arange
!----------------------------------------------------------------------------

MODULE PROCEDURE arange_real
  ! internal var
  REAL( DFP ) :: incr
  INTEGER( I4B ) :: i
  INTEGER( I4B ) :: n

  incr = INPUT( Default = 1.0_DFP, Option=increment )

  n = ( iend - istart + 0.5_DFP * incr ) / incr + 1
  ALLOCATE( Ans( n ) )
  DO CONCURRENT( i = 1:n )
    Ans( i ) = istart + ( i - 1 ) * incr
  ENDDO
END PROCEDURE arange_real

!----------------------------------------------------------------------------
!                                                                     Head
!----------------------------------------------------------------------------

MODULE PROCEDURE head_int
  ans = x( 1 )
END PROCEDURE head_int

!----------------------------------------------------------------------------
!                                                                     Head
!----------------------------------------------------------------------------

MODULE PROCEDURE head_real
  ans = x( 1 )
END PROCEDURE head_real

!----------------------------------------------------------------------------
!                                                                     Head
!----------------------------------------------------------------------------

MODULE PROCEDURE head_char
  ans( 1:1 ) = x( 1:1 )
END PROCEDURE head_char

!----------------------------------------------------------------------------
!                                                                 Tail
!----------------------------------------------------------------------------

MODULE PROCEDURE tail_int
  Ans = x(2:)
END PROCEDURE tail_int

!----------------------------------------------------------------------------
!                                                                 Tail
!----------------------------------------------------------------------------

MODULE PROCEDURE tail_real
  Ans = x(2:)
END PROCEDURE tail_real

!----------------------------------------------------------------------------
!                                                                 Tail
!----------------------------------------------------------------------------

MODULE PROCEDURE tail_char
  Ans = x(2:)
END PROCEDURE tail_char

!----------------------------------------------------------------------------
!                                                                      SPLIT
!----------------------------------------------------------------------------

MODULE PROCEDURE split_int
  if(section == 1)then
    Ans = x(1:size(x)/2)
  elseif(section == 2)then
    Ans = x(size(x)/2+1:)
  endif
END PROCEDURE

!----------------------------------------------------------------------------
!                                                                      SPLIT
!----------------------------------------------------------------------------

MODULE PROCEDURE split_real
  if(section == 1)then
    Ans = x(1:size(x)/2)
  elseif(section == 2)then
    Ans = x(size(x)/2+1:)
  endif
END PROCEDURE split_real

!----------------------------------------------------------------------------
!                                                                      SPLIT
!----------------------------------------------------------------------------

MODULE PROCEDURE split_char
  if (section == 1) then
    Ans = x(1:len(x) / 2)
  else if (section == 2) then
    Ans = x(len(x) / 2 + 1:)
  else
    Ans = ''
  end if
END PROCEDURE split_char

END SUBMODULE FunctionalFortran