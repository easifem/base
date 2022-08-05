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

SUBMODULE(FunctionalFortranUtility) Methods
USE BaseMethod
IMPLICIT NONE
CONTAINS

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


END SUBMODULE Methods