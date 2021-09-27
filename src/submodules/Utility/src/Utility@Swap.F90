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

!> authors: Vikas Sharma, Ph. D.
! date: 	22 March 2021
! summary: 	This submodule contains method for swaping

SUBMODULE( Utility ) SWAP
USE BaseMethod
IMPLICIT NONE
CONTAINS


!----------------------------------------------------------------------------
!                                                                      SWAP
!----------------------------------------------------------------------------

MODULE PROCEDURE swap_i
  INTEGER(I4B) :: dum
  dum=a
  a=b
  b=dum
END PROCEDURE swap_i

!----------------------------------------------------------------------------
!                                                                      SWAP
!----------------------------------------------------------------------------

MODULE PROCEDURE swap_r
  REAL(DFP) :: dum
  dum=a
  a=b
  b=dum
END PROCEDURE swap_r

!----------------------------------------------------------------------------
!                                                                       SWAP
!----------------------------------------------------------------------------

MODULE PROCEDURE swap_rv
  REAL(DFP), DIMENSION(SIZE(a)) :: dum
  dum=a
  a=b
  b=dum
END PROCEDURE swap_rv

!----------------------------------------------------------------------------
!                                                                       SWAP
!----------------------------------------------------------------------------

MODULE PROCEDURE swap_c
  COMPLEX(DFPC) :: dum
  dum=a
  a=b
  b=dum
END PROCEDURE swap_c

!----------------------------------------------------------------------------
!                                                                       SWAP
!----------------------------------------------------------------------------

MODULE PROCEDURE swap_cv
  COMPLEX(DFPC), DIMENSION(SIZE(a)) :: dum
  dum=a
  a=b
  b=dum
END PROCEDURE swap_cv

!----------------------------------------------------------------------------
!                                                                       SWAP
!----------------------------------------------------------------------------

MODULE PROCEDURE swap_cm
  COMPLEX(DFPC), DIMENSION(size(a,1),size(a,2)) :: dum
  dum=a
  a=b
  b=dum
END PROCEDURE swap_cm

!----------------------------------------------------------------------------
!                                                                      SWAP
!----------------------------------------------------------------------------

MODULE PROCEDURE masked_swap_rs
  REAL(DFP) :: swp
  IF (mask) THEN
    swp=a
    a=b
    b=swp
  END IF
END PROCEDURE masked_swap_rs

!----------------------------------------------------------------------------
!                                                                       SWAP
!----------------------------------------------------------------------------

MODULE PROCEDURE masked_swap_rv
  REAL(DFP), DIMENSION(size(a)) :: swp
  WHERE(mask)
    swp=a
    a=b
    b=swp
  END WHERE
END PROCEDURE masked_swap_rv

!----------------------------------------------------------------------------
!                                                                      SWAP
!----------------------------------------------------------------------------

MODULE PROCEDURE masked_swap_rm
  REAL(DFP), DIMENSION(size(a,1),size(a,2)) :: swp
  where (mask)
    swp=a
    a=b
    b=swp
  END where
END PROCEDURE masked_swap_rm

END SUBMODULE SWAP