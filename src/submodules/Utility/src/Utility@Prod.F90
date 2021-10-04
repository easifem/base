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
! summary: 	This submodule contains outerprod

SUBMODULE(Utility ) PROD
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                 VectorProd
!----------------------------------------------------------------------------

MODULE PROCEDURE vec_prod
  c(1) = a(2) * b(3) - a(3) * b(2)
  c(2) = a(3) * b(1) - a(1) * b(3)
  c(3) = a(1) * b(2) - a(2) * b(1)
END PROCEDURE  vec_prod

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE OUTERPROD1_1
  Ans = 0.0_DFP
  Ans = SPREAD(a,dim=2,ncopies=size(b)) * &
        & SPREAD(b,dim=1,ncopies=size(a))
END PROCEDURE

!--------------------------------------------------------------------
!
!--------------------------------------------------------------------

MODULE PROCEDURE OUTERPROD1_1_sym
  Ans = 0.0_DFP
  IF( Sym ) THEN
    Ans =   SPREAD( 0.5_DFP * a, dim=2, ncopies=size(b) ) &
        & * SPREAD( b, dim=1, ncopies=size(a) ) &
        & + SPREAD( 0.5_DFP * b, dim=2, ncopies=size(a) ) &
        & * SPREAD( a, dim=1, ncopies=size(b) )
  ELSE
    Ans = SPREAD(a,dim=2,ncopies=size(b)) * &
            & SPREAD(b,dim=1,ncopies=size(a))
  END IF

END PROCEDURE OUTERPROD1_1_sym

!--------------------------------------------------------------------
!
!--------------------------------------------------------------------

MODULE PROCEDURE OUTERPROD2_1
    ! Definen internal variables
    INTEGER( I4B ) :: I
    Ans = 0.0_DFP
    FORALL (I =1:SIZE( b ))
      Ans( :, :, I ) = a( :, : ) * b( I )
    END FORALL
END PROCEDURE OUTERPROD2_1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE OUTERPROD3_1
  INTEGER( I4B ) :: I
  Ans = 0.0_DFP
  FORALL (I =1:SIZE( b ) )
    Ans( :, :, :,  I ) = a( :, :, : ) * b( I )
  END FORALL
END PROCEDURE OUTERPROD3_1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE OUTERPROD2_11
  ! Definen internal variables
  REAL(DFP), DIMENSION( SIZE( a, 1 ), SIZE( a, 2 ), SIZE( b ) ) :: Dummy3
  Dummy3 = OUTERPROD2_1(a,b)
  ANS = OUTERPROD3_1( Dummy3, c )
END PROCEDURE OUTERPROD2_11

END SUBMODULE PROD