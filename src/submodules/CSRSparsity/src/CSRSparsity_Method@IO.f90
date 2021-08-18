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
! date: 13 Jul 2021
! summary: Input output related methods

SUBMODULE( CSRSparsity_Method ) IO
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                   Display
!----------------------------------------------------------------------------

MODULE PROCEDURE csr_Display
  INTEGER( I4B ) :: i
  i = INPUT( Default=stdout, Option=UnitNo )
  CALL Display( Msg, unitno=i )
  CALL Display( obj%nnz, "NNZ : ", unitno=i )
  CALL Display( obj%ncol, "NCOL : ", unitno=i )
  CALL Display( obj%nrow, "NROW : ", unitno=i )
  CALL Display( obj%dof, "DOF : ", unitno=i )
  IF( ALLOCATED( obj%IA ) ) THEN
    CALL Display( obj%IA, "IA : ", unitno=i )
  ELSE
    CALL Display( "IA is not allocated" )
  END IF
  IF( ALLOCATED( obj%JA ) ) THEN
    CALL Display( obj%JA, "JA : ", unitno=i )
  ELSE
    CALL Display( "JA is not allocated" )
  END IF
END PROCEDURE csr_Display

END SUBMODULE IO