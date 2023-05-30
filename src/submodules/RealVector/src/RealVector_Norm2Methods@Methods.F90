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

SUBMODULE(RealVector_Norm2Methods) Methods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                norm2
!----------------------------------------------------------------------------

MODULE PROCEDURE realvec_norm2_1
INTEGER(I4B) :: s(3), ii
  !!
  !!
  !!
s = GetNodeLoc( &
  & obj=dof, &
  & idof=getIDOF( &
  & obj=dof, &
  & ivar=ivar, &
  & idof=idof))
  !!
ans = 0.0_DFP
  !!
DO ii = s(1), s(2), s(3)
  ans = ans + obj%val(ii)**2
END DO
  !!
ans = SQRT(ans)
  !!
END PROCEDURE realvec_norm2_1

!----------------------------------------------------------------------------
!                                                                norm2
!----------------------------------------------------------------------------

MODULE PROCEDURE realvec_norm2_2
INTEGER(I4B) :: s(3), ii, kk
  !!
  !!
  !!
ans = 0.0_DFP
  !!
DO kk = 1, SIZE(idof)
    !!
  s = GetNodeLoc( &
    & obj=dof, &
    & idof=getIDOF( &
    & obj=dof, &
    & ivar=ivar, &
    & idof=idof(kk)))
    !!
  DO ii = s(1), s(2), s(3)
    ans = ans + obj%val(ii)**2
  END DO
    !!
END DO
  !!
ans = SQRT(ans)
  !!
END PROCEDURE realvec_norm2_2

!----------------------------------------------------------------------------
!                                                                 norm2
!----------------------------------------------------------------------------

MODULE PROCEDURE realvec_norm2_3
INTEGER(I4B) :: s(3), ii
  !!
  !!
  !!
s = GetNodeLoc(obj=dof, idof=idof)
  !!
ans = 0.0_DFP
  !!
DO ii = s(1), s(2), s(3)
  ans = ans + obj%val(ii)**2
END DO
  !!
ans = SQRT(ans)
  !!
END PROCEDURE realvec_norm2_3

!----------------------------------------------------------------------------
!                                                                 norm2
!----------------------------------------------------------------------------

MODULE PROCEDURE realvec_norm2_4
INTEGER(I4B) :: s(3), ii, kk
  !!
  !!
  !!
ans = 0.0_DFP
  !!
DO kk = 1, SIZE(idof)
    !!
  s = GetNodeLoc(obj=dof, idof=idof(kk))
    !!
  DO ii = s(1), s(2), s(3)
    ans = ans + obj%val(ii)**2
  END DO
    !!
END DO
  !!
ans = SQRT(ans)
  !!
END PROCEDURE realvec_norm2_4

!----------------------------------------------------------------------------
!                                                                norm2
!----------------------------------------------------------------------------

MODULE PROCEDURE realvec_norm2_5
INTEGER(I4B) :: s(3), ii
  !!
  !!
  !!
s = GetNodeLoc( &
  & obj=dof, &
  & idof=getIDOF( &
  & obj=dof, &
  & ivar=ivar, &
  & spaceCompo=spaceCompo, &
  & timeCompo=timeCompo))
  !!
ans = 0.0_DFP
  !!
DO ii = s(1), s(2), s(3)
  ans = ans + obj%val(ii)**2
END DO
  !!
ans = SQRT(ans)
  !!
END PROCEDURE realvec_norm2_5

!----------------------------------------------------------------------------
!                                                                norm2
!----------------------------------------------------------------------------

MODULE PROCEDURE realvec_norm2_6
INTEGER(I4B) :: s(3), ii, kk
  !!
  !!
  !!
ans = 0.0_DFP
  !!
DO kk = 1, SIZE(timeCompo)
    !!
  s = GetNodeLoc( &
    & obj=dof, &
    & idof=getIDOF( &
    & obj=dof, &
    & ivar=ivar, &
    & spaceCompo=spaceCompo, &
    & timeCompo=timeCompo(kk)))
    !!
  DO ii = s(1), s(2), s(3)
    ans = ans + obj%val(ii)**2
  END DO
    !!
END DO
  !!
ans = SQRT(ans)
  !!
END PROCEDURE realvec_norm2_6

!----------------------------------------------------------------------------
!                                                                norm2
!----------------------------------------------------------------------------

MODULE PROCEDURE realvec_norm2_7
INTEGER(I4B) :: s(3), ii, kk
  !!
  !!
  !!
ans = 0.0_DFP
  !!
DO kk = 1, SIZE(spaceCompo)
    !!
  s = GetNodeLoc( &
    & obj=dof, &
    & idof=getIDOF( &
    & obj=dof, &
    & ivar=ivar, &
    & spaceCompo=spaceCompo(kk), &
    & timeCompo=timeCompo))
    !!
  DO ii = s(1), s(2), s(3)
    ans = ans + obj%val(ii)**2
  END DO
    !!
END DO
  !!
ans = SQRT(ans)
  !!
END PROCEDURE realvec_norm2_7

END SUBMODULE Methods
