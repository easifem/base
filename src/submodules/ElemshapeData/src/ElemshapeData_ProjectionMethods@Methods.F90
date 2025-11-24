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

SUBMODULE(ElemshapeData_ProjectionMethods) Methods
USE BaseMethod

! USE FEVariable_Method, only: FEVariableGetInterpolation_ => GetInterpolation_
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                       GetProjectionOfdNdXt
!----------------------------------------------------------------------------

MODULE PROCEDURE GetProjectionOfdNdXt_1
INTEGER(I4B) :: nrow, ncol

nrow = obj%nns
ncol = obj%nips
CALL Reallocate(ans, nrow, ncol)

CALL GetProjectionOfdNdXt_(obj=obj, ans=ans, c=c, nrow=nrow, ncol=ncol)
END PROCEDURE GetProjectionOfdNdXt_1

!----------------------------------------------------------------------------
!                                                      GetProjectionOfdNdXt_
!----------------------------------------------------------------------------

MODULE PROCEDURE GetProjectionOfdNdXt1_
INTEGER(I4B) :: ii, nsd

nrow = obj%nns !!SIZE(obj%dNdXt, 1)
ncol = obj%nips !!SIZE(obj%dNdXt, 3)
nsd = obj%nsd !!SIZE(obj%dNdXt, 2)

DO ii = 1, ncol
  ans(1:nrow, ii) = MATMUL(obj%dNdXt(1:nrow, 1:nsd, ii), c(1:nsd))
END DO
END PROCEDURE GetProjectionOfdNdXt1_

!----------------------------------------------------------------------------
!                                                       GetProjectionOfdNdXt
!----------------------------------------------------------------------------

MODULE PROCEDURE GetProjectionOfdNdXt_2
INTEGER(I4B) :: nrow, ncol

nrow = obj%nns
ncol = obj%nips
CALL Reallocate(ans, nrow, ncol)
CALL GetProjectionOfdNdXt_(obj=obj, ans=ans, c=c, crank=crank, nrow=nrow, &
                           ncol=ncol)
END PROCEDURE GetProjectionOfdNdXt_2

!----------------------------------------------------------------------------
!                                                      GetProjectionOfdNdXt_
!----------------------------------------------------------------------------

MODULE PROCEDURE GetProjectionOfdNdXt2_
INTEGER(I4B) :: ips, nsd, i1
! REAL(DFP) :: cbar(SIZE(obj%dNdXt, 2), SIZE(obj%dNdXt, 3))
REAL(DFP) :: cbar(3), T(0)

nrow = obj%nns
ncol = obj%nips
nsd = obj%nsd
cbar = 0.0_DFP

! USE FEVariable_Method, only: FEVariableGetInterpolation_ => GetInterpolation_
DO ips = 1, obj%nips
  CALL GetInterpolation_( &
    obj=c, rank=crank, N=obj%N, nns=obj%nns, spaceIndx=ips, timeIndx=0_I4B, &
    T=T, nnt=0_I4B, scale=1.0_DFP, addContribution=.FALSE., ans=cbar, &
    tsize=i1)

  ans(1:nrow, ips) = MATMUL(obj%dNdXt(1:nrow, 1:nsd, ips), cbar(1:nsd))
END DO
END PROCEDURE GetProjectionOfdNdXt2_

!----------------------------------------------------------------------------
!                                                       getProjectionOfdNdXt
!----------------------------------------------------------------------------

MODULE PROCEDURE GetProjectionOfdNdXt_3
INTEGER(I4B) :: nrow, ncol

nrow = obj%nns
ncol = obj%nips
CALL Reallocate(ans, nrow, ncol)
CALL GetProjectionOfdNdXt_(obj=obj, ans=ans, c=c, nrow=nrow, ncol=ncol)
END PROCEDURE GetProjectionOfdNdXt_3

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE GetProjectionOfdNdXt3_
INTEGER(I4B) :: ips, nsd

nrow = obj%nns
ncol = obj%nips
nsd = obj%nsd

DO ips = 1, obj%nips
  ans(1:nrow, ips) = MATMUL(obj%dNdXt(1:nrow, 1:nsd, ips), c(1:nsd, ips))
END DO
END PROCEDURE GetProjectionOfdNdXt3_

!----------------------------------------------------------------------------
!                                                      getProjectionOfdNTdXt
!----------------------------------------------------------------------------

MODULE PROCEDURE GetProjectionOfdNTdXt_1
INTEGER(I4B) :: ii, nsd

CALL Reallocate(cdNTdXt, SIZE(obj%dNTdXt, 1), SIZE(obj%dNTdXt, 2), &
  & SIZE(obj%dNTdXt, 4))
nsd = SIZE(obj%dNTdXt, 3)

DO ii = 1, SIZE(cdNTdXt, 3)
  cdNTdXt(:, :, ii) = MATMUL(obj%dNTdXt(:, :, :, ii), Val(1:nsd))
END DO
END PROCEDURE GetProjectionOfdNTdXt_1

!----------------------------------------------------------------------------
!                                                      getProjectionOfdNTdXt
!----------------------------------------------------------------------------

MODULE PROCEDURE getProjectionOfdNTdXt_2
INTEGER(I4B) :: ii, nsd
REAL(DFP), ALLOCATABLE :: cbar(:, :)

CALL getInterpolation(obj=obj, val=val, ans=cbar)
CALL Reallocate(cdNTdXt, SIZE(obj%dNTdXt, 1), SIZE(obj%dNTdXt, 2), &
                SIZE(obj%dNTdXt, 4))
nsd = SIZE(obj%dNTdXt, 3)

DO ii = 1, SIZE(cdNTdXt, 3)
  cdNTdXt(:, :, ii) = MATMUL(obj%dNTdXt(:, :, :, ii), cbar(1:nsd, ii))
END DO

DEALLOCATE (cbar)
END PROCEDURE getProjectionOfdNTdXt_2

!----------------------------------------------------------------------------
!                                                      getProjectionOfdNTdXt
!----------------------------------------------------------------------------

MODULE PROCEDURE getProjectionOfdNTdXt_3
INTEGER(I4B) :: ii, jj, nsd
REAL(DFP), ALLOCATABLE :: cbar(:, :, :)

CALL getInterpolation(obj=obj, val=val, ans=cbar)

CALL Reallocate(cdNTdXt, &
  & SIZE(obj(1)%dNTdXt, 1), &
  & SIZE(obj(1)%dNTdXt, 2), &
  & SIZE(obj(1)%dNTdXt, 4), SIZE(obj))

! CALL Reallocate( &
!   & cdNTdXt, &
!   & SIZE(obj(1)%N, 1), &
!   & SIZE(obj(1)%T), &
!   & SIZE(obj(1)%N, 2), &
!   & SIZE(obj) )

nsd = SIZE(obj(1)%dNTdXt, 3)

DO jj = 1, SIZE(cbar, 3)
  DO ii = 1, SIZE(cbar, 2)

    cdNTdXt(:, :, ii, jj) = MATMUL( &
      & obj(jj)%dNTdXt(:, :, :, ii), &
      & cbar(1:nsd, ii, jj))

  END DO
END DO

DEALLOCATE (cbar)
END PROCEDURE getProjectionOfdNTdXt_3

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE Methods
