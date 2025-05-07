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
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                       getProjectionOfdNdXt
!----------------------------------------------------------------------------

MODULE PROCEDURE getProjectionOfdNdXt_1
  !! Define internal variables
INTEGER(I4B) :: ii, nsd
  !!
  !! main
  !!
CALL Reallocate(cdNdXt, SIZE(obj%dNdXt, 1), SIZE(obj%dNdXt, 3))
nsd = SIZE(obj%dNdXt, 2)
DO ii = 1, SIZE(cdNdXt, 2)
  cdNdXt(:, ii) = MATMUL(obj%dNdXt(:, :, ii), Val(1:nsd))
END DO
  !!
END PROCEDURE getProjectionOfdNdXt_1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE getProjectionOfdNdXt1_
INTEGER(I4B) :: ii, nsd

nrow = SIZE(obj%dNdXt, 1)
ncol = SIZE(obj%dNdXt, 3)
nsd = SIZE(obj%dNdXt, 2)

DO ii = 1, ncol
  cdNdXt(1:nrow, ii) = MATMUL(obj%dNdXt(1:nrow, 1:nsd, ii), Val(1:nsd))
END DO

END PROCEDURE getProjectionOfdNdXt1_

!----------------------------------------------------------------------------
!                                                       getProjectionOfdNdXt
!----------------------------------------------------------------------------

MODULE PROCEDURE getProjectionOfdNdXt_2
INTEGER(I4B) :: ii, nsd
REAL(DFP), ALLOCATABLE :: cbar(:, :)
  !!
  !! main
  !!
CALL getInterpolation(obj=obj, val=val, interpol=cbar)
CALL Reallocate(cdNdXt, SIZE(obj%dNdXt, 1), SIZE(obj%dNdXt, 3))
nsd = SIZE(obj%dNdXt, 2)
DO ii = 1, SIZE(cdNdXt, 2)
  cdNdXt(:, ii) = MATMUL(obj%dNdXt(:, :, ii), cbar(1:nsd, ii))
END DO
  !!
DEALLOCATE (cbar)
  !!
END PROCEDURE getProjectionOfdNdXt_2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE getProjectionOfdNdXt2_
INTEGER(I4B) :: ii, nsd
REAL(DFP) :: cbar(SIZE(obj%dNdXt, 2), SIZE(obj%dNdXt, 3))

CALL GetInterpolation_(obj=obj, val=val, interpol=cbar, nrow=nrow, ncol=ncol)
nsd = nrow
nrow = SIZE(obj%dNdXt, 1)

DO ii = 1, ncol
  cdNdXt(1:nrow, ii) = MATMUL(obj%dNdXt(1:nrow, 1:nsd, ii), cbar(1:nsd, ii))
END DO

END PROCEDURE getProjectionOfdNdXt2_

!----------------------------------------------------------------------------
!                                                       getProjectionOfdNdXt
!----------------------------------------------------------------------------

MODULE PROCEDURE getProjectionOfdNdXt_3
  !! Define internal variables
INTEGER(I4B) :: ii, nsd
  !!
  !! main
  !!
CALL Reallocate(cdNdXt, SIZE(obj%dNdXt, 1), SIZE(obj%dNdXt, 3))
nsd = SIZE(obj%dNdXt, 2)
DO ii = 1, SIZE(cdNdXt, 2)
  cdNdXt(:, ii) = MATMUL(obj%dNdXt(:, :, ii), val(1:nsd, ii))
END DO
  !!
END PROCEDURE getProjectionOfdNdXt_3

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE getProjectionOfdNdXt3_
INTEGER(I4B) :: ii, nsd

nrow = SIZE(obj%dNdXt, 1)
ncol = SIZE(obj%dNdXt, 3)
nsd = SIZE(obj%dNdXt, 2)

DO ii = 1, ncol
  cdNdXt(1:nrow, ii) = MATMUL(obj%dNdXt(1:nrow, 1:nsd, ii), val(1:nsd, ii))
END DO

END PROCEDURE getProjectionOfdNdXt3_

!----------------------------------------------------------------------------
!                                                      getProjectionOfdNTdXt
!----------------------------------------------------------------------------

MODULE PROCEDURE getProjectionOfdNTdXt_1
INTEGER(I4B) :: ii, nsd
  !!
  !! main
  !!
CALL Reallocate(cdNTdXt, SIZE(obj%dNTdXt, 1), SIZE(obj%dNTdXt, 2), &
  & SIZE(obj%dNTdXt, 4))
nsd = SIZE(obj%dNTdXt, 3)
  !!
DO ii = 1, SIZE(cdNTdXt, 3)
  cdNTdXt(:, :, ii) = MATMUL(obj%dNTdXt(:, :, :, ii), Val(1:nsd))
END DO
  !!
END PROCEDURE getProjectionOfdNTdXt_1

!----------------------------------------------------------------------------
!                                                      getProjectionOfdNTdXt
!----------------------------------------------------------------------------

MODULE PROCEDURE getProjectionOfdNTdXt_2
  !!
INTEGER(I4B) :: ii, nsd
REAL(DFP), ALLOCATABLE :: cbar(:, :)
  !!
  !! main
  !!
CALL getInterpolation(obj=obj, val=val, interpol=cbar)
CALL Reallocate(cdNTdXt, SIZE(obj%dNTdXt, 1), SIZE(obj%dNTdXt, 2), &
  & SIZE(obj%dNTdXt, 4))
nsd = SIZE(obj%dNTdXt, 3)
  !!
DO ii = 1, SIZE(cdNTdXt, 3)
  cdNTdXt(:, :, ii) = MATMUL(obj%dNTdXt(:, :, :, ii), cbar(1:nsd, ii))
END DO
  !!
DEALLOCATE (cbar)
END PROCEDURE getProjectionOfdNTdXt_2

!----------------------------------------------------------------------------
!                                                      getProjectionOfdNTdXt
!----------------------------------------------------------------------------

MODULE PROCEDURE getProjectionOfdNTdXt_3
  !!
INTEGER(I4B) :: ii, jj, nsd
REAL(DFP), ALLOCATABLE :: cbar(:, :, :)
  !!
  !! main
  !!
CALL getInterpolation(obj=obj, val=val, interpol=cbar)
  !!
CALL Reallocate(cdNTdXt, &
  & SIZE(obj(1)%dNTdXt, 1), &
  & SIZE(obj(1)%dNTdXt, 2), &
  & SIZE(obj(1)%dNTdXt, 4), SIZE(obj))
  !!
! CALL Reallocate( &
!   & cdNTdXt, &
!   & SIZE(obj(1)%N, 1), &
!   & SIZE(obj(1)%T), &
!   & SIZE(obj(1)%N, 2), &
!   & SIZE(obj) )
  !!
nsd = SIZE(obj(1)%dNTdXt, 3)
  !!
DO jj = 1, SIZE(cbar, 3)
  DO ii = 1, SIZE(cbar, 2)
      !!
    cdNTdXt(:, :, ii, jj) = MATMUL( &
      & obj(jj)%dNTdXt(:, :, :, ii), &
      & cbar(1:nsd, ii, jj))
      !!
  END DO
END DO
  !!
DEALLOCATE (cbar)
  !!
END PROCEDURE getProjectionOfdNTdXt_3

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE Methods
