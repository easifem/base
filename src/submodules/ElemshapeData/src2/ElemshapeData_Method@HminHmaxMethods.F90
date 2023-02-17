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

SUBMODULE(ElemshapeData_Method) HminHmaxMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                               GetHminHmax
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_GetHminHmax1
INTEGER(I4B) :: ii, nips, nsd
REAL(DFP), ALLOCATABLE :: G(:, :), w(:)
!! shape is (nsd, nsd, nips), it contains inverse of FFT
REAL(DFP) :: areal
!!
!! Main
!!
nips = SIZE(obj%N, 2)
nsd = obj%refelem%nsd
!!
CALL Reallocate(G, nsd, nsd)
CALL Reallocate(w, nsd)
CALL Reallocate(hmax, nips, hmin, nips)
!!
!! FFT and G
!!
DO ii = 1, nips
  !!
  CALL Inv(invA=G, A=MATMUL(obj%jacobian(:, :, ii), &
    & TRANSPOSE(obj%jacobian(:, :, ii))))
  !!
  w = SymEigenValuesUpto3(G)
  !!
  hmax(ii) = 2.0_DFP / SQRT(MINVAL(w))
  hmin(ii) = 2.0_DFP / SQRT(MAXVAL(w))
END DO
!!
DEALLOCATE (w, G)
!!
END PROCEDURE elemsd_GetHminHmax1

!----------------------------------------------------------------------------
!                                                               GetHminHmax
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_GetHminHmax2
INTEGER(I4B) :: ii, nips, nsd
REAL(DFP), ALLOCATABLE :: w(:)
!! shape is (nsd, nsd, nips), it contains inverse of FFT
!!
!! Main
!!
nips = SIZE(obj%N, 2)
nsd = obj%refelem%nsd
!!
CALL Reallocate(w, nsd)
CALL Reallocate(hmax, nips, hmin, nips)
!!
!! FFT and G
!!
DO ii = 1, nips
  w = SymEigenValuesUpto3(G(:, :, ii))
  hmax(ii) = 2.0_DFP / SQRT(MINVAL(w))
  hmin(ii) = 2.0_DFP / SQRT(MAXVAL(w))
END DO
!!
DEALLOCATE (w)
!!
END PROCEDURE elemsd_GetHminHmax2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_GetHminHmax3
REAL(DFP), ALLOCATABLE :: hmax0(:), hmin0(:)
!!
CALL GetHminHmax(obj=obj, hmax=hmax0, hmin=hmin0)
!!
hmax = QuadratureVariable(hmax0, TypeFEVariableScalar, &
  & TypeFEVariableSpace)
hmin = QuadratureVariable(hmin0, TypeFEVariableScalar, &
  & TypeFEVariableSpace)
END PROCEDURE elemsd_GetHminHmax3

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_GetHminHmax6
REAL(DFP), ALLOCATABLE :: hmax0(:), hmin0(:)
!!
CALL GetHminHmax(obj=obj, hmax=hmax0, hmin=hmin0, G=G)
!!
hmax = QuadratureVariable(hmax0, TypeFEVariableScalar, &
  & TypeFEVariableSpace)
hmin = QuadratureVariable(hmin0, TypeFEVariableScalar, &
  & TypeFEVariableSpace)
END PROCEDURE elemsd_GetHminHmax6

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_GetHminHmax4
INTEGER(I4B) :: ii, nips, nipt
REAL(DFP), ALLOCATABLE :: hmax0(:), hmin0(:)
!!
nips = SIZE(obj(1)%N, 2)
nipt = SIZE(obj)
!!
CALL Reallocate(hmax, nips, nipt)
CALL Reallocate(hmin, nips, nipt)
!!
DO ii = 1, SIZE(obj)
  CALL GetHminHmax(obj=obj(ii), hmax=hmax0, hmin=hmin0)
  hmax(:, ii) = hmax0(:)
  hmin(:, ii) = hmin0(:)
END DO
!!
IF (ALLOCATED(hmax0)) DEALLOCATE (hmax0)
IF (ALLOCATED(hmin0)) DEALLOCATE (hmin0)
END PROCEDURE elemsd_GetHminHmax4

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_GetHminHmax7
INTEGER(I4B) :: ii, nips, nipt
REAL(DFP), ALLOCATABLE :: hmax0(:), hmin0(:)
!!
nips = SIZE(obj(1)%N, 2)
nipt = SIZE(obj)
!!
CALL Reallocate(hmax, nips, nipt)
CALL Reallocate(hmin, nips, nipt)
!!
DO ii = 1, SIZE(obj)
  CALL GetHminHmax(obj=obj(ii), hmax=hmax0, hmin=hmin0, G=G(:, :, :, ii))
  hmax(:, ii) = hmax0(:)
  hmin(:, ii) = hmin0(:)
END DO
!!
IF (ALLOCATED(hmax0)) DEALLOCATE (hmax0)
IF (ALLOCATED(hmin0)) DEALLOCATE (hmin0)
END PROCEDURE elemsd_GetHminHmax7

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_GetHminHmax5
REAL(DFP), ALLOCATABLE :: hmax0(:, :), hmin0(:, :)
!!
CALL GetHminHmax(obj=obj, hmax=hmax0, hmin=hmin0)
!!
hmin = QuadratureVariable( &
  & hmin0, &
  & TypeFEVariableScalar, &
  & TypeFEVariableSpaceTime)
!!
hmax = QuadratureVariable( &
  & hmax0, &
  & TypeFEVariableScalar, &
  & TypeFEVariableSpaceTime)
!!
IF (ALLOCATED(hmax0)) DEALLOCATE (hmax0)
IF (ALLOCATED(hmin0)) DEALLOCATE (hmin0)
END PROCEDURE elemsd_GetHminHmax5

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_GetHminHmax8
REAL(DFP), ALLOCATABLE :: hmax0(:, :), hmin0(:, :)
!!
CALL GetHminHmax(obj=obj, hmax=hmax0, hmin=hmin0, G=G)
!!
hmin = QuadratureVariable( &
  & hmin0, &
  & TypeFEVariableScalar, &
  & TypeFEVariableSpaceTime)
!!
hmax = QuadratureVariable( &
  & hmax0, &
  & TypeFEVariableScalar, &
  & TypeFEVariableSpaceTime)
!!
IF (ALLOCATED(hmax0)) DEALLOCATE (hmax0)
IF (ALLOCATED(hmin0)) DEALLOCATE (hmin0)
END PROCEDURE elemsd_GetHminHmax8

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE HminHmaxMethods
