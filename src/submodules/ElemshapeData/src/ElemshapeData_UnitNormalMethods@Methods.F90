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

SUBMODULE(ElemshapeData_UnitNormalMethods) Methods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                             getUnitNormal
!----------------------------------------------------------------------------

MODULE PROCEDURE getUnitNormal_1
! Define internal variables
REAL(DFP), ALLOCATABLE :: dp(:, :), p(:), pnorm(:)
INTEGER(I4B) :: ii
!! main
CALL getInterpolation(obj=obj, Val=val, Interpol=p)
CALL getSpatialGradient(obj=obj, lg=dp, Val=Val)
CALL Reallocate(R, obj%refelem%NSD, SIZE(obj%N, 2))
pnorm = NORM2(dp, DIM=1)
!!
DO ii = 1, SIZE(p)
  IF (pnorm(ii) .GT. zero) THEN
    IF (p(ii) .GE. 0.0_DFP) THEN
      R(:, ii) = dp(:, ii) / pnorm(ii)
    ELSE
      R(:, ii) = -dp(:, ii) / pnorm(ii)
    END IF
  END IF
END DO
!!
IF (ALLOCATED(dp)) DEALLOCATE (dp)
IF (ALLOCATED(p)) DEALLOCATE (p)
IF (ALLOCATED(pnorm)) DEALLOCATE (pnorm)
END PROCEDURE getUnitNormal_1

!----------------------------------------------------------------------------
!                                                             getUnitNormal
!----------------------------------------------------------------------------

MODULE PROCEDURE getUnitNormal_2
REAL(DFP), ALLOCATABLE :: dp(:, :, :)
REAL(DFP), ALLOCATABLE :: p(:, :)
REAL(DFP), ALLOCATABLE :: mv(:)
REAL(DFP), ALLOCATABLE :: pnorm(:)
REAL(DFP) :: nrm
INTEGER(I4B) :: i
!! main
!! interpolate the vector
CALL getInterpolation(obj=obj, Interpol=p, Val=val)
!! get gradient of nodal values
CALL getSpatialGradient(obj=obj, lg=dp, Val=val)
pnorm = NORM2(p, DIM=1)
CALL Reallocate(R, obj%RefElem%NSD, SIZE(obj%N, 2))
DO i = 1, SIZE(pnorm)
  IF (pnorm(i) .GT. Zero) THEN
    p(:, i) = p(:, i) / pnorm(i)
  ELSE
    p(:, i) = 1.0
  END IF
  mv = MATMUL(p(:, i), dp(:, :, i))
  nrm = NORM2(mv)
  IF (nrm .GT. Zero) THEN
    R(:, i) = mv / nrm
  END IF
END DO
IF (ALLOCATED(dp)) DEALLOCATE (dp)
IF (ALLOCATED(p)) DEALLOCATE (p)
IF (ALLOCATED(mv)) DEALLOCATE (mv)
IF (ALLOCATED(pnorm)) DEALLOCATE (pnorm)
END PROCEDURE getUnitNormal_2

!----------------------------------------------------------------------------
!                                                             getUnitNormal
!----------------------------------------------------------------------------

MODULE PROCEDURE getUnitNormal_3
  !!
IF (val%rank .EQ. scalar) THEN
  CALL scalar_getUnitNormal_3(obj=obj, r=r, val=val)
ELSEIF (val%rank .EQ. vector) THEN
  CALL vector_getUnitNormal_3(obj=obj, r=r, val=val)
END IF
  !!
CONTAINS
  !!
PURE SUBROUTINE scalar_getUnitNormal_3(obj, r, val)
  CLASS(ElemshapeData_), INTENT(IN) :: obj
  REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: r(:, :)
  TYPE(FEVariable_), INTENT(IN) :: val
! Define internal variables
REAL(DFP), ALLOCATABLE :: dp(:, :), p(:), pnorm(:)
INTEGER(I4B) :: ii
!! main
CALL getInterpolation(obj=obj, Val=val, Interpol=p)
CALL getSpatialGradient(obj=obj, lg=dp, Val=Val)
CALL Reallocate(R, obj%refelem%NSD, SIZE(obj%N, 2))
pnorm = NORM2(dp, DIM=1)
!!
DO ii = 1, SIZE(p)
  IF (pnorm(ii) .GT. zero) THEN
    IF (p(ii) .GE. 0.0_DFP) THEN
      R(:, ii) = dp(:, ii) / pnorm(ii)
    ELSE
      R(:, ii) = -dp(:, ii) / pnorm(ii)
    END IF
  END IF
END DO
!!
IF (ALLOCATED(dp)) DEALLOCATE (dp)
IF (ALLOCATED(p)) DEALLOCATE (p)
IF (ALLOCATED(pnorm)) DEALLOCATE (pnorm)
END SUBROUTINE scalar_getUnitNormal_3
  !!
PURE SUBROUTINE vector_getUnitNormal_3(obj, r, val)
  CLASS(ElemshapeData_), INTENT(IN) :: obj
  REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: r(:, :)
  TYPE(FEVariable_), INTENT(IN) :: val
!! Define internal variables
REAL(DFP), ALLOCATABLE :: dp(:, :, :)
REAL(DFP), ALLOCATABLE :: p(:, :)
REAL(DFP), ALLOCATABLE :: mv(:)
REAL(DFP), ALLOCATABLE :: pnorm(:)
REAL(DFP) :: nrm
INTEGER(I4B) :: i
!! main
!! interpolate the vector
CALL getInterpolation(obj=obj, Interpol=p, Val=val)
!! get gradient of nodal values
CALL getSpatialGradient(obj=obj, lg=dp, Val=val)
pnorm = NORM2(p, DIM=1)
CALL Reallocate(R, obj%RefElem%NSD, SIZE(obj%N, 2))
DO i = 1, SIZE(pnorm)
  IF (pnorm(i) .GT. Zero) THEN
    p(:, i) = p(:, i) / pnorm(i)
  ELSE
    p(:, i) = 1.0
  END IF
  mv = MATMUL(p(:, i), dp(:, :, i))
  nrm = NORM2(mv)
  IF (nrm .GT. Zero) THEN
    R(:, i) = mv / nrm
  END IF
END DO
IF (ALLOCATED(dp)) DEALLOCATE (dp)
IF (ALLOCATED(p)) DEALLOCATE (p)
IF (ALLOCATED(mv)) DEALLOCATE (mv)
IF (ALLOCATED(pnorm)) DEALLOCATE (pnorm)
END SUBROUTINE vector_getUnitNormal_3
  !!
END PROCEDURE getUnitNormal_3

END SUBMODULE Methods
