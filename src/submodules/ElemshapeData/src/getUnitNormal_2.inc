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
