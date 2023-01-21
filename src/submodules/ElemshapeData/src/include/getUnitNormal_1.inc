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
