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
INTEGER(I4B) :: nips, ips
REAL(DFP), ALLOCATABLE :: p(:, :), realVal(:)
!! main
CALL Reallocate(ans, SIZE(test%N, 1), SIZE(trial%N, 1))
nips = SIZE(trial%N, 2)
realval = trial%js * trial%ws * trial%thickness
!! make cdNdXt based upon the case.
IF (term1 .EQ. 1 .AND. term2 .EQ. 0) THEN
  CALL GetProjectionOfdNdXt(obj=test, cdNdXt=p, val=c)
  DO ips = 1, SIZE(trial%N, 2)
    ans = ans + outerprod(a=p(:, ips), &
         & b=trial%N(:, ips)) * realval(ips)
  END DO
ELSE IF (term1 .EQ. 0 .AND. term2 .EQ. 1) THEN
  CALL GetProjectionOfdNdXt(obj=trial, cdNdXt=p, val=c)
  DO ips = 1, SIZE(trial%N, 2)
    ans = ans + outerprod(a=test%N(:, ips), &
         & b=p(:, ips)) * realval(ips)
  END DO
END IF
!! cleanup
IF(ALLOCATED(realval) ) DEALLOCATE (realval)
IF(ALLOCATED(p) ) DEALLOCATE (p)
