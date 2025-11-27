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

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

PURE SUBROUTINE CM_1(ans, test, trial, c, term1, term2, opt)
  !! Intent of dummy variable
  REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: ans(:, :)
  CLASS(ElemshapeData_), INTENT(IN) :: test
  CLASS(ElemshapeData_), INTENT(IN) :: trial
  TYPE(FEVariable_), INTENT(IN) :: c
  !! Vector variable denoting the convective variable
  INTEGER(I4B), INTENT(IN) :: term1
  !! del_none
  INTEGER(I4B), INTENT(IN) :: term2
  !! del_x, del_y, del_z, del_x_all
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: opt
  !!
  !! Define internal variables
  !!
  INTEGER(I4B) :: ips
  REAL(DFP), ALLOCATABLE :: p(:, :)
  REAL(DFP), ALLOCATABLE :: realVal(:)
  !!
  !! main
  !!
  CALL Reallocate(ans, SIZE(test%N, 1), SIZE(trial%N, 1))
  !!
  realval = trial%js * trial%ws * trial%thickness
  !!
  !! projection on trial
  !!
  CALL GetProjectionOfdNdXt(obj=trial, ans=p, c=c, crank=TypeFEVariableVector)
  !!
  DO ips = 1, SIZE(trial%N, 2)
    ans = ans + outerprod(a=test%N(:, ips), &
          & b=p(:, ips)) * realval(ips)
  END DO
  !!
  IF (PRESENT(opt)) CALL MakeDiagonalCopies(ans, opt)
  !! cleanup
  DEALLOCATE (p, realval)
END SUBROUTINE CM_1
