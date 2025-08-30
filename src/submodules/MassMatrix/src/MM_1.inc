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
!                                                                MassMatrix
!----------------------------------------------------------------------------

PURE SUBROUTINE MM_1(ans, test, trial, rho, opt)
  REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: ans(:, :)
  CLASS(ElemshapeData_), INTENT(IN) :: test
  !! Shapedata for test function
  CLASS(ElemshapeData_), INTENT(IN) :: trial
  !! Shapedata for trial function
  CLASS(FEVariable_), INTENT(IN) :: rho
  !! scalar variable
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: opt
  !! ncopy
  !!
  !! Internal variable
  !!
  REAL(DFP), ALLOCATABLE :: realval(:)
  INTEGER(I4B) :: ips
  !!
  !! main
  !!
  CALL reallocate(ans, SIZE(test%N, 1), SIZE(trial%N, 1))
  CALL getInterpolation(obj=trial, ans=realval, val=rho)
  realval = trial%js * trial%ws * trial%thickness * realval
  !!
  DO ips = 1, size(realval)
    ans = ans + realval(ips) * &
      & OUTERPROD(a=test%N(:, ips), b=trial%N(:, ips))
  END DO
  !!
  if( present( opt ) ) CALL MakeDiagonalCopies(ans, opt)
  !!
  DEALLOCATE (realval)
END SUBROUTINE MM_1
