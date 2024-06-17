
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
!                                                            DiffusionMatrix
!----------------------------------------------------------------------------
!!
!! vector
!! vector
!!
PURE SUBROUTINE DM_4(ans, test, trial, c1, c2, opt)
  REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: ans(:, :)
  CLASS(ElemshapeData_), INTENT(IN) :: test
  !! test function
  CLASS(ElemshapeData_), INTENT(IN) :: trial
  !! trial function
  CLASS(FEVariable_), INTENT(IN) :: c1
  !! a vector variable
  CLASS(FEVariable_), INTENT(IN) :: c2
  !! a vector variable
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: opt
  !!
  !! Internal variable
  REAL(DFP), ALLOCATABLE :: c1bar(:, :), c2bar(:, :), realval(:)
  INTEGER(I4B) :: ii
  !!
  !! main
  !!
  CALL reallocate(ans, SIZE(test%N, 1), SIZE(trial%N, 1))
  !!
  CALL getProjectionOfdNdXt(obj=test, cdNdXt=c1bar, val=c1)
  !!
  CALL getProjectionOfdNdXt(obj=trial, cdNdXt=c2bar, val=c2)
  !!
  realval = trial%js * trial%ws * trial%thickness
  !!
  DO ii = 1, SIZE(realval)
    ans = ans + realval(ii) * OUTERPROD(c1bar(:, ii), c2bar(:, ii))
  END DO
  !!
  IF (PRESENT(opt)) CALL MakeDiagonalCopies(ans, opt)
  !!
  DEALLOCATE (c1bar, c2bar, realval)
  !!
END SUBROUTINE DM_4
