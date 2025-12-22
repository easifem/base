
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

PURE SUBROUTINE DM_3(ans, test, trial, k, opt)
  REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: ans(:, :)
  CLASS(ElemshapeData_), INTENT(IN) :: test
  !! test function
  CLASS(ElemshapeData_), INTENT(IN) :: trial
  !! trial function
  CLASS(FEVariable_), INTENT(IN) :: k
  !! a matrix variable
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: opt
  !! ncopy
  !!
  !! internal variable
  REAL(DFP), ALLOCATABLE :: kbar(:, :, :)
  REAL(DFP), ALLOCATABLE :: realval(:)
  INTEGER(I4B) :: ii
  !! main
  CALL getInterpolation(obj=trial, ans=kbar, val=k)
  !!
  realval = trial%js * trial%ws * trial%thickness
  !!
  CALL reallocate(ans, SIZE(test%N, 1), SIZE(trial%N, 1))
  !!
  DO ii = 1, SIZE(realval)
    ans = ans + realval(ii) * MATMUL(&
      & MATMUL(test%dNdXt(:, :, ii), kbar(:, :, ii)), &
      & TRANSPOSE(trial%dNdXt(:, :, ii)))
  END DO
  !!
  IF (PRESENT(opt)) CALL MakeDiagonalCopies(ans, opt)
  !!
  DEALLOCATE (kbar, realval)
  !!
END SUBROUTINE DM_3
