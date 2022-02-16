
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
!! matrix
!! matrix
!!
PURE SUBROUTINE DM_8(ans, test, trial, c1, c2, opt)
  REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: ans(:, :)
  CLASS(ElemshapeData_), INTENT(IN) :: test
  !! test function
  CLASS(ElemshapeData_), INTENT(IN) :: trial
  !! trial function
  CLASS(FEVariable_), INTENT(IN) :: c1
  !! a matrix variable
  CLASS(FEVariable_), INTENT(IN) :: c2
  !! a matrix variable
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: opt
  !!
  !! internal variable
  REAL(DFP), ALLOCATABLE :: k1bar(:, :, :), k2bar(:, :, :), realval(:)
  INTEGER(I4B) :: ii
  !! main
  CALL getInterpolation(obj=trial, Interpol=k1bar, val=c1)
  !!
  CALL getInterpolation(obj=trial, Interpol=k2bar, val=c2)
  !!
  realval = trial%js * trial%ws * trial%thickness
  !!
  CALL reallocate(ans, SIZE(test%N, 1), SIZE(trial%N, 1))
  !!
  DO ii = 1, SIZE(realval)
    !!
    ans = ans + realval(ii) * MATMUL( &
        & MATMUL(test%dNdXt(:, :, ii),&
        & MATMUL(k1bar(:, :, ii), k2bar(:, :, ii))), &
        & TRANSPOSE(trial%dNdXt(:, :, ii)))
    !!
  END DO
  !!
  IF (PRESENT(opt)) CALL MakeDiagonalCopies(ans, opt)
  !!
  DEALLOCATE (k1bar, k2bar, realval)
  !!
END SUBROUTINE DM_8
