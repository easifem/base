
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
!! scalar
!! scalar
!!
PURE SUBROUTINE DM_6(ans, test, trial, c1, c2, opt)
  REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: ans(:, :)
  CLASS(ElemshapeData_), INTENT(IN) :: test
  !! test function
  CLASS(ElemshapeData_), INTENT(IN) :: trial
  !! trial function
  CLASS(FEVariable_), INTENT(IN) :: c1
  !! a scalar variable
  CLASS(FEVariable_), INTENT(IN) :: c2
  !! a scalar variable
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: opt
  !!
  !! internal variable
  !!
  REAL(DFP), ALLOCATABLE :: realval(:), cbar(:)
  INTEGER(I4B) :: ii
  !! main
  CALL getInterpolation(obj=trial, ans=cbar, val=c1)
  CALL getInterpolation(obj=trial, ans=realval, val=c2)
  realval = realval * trial%js * trial%ws * trial%thickness * cbar
  CALL reallocate(ans, SIZE(test%N, 1), SIZE(trial%N, 1))
  DO ii = 1, SIZE(realval)
    ans = ans + realval(ii) * MATMUL(test%dNdXt(:, :, ii), &
      & TRANSPOSE(trial%dNdXt(:, :, ii)))
  END DO
  !!
  IF (PRESENT(opt)) CALL MakeDiagonalCopies(ans, opt)
  !!
  DEALLOCATE (cbar, realval)
END SUBROUTINE DM_6
