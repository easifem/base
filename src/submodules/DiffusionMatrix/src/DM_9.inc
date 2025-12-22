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
!!
!! matrix
!! vector
!!
PURE SUBROUTINE DM_9(ans, test, trial, c1, c2, opt)
  REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: ans(:, :)
  CLASS(ElemshapeData_), INTENT(IN) :: test
  CLASS(ElemshapeData_), INTENT(IN) :: trial
  TYPE(FEVariable_), INTENT(IN) :: c1
  !! matrix variable
  TYPE(FEVariable_), INTENT(IN) :: c2
  !! vector variable
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: opt
  !!
  !! internal variable
  REAL(DFP), ALLOCATABLE :: matbar(:, :, :)
  REAL(DFP), ALLOCATABLE :: c1bar(:, :)
  REAL(DFP), ALLOCATABLE :: c2bar(:, :)
  REAL(DFP), ALLOCATABLE :: realval(:)
  TYPE(FEVariable_) :: k
  INTEGER(I4B) :: ii
  !! main
  CALL getInterpolation(obj=trial, ans=matbar, val=c1)
  CALL getInterpolation(obj=trial, ans=c2bar, val=c2)
  CALL Reallocate( c1bar, SIZE(matbar, 1),  SIZE(matbar, 3))
  !!
  DO ii = 1, SIZE(c2bar, 2)
    c1bar(:,ii) = MATMUL(matbar(:,:,ii), c2bar(:,ii))
  END DO
  !!
  k = QuadratureVariable(c1bar, typeFEVariableVector, typeFEVariableSpace )
  CALL getProjectionOfdNdXt(obj=test, cdNdXt=c1bar, val=k)
  CALL getProjectionOfdNdXt(obj=trial, cdNdXt=c2bar, val=k)
  realval = trial%js * trial%ws * trial%thickness
  CALL reallocate(ans, SIZE(test%N, 1), SIZE(trial%N, 1))
  DO ii = 1, SIZE(realval)
    ans = ans + realval(ii) * OUTERPROD(c1bar(:, ii), c2bar(:, ii))
  END DO
  !!
  IF (PRESENT(opt)) CALL MakeDiagonalCopies(ans, opt)
  !!
  DEALLOCATE (c1bar, c2bar, realval, matbar)
END SUBROUTINE DM_9
