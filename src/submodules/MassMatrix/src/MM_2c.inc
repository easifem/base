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

!----------------------------------------------------------------------------
!                                                                MassMatrix
!----------------------------------------------------------------------------

PURE SUBROUTINE MM_2c(ans, test, trial, rho, opt)
  REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: ans(:, :)
  CLASS(ElemshapeData_), INTENT(IN) :: test
  !! Shapedata for test function
  CLASS(ElemshapeData_), INTENT(IN) :: trial
  !! Shapedata for trial function
  CLASS(FEVariable_), INTENT(IN) :: rho
  !! vector variable
  INTEGER( I4B ), INTENT( IN ) :: opt
  !! 3
  !! Define internal variable
  REAL(DFP), ALLOCATABLE :: realval(:)
  REAL(DFP), ALLOCATABLE :: m2(:, :)
  REAL(DFP), ALLOCATABLE :: vbar(:, :)
  REAL(DFP), ALLOCATABLE :: m4(:, :, :, :)
  INTEGER(I4B) :: ips, ii
  !!
  !! main
  !!
  CALL getInterpolation(obj=trial, interpol=vbar, val=rho)
  !!
  CALL reallocate(m4, SIZE(test%N, 1), SIZE(trial%N, 1), &
    & SIZE(vbar, 1), SIZE(vbar, 1))
  !!
  realval = trial%js * trial%ws * trial%thickness
  !!
  DO ips = 1, SIZE(vbar, 2)
    m2 = OUTERPROD(a=test%N(:, ips), b=trial%N(:, ips))
    DO ii = 1, SIZE(vbar, 1)
      m4(:, :, ii, ii) = m4(:, :, ii, ii) &
        & + realval(ips) * vbar(ii, ips) * m2
    END DO
  END DO
  !!
  CALL Convert(from=m4, to=ans)
  !!
  DEALLOCATE (realval, m2, vbar, m4)
  !!
END SUBROUTINE MM_2c
