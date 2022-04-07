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

PURE SUBROUTINE STDM_3(ans, test, trial, k, opt)
  REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: ans(:, :, :, :)
  CLASS(STElemshapeData_), INTENT(IN) :: test(:)
  !! test function
  CLASS(STElemshapeData_), INTENT(IN) :: trial(:)
  !! trial function
  CLASS(FEVariable_), INTENT(IN) :: k
  !! Matrix variable
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: opt
  !!
  !! Internal variable
  REAL(DFP), ALLOCATABLE :: realval(:)
  REAL(DFP), ALLOCATABLE :: IaJb(:, :, :, :)
  REAL(DFP), ALLOCATABLE :: kbar(:, :, :, :)
  INTEGER(I4B) :: ips, ipt, ii, jj, nsd
  !!
  !! main
  CALL Reallocate(IaJb, SIZE(test(1)%N, 1), SIZE(test(1)%T), &
    & SIZE(trial(1)%N, 1), SIZE(trial(1)%T))
  !!
  CALL getInterpolation(obj=trial, interpol=kbar, val=k)
  !!
  nsd = trial(1)%refelem%nsd
  !!
  DO ipt = 1, SIZE(trial)
    !!
    realval = trial(ipt)%js * trial(ipt)%ws * trial(ipt)%thickness &
     & * trial(ipt)%wt * trial(ipt)%jt
    !!
    DO ips = 1, SIZE(realval)
      !!
      DO jj = 1, nsd
        !!
        DO ii = 1, nsd
          !!
          IaJb = IaJb + realval(ips) * kbar(ii, jj, ips, ipt) * &
            & OUTERPROD(test(ipt)%dNTdXt(:, :, ii, ips), &
            & trial(ipt)%dNTdXt(:, :, jj, ips))
          !!
        END DO
      END DO
    END DO
  END DO
  !!
  CALL SWAP(a=ans, b=IaJb, i1=1, i2=3, i3=2, i4=4)
  if(present(opt)) call MakeDiagonalCopiesIJab(ans, opt)
  !!
  DEALLOCATE (realval, KBar, IaJb)
END SUBROUTINE STDM_3
