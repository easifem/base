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

#define _NT1_ test(ipt)%dNTdt(:,:,ips)
#define _NT2_ OUTERPROD(trial(ipt)%N(:,ips), trial(ipt)%T)

PURE SUBROUTINE STMM_6(ans, test, trial, term1, term2, rho, opt)
  REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: ans(:, :, :, :)
  CLASS(STElemshapeData_), INTENT(IN) :: test(:)
  CLASS(STElemshapeData_), INTENT(IN) :: trial(:)
  INTEGER(I4B), INTENT(IN) :: term1
  !! del_none
  INTEGER(I4B), INTENT(IN) :: term2
  !! del_none
  CLASS(FEVariable_), INTENT(IN) :: rho
  !! scalar
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: opt
  !!
  !! Internal variable
  REAL(DFP), ALLOCATABLE :: iajb(:, :, :, :)
  REAL(DFP), ALLOCATABLE :: rhobar(:, :)
  REAL(DFP), ALLOCATABLE :: realval(:)
  INTEGER(I4B) :: ipt, ips
  !!
  !! main
  !!
  CALL Reallocate(iajb, SIZE(test(1)%N, 1), SIZE(test(1)%T), &
    & SIZE(trial(1)%N, 1), SIZE(trial(1)%T))
  CALL getInterpolation(obj=trial, interpol=rhobar, val=rho)
  !!
  DO ipt = 1, SIZE(trial)
    realval = trial(ipt)%js * trial(ipt)%ws * trial(ipt)%thickness * &
         & trial(ipt)%wt * trial(ipt)%jt * rhobar(:, ipt)
    DO ips = 1, SIZE(realval)
      iajb = iajb + realval(ips) * OUTERPROD(_NT1_, _NT2_)
    END DO
  END DO
  !!
  CALL SWAP(a=ans, b=iajb, i1=1, i2=3, i3=2, i4=4)
  !!
  if(present(opt)) call MakeDiagonalCopiesIJab(ans, opt)
  !!
  DEALLOCATE (iajb, realval)
  !!
END SUBROUTINE STMM_6
#undef _NT1_
#undef _NT2_
