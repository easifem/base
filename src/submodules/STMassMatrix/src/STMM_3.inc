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

#define _NT1_ OUTERPROD(test(ipt)%N(:,ips), test(ipt)%T)
#define _NT2_ trial(ipt)%dNTdt(:,:,ips)

PURE SUBROUTINE STMM_3(ans, test, trial, term1, term2, opt)
  REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: ans(:, :, :, :)
  CLASS(STElemshapeData_), INTENT(IN) :: test(:)
  CLASS(STElemshapeData_), INTENT(IN) :: trial(:)
  INTEGER(I4B), INTENT(IN) :: term1
  !! del_t
  INTEGER(I4B), INTENT(IN) :: term2
  !! del_none
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: opt
  !!
  !! Internal variable
  REAL(DFP), ALLOCATABLE :: iajb(:, :, :, :)
  REAL(DFP), ALLOCATABLE :: realval(:)
  INTEGER( I4B ) :: ips, ipt
  !!
  !! main
  !!
  CALL Reallocate(iajb, SIZE(test(1)%N, 1), SIZE(test(1)%T), &
    & SIZE(trial(1)%N, 1), SIZE(trial(1)%T))
  !!
  DO ipt = 1, SIZE(trial)
    realval = trial(ipt)%js * trial(ipt)%ws * trial(ipt)%thickness * &
         & trial(ipt)%wt * trial(ipt)%jt
    DO ips = 1, SIZE(realval)
      iajb = iajb + realval(ips) * OUTERPROD(_NT1_, _NT2_)
    END DO
  END DO
  !!
  CALL SWAP(a=ans, b=iajb, i1=1, i2=3, i3=2, i4=4)
  !!
  if(present(opt)) call MakeDiagonalCopiesIJab(ans, opt)
  !!
  DEALLOCATE(iajb, realval)
  !!
END SUBROUTINE STMM_3
#undef _NT1_
#undef _NT2_
