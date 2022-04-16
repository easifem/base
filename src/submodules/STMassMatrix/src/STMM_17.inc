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
#define _NT2_ OUTERPROD(trial(ipt)%N(:,ips), trial(ipt)%T)
PURE SUBROUTINE STMM_17(ans, test, trial, term1, term2, c1, c2, opt)
#include "./STMM_17_20.inc"
END SUBROUTINE STMM_17
#undef _NT1_
#undef _NT2_
