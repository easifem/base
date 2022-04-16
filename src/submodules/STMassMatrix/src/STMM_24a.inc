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
!
!----------------------------------------------------------------------------

#define _KIJ_ outerprod(vbar(:,ips, ipt), [1.0_DFP])
#define _DIM1_ SIZE(vbar, 1)
#define _DIM2_ 1

PURE SUBROUTINE STMM_24a(ans, test, trial, term1, term2, c1, c2)
#include "./STMM_24.inc"
END SUBROUTINE STMM_24a

#undef _DIM1_
#undef _DIM2_
#undef _KIJ_