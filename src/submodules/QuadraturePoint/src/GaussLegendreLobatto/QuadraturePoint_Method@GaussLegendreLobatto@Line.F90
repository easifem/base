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

SUBMODULE(QuadraturePoint_Method:GaussLegendreLobatto) GLL_Line
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                       Line
!----------------------------------------------------------------------------

MODULE PROCEDURE getGaussLegendreLobattoQPLine1
INTEGER(I4B) :: np
REAL(DFP) :: pt(2 + INT(order / 2, KIND=I4B))
REAL(DFP) :: wt(2 + INT(order / 2, KIND=I4B))
np = SIZE(pt)
IF (np .EQ. 2_I4B) THEN
  pt = [-1.0_DFP, 1.0_DFP]
  wt = 1.0_DFP
ELSE
  CALL LegendreQuadrature(n=np, pt=pt, wt=wt, quadType=GaussLobatto)
END IF
CALL Initiate(obj=obj, points=pt.ROWCONCAT.wt)
END PROCEDURE getGaussLegendreLobattoQPLine1

!----------------------------------------------------------------------------
!                                                                       Line
!----------------------------------------------------------------------------

MODULE PROCEDURE getGaussLegendreLobattoQPLine2
REAL(DFP) :: pt(nips(1))
REAL(DFP) :: wt(nips(1))
CALL LegendreQuadrature(n=nips(1), pt=pt, wt=wt, quadType=GaussLobatto)
CALL Initiate(obj=obj, points=pt.ROWCONCAT.wt)
END PROCEDURE getGaussLegendreLobattoQPLine2

END SUBMODULE GLL_Line
