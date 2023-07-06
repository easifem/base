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

SUBMODULE(QuadraturePoint_Method:GaussLegendre) Line
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                       Line
!----------------------------------------------------------------------------

MODULE PROCEDURE getGaussLegendreQPLine1
INTEGER(I4B) :: np
REAL(DFP) :: pt(1 + INT(order / 2, KIND=I4B))
REAL(DFP) :: wt(1 + INT(order / 2, KIND=I4B))
! REAL(DFP) :: points(2, 1 + INT(order / 2, KIND=I4B))
np = SIZE(pt)
CALL LegendreQuadrature(n=np, pt=pt, wt=wt, quadType=Gauss)
! points = pt.ROWCONCAT.wt
CALL Initiate(obj=obj, points=pt.ROWCONCAT.wt)
END PROCEDURE getGaussLegendreQPLine1

!----------------------------------------------------------------------------
!                                                                       Line
!----------------------------------------------------------------------------

MODULE PROCEDURE getGaussLegendreQPLine2
REAL(DFP) :: pt(nips(1))
REAL(DFP) :: wt(nips(1))
! REAL(DFP) :: points(2, nips(1))
CALL LegendreQuadrature(n=nips(1), pt=pt, wt=wt, quadType=Gauss)
! points = pt.ROWCONCAT.wt
CALL Initiate(obj=obj, points=pt.ROWCONCAT.wt)
END PROCEDURE getGaussLegendreQPLine2

END SUBMODULE Line
