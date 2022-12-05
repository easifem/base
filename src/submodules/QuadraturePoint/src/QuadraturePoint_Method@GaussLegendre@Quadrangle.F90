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

SUBMODULE(QuadraturePoint_Method:GaussLegendre) Quadrangle
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                 Quadrangle
!----------------------------------------------------------------------------

MODULE PROCEDURE getGaussLegendreQPQuadrangle1
TYPE(QuadraturePoint_) :: obj1
obj1 = getGaussLegendreQPLine1(order=order)
obj = Outerprod(obj1=obj1, obj2=obj1)
CALL Deallocate (obj1)
END PROCEDURE getGaussLegendreQPQuadrangle1

!----------------------------------------------------------------------------
!                                                                Quadrangle
!----------------------------------------------------------------------------

MODULE PROCEDURE getGaussLegendreQPQuadrangle2
TYPE(QuadraturePoint_) :: obj1, obj2
INTEGER(I4B) :: np(2)
!!
IF (SIZE(nips) .EQ. 1) THEN
  np = INT(SQRT(REAL(nips(1))), KIND=I4B)
  obj1 = getGaussLegendreQPLine2(nips=np(1:1))
  obj = Outerprod(obj1=obj1, obj2=obj1)
ELSE
  np = nips
  obj1 = getGaussLegendreQPLine2(nips=np(1:1))
  obj2 = getGaussLegendreQPLine2(nips=np(2:2))
  obj = Outerprod(obj1=obj1, obj2=obj2)
END IF
CALL Deallocate (obj1)
CALL Deallocate (obj2)
END PROCEDURE getGaussLegendreQPQuadrangle2

!----------------------------------------------------------------------------
!                                                                 Quadrangle
!----------------------------------------------------------------------------

MODULE PROCEDURE getGaussLegendreQPQuadrangle3
TYPE(QuadraturePoint_) :: obj1, obj2
obj1 = getGaussLegendreQPLine1(order=p)
obj2 = getGaussLegendreQPLine1(order=q)
obj = Outerprod(obj1=obj1, obj2=obj2)
CALL Deallocate (obj1)
CALL Deallocate (obj2)
END PROCEDURE getGaussLegendreQPQuadrangle3

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE
