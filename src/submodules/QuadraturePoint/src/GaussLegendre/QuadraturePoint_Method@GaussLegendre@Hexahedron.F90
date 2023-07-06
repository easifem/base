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

SUBMODULE(QuadraturePoint_Method:GaussLegendre) Hexahedron
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE getGaussLegendreQPHexahedron1
TYPE(QuadraturePoint_) :: obj1
obj1 = getGaussLegendreQPLine1(order=order)
obj = Outerprod(Outerprod(obj1=obj1, obj2=obj1), obj1)
CALL DEALLOCATE (obj1)
END PROCEDURE getGaussLegendreQPHexahedron1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE getGaussLegendreQPHexahedron2
IF (SIZE(nips) .EQ. 1) THEN
  CALL ErrorMsg(&
  & msg="three nips (in x, y, and z) direction should be given", &
  & file=__FILE__, &
  & line=__LINE__, &
  & routine="getGaussLegendreQPHexahedron2()", &
  & unitNo=stderr)
  RETURN
ELSE
  obj = Outerprod(&
    & Outerprod(&
      & obj1=getGaussLegendreQPLine2(nips=nips(1:1)), &
      & obj2=getGaussLegendreQPLine2(nips=nips(2:2))), &
    & getGaussLegendreQPLine2(nips=nips(3:3)))
END IF
END PROCEDURE getGaussLegendreQPHexahedron2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE getGaussLegendreQPHexahedron3
obj = Outerprod(&
  & Outerprod(&
    & obj1=getGaussLegendreQPLine1(order=p), &
    & obj2=getGaussLegendreQPLine1(order=q)), &
  & getGaussLegendreQPLine1(order=r))
END PROCEDURE getGaussLegendreQPHexahedron3

END SUBMODULE
