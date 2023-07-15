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

MODULE GaussLegendreLobatto_Prism_Methods
USE BaseMethod
IMPLICIT NONE
PRIVATE
PUBLIC :: getGaussLegendreLobattoQPPrism1
PUBLIC :: getGaussLegendreLobattoQPPrism2
CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

FUNCTION getGaussLegendreLobattoQPPrism1(order) &
  & RESULT(obj)
  INTEGER(I4B), INTENT(IN) :: order
  TYPE(QuadraturePoint_) :: obj
  CALL ErrorMsg(&
    & msg="This routine is under development",  &
    & file=__FILE__,  &
    & routine="getGaussLegendreQPPrism1()", &
    & line=__LINE__, &
    & unitno=stderr)
END FUNCTION getGaussLegendreLobattoQPPrism1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

FUNCTION getGaussLegendreLobattoQPPrism2(nips) &
  & RESULT(obj)
  INTEGER(I4B), INTENT(IN) :: nips(1)
  TYPE(QuadraturePoint_) :: obj
  CALL ErrorMsg(&
    & msg="This routine is under development",  &
    & file=__FILE__,  &
    & routine="getGaussLegendreQPPrism2()", &
    & line=__LINE__, &
    & unitno=stderr)
END FUNCTION getGaussLegendreLobattoQPPrism2

END MODULE GaussLegendreLobatto_Prism_Methods
