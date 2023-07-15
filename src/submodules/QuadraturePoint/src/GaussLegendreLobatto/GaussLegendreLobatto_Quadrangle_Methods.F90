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

MODULE GaussLegendreLobatto_Quadrangle_Methods
USE BaseMethod
USE GaussLegendreLobatto_Line_Methods
IMPLICIT NONE
PRIVATE
PUBLIC :: getGaussLegendreLobattoQPQuadrangle1
PUBLIC :: getGaussLegendreLobattoQPQuadrangle2
PUBLIC :: getGaussLegendreLobattoQPQuadrangle3
CONTAINS

!----------------------------------------------------------------------------
!                                                                 Quadrangle
!----------------------------------------------------------------------------

FUNCTION getGaussLegendreLobattoQPQuadrangle1(order) &
  & RESULT(obj)
  INTEGER(I4B), INTENT(IN) :: order
  TYPE(QuadraturePoint_) :: obj
  TYPE(QuadraturePoint_) :: obj1
  obj1 = getGaussLegendreLobattoQPLine1(order=order)
  obj = Outerprod(obj1=obj1, obj2=obj1)
  CALL DEALLOCATE (obj1)
END FUNCTION getGaussLegendreLobattoQPQuadrangle1

!----------------------------------------------------------------------------
!                                                                Quadrangle
!----------------------------------------------------------------------------

FUNCTION getGaussLegendreLobattoQPQuadrangle2(nips) &
  & RESULT(obj)
  INTEGER(I4B), INTENT(IN) :: nips(:)
  TYPE(QuadraturePoint_) :: obj
  TYPE(QuadraturePoint_) :: obj1, obj2
  INTEGER(I4B) :: np(2)
  !!
  IF (SIZE(nips) .EQ. 1) THEN
    np = INT(SQRT(REAL(nips(1))), KIND=I4B)
    obj1 = getGaussLegendreLobattoQPLine2(nips=np(1:1))
    obj = Outerprod(obj1=obj1, obj2=obj1)
  ELSE
    np = nips
    obj1 = getGaussLegendreLobattoQPLine2(nips=np(1:1))
    obj2 = getGaussLegendreLobattoQPLine2(nips=np(2:2))
    obj = Outerprod(obj1=obj1, obj2=obj2)
  END IF
  CALL DEALLOCATE (obj1)
  CALL DEALLOCATE (obj2)
END FUNCTION getGaussLegendreLobattoQPQuadrangle2

!----------------------------------------------------------------------------
!                                                                 Quadrangle
!----------------------------------------------------------------------------

FUNCTION getGaussLegendreLobattoQPQuadrangle3(p, q) &
  & RESULT(obj)
  INTEGER(I4B), INTENT(IN) :: p
  INTEGER(I4B), INTENT(IN) :: q
  TYPE(QuadraturePoint_) :: obj
  TYPE(QuadraturePoint_) :: obj1, obj2
  obj1 = getGaussLegendreLobattoQPLine1(order=p)
  obj2 = getGaussLegendreLobattoQPLine1(order=q)
  obj = Outerprod(obj1=obj1, obj2=obj2)
  CALL DEALLOCATE (obj1)
  CALL DEALLOCATE (obj2)
END FUNCTION getGaussLegendreLobattoQPQuadrangle3

END MODULE GaussLegendreLobatto_Quadrangle_Methods
