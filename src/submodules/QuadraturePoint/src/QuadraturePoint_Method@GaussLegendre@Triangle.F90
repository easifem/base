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

SUBMODULE(QuadraturePoint_Method:GaussLegendre) Triangle
IMPLICIT NONE

#include "./GaussLegendre/QuadDataTriangle.inc"

CONTAINS

!----------------------------------------------------------------------------
!                                                                  Triangle
!----------------------------------------------------------------------------

MODULE PROCEDURE getGaussLegendreQPTriangle1
SELECT CASE (order)
CASE (0, 1)
  CALL Initiate(obj, TPW1)
CASE (2)
  CALL Initiate(obj, TPW3)
CASE (3)
  CALL Initiate(obj, TPW4)
CASE (4)
  CALL Initiate(obj, TPW6)
CASE (5)
  CALL Initiate(obj, TPW7)
CASE (6)
  CALL Initiate(obj, TPW12)
CASE (7)
  CALL Initiate(obj, TPW13)
CASE (8)
  CALL Initiate(obj, TPW19)
CASE (9)
  CALL Initiate(obj, TPW19b)
CASE (10, 11)
  CALL Initiate(obj, TPW28)
END SELECT
END PROCEDURE getGaussLegendreQPTriangle1

!----------------------------------------------------------------------------
!                                                                  Triangle
!----------------------------------------------------------------------------

MODULE PROCEDURE getGaussLegendreQPTriangle2
SELECT CASE (nips(1))
CASE (1)
  CALL Initiate(obj, TPW1)
CASE (3)
  CALL Initiate(obj, TPW3)
CASE (4)
  CALL Initiate(obj, TPW4)
CASE (6)
  CALL Initiate(obj, TPW6)
CASE (7)
  CALL Initiate(obj, TPW7)
CASE (12)
  CALL Initiate(obj, TPW12)
CASE (13)
  CALL Initiate(obj, TPW13)
CASE (19)
  CALL Initiate(obj, TPW19)
CASE (28)
  CALL Initiate(obj, TPW28)
END SELECT
END PROCEDURE getGaussLegendreQPTriangle2

END SUBMODULE
