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
  SELECT CASE( Order )
  CASE( 0, 1 )
    CALL Initiate( obj, Pw1 )
  CASE( 2, 3 )
    CALL Initiate( obj, Pw2 )
  CASE( 4, 5 )
    CALL Initiate( obj, Pw3 )
  CASE( 6, 7 )
    CALL Initiate( obj, Pw4 )
  CASE( 8, 9 )
    CALL Initiate( obj, Pw5 )
  CASE( 10, 11 )
    CALL Initiate( obj, Pw6 )
  CASE( 12, 13 )
    CALL Initiate( obj, Pw7 )
  CASE( 14, 15 )
    CALL Initiate( obj, Pw8 )
  CASE( 16, 17 )
    CALL Initiate( obj, Pw9 )
  CASE( 18, 19 )
    CALL Initiate( obj, Pw10 )
  CASE( 20, 21 )
    CALL Initiate( obj, Pw11 )
  CASE( 22, 23 )
    CALL Initiate( obj, Pw12 )
  END SELECT
END PROCEDURE getGaussLegendreQPLine1

!----------------------------------------------------------------------------
!                                                                       Line
!----------------------------------------------------------------------------

MODULE PROCEDURE getGaussLegendreQPLine2
  SELECT CASE( NIPS( 1 ) )
  CASE( 1 )
    CALL Initiate( obj, Pw1 )
  CASE( 2 )
    CALL Initiate( obj, Pw2 )
  CASE( 3 )
    CALL Initiate( obj, Pw3 )
  CASE( 4 )
    CALL Initiate( obj, Pw4 )
  CASE( 5 )
    CALL Initiate( obj, Pw5 )
  CASE( 6 )
    CALL Initiate( obj, Pw6 )
  CASE( 7 )
    CALL Initiate( obj, Pw7 )
  CASE( 8 )
    CALL Initiate( obj, Pw8 )
  CASE( 9 )
    CALL Initiate( obj, Pw9 )
  CASE( 10 )
    CALL Initiate( obj, Pw10 )
  CASE( 11 )
    CALL Initiate( obj, Pw11 )
  CASE( 12 )
    CALL Initiate( obj, Pw12 )
  END SELECT
END PROCEDURE getGaussLegendreQPLine2

END SUBMODULE Line