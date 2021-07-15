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

SUBMODULE( QuadraturePoint_Method:GaussLegendre ) Quadrangle
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                 Quadrangle
!----------------------------------------------------------------------------

PURE FUNCTION TensorProd( PW, n ) RESULT( Ans )
  ! Define intent of dummy variables
  INTEGER( I4B ), INTENT( IN ) :: n
  REAL( DFP ), INTENT( IN ) :: PW( :, : )
  REAL( DFP ) :: Ans( 3, n*n )
  ! define internal variable
  INTEGER( I4B ) :: i, a, b
  DO i = 1, n
    a = ( i - 1 ) * n + 1
    b = i * n
    Ans( 1, a:b ) = PW( 1, : )
    Ans( 2, a:b ) = PW( 1, i )
    Ans( 3, a:b ) = PW( 2, i ) * PW( 2, : )
  END DO
END FUNCTION TensorProd

!----------------------------------------------------------------------------
!                                                                  Quadrangle
!----------------------------------------------------------------------------

MODULE PROCEDURE getGaussLegendreQPQuadrangle1
  SELECT CASE(Order)
  CASE(0, 1)
    ! 1 x 1
    CALL Initiate( obj, TensorProd( PW1, 1 ) )
  CASE(2, 3)
    ! 2 x 2
    CALL Initiate( obj, TensorProd( PW2, 2 ) )
  CASE(4, 5)
    CALL Initiate( obj, TensorProd( PW3, 3 ) )
  CASE( 6, 7 )
    CALL Initiate( obj, TensorProd( PW4, 4 ) )
  CASE( 8, 9 )
    CALL Initiate( obj, TensorProd( PW5, 5 ) )
  CASE( 10, 11 )
    CALL Initiate( obj, TensorProd( PW6, 6 ) )
  CASE( 12, 13 )
    CALL Initiate( obj, TensorProd( PW7, 7 ) )
  CASE( 14, 15 )
    CALL Initiate( obj, TensorProd( PW8, 8 ) )
  CASE( 16, 17 )
    CALL Initiate( obj, TensorProd( PW9, 9 ) )
  CASE( 18, 19 )
    CALL Initiate( obj, TensorProd( PW10, 10 ) )
  CASE( 20, 21 )
    CALL Initiate( obj, TensorProd( PW11, 11 ) )
  CASE( 22, 23 )
    CALL Initiate( obj, TensorProd( PW12, 12 ) )
  END SELECT
END PROCEDURE getGaussLegendreQPQuadrangle1

!----------------------------------------------------------------------------
!                                                                Quadrangle
!----------------------------------------------------------------------------

MODULE PROCEDURE getGaussLegendreQPQuadrangle2
  SELECT CASE( NIPS( 1 ) )
  CASE( 1 )
    ! 1 x 1
    CALL Initiate( obj, TensorProd( PW1, 1 ) )
  CASE( 4 )
    ! 2 x 2
    CALL Initiate( obj, TensorProd( PW2, 2 ) )
  CASE( 9 )
    CALL Initiate( obj, TensorProd( PW3, 3 ) )
  CASE( 16 )
    CALL Initiate( obj, TensorProd( PW4, 4 ) )
  CASE( 25 )
    CALL Initiate( obj, TensorProd( PW5, 5 ) )
  CASE( 36 )
    CALL Initiate( obj, TensorProd( PW6, 6 ) )
  CASE( 49 )
    CALL Initiate( obj, TensorProd( PW7, 7 ) )
  CASE( 64 )
    CALL Initiate( obj, TensorProd( PW8, 8 ) )
  CASE( 81 )
    CALL Initiate( obj, TensorProd( PW9, 9 ) )
  CASE( 100 )
    CALL Initiate( obj, TensorProd( PW10, 10 ) )
  CASE( 121 )
    CALL Initiate( obj, TensorProd( PW11, 11 ) )
  CASE( 144 )
    CALL Initiate( obj, TensorProd( PW12, 12 ) )
  END SELECT
END PROCEDURE getGaussLegendreQPQuadrangle2

END SUBMODULE
