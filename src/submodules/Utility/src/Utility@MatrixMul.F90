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

!> authors: Vikas Sharma, Ph. D.
! date: 	3 April 2021
! summary: 	Methods for matrix multiplication

SUBMODULE( Utility ) MatrixMul
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                     MATMUL
!----------------------------------------------------------------------------

MODULE PROCEDURE matmul_r3_r1
  INTEGER( I4B ) :: ii
  Ans = a2( 1 ) * a1( :, :, 1 )
  DO ii = 2, SIZE( a2 )
    Ans = Ans + a2( ii ) * a1( :, :, ii )
  END DO
END PROCEDURE matmul_r3_r1

!----------------------------------------------------------------------------
!                                                                     MATMUL
!----------------------------------------------------------------------------

MODULE PROCEDURE matmul_r1_r3
  INTEGER( I4B ) :: ii
  Ans = a1(1)*a2(1,:,:)
  DO ii = 2, SIZE( a1 )
    Ans = Ans + a1(ii)*a2(ii,:,:)
  END DO
END PROCEDURE matmul_r1_r3

!----------------------------------------------------------------------------
!                                                                     MATMUL
!----------------------------------------------------------------------------

MODULE PROCEDURE matmul_r2_r3
  INTEGER( I4B ) :: ii
  DO ii = 1, SIZE( a2, 3 )
    Ans( :, :, ii ) = MATMUL( a1, a2( :, :, ii ) )
  END DO
END PROCEDURE matmul_r2_r3

!----------------------------------------------------------------------------
!                                                                     MATMUL
!----------------------------------------------------------------------------

MODULE PROCEDURE matmul_r4_r1
  INTEGER( I4B ) :: ii
  Ans = a2( 1 ) * a1( :, :, :, 1 )
  DO ii = 2, SIZE( a2 )
    Ans = Ans + a2( ii ) * a1( :, :, :, ii )
  END DO
END PROCEDURE matmul_r4_r1

!----------------------------------------------------------------------------
!                                                                     MATMUL
!----------------------------------------------------------------------------

MODULE PROCEDURE matmul_r3_r2
  INTEGER( I4B ) :: ip
  DO ip = 1, SIZE( a2, 2)
    Ans( :,:, ip ) = MATMUL( a1, a2( :, ip ) )
  END DO
END PROCEDURE matmul_r3_r2

END SUBMODULE MatrixMul