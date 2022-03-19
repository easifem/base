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

SUBMODULE(RealMatrix_Method) GetValuesMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                Get
!----------------------------------------------------------------------------

MODULE PROCEDURE realmat_get1
  Ans = obj%Val
END PROCEDURE realmat_get1

!----------------------------------------------------------------------------
!                                                                Get
!----------------------------------------------------------------------------

MODULE PROCEDURE realmat_get2
  Ans = obj%Val( RIndx, CIndx )
END PROCEDURE realmat_get2

!----------------------------------------------------------------------------
!                                                                Get
!----------------------------------------------------------------------------

MODULE PROCEDURE realmat_get3
#define Indx iStart:iEnd:Stride
  Ans = obj%Val( Indx, Indx )
#undef Indx
END PROCEDURE realmat_get3

!----------------------------------------------------------------------------
!                                                                       Get
!----------------------------------------------------------------------------

MODULE PROCEDURE realmat_get4
  Ans%Val = obj%Val
  CALL SetTotalDimension( Ans, 2_I4B )
END PROCEDURE realmat_get4

!----------------------------------------------------------------------------
!                                                                        Get
!----------------------------------------------------------------------------

MODULE PROCEDURE realmat_get5
  Ans%Val = obj%Val( RIndx, CIndx )
  CALL SetTotalDimension( Ans, 2_I4B )
END PROCEDURE realmat_get5

!----------------------------------------------------------------------------
!                                                                        Get
!----------------------------------------------------------------------------

MODULE PROCEDURE realmat_get6
#define Indx iStart:iEnd:Stride
  Ans%Val= obj%Val( Indx, Indx )
#undef Indx
  CALL SetTotalDimension( Ans, 2_I4B )
END PROCEDURE realmat_get6

!----------------------------------------------------------------------------
!                                                                      Get
!----------------------------------------------------------------------------

MODULE PROCEDURE realmat_get7
  INTEGER( I4B ) :: s( 2 ), i, j, r1, r2, c1, c2
  INTEGER( I4B ), ALLOCATABLE :: rc( :, : )
  !!
  !! main
  !!
  s = SHAPE( obj )
  ALLOCATE( rc( 0 : 2, 0 : ( s( 1 ) * s( 2 ) ) ) )
  rc = 0
  !!
  DO j = 1, s( 2 )
    DO i = 1, s( 1 )
      rc( 1:2, i+( j-1 )*s( 1 ) ) = SHAPE( obj( i, j ) )
    END DO
  END DO
  !!
  i = MAXVAL( SUM( RESHAPE( rc( 1, 1: ), SHAPE( obj ) ), 1 ) )
  j = MAXVAL( SUM( RESHAPE( rc( 2, 1: ), SHAPE( obj ) ), 2 ) )
  !!
  ALLOCATE( Ans( i, j ) ); Ans = 0.0_DFP
  !!
  c1 = 0; c2 = 0
  !!
  DO j = 1, s( 2 )
    c1 = 1 + c2
    c2 = c1 + rc( 2, j ) - 1
    r1 = 0; r2 = 0
    DO i = 1, s( 1 )
      r1 = 1 + r2
      r2 = r1 + rc( 1, i ) - 1
      Ans( r1:r2, c1:c2 ) = obj( i, j )%Val
    END DO
  END DO
  !!
END PROCEDURE realmat_get7

!----------------------------------------------------------------------------
!                                                                        Get
!----------------------------------------------------------------------------

MODULE PROCEDURE realmat_get8
  Ans%Val = Get( obj, TypeDFP )
  CALL setTotalDimension( Ans, 2_I4B )
END PROCEDURE realmat_get8

!----------------------------------------------------------------------------
!                                                                       Copy
!----------------------------------------------------------------------------

MODULE PROCEDURE realmat_copy1
  To = From%Val
END PROCEDURE realmat_copy1

!----------------------------------------------------------------------------
!                                                                       Copy
!----------------------------------------------------------------------------

MODULE PROCEDURE realmat_copy2
  To%Val = From%Val
  CALL SetTotalDimension( To, 2_I4B )
END PROCEDURE realmat_copy2

!----------------------------------------------------------------------------
!                                                                       Copy
!----------------------------------------------------------------------------

MODULE PROCEDURE realmat_copy3
  To%Val = From
  CALL SetTotalDimension( To, 2_I4B )
END PROCEDURE realmat_copy3

!----------------------------------------------------------------------------
!                                                               ArrayPointer
!----------------------------------------------------------------------------

MODULE PROCEDURE realmat_getPointer
  Ans => obj%Val
END PROCEDURE realmat_getPointer

END SUBMODULE GetValuesMethods