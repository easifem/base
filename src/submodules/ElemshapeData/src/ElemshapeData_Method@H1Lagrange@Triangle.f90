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

SUBMODULE( ElemshapeData_Method : H1Lagrange ) Triangle
USE BaseMethod
IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                                  Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE Triangle_H1_Lagrange
  INTEGER( I4B ) :: nips
  REAL( DFP ), ALLOCATABLE :: XiEta( :, : )

  CALL Initiate( Obj % RefElem, RefElem )
  CALL getQuadraturePoints( Obj = Quad, Point = XiEta,  Weight = Obj % Ws )
  Obj % Quad = Quad
  nips = SIZE( Obj % Ws )

  SELECT CASE( RefElem%order )

  !---------------------------------------------------------------!
  !---------------------------------------------------------------!

  CASE( 1 )
    BLOCK
      !                 define internal variables
      INTEGER( I4B ) :: ips
      !         ....................................         !

      call AllocateData( obj = obj, nsd = refelem % nsd, &
        & xidim = refelem % xidimension, nns = 3, nips = nips )

      Obj % N( 1, : ) = 1.0_DFP - xieta( 1, : ) - xieta( 2, : )
      Obj % N( 2, : ) = xieta( 1, : )
      Obj % N( 3, : ) = xieta( 2, : )

      Obj % dNdXi( :, :, 1  ) = RESHAPE( [ -1.0, 1.0, 0.0, -1.0, 0.0, 1.0 ], &
        & [3, 2] )
      DO ips = 2, nips
        Obj % dNdXi( :, :, ips ) = Obj % dNdXi( :, :, 1 )
      END DO
    END BLOCK

  !---------------------------------------------------------------!
  !---------------------------------------------------------------!

  CASE( 2 )
    BLOCK
      !                 define internal variables
      REAL( DFP ) ::  l1( nips ), l2( nips ), l3( nips ), dldxi( 3, 2 ), &
        & dndl( 6, 3 )

      INTEGER( I4B ) :: ips
      !         ....................................         !

      call AllocateData( obj = obj, nsd = refelem % nsd, &
        & xidim = refelem % xidimension, nns = 6, nips = nips )

      dldxi = RESHAPE( [ -1.0, 1.0, 0.0, -1.0, 0.0, 1.0 ], [3, 2] )
      l1( : ) = 1.0_DFP - xieta( 1, : ) - xieta( 2, : )
      l2( : ) = xieta( 1, : )
      l3( : ) = xieta( 2, : )

      Obj % N( 1, : ) = l1 * ( 2.0 * l1 - 1.0 )
      Obj % N( 2, : ) = l2 * ( 2.0 * l2 - 1.0 )
      Obj % N( 3, : ) = l3 * ( 2.0 * l3 - 1.0 )
      Obj % N( 4, : ) = l1 * l2
      Obj % N( 5, : ) = l2 * l3
      Obj % N( 6, : ) = l3 * l1

      DO ips = 1, nips
        dndl = RESHAPE( &
          &   [ 4.0_DFP * l1( ips ) - 1.0_DFP, 0.0_DFP, 0.0_DFP, l2( ips ), &
              & 0.0_DFP, l3( ips ),  &
              & 0.0_DFP, 4.0_DFP * l2( ips ) - 1.0_DFP, 0.0_DFP, l1( ips ), &
              & l3( ips ), 0.0_DFP,  &
              & 0.0_DFP, 0.0_DFP, 4.0_DFP * l3( ips ) - 1.0_DFP, 0.0_DFP, &
              & l2( ips ), l1( ips )   &
              & ], [6, 3] &
          & )
        Obj % dNdXi( :, :, ips ) = MATMUL( dndl, dldxi )
      END DO
    END BLOCK

  CASE( 3 )
    BLOCK
      !                 define internal variables
      REAL( DFP ) :: l1( nips ), l2( nips ), l3( nips ), dldxi( 3, 2 ), &
        & dndl( 10, 3 ), a1, a2
      INTEGER( I4B ) :: ips
      !         ....................................         !

      call AllocateData( obj = obj, nsd = refelem % nsd, &
        & xidim = refelem % xidimension, nns = 10, nips = nips )

      a1 = 1.0/3.0; a2 = 2.0/3.0
      dldxi = RESHAPE( [ -1.0, 1.0, 0.0, -1.0, 0.0, 1.0 ], [3, 2] )
      l1( : ) = 1.0_DFP - xieta( 1, : ) - xieta( 2, : )
      l2( : ) = xieta( 1, : )
      l3( : ) = xieta( 2, : )


      Obj % N( 1, : ) = &
        &   ( l1 - a2 ) &
        & * ( l1 - a1 ) &
        & * ( l1 - 0.0_DFP ) &
        & / ( 1.0_DFP - a2 ) &
        & / ( 1.0_DFP - a1 ) &
        & / ( 1.0_DFP - 0.0_DFP )

      Obj % N( 2, : ) = &
        &   ( l2 - a2 ) &
        & * ( l2 - a1 ) &
        & * ( l2 - 0.0_DFP ) &
        & / ( 1.0_DFP - a2 ) &
        & / ( 1.0_DFP - a1 ) &
        & / ( 1.0_DFP - 0.0_DFP )

      Obj % N( 3, : ) = &
        &   ( l3 - a2 ) &
        & * ( l3 - a1 ) &
        & * ( l3 - 0.0_DFP ) &
        & / ( 1.0_DFP - a2 ) &
        & / ( 1.0_DFP - a1 ) &
        & / ( 1.0_DFP - 0.0_DFP )

      Obj % N( 4, : )  = l1 * l2 * ( l1 - a1 )
      Obj % N( 5, : )  = l1 * l2 * ( l2 - a1 )
      Obj % N( 6, : )  = l2 * l3 * ( l2 - a1 )
      Obj % N( 7, : )  = l2 * l3 * ( l3 - a1 )
      Obj % N( 8, : )  = l3 * l1 * ( l3 - a1 )
      Obj % N( 9, : )  = l3 * l1 * ( l1 - a1 )
      Obj % N( 10, : ) = l1 * l2 * l3

      DO ips = 1, nips
        dndl = RESHAPE( &
          & [ &
          & 9.0_DFP * (l1**2) - 7.0_DFP * l1 + 2.0_DFP, 0.0_DFP, 0.0_DFP, &
          & l2 * ( 2.0_DFP * l1 - a1 ), ( l2**2 ) - l2/3.0_DFP, 0.0_DFP, &
          & 0.0_DFP, l3**2 - l3/3.0_DFP, l3 * ( 2.0_DFP * l1 - a1 ), &
          & l2 * l3, &
          & 0.0_DFP, 9.0_DFP * (l2**2) - 7.0_DFP * l2 + 2.0_DFP, 0.0_DFP, &
          & (l1**2) - l1/3.0_DFP, l1 * ( 2.0_DFP * l2 - a1 ), &
          & l3 * ( 2.0_DFP * l2 - a1 ), &
          & (l3**2) - l3/3.0_DFP, 0.0_DFP, 0.0_DFP, &
          & l1 * l3, &
          & 0.0_DFP, 0.0_DFP, 9.0_DFP * ( l3 ** 2 ) - 7.0_DFP * l3 + 1.0_DFP,&
          & 0.0_DFP, 0.0_DFP, l2**2 - l2/3.0_DFP, &
          & l2 * ( 2.0_DFP * l3 - a1 ), l1 * ( 2.0_DFP * l3 - a1 ), &
          & l1 ** 2 - l1/3.0_DFP, &
          & l1 * l2 &
          & ], &
          & [10, 3] &
        & )
        Obj % dNdXi( :, :, ips ) = MATMUL( dndl, dldxi )
      END DO
    END BLOCK
  END SELECT
END PROCEDURE Triangle_H1_Lagrange

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE Triangle