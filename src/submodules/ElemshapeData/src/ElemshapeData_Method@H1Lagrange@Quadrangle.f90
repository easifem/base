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

SUBMODULE( ElemshapeData_Method: H1Lagrange ) Quadrangle
USE BaseMethod
IMPLICIT NONE

CONTAINS
!----------------------------------------------------------------------------
!                                                                  Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE Quadrangle_H1_Lagrange
  INTEGER( I4B ) :: nips
  REAL( DFP ), ALLOCATABLE :: XiEta( :, : )

  CALL Initiate( Obj%RefElem, RefElem )
  CALL getQuadraturePoints( Obj = Quad, Point = XiEta,  Weight = Obj%Ws )
  Obj%Quad = Quad
  nips = SIZE( Obj%Ws )

  SELECT CASE( refelem%order )
  CASE( 1 )
    call AllocateData( obj = obj, nsd = refelem%nsd, &
      & xidim = refelem%xidimension, nns = 4, nips = nips )

    obj%N( 1, : ) = (1.0_DFP - XiEta( 1, : ) )*(1.0_DFP - XiEta( 2, : ) )/4.0_DFP
    obj%N( 2, : ) = (1.0_DFP + XiEta( 1, : ) )*(1.0_DFP - XiEta( 2, : ) )/4.0_DFP
    obj%N( 3, : ) = (1.0_DFP + XiEta( 1, : ) )*(1.0_DFP + XiEta( 2, : ) )/4.0_DFP
    obj%N( 4, : ) = (1.0_DFP - XiEta( 1, : ) )*(1.0_DFP + XiEta( 2, : ) )/4.0_DFP

    obj%dNdXi( 1, 1, : ) = -(1.0_DFP - XiEta( 2, : ))/4.0_DFP
    obj%dNdXi( 1, 2, : ) = -(1.0_DFP - XiEta( 1, : ))/4.0_DFP
    obj%dNdXi( 2, 1, : ) =  (1.0_DFP - XiEta( 2, : ))/4.0_DFP
    obj%dNdXi( 2, 2, : ) = -(1.0_DFP + XiEta( 1, : ))/4.0_DFP
    obj%dNdXi( 3, 1, : ) =  (1.0_DFP + XiEta( 2, : ))/4.0_DFP
    obj%dNdXi( 3, 2, : ) =  (1.0_DFP + XiEta( 1, : ))/4.0_DFP
    obj%dNdXi( 4, 1, : ) = -(1.0_DFP + XiEta( 2, : ))/4.0_DFP
    obj%dNdXi( 4, 2, : ) =  (1.0_DFP - XiEta( 1, : ))/4.0_DFP

  CASE DEFAULT
  END SELECT
  DEALLOCATE( XiEta )

END PROCEDURE Quadrangle_H1_Lagrange

END SUBMODULE Quadrangle