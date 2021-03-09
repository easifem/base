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
! date: 4 March 2021
! summary: 	This submodule contains constructor method for [[ElemshapeData_]] and [[STElemShapeData_]]

SUBMODULE( ElemshapeData_Method ) Constructor
USE BaseMethod
IMPLICIT NONE

CONTAINS
!----------------------------------------------------------------------------
!                                                                  Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE initiate_obj
  CALL reallocate( Obj % N, nns, nips )
  CALL reallocate( Obj % dNdXi, nns, xidim, nips )
  CALL reallocate( Obj % Normal, 3, nips )
  CALL reallocate( Obj % dNdXt, nns, nsd, nips )
  CALL reallocate( Obj % Jacobian, nsd, xidim, nips )
  CALL reallocate( Obj % Js, nips )
  CALL reallocate( Obj % Thickness, nips )
  Obj % Thickness = 1.0_DFP
  CALL reallocate( Obj % Coord, nsd, nips )
END PROCEDURE initiate_obj

!----------------------------------------------------------------------------
!                                                                 Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE stsd_initiate
  INTEGER( I4B ) :: tip, ip
  REAL( DFP ) :: x( 3 )
  tip = SIZE( elemsd % N, 2 )
  IF( ALLOCATED( obj ) ) DEALLOCATE( obj )
  ALLOCATE( obj( tip ) )
  DO ip = 1, tip
    Obj( ip ) % T = elemsd % N( :, ip )
    Obj( ip ) % dTdTheta = elemsd % dNdXi( :, 1, ip )
    Obj( ip ) % Jt = elemsd % Js( ip )
    CALL getQuadraturePoints( Obj = elemsd % Quad, Weight = Obj( ip ) % Wt,&
      & Num = ip, Point = x )
    Obj( ip ) % Theta = x( 1 )
  END DO
END PROCEDURE stsd_initiate

!----------------------------------------------------------------------------
!                                                             DeallocateData
!----------------------------------------------------------------------------

MODULE PROCEDURE deallocate_data
  IF( ALLOCATED( Obj % Normal ) ) DEALLOCATE( Obj % Normal )
  IF( ALLOCATED( Obj % N ) ) DEALLOCATE( Obj % N )
  IF( ALLOCATED( Obj % dNdXi ) ) DEALLOCATE( Obj % dNdXi )
  IF( ALLOCATED( Obj % dNdXt ) ) DEALLOCATE( Obj % dNdXt )
  IF( ALLOCATED( Obj % Jacobian ) ) DEALLOCATE( Obj % Jacobian )
  IF( ALLOCATED( Obj % Js ) ) DEALLOCATE( Obj % Js )
  IF( ALLOCATED( Obj % Ws ) ) DEALLOCATE( Obj % Ws )
  IF( ALLOCATED( Obj % Thickness ) ) DEALLOCATE( Obj % Thickness )
  IF( ALLOCATED( Obj % Coord ) ) DEALLOCATE( Obj % Coord )
  CALL DeallocateData( Obj % Quad )
  CALL DeallocateData( Obj % RefElem )
  SELECT TYPE( Obj )
  TYPE IS (STElemShapeData_)
    IF( ALLOCATED( Obj % T ) ) DEALLOCATE( Obj % T )
    IF( ALLOCATED( Obj % dTdTheta ) ) DEALLOCATE( Obj % dTdTheta )
    IF( ALLOCATED( Obj % dNTdt ) ) DEALLOCATE( Obj % dNTdt )
    IF( ALLOCATED( Obj % dNTdXt ) ) DEALLOCATE( Obj % dNTdXt )
  END SELECT
END PROCEDURE deallocate_data

END SUBMODULE Constructor

