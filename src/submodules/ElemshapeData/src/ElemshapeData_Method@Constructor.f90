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
  CALL reallocate( obj%N, nns, nips )
  CALL reallocate( obj%dNdXi, nns, xidim, nips )
  CALL reallocate( obj%Normal, 3, nips )
  CALL reallocate( obj%dNdXt, nns, nsd, nips )
  CALL reallocate( obj%Jacobian, nsd, xidim, nips )
  CALL reallocate( obj%Js, nips )
  CALL reallocate( obj%Thickness, nips )
  obj%Thickness = 1.0_DFP
  CALL reallocate( obj%Coord, nsd, nips )
END PROCEDURE initiate_obj

!----------------------------------------------------------------------------
!                                                                 Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE stsd_initiate
  INTEGER( I4B ) :: tip, ip
  REAL( DFP ) :: x( 3 )
  tip = SIZE( elemsd%N, 2 )
  IF( ALLOCATED( obj ) ) DEALLOCATE( obj )
  ALLOCATE( obj( tip ) )
  DO ip = 1, tip
    obj( ip )%T = elemsd%N( :, ip )
    obj( ip )%dTdTheta = elemsd%dNdXi( :, 1, ip )
    obj( ip )%Jt = elemsd%Js( ip )
    CALL getQuadraturePoints( obj = elemsd%Quad, Weight = obj( ip )%Wt,&
      & Num = ip, Point = x )
    obj( ip )%Theta = x( 1 )
  END DO
END PROCEDURE stsd_initiate

!----------------------------------------------------------------------------
!                                                             DeallocateData
!----------------------------------------------------------------------------

MODULE PROCEDURE deallocate_data
  IF( ALLOCATED( obj%Normal ) ) DEALLOCATE( obj%Normal )
  IF( ALLOCATED( obj%N ) ) DEALLOCATE( obj%N )
  IF( ALLOCATED( obj%dNdXi ) ) DEALLOCATE( obj%dNdXi )
  IF( ALLOCATED( obj%dNdXt ) ) DEALLOCATE( obj%dNdXt )
  IF( ALLOCATED( obj%Jacobian ) ) DEALLOCATE( obj%Jacobian )
  IF( ALLOCATED( obj%Js ) ) DEALLOCATE( obj%Js )
  IF( ALLOCATED( obj%Ws ) ) DEALLOCATE( obj%Ws )
  IF( ALLOCATED( obj%Thickness ) ) DEALLOCATE( obj%Thickness )
  IF( ALLOCATED( obj%Coord ) ) DEALLOCATE( obj%Coord )
  CALL DeallocateData( obj%Quad )
  CALL DeallocateData( obj%RefElem )
  SELECT TYPE( obj )
  TYPE IS (STElemShapeData_)
    IF( ALLOCATED( obj%T ) ) DEALLOCATE( obj%T )
    IF( ALLOCATED( obj%dTdTheta ) ) DEALLOCATE( obj%dTdTheta )
    IF( ALLOCATED( obj%dNTdt ) ) DEALLOCATE( obj%dNTdt )
    IF( ALLOCATED( obj%dNTdXt ) ) DEALLOCATE( obj%dNTdXt )
  END SELECT
END PROCEDURE deallocate_data

END SUBMODULE Constructor

