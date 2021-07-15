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
! date: 1 March 2021
! summary: This submodule contains constructor methods of [[ReferenceElement_]]

SUBMODULE( ReferenceElement_Method ) Constructor
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                         ReferenceTopology
!----------------------------------------------------------------------------

MODULE PROCEDURE reference_topology
  obj%Nptrs = Nptrs
  obj%Name = Name
  obj%XiDimension = XiDimension( Name )
END PROCEDURE reference_topology

!----------------------------------------------------------------------------
!                                                            DeallocateData
!----------------------------------------------------------------------------

MODULE PROCEDURE deallocatedata_ref_topology
  IF( ALLOCATED( obj%Nptrs ) ) DEALLOCATE( obj%Nptrs )
  obj%Name = -1
  obj%XiDimension = -1
END PROCEDURE deallocatedata_ref_topology

!----------------------------------------------------------------------------
!                                                                        NNE
!----------------------------------------------------------------------------

MODULE PROCEDURE tNodes_RefTopo
  IF( ALLOCATED( obj%Nptrs ) ) THEN
    Ans = SIZE( obj%Nptrs )
  ELSE
    Ans = 0
  END IF
END PROCEDURE tNodes_RefTopo

!----------------------------------------------------------------------------
!                                                                        NNE
!----------------------------------------------------------------------------

MODULE PROCEDURE tNodes_RefElem
  IF( ALLOCATED( obj%XiJ ) ) THEN
    Ans = SIZE( obj%XiJ, 2 )
  ELSE
    Ans = 0
  END IF
END PROCEDURE tNodes_RefElem

!----------------------------------------------------------------------------
!                                                            DeallocateData
!----------------------------------------------------------------------------

MODULE PROCEDURE deallocatedata_ref_elem
  IF( ALLOCATED( obj%XiJ ) ) DEALLOCATE( obj%XiJ )
  obj%EntityCounts = 0
  IF( ALLOCATED( obj%Topology ) ) DEALLOCATE( obj%Topology )
  obj%XiDimension = -1
  obj%Name = -1
  obj%NSD = -1
END PROCEDURE deallocatedata_ref_elem

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE init_refelem
  IF( ALLOCATED( Anotherobj%XiJ ) ) obj%XiJ = Anotherobj%XiJ
  obj%EntityCounts = Anotherobj%EntityCounts
  obj%XiDimension = Anotherobj%XiDimension
  obj%NSD = Anotherobj%NSD
  obj%Order = Anotherobj%Order
  obj%Name = Anotherobj%Name
  IF( ALLOCATED( Anotherobj%Topology ) ) THEN
    obj%Topology = Anotherobj%Topology
  END IF
  obj%LagrangeElement => Anotherobj%LagrangeElement
END PROCEDURE init_refelem

!----------------------------------------------------------------------------
!                                                              getNptrs
!----------------------------------------------------------------------------

MODULE PROCEDURE RefElem_getNptrs
  ans = obj%Topology( SUM(obj%EntityCounts) )%nptrs
END PROCEDURE RefElem_getNptrs

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE Constructor
