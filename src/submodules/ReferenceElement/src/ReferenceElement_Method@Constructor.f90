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
  Obj%Nptrs = Nptrs
  Obj%Name = Name
  Obj%XiDimension = XiDimension( Name )
END PROCEDURE reference_topology

!----------------------------------------------------------------------------
!                                                            DeallocateData
!----------------------------------------------------------------------------

MODULE PROCEDURE deallocatedata_ref_topology
  IF( ALLOCATED( Obj%Nptrs ) ) DEALLOCATE( Obj%Nptrs )
  Obj%Name = -1
  Obj%XiDimension = -1
END PROCEDURE deallocatedata_ref_topology

!----------------------------------------------------------------------------
!                                                                        NNE
!----------------------------------------------------------------------------

MODULE PROCEDURE tNodes_RefTopo
  IF( ALLOCATED( Obj%Nptrs ) ) THEN
    Ans = SIZE( Obj%Nptrs )
  ELSE
    Ans = 0
  END IF
END PROCEDURE tNodes_RefTopo

!----------------------------------------------------------------------------
!                                                                        NNE
!----------------------------------------------------------------------------

MODULE PROCEDURE tNodes_RefElem
  IF( ALLOCATED( Obj%XiJ ) ) THEN
    Ans = SIZE( Obj%XiJ, 2 )
  ELSE
    Ans = 0
  END IF
END PROCEDURE tNodes_RefElem

!----------------------------------------------------------------------------
!                                                            DeallocateData
!----------------------------------------------------------------------------

MODULE PROCEDURE deallocatedata_ref_elem
  IF( ALLOCATED( Obj%XiJ ) ) DEALLOCATE( Obj%XiJ )
  Obj%EntityCounts = 0
  IF( ALLOCATED( Obj%Topology ) ) DEALLOCATE( Obj%Topology )
  Obj%XiDimension = -1
  Obj%Name = -1
  Obj%NSD = -1
END PROCEDURE deallocatedata_ref_elem

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE init_refelem
  IF( ALLOCATED( AnotherObj%XiJ ) ) Obj%XiJ = AnotherObj%XiJ
  Obj%EntityCounts = AnotherObj%EntityCounts
  Obj%XiDimension = AnotherObj%XiDimension
  Obj%NSD = AnotherObj%NSD
  Obj%Order = AnotherObj%Order
  Obj%Name = AnotherObj%Name
  IF( ALLOCATED( AnotherObj%Topology ) ) THEN
    Obj%Topology = AnotherObj%Topology
  END IF
  Obj%LagrangeElement => AnotherObj%LagrangeElement
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
