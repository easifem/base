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

SUBMODULE(ReferenceElement_Method) Constructor
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                         ReferenceTopology
!----------------------------------------------------------------------------

MODULE PROCEDURE refelem_ReferenceTopology
  obj%Nptrs = Nptrs
  obj%Name = Name
  obj%XiDimension = XiDimension( Name )
END PROCEDURE refelem_ReferenceTopology

!----------------------------------------------------------------------------
!                                                            DeallocateData
!----------------------------------------------------------------------------

MODULE PROCEDURE refelem_DeallocateData1
  IF( ALLOCATED( obj%Nptrs ) ) DEALLOCATE( obj%Nptrs )
  obj%Name = -1
  obj%XiDimension = -1
END PROCEDURE refelem_DeallocateData1

!----------------------------------------------------------------------------
!                                                            DeallocateData
!----------------------------------------------------------------------------

MODULE PROCEDURE refelem_DeallocateData2
  IF( ALLOCATED( obj%XiJ ) ) DEALLOCATE( obj%XiJ )
  obj%EntityCounts = 0
  IF( ALLOCATED( obj%Topology ) ) DEALLOCATE( obj%Topology )
  obj%XiDimension = -1
  obj%Name = -1
  obj%NSD = -1
END PROCEDURE refelem_DeallocateData2

!----------------------------------------------------------------------------
!                                                                        NNE
!----------------------------------------------------------------------------

MODULE PROCEDURE refelem_NNE1
  IF( ALLOCATED( obj%Nptrs ) ) THEN
    Ans = SIZE( obj%Nptrs )
  ELSE
    Ans = 0
  END IF
END PROCEDURE refelem_NNE1

!----------------------------------------------------------------------------
!                                                                        NNE
!----------------------------------------------------------------------------

MODULE PROCEDURE refelem_NNE2
  IF( ALLOCATED( obj%XiJ ) ) THEN
    Ans = SIZE( obj%XiJ, 2 )
  ELSE
    Ans = 0
  END IF
END PROCEDURE refelem_NNE2

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE refelem_Initiate1
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
END PROCEDURE refelem_Initiate1

!----------------------------------------------------------------------------
!                                                  ReferenceElement_Pointer
!----------------------------------------------------------------------------

MODULE PROCEDURE refelem_constructor_1
  CLASS( ReferenceElement_ ), POINTER :: refelemOut
  !> Internal variable
  TYPE( String ) :: dsetname
  CLASS( ReferenceElement_ ), POINTER :: refelem
  INTEGER( I4B ) :: elemOrder

  refelem=>NULL()
  SELECT CASE( xidim )
  CASE( 0 )
    ans => ReferencePoint_Pointer(nsd=nsd)
  CASE( 1 )
    elemOrder = ElementOrder( elemType )
    IF( elemOrder .NE. 1 ) THEN
      refelem => ReferenceLine_Pointer(nsd=nsd)
      ALLOCATE( ReferenceLine_ :: ans )
      CALL refelem%LagrangeElement( order=elemOrder, &
        & HighOrderObj=ans )
      CALL DeallocateData( refelem ); DEALLOCATE( refelem );refelem => NULL()
    ELSE
      ans => ReferenceLine_Pointer(nsd=nsd)
    END IF
  CASE( 2 )
    elemOrder = ElementOrder( elemType )
    IF( isTriangle( elemType ) ) THEN
      IF( elemOrder .NE. 1 ) THEN
        refelem => ReferenceTriangle_Pointer(nsd=nsd)
        ALLOCATE( ReferenceTriangle_ :: ans )
        CALL refelem%LagrangeElement( order=elemOrder, &
          & HighOrderObj=ans )
        CALL DeallocateData( refelem );DEALLOCATE( refelem );refelem => NULL()
      ELSE
        ans => ReferenceTriangle_Pointer(nsd=nsd)
      END IF
    ELSE IF( isQuadrangle( elemType ) ) THEN
      IF( elemOrder .NE. 1 ) THEN
        refelem => ReferenceQuadrangle_Pointer(nsd=nsd)
        ALLOCATE( ReferenceQuadrangle_ :: ans )
        CALL refelem%LagrangeElement( order=elemOrder, &
          & HighOrderObj=ans )
        CALL DeallocateData( refelem );DEALLOCATE( refelem );refelem => NULL()
      ELSE
        ans => ReferenceQuadrangle_Pointer(nsd=nsd)
      END IF
    END IF
  CASE( 3 )
    elemOrder = ElementOrder( elemType )
    IF( isTetrahedron( elemType ) ) THEN
      IF( elemOrder .NE. 1 ) THEN
        refelem => ReferenceTetrahedron_Pointer(nsd=nsd)
        ALLOCATE( ReferenceTetrahedron_ :: ans )
        CALL refelem%LagrangeElement( order=elemOrder, &
          & HighOrderObj=ans )
        CALL DeallocateData( refelem );DEALLOCATE( refelem );refelem => NULL()
      ELSE
        ans => ReferenceTetrahedron_Pointer(nsd=nsd)
      END IF
    ELSE IF( isHexahedron( elemType ) ) THEN
      IF( elemOrder .NE. 1 ) THEN
        refelem => ReferenceHexahedron_Pointer(nsd=nsd)
        ALLOCATE( ReferenceHexahedron_ :: ans )
        CALL refelem%LagrangeElement( order=elemOrder, &
          & HighOrderObj=ans )
        CALL DeallocateData( refelem );DEALLOCATE( refelem );refelem => NULL()
      ELSE
        ans => ReferenceHexahedron_Pointer(nsd=nsd)
      END IF
    ELSE IF( isPrism( elemType ) ) THEN
      IF( elemOrder .NE. 1 ) THEN
        refelem => ReferencePrism_Pointer(nsd=nsd)
        ALLOCATE( ReferencePrism_ :: ans )
        CALL refelem%LagrangeElement( order=elemOrder, &
          & HighOrderObj=ans )
        CALL DeallocateData( refelem );DEALLOCATE( refelem );refelem => NULL()
      ELSE
        ans => ReferencePrism_Pointer(nsd=nsd)
      END IF
    ELSE IF( isPyramid( elemType ) ) THEN
      IF( elemOrder .NE. 1 ) THEN
        refelem => ReferencePyramid_Pointer(nsd=nsd)
        ALLOCATE( ReferencePyramid_ :: ans )
        CALL refelem%LagrangeElement( order=elemOrder, &
          & HighOrderObj=ans )
        CALL DeallocateData( refelem );DEALLOCATE( refelem );refelem => NULL()
      ELSE
        ans => ReferencePyramid_Pointer(nsd=nsd)
      END IF
    END IF
  END SELECT
END PROCEDURE refelem_constructor_1

!----------------------------------------------------------------------------
!                                                              getNptrs
!----------------------------------------------------------------------------

MODULE PROCEDURE refelem_getNptrs
  ans = obj%Topology( SUM(obj%EntityCounts) )%nptrs
END PROCEDURE refelem_getNptrs

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE Constructor
