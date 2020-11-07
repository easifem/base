!>
! The module `Mesh_Class` contains three data types `Mesh_`, `MeshData_`, and
! `MeshConnectivity_`.

MODULE Mesh_Class
USE BaseType
USE GlobalData
USE StringiFor
USE FE
IMPLICIT NONE

PRIVATE

REAL( DFP ), PARAMETER :: default_factor = 1.5_DFP

!----------------------------------------------------------------------------
!                                                                      Mesh_
!----------------------------------------------------------------------------

!>
! author: Dr Vikas Sharma
!
! In **EASIFEM** `Mesh_` data type denotes a collection of finite elements.
! To do so I have encapsulated a vector of `ElementPointer_`. In this way
! adding and removing an element becomes really simply as one is dealing with
! vector of addresses.

TYPE :: Mesh_
  TYPE( ElementPointer_ ), ALLOCATABLE :: Elem( : )
    !! Vector of `ElementPointer_` denotes the elements in the mesh
  INTEGER( I4B ) :: NSD
    !! Spatial dimension
  INTEGER( I4B ) :: tElements
    !! Total elements in the mesh
  INTEGER( I4B ) :: maxElements
    !! Maximum storage length `maxElements= default_factor * tElements`

  CONTAINS

    PROCEDURE, PUBLIC, PASS( Obj ) :: Initiate => allocateMeshSize
      !! Constructor for mesh
END TYPE Mesh_

PUBLIC :: Mesh_

!----------------------------------------------------------------------------
!                                                       Initiate@MeshMethods
!----------------------------------------------------------------------------

INTERFACE
!> author: Dr Vikas Sharma
!  Initiate-->allocateMeshSize  allocate the mesh size

MODULE PURE SUBROUTINE allocateMeshSize( Obj, NSD, tElements, factor )
  CLASS( Mesh_ ), INTENT( INOUT) :: Obj
    !! Mesh which will be allocated
  INTEGER( I4B ), INTENT( IN ) :: tElements
    !! Total number of elements
  INTEGER( I4B ), INTENT( IN ) :: NSD
    !! Spatial dimension of the problem
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: factor
    !! maxElements = factor * tElements
END SUBROUTINE allocateMeshSize
END INTERFACE

!>
! Generic subroutine for constructing the [[Mesh_]]

INTERFACE Initiate
  MODULE PROCEDURE allocateMeshSize
END INTERFACE Initiate

PUBLIC :: Initiate

!----------------------------------------------------------------------------
!                                                                 MeshData_
!----------------------------------------------------------------------------

!>
! author: Dr Vikas Sharma
!
! In **EASIFEM** `Meshdata_` data type denotes a data realated a mesh

TYPE :: MeshData_
  INTEGER( I4B ) :: MaxNptrs, MinNptrs, tNodes
  LOGICAL( LGT ) :: isInitiated
  INTEGER( I4B ), ALLOCATABLE :: LBndyIndex( : )
  INTEGER( I4B ), ALLOCATABLE :: Nptrs( : )
  INTEGER( I4B ), ALLOCATABLE :: BoundaryNptrs( : )
  INTEGER( I4B ), ALLOCATABLE :: InternalNptrs( : )
  INTEGER( I4B ), ALLOCATABLE :: Local_Nptrs( : )
  TYPE( IntVector_ ), ALLOCATABLE :: NodeToElem( : )
  TYPE( IntVector_ ), ALLOCATABLE :: ElemToElem( : )
  TYPE( IntVector_ ), ALLOCATABLE :: NTN( : )
  TYPE( IntVector_ ), ALLOCATABLE :: ElemToNode( : )
  TYPE( IntVector_ ), ALLOCATABLE :: BoundaryData( : )
  TYPE( IntVector_ ), ALLOCATABLE :: InternalBndyElemNum( : )
  TYPE( IntVector_ ), ALLOCATABLE :: InternalBoundaryData( : )
  TYPE( BoundingBox_ ) :: BBox

  CONTAINS
    PROCEDURE, PUBLIC, PASS( Obj ) :: Initiate => init_meshdata_1
      !! Constructor for [[MeshData_]] object

END TYPE

PUBLIC :: MeshData_

!----------------------------------------------------------------------------
!                                                    Initiate@MeshDataMethods
!----------------------------------------------------------------------------

!> author: Dr Vikas Sharma
!  Initiate-->init_mesh_data_1  constructor for [[MeshData_]]
INTERFACE
MODULE PURE SUBROUTINE init_meshdata_1( Obj, MeshObj )
  CLASS( MeshData_ ), INTENT( INOUT ) :: Obj
  CLASS( Mesh_ ), INTENT( INOUT ) :: MeshObj
END SUBROUTINE init_meshdata_1
END INTERFACE

!>
! Generic subroutine for constructing [[MeshData_]]

INTERFACE Initiate
  MODULE PROCEDURE init_meshdata_1
END INTERFACE Initiate

PUBLIC :: Initiate

END MODULE Mesh_Class