MODULE QuadraturePoint_Method
USE BaseType
USE GlobalData

IMPLICIT NONE

PRIVATE

!----------------------------------------------------------------------------
!                                                       Initiate@Constructor
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE initiate_obj( Obj, Points )
	CLASS( QuadraturePoint_ ), INTENT( INOUT ) :: Obj
	REAL( DFP ), INTENT( IN ) :: Points( :, : )
END SUBROUTINE initiate_obj
END INTERFACE

INTERFACE
MODULE PURE SUBROUTINE initiate_obj_txi( Obj, tXi, tPoints )
	CLASS( QuadraturePoint_ ), INTENT( INOUT) :: Obj
	INTEGER( I4B ), INTENT( IN ) :: tXi, tPoints
END SUBROUTINE initiate_obj_txi
END INTERFACE

INTERFACE Initiate
	MODULE PROCEDURE initiate_obj, initiate_obj_txi
END INTERFACE

PUBLIC :: Initiate

!----------------------------------------------------------------------------
!                                                QuadraturePoint@Constructor
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION Constructor_1( Points ) RESULT( Obj )
	CLASS( QuadraturePoint_ ), POINTER :: Obj
	REAL( DFP ), INTENT( IN ) :: Points( :, : )
END FUNCTION Constructor_1

MODULE PURE FUNCTION Constructor1( Points ) RESULT( Obj )
	TYPE( QuadraturePoint_ ) :: Obj
	REAL( DFP ), INTENT( IN ) :: Points( :, : )
END FUNCTION Constructor1
END INTERFACE

INTERFACE QuadraturePoint
	MODULE PROCEDURE Constructor1
END INTERFACE QuadraturePoint

INTERFACE QuadraturePoint_Pointer
	MODULE PROCEDURE Constructor_1
END INTERFACE QuadraturePoint_Pointer

PUBLIC :: QuadraturePoint, QuadraturePoint_Pointer

!----------------------------------------------------------------------------
!                                                 DeallocateData@Constructor
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE deallocate_data(Obj)
	CLASS( QuadraturePoint_ ), INTENT( INOUT ) :: Obj
END SUBROUTINE deallocate_data
END INTERFACE

INTERFACE DeallocateData
	MODULE PROCEDURE deallocate_data
END INTERFACE DeallocateData

PUBLIC :: DeallocateData

!----------------------------------------------------------------------------
!                                                           SIZE@Constructor
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION size_obj( Obj, dims ) RESULT( Ans )
	CLASS( QuadraturePoint_ ), INTENT( IN ) :: Obj
	INTEGER( I4B ), INTENT( IN ) :: dims
	INTEGER( I4B ) :: Ans
END FUNCTION size_obj
END INTERFACE

INTERFACE SIZE
	MODULE PROCEDURE size_obj
END INTERFACE SIZE

PUBLIC :: SIZE

!----------------------------------------------------------------------------
!                                                        Display@Constructor
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE display_obj( Obj, msg, unitno )
	CLASS( QuadraturePoint_ ), INTENT( IN ) :: Obj
	CHARACTER( LEN = * ), INTENT( IN ) :: msg
	INTEGER( I4B ), INTENT( IN ), OPTIONAL :: unitno
END SUBROUTINE display_obj
END INTERFACE

INTERFACE Display
	MODULE PROCEDURE display_obj
END INTERFACE Display

PUBLIC :: Display

!----------------------------------------------------------------------------
!                                             GetQuadraturePoint@Constructor
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE getQP1( Obj, Point, Weight, Num )
	CLASS( QuadraturePoint_ ), INTENT( IN ) :: Obj
	REAL( DFP ), INTENT( INOUT ) :: Point( 3 )
	REAL( DFP ), INTENT( INOUT ) :: Weight
	INTEGER( I4B ), INTENT( IN ) :: Num
END SUBROUTINE getQP1

MODULE PURE SUBROUTINE getQP2( Obj, Point, Weight )
	CLASS( QuadraturePoint_ ), INTENT( IN ) :: Obj
	REAL( DFP ), ALLOCATABLE, INTENT( INOUT ) :: Point( :, : )
	REAL( DFP ), ALLOCATABLE, INTENT( INOUT ) :: Weight( : )
END SUBROUTINE getQP2
END INTERFACE

INTERFACE GetQuadraturePoints
	MODULE PROCEDURE getQP1, getQP2
END INTERFACE

PUBLIC :: GetQuadraturePoints

!----------------------------------------------------------------------------
!                                      GaussLegendreQuadrature@GaussLegendre
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION getGaussLegendreQP1( RefElem, Order ) RESULT( Obj )
	INTEGER( I4B ), INTENT( IN ) :: Order
	CLASS( ReferenceElement_ ), INTENT( IN ) :: RefElem
	TYPE( QuadraturePoint_ ) :: Obj
END FUNCTION getGaussLegendreQP1

MODULE PURE FUNCTION getGaussLegendreQP2( RefElem, NIPS ) RESULT( Obj )
	INTEGER( I4B ), INTENT( IN ) :: NIPS( 1 )
	CLASS( ReferenceElement_ ), INTENT( IN ) :: RefElem
	TYPE( QuadraturePoint_ ) :: Obj
END FUNCTION getGaussLegendreQP2
END INTERFACE

INTERFACE GaussLegendreQuadrature
	MODULE PROCEDURE getGaussLegendreQP1, getGaussLegendreQP2
END INTERFACE GaussLegendreQuadrature

PUBLIC :: GaussLegendreQuadrature

!----------------------------------------------------------------------------
!                                                GaussLegendre@GaussLegendre
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION getGaussLegendreQPLine1( RefElem, Order ) RESULT( Obj )
	INTEGER( I4B ), INTENT( IN ) :: Order
	TYPE( ReferenceLine_ ), INTENT( IN ) :: RefElem
	TYPE( QuadraturePoint_ ) :: Obj
END FUNCTION getGaussLegendreQPLine1

MODULE PURE FUNCTION getGaussLegendreQPLine2( RefElem, NIPS ) RESULT( Obj )
	INTEGER( I4B ), INTENT( IN ) :: NIPS( 1 )
	TYPE( ReferenceLine_ ), INTENT( IN ) :: RefElem
	TYPE( QuadraturePoint_ ) :: Obj
END FUNCTION getGaussLegendreQPLine2
END INTERFACE

! INTERFACE GaussLegendreQuadrature
!   MODULE PROCEDURE getGaussLegendreQPLine1, getGaussLegendreQPLine2
! END INTERFACE GaussLegendreQuadrature

! PUBLIC :: GaussLegendreQuadrature

!----------------------------------------------------------------------------
!                                                GaussLegendre@GaussLegendre
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION getGaussLegendreQPTriangle1( RefElem, Order ) RESULT(Obj)
	INTEGER( I4B ), INTENT( IN ) :: Order
	TYPE( ReferenceTriangle_ ), INTENT( IN ) :: RefElem
	TYPE( QuadraturePoint_ ) :: Obj
END FUNCTION getGaussLegendreQPTriangle1

MODULE PURE FUNCTION getGaussLegendreQPTriangle2( RefElem, NIPS ) RESULT(Obj)
	INTEGER( I4B ), INTENT( IN ) :: NIPS( 1 )
	TYPE( ReferenceTriangle_ ), INTENT( IN ) :: RefElem
	TYPE( QuadraturePoint_ ) :: Obj
END FUNCTION getGaussLegendreQPTriangle2
END INTERFACE

! INTERFACE GaussLegendreQuadrature
!   MODULE PROCEDURE getGaussLegendreQPTriangle1, getGaussLegendreQPTriangle2
! END INTERFACE GaussLegendreQuadrature

!----------------------------------------------------------------------------
!                                                GaussLegendre@GaussLegendre
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION getGaussLegendreQPQuadrangle1( RefElem, Order)RESULT(Obj)
	INTEGER( I4B ), INTENT( IN ) :: Order
	TYPE( ReferenceQuadrangle_ ), INTENT( IN ) :: RefElem
	TYPE( QuadraturePoint_ ) :: Obj
END FUNCTION getGaussLegendreQPQuadrangle1

MODULE PURE FUNCTION getGaussLegendreQPQuadrangle2( RefElem, NIPS)RESULT(Obj)
	INTEGER( I4B ), INTENT( IN ) :: NIPS( 1 )
	TYPE( ReferenceQuadrangle_ ), INTENT( IN ) :: RefElem
	TYPE( QuadraturePoint_ ) :: Obj
END FUNCTION getGaussLegendreQPQuadrangle2
END INTERFACE

! INTERFACE GaussLegendreQuadrature
!   MODULE PROCEDURE getGaussLegendreQPQuadrangle1, getGaussLegendreQPQuadrangle2
! END INTERFACE GaussLegendreQuadrature

!----------------------------------------------------------------------------
!                                                GaussLegendre@GaussLegendre
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION getGaussLegendreQPTetrahedron1( RefElem, Order)RESULT(Obj)
	INTEGER( I4B ), INTENT( IN ) :: Order
	TYPE( ReferenceTetrahedron_ ), INTENT( IN ) :: RefElem
	TYPE( QuadraturePoint_ ) :: Obj
END FUNCTION getGaussLegendreQPTetrahedron1

MODULE PURE FUNCTION getGaussLegendreQPTetrahedron2( RefElem, NIPS)RESULT(Obj)
	INTEGER( I4B ), INTENT( IN ) :: NIPS( 1 )
	TYPE( ReferenceTetrahedron_ ), INTENT( IN ) :: RefElem
	TYPE( QuadraturePoint_ ) :: Obj
END FUNCTION getGaussLegendreQPTetrahedron2
END INTERFACE

! INTERFACE GaussLegendreQuadrature
!   MODULE PROCEDURE getGaussLegendreQPTetrahedron1, getGaussLegendreQPTetrahedron2
! END INTERFACE GaussLegendreQuadrature

!----------------------------------------------------------------------------
!                                                GaussLegendre@GaussLegendre
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION getGaussLegendreQPPyramid1( RefElem, Order)RESULT(Obj)
	INTEGER( I4B ), INTENT( IN ) :: Order
	TYPE( ReferencePyramid_ ), INTENT( IN ) :: RefElem
	TYPE( QuadraturePoint_ ) :: Obj
END FUNCTION getGaussLegendreQPPyramid1

MODULE PURE FUNCTION getGaussLegendreQPPyramid2( RefElem, NIPS)RESULT(Obj)
	INTEGER( I4B ), INTENT( IN ) :: NIPS( 1 )
	TYPE( ReferencePyramid_ ), INTENT( IN ) :: RefElem
	TYPE( QuadraturePoint_ ) :: Obj
END FUNCTION getGaussLegendreQPPyramid2
END INTERFACE

! INTERFACE GaussLegendreQuadrature
!   MODULE PROCEDURE getGaussLegendreQPPyramid1, getGaussLegendreQPPyramid2
! END INTERFACE GaussLegendreQuadrature

!----------------------------------------------------------------------------
!                                                GaussLegendre@GaussLegendre
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION getGaussLegendreQPPrism1( RefElem, Order ) RESULT( Obj )
	INTEGER( I4B ), INTENT( IN ) :: Order
	TYPE( ReferencePrism_ ), INTENT( IN ) :: RefElem
	TYPE( QuadraturePoint_ ) :: Obj
END FUNCTION getGaussLegendreQPPrism1

MODULE PURE FUNCTION getGaussLegendreQPPrism2( RefElem, NIPS ) RESULT( Obj )
	INTEGER( I4B ), INTENT( IN ) :: NIPS( 1 )
	TYPE( ReferencePrism_ ), INTENT( IN ) :: RefElem
	TYPE( QuadraturePoint_ ) :: Obj
END FUNCTION getGaussLegendreQPPrism2
END INTERFACE

! INTERFACE GaussLegendreQuadrature
!   MODULE PROCEDURE getGaussLegendreQPPrism1, getGaussLegendreQPPrism2
! END INTERFACE GaussLegendreQuadrature

!----------------------------------------------------------------------------
!                                                GaussLegendre@GaussLegendre
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION getGaussLegendreQPHexahedron1( RefElem, Order ) &
		& RESULT( Obj )
	INTEGER( I4B ), INTENT( IN ) :: Order
	TYPE( ReferenceHexahedron_ ), INTENT( IN ) :: RefElem
	TYPE( QuadraturePoint_ ) :: Obj
END FUNCTION getGaussLegendreQPHexahedron1

MODULE PURE FUNCTION getGaussLegendreQPHexahedron2( RefElem, NIPS ) &
		& RESULT( Obj )
	INTEGER( I4B ), INTENT( IN ) :: NIPS( 1 )
	TYPE( ReferenceHexahedron_ ), INTENT( IN ) :: RefElem
	TYPE( QuadraturePoint_ ) :: Obj
END FUNCTION getGaussLegendreQPHexahedron2
END INTERFACE

! INTERFACE GaussLegendreQuadrature
!   MODULE PROCEDURE getGaussLegendreQPHexahedron1, getGaussLegendreQPHexahedron2
! END INTERFACE GaussLegendreQuadrature

!----------------------------------------------------------------------------
!                                                                  Contains
!----------------------------------------------------------------------------

#include "./contains.part"
END MODULE QuadraturePoint_Method
