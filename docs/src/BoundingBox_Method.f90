MODULE BoundingBox_Method
USE GlobalData
USE IO
USE BaseType
IMPLICIT NONE

PRIVATE

!----------------------------------------------------------------------------
!                                                        Initiate@Constructor
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE initiate_1( Obj, nsd, lim )
    CLASS( BoundingBox_ ), INTENT( INOUT ) :: Obj
    INTEGER( I4B ), INTENT( IN ) :: NSD
    REAL( DFP ), INTENT( IN ) :: lim( 6 )
END SUBROUTINE initiate_1
END INTERFACE

INTERFACE
MODULE PURE SUBROUTINE initiate_2( Obj, AnotherObj )
    CLASS( BoundingBox_ ), INTENT( INOUT) :: Obj
    CLASS( BoundingBox_ ), INTENT( IN) :: AnotherObj
END SUBROUTINE initiate_2
END INTERFACE

INTERFACE Initiate
    MODULE PROCEDURE initiate_1, initiate_2
END INTERFACE Initiate

PUBLIC :: Initiate

!----------------------------------------------------------------------------
!                                                     BoundingBox@Constructor
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION Constructor1( nsd, lim ) RESULT( Ans )
    TYPE( BoundingBox_ ) :: Ans
    INTEGER( I4B ), INTENT( IN ) :: nsd
    REAL( DFP ), INTENT( IN ) :: lim( 6 )
END FUNCTION Constructor1
END INTERFACE

!----------------------------------------------------------------------------
!                                                                BoundingBox
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION Constructor2( AnotherObj ) RESULT( Ans )
    TYPE( BoundingBox_ ) :: Ans
    CLASS( BoundingBox_ ), INTENT( IN ) :: AnotherObj
END FUNCTION Constructor2
END INTERFACE

!----------------------------------------------------------------------------
!                                                                BoundingBox
!----------------------------------------------------------------------------

INTERFACE
!! Return the bouding box for a given set of coordinates

!> authors: Dr. Vikas Sharma
!
! Return the bouding box for a given set of coordinates
!
!### Usage
!
! ```fortran
!	bbox = BoundingBox( XiJ )
! ```

MODULE PURE FUNCTION Constructor3( xij ) RESULT( Ans )
  REAL( DFP ), INTENT( IN ) :: xij( :, : )
    !! Nodal coordinates xij( 1:nsd, 1:tnodes )
  TYPE( BoundingBox_ ) :: Ans
END FUNCTION Constructor3
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 BoundingBox
!----------------------------------------------------------------------------

INTERFACE BoundingBox
    MODULE PROCEDURE Constructor1, Constructor2, Constructor3
END INTERFACE BoundingBox

PUBLIC :: BoundingBox

!----------------------------------------------------------------------------
!                                             BoundingBox_Pointer@Constructor
!----------------------------------------------------------------------------

INTERFACE
MODULE FUNCTION Constructor_1( nsd, lim ) RESULT( Ans )
    CLASS( BoundingBox_ ), POINTER :: Ans
    INTEGER( I4B ), INTENT( IN ) :: nsd
    REAL( DFP ), INTENT( IN ) :: lim( 6 )
END FUNCTION Constructor_1
END INTERFACE

INTERFACE
MODULE FUNCTION Constructor_2( AnotherObj ) RESULT( Ans )
    CLASS( BoundingBox_ ), POINTER :: Ans
    CLASS( BoundingBox_ ), INTENT( IN ) :: AnotherObj
END FUNCTION Constructor_2
END INTERFACE

INTERFACE BoundingBox_Pointer
    MODULE PROCEDURE Constructor_1, Constructor_2
END INTERFACE BoundingBox_Pointer

PUBLIC :: BoundingBox_Pointer

!----------------------------------------------------------------------------
!                                                        Display@Constructor
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE display_obj( Obj, msg, unitno )
    CLASS( BoundingBox_ ), INTENT( IN ) :: Obj
    CHARACTER( LEN = * ), INTENT( IN ) :: msg
    INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: unitNo
END SUBROUTINE display_obj
END INTERFACE

INTERFACE Display
    MODULE PROCEDURE display_obj
END INTERFACE Display

PUBLIC :: Display

!----------------------------------------------------------------------------
!                                                                        set
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE setXmin( Obj, Val )
    CLASS( BoundingBox_ ), INTENT( INOUT ) :: Obj
    REAL( DFP ), INTENT( IN ) :: Val
END SUBROUTINE setXmin
END INTERFACE

INTERFACE
MODULE PURE SUBROUTINE setXmax( Obj, Val )
    CLASS( BoundingBox_ ), INTENT( INOUT ) :: Obj
    REAL( DFP ), INTENT( IN ) :: Val
END SUBROUTINE setXmax
END INTERFACE

INTERFACE
MODULE PURE SUBROUTINE setYmin( Obj, Val )
    CLASS( BoundingBox_ ), INTENT( INOUT ) :: Obj
    REAL( DFP ), INTENT( IN ) :: Val
END SUBROUTINE setYmin
END INTERFACE

INTERFACE
MODULE PURE SUBROUTINE setYmax( Obj, Val )
    CLASS( BoundingBox_ ), INTENT( INOUT ) :: Obj
    REAL( DFP ), INTENT( IN ) :: Val
END SUBROUTINE setYmax
END INTERFACE

INTERFACE
MODULE PURE SUBROUTINE setZmin( Obj, Val )
    CLASS( BoundingBox_ ), INTENT( INOUT ) :: Obj
    REAL( DFP ), INTENT( IN ) :: Val
END SUBROUTINE setZmin
END INTERFACE

INTERFACE
MODULE PURE SUBROUTINE setZmax( Obj, Val )
    CLASS( BoundingBox_ ), INTENT( INOUT ) :: Obj
    REAL( DFP ), INTENT( IN ) :: Val
END SUBROUTINE setZmax
END INTERFACE

!----------------------------------------------------------------------------
!                                                                       get
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION getXmin( Obj ) RESULT( Ans )
    CLASS( BoundingBox_ ), INTENT( IN ) :: Obj
    REAL( DFP ) :: Ans
END FUNCTION getXmin
END INTERFACE

INTERFACE Operator( .Xmin. )
    MODULE PROCEDURE getXmin
END INTERFACE Operator( .Xmin. )

PUBLIC :: Operator( .Xmin. )

INTERFACE
MODULE PURE FUNCTION getXmax( Obj ) RESULT( Ans )
    CLASS( BoundingBox_ ), INTENT( IN ) :: Obj
    REAL( DFP ) :: Ans
END FUNCTION getXmax
END INTERFACE

INTERFACE Operator( .Xmax. )
    MODULE PROCEDURE getXmax
END INTERFACE Operator( .Xmax. )

PUBLIC :: Operator( .Xmax. )

INTERFACE
MODULE PURE FUNCTION getYmin( Obj ) RESULT( Ans )
    CLASS( BoundingBox_ ), INTENT( IN ) :: Obj
    REAL( DFP ) :: Ans
END FUNCTION getYmin
END INTERFACE

INTERFACE Operator( .Ymin. )
    MODULE PROCEDURE getYmin
END INTERFACE Operator( .Ymin. )

PUBLIC :: Operator( .Ymin. )

INTERFACE
MODULE PURE FUNCTION getYmax( Obj ) RESULT( Ans )
    CLASS( BoundingBox_ ), INTENT( IN ) :: Obj
    REAL( DFP ) :: Ans
END FUNCTION getYmax
END INTERFACE

INTERFACE Operator( .Ymax. )
    MODULE PROCEDURE getYmax
END INTERFACE Operator( .Ymax. )

PUBLIC :: Operator( .Ymax. )

INTERFACE
MODULE PURE FUNCTION getZmin( Obj ) RESULT( Ans )
  CLASS( BoundingBox_ ), INTENT( IN ) :: Obj
  REAL( DFP ) :: Ans
END FUNCTION getZmin
END INTERFACE

INTERFACE Operator( .Zmin. )
  MODULE PROCEDURE getZmin
END INTERFACE Operator( .Zmin. )

PUBLIC :: Operator( .Zmin. )

INTERFACE
MODULE PURE FUNCTION getZmax( Obj ) RESULT( Ans )
  CLASS( BoundingBox_ ), INTENT( IN ) :: Obj
  REAL( DFP ) :: Ans
END FUNCTION getZmax
END INTERFACE

INTERFACE Operator( .Zmax. )
  MODULE PROCEDURE getZmax
END INTERFACE Operator( .Zmax. )

PUBLIC :: Operator( .Zmax. )

!----------------------------------------------------------------------------
!                                                          is_intersect_in_X
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION is_intersect_in_X( Obj, Obj2 ) RESULT( Ans )
  CLASS( BoundingBox_ ), INTENT( IN ) :: Obj, Obj2
  LOGICAL( LGT ) :: Ans
END FUNCTION is_intersect_in_X
END INTERFACE

INTERFACE isIntersectInX
    MODULE PROCEDURE is_intersect_in_X
END INTERFACE isIntersectInX

PUBLIC :: isIntersectInX

!----------------------------------------------------------------------------
!                                                           is_intersect_in_Y
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION is_intersect_in_Y( Obj, Obj2 ) RESULT( Ans )
  CLASS( BoundingBox_ ), INTENT( IN ) :: Obj, Obj2
  LOGICAL( LGT ) :: Ans
END FUNCTION is_intersect_in_Y
END INTERFACE

INTERFACE isIntersectInY
    MODULE PROCEDURE is_intersect_in_Y
END INTERFACE isIntersectInY

PUBLIC :: isIntersectInY

!----------------------------------------------------------------------------
!                                                          is_intersect_in_Z
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION is_intersect_in_Z( Obj, Obj2 ) RESULT( Ans )
  CLASS( BoundingBox_ ), INTENT( IN ) :: Obj, Obj2
  LOGICAL( LGT ) :: Ans
END FUNCTION is_intersect_in_Z
END INTERFACE

INTERFACE isIntersectInZ
  MODULE PROCEDURE is_intersect_in_Z
END INTERFACE isIntersectInZ

PUBLIC :: isIntersectInZ


!----------------------------------------------------------------------------
!                                                                isIntersect
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION is_intersect( Obj, Obj2 ) RESULT( Ans )
  CLASS( BoundingBox_ ), INTENT( IN ) :: Obj, Obj2
  LOGICAL( LGT ) :: Ans
END FUNCTION is_intersect
END INTERFACE

INTERFACE Operator( .isIntersect. )
    MODULE PROCEDURE is_intersect
END INTERFACE Operator( .isIntersect. )

PUBLIC :: Operator( .isIntersect. )

INTERFACE isIntersect
  MODULE PROCEDURE is_intersect
END INTERFACE isIntersect

PUBLIC :: isIntersect

!----------------------------------------------------------------------------
!                                                            getIntersection
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION get_intersection( Obj, Obj2 ) RESULT( Ans )
  CLASS( BoundingBox_ ), INTENT( IN ) :: Obj, Obj2
  TYPE( BoundingBox_ ) :: Ans
END FUNCTION get_intersection
END INTERFACE

INTERFACE OPERATOR( .Intersection. )
  MODULE PROCEDURE get_intersection
END INTERFACE OPERATOR( .Intersection. )

PUBLIC :: OPERATOR( .Intersection. )

INTERFACE Intersection
  MODULE PROCEDURE get_intersection
END INTERFACE Intersection

PUBLIC :: Intersection

!----------------------------------------------------------------------------
!                                                                 getUnion
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION get_Union( Obj, Obj2 ) RESULT( Ans )
  CLASS( BoundingBox_ ), INTENT( IN ) :: Obj, Obj2
  TYPE( BoundingBox_ ) :: Ans
END FUNCTION get_Union
END INTERFACE

INTERFACE OPERATOR(.UNION.)
    MODULE PROCEDURE get_Union
END INTERFACE OPERATOR(.UNION.)

PUBLIC :: OPERATOR(.UNION.)

INTERFACE Union
    MODULE PROCEDURE get_Union
END INTERFACE Union

PUBLIC :: Union

!----------------------------------------------------------------------------
!                                                                 getCenter
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION get_Center( Obj ) RESULT( Ans )
    CLASS( BoundingBox_ ), INTENT( IN ) :: Obj
    REAL( DFP ) :: Ans( 3 )
END FUNCTION get_Center
END INTERFACE

INTERFACE Center
    MODULE PROCEDURE get_Center
END INTERFACE Center

PUBLIC :: Center

INTERFACE OPERATOR(.Center.)
    MODULE PROCEDURE get_Center
END INTERFACE OPERATOR(.Center.)

PUBLIC :: OPERATOR(.Center.)

!----------------------------------------------------------------------------
!                                                                  isInside
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION is_Inside( Obj, Val ) RESULT( Ans )
  CLASS( BoundingBox_ ), INTENT( IN ) :: Obj
  REAL( DFP ), INTENT( IN ) :: Val( : )
  LOGICAL( LGT ) :: Ans
END FUNCTION is_Inside
END INTERFACE

INTERFACE OPERATOR( .isInside. )
    MODULE PROCEDURE is_Inside
END INTERFACE OPERATOR( .isInside. )

PUBLIC :: OPERATOR( .isInside. )

INTERFACE isInside
    MODULE PROCEDURE is_Inside
END INTERFACE isInside

PUBLIC :: isInside

!----------------------------------------------------------------------------
!                                                                  getNptrs
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION get_nptrs( Obj, xij ) RESULT( Ans )
  CLASS( BoundingBox_ ), INTENT( IN ) :: Obj
  REAL( DFP ), INTENT( IN ) :: xij( :, : )
  INTEGER( I4B ), ALLOCATABLE :: Ans( : )
END FUNCTION get_nptrs
END INTERFACE

INTERFACE OPERATOR( .Nptrs. )
  MODULE PROCEDURE get_nptrs
END INTERFACE

PUBLIC :: OPERATOR( .Nptrs. )

!----------------------------------------------------------------------------
!                                                                    Contains
!----------------------------------------------------------------------------

END MODULE BoundingBox_Method
