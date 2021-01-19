MODULE IndexValue_Method
USE GlobalData
USE BaseType

IMPLICIT NONE

PRIVATE

!----------------------------------------------------------------------------
!                                                                 Constructor
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION Constructor1( Indx, Val ) RESULT( Obj )
    INTEGER( I4B ), INTENT( IN ) :: Indx
    REAL( DFP ), INTENT( IN ) :: Val
    TYPE(IndexValue_) :: Obj
  END FUNCTION Constructor1

  MODULE PURE FUNCTION Constructor2( Indx, Val ) RESULT( Obj )
    INTEGER( I4B ), INTENT( IN ) :: Indx( : )
    REAL( DFP ), INTENT( IN ) :: Val( : )
    TYPE(IndexValue_), ALLOCATABLE :: Obj( : )
  END FUNCTION Constructor2

  MODULE PURE FUNCTION Constructor3( Indx, Val ) RESULT( Obj )
    INTEGER( I4B ), INTENT( IN ) :: Indx( : )
    REAL( DFP ), INTENT( IN ) :: Val
    TYPE(IndexValue_), ALLOCATABLE :: Obj( : )
  END FUNCTION Constructor3

END INTERFACE

INTERFACE IndexValue
  MODULE PROCEDURE Constructor1, Constructor2, Constructor3
END INTERFACE IndexValue

PUBLIC :: IndexValue

!----------------------------------------------------------------------------
!                                                                 Contains
!----------------------------------------------------------------------------

END MODULE IndexValue_Method