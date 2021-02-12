MODULE Buffer_Method
USE BaseType
USE GlobalData
IMPLICIT NONE

PRIVATE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE buffer_append_str( Obj, Entry )
  TYPE( Buffer_ ), INTENT( INOUT ) :: Obj
  TYPE( String ), INTENT( IN ), TARGET :: Entry
END SUBROUTINE buffer_append_str
END INTERFACE

INTERFACE Append
  MODULE PROCEDURE buffer_append_str
END INTERFACE Append

PUBLIC :: Append

END MODULE Buffer_Method