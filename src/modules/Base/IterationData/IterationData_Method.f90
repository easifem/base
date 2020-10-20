MODULE IterationData_Method
USE GlobalData
USE BaseType
IMPLICIT NONE

PRIVATE

!----------------------------------------------------------------------------
!                                                                  Initiate
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE initiate_obj( Obj, MaxIter, IterationNumber, &
  & Tolerance, ErrorAtStart, ErrorAtEnd, TimeAtStart, TimeAtEnd, &
  & ConvergenceType, ConvergenceIn, NormType, Converged )
  TYPE( IterationData_ ), INTENT( INOUT ) :: Obj
  INTEGER( I4B ), INTENT( IN ), OPTIONAL :: MaxIter, IterationNumber, &
    & ConvergenceType, NormType, ConvergenceIn
  REAL( DFP ), INTENT( IN ), OPTIONAL :: Tolerance, ErrorAtEnd, &
    & ErrorAtStart, TimeAtStart, TimeAtEnd
  LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: Converged
END SUBROUTINE initiate_obj
END INTERFACE

INTERFACE Initiate
  MODULE PROCEDURE initiate_obj
END INTERFACE Initiate

PUBLIC :: Initiate

!----------------------------------------------------------------------------
!                                                               isConverged
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION is_converged( Obj ) RESULT( Ans )
  TYPE( IterationData_ ), INTENT( IN ) :: Obj
  LOGICAL( LGT ) :: Ans
END FUNCTION is_converged
END INTERFACE

INTERFACE isConverged
  MODULE PROCEDURE is_converged
END INTERFACE isConverged

PUBLIC :: isConverged

!----------------------------------------------------------------------------
!                                                                    Display
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE Display_obj( Obj, msg, UnitNo )
  TYPE( IterationData_ ), INTENT( IN ) :: Obj
  CHARACTER( LEN = * ), INTENT( IN ) :: msg
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: UnitNo
END SUBROUTINE Display_obj
END INTERFACE

INTERFACE Display
  MODULE PROCEDURE Display_obj
END INTERFACE Display

PUBLIC :: Display

!----------------------------------------------------------------------------
!                                                                 Contains
!----------------------------------------------------------------------------

#include "./contains.part"

END MODULE IterationData_Method