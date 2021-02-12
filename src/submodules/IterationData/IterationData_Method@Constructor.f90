SUBMODULE( IterationData_Method ) Constructor
USE BaseMethod
IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                                 Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE initiate_obj
  IF( PRESENT( MaxIter ) ) Obj % MaxIter = MaxIter
  IF( PRESENT( IterationNumber ) ) Obj % IterationNumber = IterationNumber
  IF( PRESENT( ConvergenceType ) ) Obj % ConvergenceType = ConvergenceType
  IF( PRESENT( ConvergenceIn ) ) Obj % ConvergenceIn = ConvergenceIn
  IF( PRESENT( NormType ) ) Obj % NormType = NormType
  IF( PRESENT( Tolerance ) ) Obj % Tolerance = Tolerance
  IF( PRESENT( ErrorAtEnd ) ) Obj % ErrorAtEnd = ErrorAtEnd
  IF( PRESENT( ErrorAtStart ) ) Obj % ErrorAtStart = ErrorAtStart
  IF( PRESENT( TimeAtStart ) ) Obj % TimeAtStart = TimeAtStart
  IF( PRESENT( TimeAtEnd ) ) Obj % TimeAtEnd = TimeAtEnd
  IF( PRESENT( Converged ) ) Obj % Converged = Converged
END PROCEDURE initiate_obj

!----------------------------------------------------------------------------
!                                                                           
!----------------------------------------------------------------------------

MODULE PROCEDURE is_converged
  SELECT CASE( Obj % ConvergenceType )
  CASE( RelativeConvergence )
    IF( Obj % ErrorAtEnd .LE. Obj % Tolerance * Obj % ErrorAtStart ) THEN
      Ans = .TRUE.
    ELSE
      Ans = .FALSE.
    END IF
  CASE( AbsoluteConvergence )
    IF( Obj % ErrorAtEnd .LE. Obj % Tolerance ) THEN
      Ans = .TRUE.
    ELSE
      Ans = .FALSE.
    END IF
  END SELECT
END PROCEDURE is_converged

!----------------------------------------------------------------------------
!                                                                   Display
!----------------------------------------------------------------------------

MODULE PROCEDURE display_obj
  ! Define internal variables
  INTEGER( I4B ) :: I
  IF( PRESENT( UnitNo ) ) THEN
    I = UnitNo 
  ELSE
    I = StdOut
  END IF

  CALL BlankLines( UnitNo = I )
  IF( LEN_TRIM( msg ) .NE. 0 ) WRITE( I, "(A)" ) TRIM( msg )

  WRITE( I, "( A, L6 )" )    "Convergence            :: ", Obj % Converged
  WRITE( I, "( A, I6 )" )    "Maximum Iteration      :: ", Obj % MaxIter 
  WRITE( I, "( A, I6 )" )    "Total Iteration Taken  :: ", Obj % IterationNumber
  WRITE( I, "( A, G17.6 )" ) "Tolerance              :: ", Obj % Tolerance
  WRITE( I, "( A, G17.6 )" ) "Error At Start         :: ", Obj % ErrorAtStart
  WRITE( I, "( A, G17.6 )" ) "Error At End           :: ", Obj % ErrorAtEnd
  WRITE( I, "( A, G17.6 )" ) "Time Taken             :: ", &
    & Obj % TimeAtEnd - Obj % TimeAtStart
  CALL BlankLines( UnitNo = I )
END PROCEDURE display_obj

!----------------------------------------------------------------------------
!                                                                 
!----------------------------------------------------------------------------

END SUBMODULE Constructor