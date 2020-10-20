SUBMODULE( FEVariable_Method ) Constructor
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                 Display
!----------------------------------------------------------------------------
MODULE PROCEDURE Display_Obj
  INTEGER( I4B ) :: I

  I = stdout

  IF( PRESENT( UnitNo ) ) I = UnitNo

  IF( LEN_TRIM( Msg ) .NE. 0 ) WRITE( I, "(A)" ) TRIM( Msg )

  SELECT CASE( Obj % Rank )
  CASE( Scalar )
    WRITE( I, "(A)") "RANK :: 0 (SCALAR)"

    SELECT CASE( Obj % VarType )

    CASE( Constant )
      WRITE( I, "(A)") "CONTSTANT IN SPACE-TIME"
      CALL Display( Obj % R0, 'VALUE' )

    CASE( Space )
      WRITE( I, "(A)") "VARIABLE IN SPACE ONLY"
      CALL Display( Obj % R1, 'VALUE' )

    CASE( SpaceTime )
      WRITE( I, "(A)") "VARIABLE IN SPACE AND TIME"
      CALL Display( Obj % R2, 'VALUE' )

    END SELECT

  CASE( Vector )
    WRITE( I, "(A)") "RANK :: 1 (VECTOR)"

    SELECT CASE( Obj % VarType )

    CASE( Constant )
      WRITE( I, "(A)") "CONTSTANT IN SPACE-TIME"
      CALL Display( Obj % R1, 'VALUE' )

    CASE( Space )
      WRITE( I, "(A)") "VARIABLE IN SPACE ONLY"
      CALL Display( Obj % R2, 'VALUE' )

    CASE( SpaceTime )
      WRITE( I, "(A)") "VARIABLE IN SPACE AND TIME"
      CALL Display( Obj % R3, 'VALUE' )

    END SELECT

  CASE( Matrix )
    WRITE( I, "(A)") "RANK :: 2 (MATRIX)"

    SELECT CASE( Obj % VarType )

    CASE( Constant )
      WRITE( I, "(A)") "CONTSTANT IN SPACE-TIME"
      CALL Display( Obj % R2, 'VALUE' )

    CASE( Space )
      WRITE( I, "(A)") "VARIABLE IN SPACE ONLY"
      CALL Display( Obj % R3, 'VALUE' )

    CASE( SpaceTime )
      WRITE( I, "(A)") "VARIABLE IN SPACE AND TIME"
      CALL Display( Obj % R4, 'VALUE' )

    END SELECT

  END SELECT
END PROCEDURE Display_Obj

!----------------------------------------------------------------------------
!                                                            DeallocateData
!----------------------------------------------------------------------------

MODULE PROCEDURE fe_deallocate
  IF( ALLOCATED( Obj % R1 ) ) DEALLOCATE( Obj % R1 )
  IF( ALLOCATED( Obj % R2 ) ) DEALLOCATE( Obj % R2 )
  IF( ALLOCATED( Obj % R3 ) ) DEALLOCATE( Obj % R3 )
  IF( ALLOCATED( Obj % R4 ) ) DEALLOCATE( Obj % R4 )
  Obj % R0 = 0.0_DFP
  Obj % DefineOn = 0
  Obj % VarType = 0
  Obj % Rank = 0
  Obj % CaseType = 0
END PROCEDURE fe_deallocate
!----------------------------------------------------------------------------
!                                                             NodalVariable
!----------------------------------------------------------------------------

MODULE PROCEDURE Nodal_Scalar_Constant
  Obj % R0 = Val
  Obj % DefineOn = Nodal
  Obj % Rank = Scalar
  Obj % VarType = Constant
  Obj % CaseType = 1
END PROCEDURE Nodal_Scalar_Constant

!----------------------------------------------------------------------------
!                                                             NodalVariable
!----------------------------------------------------------------------------

MODULE PROCEDURE Nodal_Scalar_Space
  Obj % R1 = Val
  Obj % DefineOn = Nodal
  Obj % Rank = Scalar
  Obj % VarType = Space
  Obj % CaseType = 2
END PROCEDURE Nodal_Scalar_Space

!----------------------------------------------------------------------------
!                                                             NodalVariable
!----------------------------------------------------------------------------

MODULE PROCEDURE Nodal_Scalar_Spacetime
  Obj % R2 = Val
  Obj % DefineOn = Nodal
  Obj % Rank = Scalar
  Obj % VarType = Spacetime
  Obj % CaseType = 3
END PROCEDURE Nodal_Scalar_Spacetime

!----------------------------------------------------------------------------
!                                                             NodalVariable
!----------------------------------------------------------------------------

MODULE PROCEDURE Nodal_Vector_Constant
  Obj % R1 = Val
  Obj % DefineOn = Nodal
  Obj % Rank = Vector
  Obj % VarType = Constant
  Obj % CaseType = 4
END PROCEDURE Nodal_Vector_Constant

!----------------------------------------------------------------------------
!                                                             NodalVariable
!----------------------------------------------------------------------------

MODULE PROCEDURE Nodal_Vector_Space
  Obj % R2 = Val
  Obj % DefineOn = Nodal
  Obj % Rank = Vector
  Obj % VarType = Space
  Obj % CaseType = 5
END PROCEDURE Nodal_Vector_Space

!----------------------------------------------------------------------------
!                                                             NodalVariable
!----------------------------------------------------------------------------

MODULE PROCEDURE Nodal_Vector_Spacetime
  Obj % R3 = Val
  Obj % DefineOn = Nodal
  Obj % Rank = Vector
  Obj % VarType = Spacetime
  Obj % CaseType = 6
END PROCEDURE Nodal_Vector_Spacetime

!----------------------------------------------------------------------------
!                                                             NodalVariable
!----------------------------------------------------------------------------

MODULE PROCEDURE Nodal_Matrix_Constant
  Obj % R2 = Val
  Obj % DefineOn = Nodal
  Obj % Rank = Matrix
  Obj % VarType = Constant
  Obj % CaseType = 7
END PROCEDURE Nodal_Matrix_Constant

!----------------------------------------------------------------------------
!                                                             NodalVariable
!----------------------------------------------------------------------------

MODULE PROCEDURE Nodal_Matrix_Space
  Obj % R3 = Val
  Obj % DefineOn = Nodal
  Obj % Rank = Matrix
  Obj % VarType = Space
  Obj % CaseType = 8
END PROCEDURE Nodal_Matrix_Space

!----------------------------------------------------------------------------
!                                                             NodalVariable
!----------------------------------------------------------------------------

MODULE PROCEDURE Nodal_Matrix_Spacetime
  Obj % R4 = Val
  Obj % DefineOn = Nodal
  Obj % Rank = Matrix
  Obj % VarType = Spacetime
  Obj % CaseType = 9
END PROCEDURE Nodal_Matrix_Spacetime

!----------------------------------------------------------------------------
!                                                             NodalVariable
!----------------------------------------------------------------------------

MODULE PROCEDURE Quadrature_Scalar_Constant
  Obj % R0 = Val
  Obj % DefineOn = Quadrature
  Obj % Rank = Scalar
  Obj % VarType = Constant
  Obj % CaseType = 10
END PROCEDURE Quadrature_Scalar_Constant

!----------------------------------------------------------------------------
!                                                             QuadratureVariable
!----------------------------------------------------------------------------

MODULE PROCEDURE Quadrature_Scalar_Space
  Obj % R1 = Val
  Obj % DefineOn = Quadrature
  Obj % Rank = Scalar
  Obj % VarType = Space
  Obj % CaseType = 11
END PROCEDURE Quadrature_Scalar_Space

!----------------------------------------------------------------------------
!                                                             QuadratureVariable
!----------------------------------------------------------------------------

MODULE PROCEDURE Quadrature_Scalar_Spacetime
  Obj % R2 = Val
  Obj % DefineOn = Quadrature
  Obj % Rank = Scalar
  Obj % VarType = Spacetime
  Obj % CaseType = 12
END PROCEDURE Quadrature_Scalar_Spacetime

!----------------------------------------------------------------------------
!                                                             QuadratureVariable
!----------------------------------------------------------------------------

MODULE PROCEDURE Quadrature_Vector_Constant
  Obj % R1 = Val
  Obj % DefineOn = Quadrature
  Obj % Rank = Vector
  Obj % VarType = Constant
  Obj % CaseType = 13
END PROCEDURE Quadrature_Vector_Constant

!----------------------------------------------------------------------------
!                                                             QuadratureVariable
!----------------------------------------------------------------------------

MODULE PROCEDURE Quadrature_Vector_Space
  Obj % R2 = Val
  Obj % DefineOn = Quadrature
  Obj % Rank = Vector
  Obj % VarType = Space
  Obj % CaseType = 14
END PROCEDURE Quadrature_Vector_Space

!----------------------------------------------------------------------------
!                                                             QuadratureVariable
!----------------------------------------------------------------------------

MODULE PROCEDURE Quadrature_Vector_Spacetime
  Obj % R3 = Val
  Obj % DefineOn = Quadrature
  Obj % Rank = Vector
  Obj % VarType = Spacetime
  Obj % CaseType = 15
END PROCEDURE Quadrature_Vector_Spacetime

!----------------------------------------------------------------------------
!                                                             QuadratureVariable
!----------------------------------------------------------------------------

MODULE PROCEDURE Quadrature_Matrix_Constant
  Obj % R2 = Val
  Obj % DefineOn = Quadrature
  Obj % Rank = Matrix
  Obj % VarType = Constant
  Obj % CaseType = 16
END PROCEDURE Quadrature_Matrix_Constant

!----------------------------------------------------------------------------
!                                                             QuadratureVariable
!----------------------------------------------------------------------------

MODULE PROCEDURE Quadrature_Matrix_Space
  Obj % R3 = Val
  Obj % DefineOn = Quadrature
  Obj % Rank = Matrix
  Obj % VarType = Space
  Obj % CaseType = 17
END PROCEDURE Quadrature_Matrix_Space

!----------------------------------------------------------------------------
!                                                             QuadratureVariable
!----------------------------------------------------------------------------

MODULE PROCEDURE Quadrature_Matrix_Spacetime
  Obj % R4 = Val
  Obj % DefineOn = Quadrature
  Obj % Rank = Matrix
  Obj % VarType = Spacetime
  Obj % CaseType = 18
END PROCEDURE Quadrature_Matrix_Spacetime

END SUBMODULE Constructor