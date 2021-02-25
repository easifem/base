SUBMODULE( VoigtRank2Tensor_Method ) Constructor
USE BaseMethod
IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE init_from_vec

ASSOCIATE( V => Obj % V )

  V = 0.0_DFP

  V( 1 ) = VoigtType
  V( 2 ) = 2 + SIZE( Vec )
  V( 3 : INT( V( 2 ) ) ) = Vec

  SELECT CASE( VoigtType )
  CASE( StrainTypeVoigt )
    V( 9 ) = 0.5_DFP
  CASE( StressTypeVoigt )
    V( 9 ) = 1.0_DFP
  END SELECT

END ASSOCIATE

END PROCEDURE init_from_vec

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE init_from_mat
ASSOCIATE( V => Obj % V )
  V( 1 ) = VoigtType
  V( 2 ) = 8
  V( 3 ) = T( 1, 1 )
  V( 4 ) = T( 2, 2 )
  V( 5 ) = T( 3, 3 )
  V( 6 ) = T( 1, 2 ) + T( 2, 1 )
  V( 7 ) = T( 2, 3 ) + T( 3, 2 )
  V( 8 ) = T( 1, 3 ) + T( 3, 1 )
  SELECT CASE( VoigtType )
  CASE( StressTypeVoigt )
    V( 6 : 8 ) = 0.5_DFP * V( 6 : 8 )
    V( 9 ) = 1.0_DFP
  CASE( StrainTypeVoigt )
    V( 9 ) = 0.5_DFP
  END SELECT
END ASSOCIATE
END PROCEDURE init_from_mat

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE init_from_r2tensor
  call Initiate( obj, T % T, VoigtType )
END PROCEDURE init_from_r2tensor

!----------------------------------------------------------------------------
!                                                            VoigtRank2Tensor
!----------------------------------------------------------------------------

MODULE PROCEDURE constructor1
  CALL Initiate( Obj, Vec, VoigtType )
END PROCEDURE constructor1

!----------------------------------------------------------------------------
!                                                   VoigtRank2Tensor_Pointer
!----------------------------------------------------------------------------

MODULE PROCEDURE constructor_1
  ALLOCATE( Obj )
  CALL Initiate( Obj, Vec, VoigtType )
END PROCEDURE constructor_1

!----------------------------------------------------------------------------
!                                                                    Display
!----------------------------------------------------------------------------

MODULE PROCEDURE display_obj
  INTEGER( I4B ) :: I, j

  ASSOCIATE( V => Obj % V )

    IF( PRESENT( UnitNo ) ) THEN
      I = UnitNo
    ELSE
      I = StdOut
    END IF

    CALL BlankLines( NOL = 1, UnitNo = I )

    WRITE( I, "(A)" ) TRIM( Msg ) // "="

    SELECT CASE( INT( V( 1 ), I4B ) )
    CASE( StressTypeVoigt )
      WRITE( I, "(4X, A)" ) "Stress Like Voigt Type"
    CASE( StrainTypeVoigt )
      WRITE( I, "(4X, A)" ) "Strain Like Voigt Type"
    END SELECT

    DO j = 1, INT( V( 2 ) ) - 2
      WRITE( I, "(4X, G15.7)", ADVANCE="NO" ) V( 2 + j )
    END DO

    CALL BlankLines( NOL = 1, UnitNo = I )

  END ASSOCIATE

END PROCEDURE display_obj

END SUBMODULE Constructor