SUBMODULE( VoigtRank2Tensor_Method ) GetMethod

IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                                    Convert
!----------------------------------------------------------------------------


MODULE PROCEDURE voigt_to_mat

  ASSOCIATE( V => From % V )
    To = 0.0_DFP
  SELECT CASE( INT( V( 2 ), I4B ) - 2 )
  CASE( 1 )
    To( 1, 1 ) = V( 3 )
  CASE( 2 )
    To( 1, 1 ) = V( 3 )
    To( 2, 2 ) = V( 4 )
  CASE( 3 )
    To( 1, 1 ) = V( 3 )
    To( 2, 2 ) = V( 4 )
    To( 1, 2 ) = V( 9 ) * V( 5 )
    To( 2, 1 ) = To( 1, 2 )
  CASE( 4 )
    To( 1, 1 ) = V( 3 )
    To( 2, 2 ) = V( 4 )
    To( 1, 2 ) = V( 9 ) * V( 5 )
    To( 2, 1 ) = To( 1, 2 )
    To( 3, 3 ) = V( 6 )
  CASE( 6 )
    To( 1, 1 ) = V( 3 )
    To( 2, 2 ) = V( 4 )
    To( 3, 3 ) = V( 5 )
    To( 1, 2 ) = V( 9 ) * V( 6 )
    To( 2, 1 ) = To( 1, 2 )
    To( 2, 3 ) = V( 9 ) * V( 7 )
    To( 3, 2 ) = To( 2, 3 )
    To( 1, 3 ) = V( 9 ) * V( 8 )
    To( 3, 1 ) = To( 1, 3 )
  END SELECT

  END ASSOCIATE

END PROCEDURE voigt_to_mat

!----------------------------------------------------------------------------
!                                                                    Convert
!----------------------------------------------------------------------------


MODULE PROCEDURE voigt_to_vec

  ASSOCIATE( V => From % V )
    To( 1 : INT( V( 2 ) ) + 2 ) = V( 3 : INT( V( 2 ) ) )
  END ASSOCIATE

END PROCEDURE voigt_to_vec

!----------------------------------------------------------------------------
!                                                                    Convert
!----------------------------------------------------------------------------

MODULE PROCEDURE voigt_to_tensor
  CALL Convert( From, To % T )
END PROCEDURE voigt_to_tensor

!----------------------------------------------------------------------------
!                                                                    Convert
!----------------------------------------------------------------------------

MODULE PROCEDURE tensor_to_voigt
  CALL initiate( To, From, VoigtType )
END PROCEDURE tensor_to_voigt

END SUBMODULE GetMethod