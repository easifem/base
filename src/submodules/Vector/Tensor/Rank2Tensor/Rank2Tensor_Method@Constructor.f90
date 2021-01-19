SUBMODULE( Rank2Tensor_Method ) Constructor

USE BaseMethod

IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE init_by_mat

  Obj % T( 1:3, 1:3 ) = Mat

END PROCEDURE init_by_mat

!----------------------------------------------------------------------------
!                                                                    Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE init_by_vec

  TYPE( VoigtRank2Tensor_ ) :: VoigtObj
  CALL Initiate( VoigtObj, Vec, VoigtType )
  CALL Convert( From = VoigtObj, To = Obj % T )

END PROCEDURE init_by_vec

!----------------------------------------------------------------------------
!                                                                    Convert
!----------------------------------------------------------------------------

MODULE PROCEDURE mat_to_rank2
  To % T( 1:3, 1:3 ) = From
END PROCEDURE mat_to_rank2

MODULE PROCEDURE rank2_to_mat
  To = From % T( 1:3, 1:3 )
END PROCEDURE rank2_to_mat

MODULE PROCEDURE rank2_equal_mat
  Obj % T = Mat
END PROCEDURE rank2_equal_mat

MODULE PROCEDURE mat_equal_rank2
  Mat = Obj % T
END PROCEDURE mat_equal_rank2

!----------------------------------------------------------------------------
!                                                                 Rank2Tensor
!----------------------------------------------------------------------------

MODULE PROCEDURE r2t_by_mat
  CALL Initiate( Obj, Mat )
END PROCEDURE r2t_by_mat

MODULE PROCEDURE r2t_by_voigtvec
  CALL Initiate( Obj, VoigtVec, VoigtType )
END PROCEDURE r2t_by_voigtvec

!----------------------------------------------------------------------------
!                                                        Rank2Tensor_Pointer
!----------------------------------------------------------------------------

MODULE PROCEDURE r2tp_by_mat
  ALLOCATE( Obj )
  CALL Initiate( Obj, Mat )
END PROCEDURE r2tp_by_mat

MODULE PROCEDURE r2tp_by_voigtvec
  ALLOCATE( Obj )
  CALL Initiate( Obj, VoigtVec, VoigtType )
END PROCEDURE r2tp_by_voigtvec

!----------------------------------------------------------------------------
!                                                                  Identity
!----------------------------------------------------------------------------

MODULE PROCEDURE identity_rank2

  ASSOCIATE( T => Obj % T )
    T = 0.0_DFP
    T( 1, 1 ) = 1.0_DFP
    T( 2, 2 ) = 1.0_DFP
    T( 3, 3 ) = 1.0_DFP
  END ASSOCIATE

END PROCEDURE identity_rank2

!----------------------------------------------------------------------------
!                                                                       ones
!----------------------------------------------------------------------------

MODULE PROCEDURE ones_rank2

  ASSOCIATE( T => Obj % T )
    T = 1.0_DFP
  END ASSOCIATE

END PROCEDURE ones_rank2

!----------------------------------------------------------------------------
!                                                                       zeros
!----------------------------------------------------------------------------

MODULE PROCEDURE zeros_rank2

  ASSOCIATE( T => Obj % T )
    T = 1.0_DFP
  END ASSOCIATE

END PROCEDURE zeros_rank2

!----------------------------------------------------------------------------
!                                                             IsotropicTensor
!----------------------------------------------------------------------------

MODULE PROCEDURE isotropic_rank2

  ASSOCIATE( T => Obj % T )

    T = 0.0_DFP

    T( 1, 1 ) = Lambda
    T( 2, 2 ) = Lambda
    T( 3, 3 ) = Lambda

  END ASSOCIATE

END PROCEDURE isotropic_rank2

!----------------------------------------------------------------------------
!                                                          Display@constructor
!----------------------------------------------------------------------------

MODULE PROCEDURE display_obj

  INTEGER( I4B ) :: I

  IF( PRESENT( UnitNo ) ) THEN
    I = UnitNo
  ELSE
    I = StdOut
  END IF

  CALL BlankLines( UnitNo = I, NOL = 1 )

  WRITE( I, "(A)") TRIM( Msg )//" = "
  CALL EqualLine( UnitNo = I )

  WRITE( I, "(4X, 3G15.7 )" ) Obj % T( 1, 1:3 )
  WRITE( I, "(4X, 3G15.7 )" ) Obj % T( 2, 1:3 )
  WRITE( I, "(4X, 3G15.7 )" ) Obj % T( 3, 1:3 )

  CALL BlankLines( UnitNo = I, NOL = 1 )

END PROCEDURE display_obj

END SUBMODULE Constructor