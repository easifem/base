MODULE VoigtRank2Tensor_Method

USE GlobalData
USE IO
USE BaseType

IMPLICIT NONE

PRIVATE

!----------------------------------------------------------------------------
!                                                       Initiate@Constructor
!----------------------------------------------------------------------------

INTERFACE

MODULE PURE SUBROUTINE init_from_vec( Obj, Vec, VoigtType )
  CLASS( VoigtRank2Tensor_ ), INTENT( INOUT ) :: Obj
  REAL( DFP ), INTENT( IN ) :: Vec( : )
  INTEGER( I4B ), INTENT( IN ) :: VoigtType
END SUBROUTINE init_from_vec

MODULE PURE SUBROUTINE init_from_mat( Obj, T, VoigtType )
  CLASS( VoigtRank2Tensor_ ), INTENT( INOUT ) :: Obj
  REAL( DFP ), INTENT( IN ) :: T( 3, 3 )
  INTEGER( I4B ), INTENT( IN ) :: VoigtType
END SUBROUTINE init_from_mat

MODULE PURE SUBROUTINE init_from_r2tensor( Obj, T, VoigtType )
  CLASS( VoigtRank2Tensor_ ), INTENT( INOUT ) :: Obj
  CLASS( Rank2Tensor_ ), INTENT( IN ) :: T
  INTEGER( I4B ), INTENT( IN ) :: VoigtType
END SUBROUTINE init_from_r2tensor

END INTERFACE

INTERFACE Initiate
  MODULE PROCEDURE init_from_vec, init_from_r2tensor, init_from_mat
END INTERFACE Initiate

PUBLIC :: Initiate

!----------------------------------------------------------------------------
!                                               VoigtRank2Tensor@Constructor
!----------------------------------------------------------------------------

INTERFACE

MODULE PURE FUNCTION constructor1( Vec, VoigtType ) RESULT( Obj )
  REAL( DFP ), INTENT( IN ) :: Vec( : )
  INTEGER( I4B ), INTENT( IN ) :: VoigtType
  TYPE( VoigtRank2Tensor_ ) :: Obj
END FUNCTION constructor1

END INTERFACE

INTERFACE VoigtRank2Tensor
  MODULE PROCEDURE constructor1
END INTERFACE VoigtRank2Tensor

PUBLIC :: VoigtRank2Tensor

!----------------------------------------------------------------------------
!                                       VoigtRank2Tensor_Pointer@Constructor
!----------------------------------------------------------------------------

INTERFACE

MODULE PURE FUNCTION constructor_1( Vec, VoigtType ) RESULT( Obj )
  REAL( DFP ), INTENT( IN ) :: Vec( : )
  INTEGER( I4B ), INTENT( IN ) :: VoigtType
  CLASS( VoigtRank2Tensor_ ), POINTER :: Obj
END FUNCTION constructor_1

END INTERFACE

INTERFACE VoigtRank2Tensor_Pointer
  MODULE PROCEDURE constructor_1
END INTERFACE VoigtRank2Tensor_Pointer

PUBLIC :: VoigtRank2Tensor_Pointer

!----------------------------------------------------------------------------
!                                                        Display@Constructor
!----------------------------------------------------------------------------

INTERFACE

MODULE SUBROUTINE display_obj( Obj, Msg, UnitNo )
  CLASS( VoigtRank2Tensor_ ), INTENT( IN ) :: Obj
  CHARACTER( LEN = * ), INTENT( IN ) :: Msg
  INTEGER( I4B ), INTENT( IN ), OPTIONAL :: UnitNo
END SUBROUTINE display_obj

END INTERFACE

INTERFACE Display
  MODULE PROCEDURE display_obj
END INTERFACE Display

PUBLIC :: Display

!----------------------------------------------------------------------------
!                                                           Convert@GetMethod
!----------------------------------------------------------------------------

INTERFACE

MODULE PURE SUBROUTINE voigt_to_mat( From, To )
  CLASS( VoigtRank2Tensor_ ), INTENT( IN ) :: From
  REAL( DFP ), INTENT( INOUT ) :: To( 3, 3 )
END SUBROUTINE voigt_to_mat

MODULE PURE SUBROUTINE voigt_to_vec( From, To )
  CLASS( VoigtRank2Tensor_ ), INTENT( IN ) :: From
  REAL( DFP ), INTENT( INOUT ) :: To( : )
END SUBROUTINE voigt_to_vec

MODULE PURE SUBROUTINE voigt_to_tensor( From, To )
  CLASS( VoigtRank2Tensor_ ), INTENT( IN ) :: From
  CLASS( Rank2Tensor_ ), INTENT( INOUT ) :: To
END SUBROUTINE voigt_to_tensor

MODULE PURE SUBROUTINE tensor_to_voigt( From, To, VoigtType )
  CLASS( Rank2Tensor_ ), INTENT( IN ) :: From
  CLASS( VoigtRank2Tensor_ ), INTENT( INOUT ) :: To
  INTEGER( I4B ), INTENT( IN ) :: VoigtType
END SUBROUTINE tensor_to_voigt

END INTERFACE

INTERFACE Convert
  MODULE PROCEDURE voigt_to_mat, voigt_to_vec, voigt_to_tensor, &
    & tensor_to_voigt
END INTERFACE Convert

PUBLIC :: Convert

!----------------------------------------------------------------------------
!                                                                 Contains
!----------------------------------------------------------------------------

END MODULE VoigtRank2Tensor_Method