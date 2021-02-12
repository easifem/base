MODULE Rank2Tensor_Method
USE GlobalData
USE IO
USE BaseType
IMPLICIT NONE

PRIVATE

!----------------------------------------------------------------------------
!                                                         initiate@constructor
!----------------------------------------------------------------------------

INTERFACE

MODULE PURE SUBROUTINE init_by_mat( Obj, Mat )
  CLASS( Rank2Tensor_ ), INTENT( INOUT ) :: Obj
  REAL( DFP ), INTENT( IN ) :: Mat( 3, 3 )
END SUBROUTINE init_by_mat

MODULE PURE SUBROUTINE init_by_vec( Obj, Vec, VoigtType )
  CLASS( Rank2Tensor_ ), INTENT( INOUT ) :: Obj
  REAL( DFP ), INTENT( IN ) :: Vec( : )
  INTEGER( I4B ), INTENT( IN ) :: VoigtType
END SUBROUTINE init_by_vec

END INTERFACE

INTERFACE Initiate
  MODULE PROCEDURE init_by_mat, init_by_vec
END INTERFACE Initiate

PUBLIC :: Initiate

!----------------------------------------------------------------------------
!                                                          convert@constructor
!----------------------------------------------------------------------------

INTERFACE

MODULE PURE SUBROUTINE mat_to_rank2( From, To )
  CLASS( Rank2Tensor_ ), INTENT( INOUT ) :: To
  REAL( DFP ), INTENT( IN ) :: From( 3, 3 )
END SUBROUTINE mat_to_rank2

MODULE PURE SUBROUTINE rank2_to_mat( From, To )
  CLASS( Rank2Tensor_ ), INTENT( IN ) :: From
  REAL( DFP ), INTENT( INOUT ) :: To( 3, 3 )
END SUBROUTINE rank2_to_mat

MODULE PURE SUBROUTINE rank2_equal_mat( Obj, Mat )
  CLASS( Rank2Tensor_ ), INTENT( INOUT ) :: Obj
  REAL( DFP ), INTENT( IN ) :: Mat( 3, 3 )
END SUBROUTINE rank2_equal_mat

MODULE PURE SUBROUTINE mat_equal_rank2( Mat, Obj )
  REAL( DFP ), INTENT( INOUT ) :: Mat( 3, 3 )
  CLASS( Rank2Tensor_ ), INTENT( IN ) :: Obj
END SUBROUTINE mat_equal_rank2

END INTERFACE

INTERFACE ASSIGNMENT( = )
  MODULE PROCEDURE rank2_equal_mat, mat_equal_rank2
END INTERFACE

INTERFACE Convert
  MODULE PROCEDURE mat_to_rank2, rank2_to_mat
END INTERFACE Convert

PUBLIC :: Convert, ASSIGNMENT( = )

!----------------------------------------------------------------------------
!                                                    Rank2Tensor@constructor
!----------------------------------------------------------------------------

INTERFACE

MODULE PURE FUNCTION r2t_by_mat( Mat  ) RESULT( Obj )
  REAL( DFP ), INTENT( IN ) :: Mat( 3, 3 )
  TYPE( Rank2Tensor_ ) :: Obj
END FUNCTION r2t_by_mat

MODULE PURE FUNCTION r2t_by_voigtvec( VoigtVec, VoigtType ) RESULT( Obj )
  REAL( DFP ), INTENT( IN ) :: VoigtVec( : )
  INTEGER( I4B ), INTENT( IN ) :: VoigtType
  TYPE( Rank2Tensor_ ) :: Obj
END FUNCTION r2t_by_voigtvec

END INTERFACE

INTERFACE Rank2Tensor
  MODULE PROCEDURE r2t_by_mat, r2t_by_voigtvec
END INTERFACE Rank2Tensor

PUBLIC :: Rank2Tensor

!----------------------------------------------------------------------------
!                                            Rank2Tensor_Pointer@constructor
!----------------------------------------------------------------------------

INTERFACE

MODULE PURE FUNCTION r2tp_by_mat( Mat ) RESULT( Obj )
  REAL( DFP ), INTENT( IN ) :: Mat( 3, 3 )
  CLASS( Rank2Tensor_ ), POINTER :: Obj
END FUNCTION r2tp_by_mat

MODULE PURE FUNCTION r2tp_by_voigtvec( VoigtVec, VoigtType ) RESULT( Obj )
  REAL( DFP ), INTENT( IN ) :: VoigtVec( : )
  INTEGER( I4B ), INTENT( IN ) :: VoigtType
  CLASS( Rank2Tensor_ ), POINTER :: Obj
END FUNCTION r2tp_by_voigtvec

END INTERFACE

INTERFACE Rank2Tensor_Pointer
  MODULE PROCEDURE r2tp_by_mat, r2tp_by_voigtvec
END INTERFACE Rank2Tensor_Pointer

PUBLIC :: Rank2Tensor_Pointer

!----------------------------------------------------------------------------
!                                                 IdentityTensor@constructor
!----------------------------------------------------------------------------

INTERFACE

MODULE PURE SUBROUTINE identity_rank2( Obj )
  CLASS( Rank2Tensor_ ), INTENT( INOUT ) :: Obj
END SUBROUTINE identity_rank2

END INTERFACE

INTERFACE IdentityTensor
  MODULE PROCEDURE identity_rank2
END INTERFACE IdentityTensor

PUBLIC :: IdentityTensor

!----------------------------------------------------------------------------
!                                                     OnesTensor@constructor
!----------------------------------------------------------------------------

INTERFACE

MODULE PURE SUBROUTINE Ones_rank2( Obj )
  CLASS( Rank2Tensor_ ), INTENT( INOUT ) :: Obj
END SUBROUTINE Ones_rank2

END INTERFACE

INTERFACE Ones
  MODULE PROCEDURE Ones_rank2
END INTERFACE Ones

PUBLIC :: Ones

!----------------------------------------------------------------------------
!                                                   ZerosTensor@constructor
!----------------------------------------------------------------------------

INTERFACE

MODULE PURE SUBROUTINE Zeros_rank2( Obj )
  CLASS( Rank2Tensor_ ), INTENT( INOUT ) :: Obj
END SUBROUTINE Zeros_rank2

END INTERFACE

INTERFACE Zeros
  MODULE PROCEDURE Zeros_rank2
END INTERFACE Zeros

PUBLIC :: Zeros

!----------------------------------------------------------------------------
!                                                IsotropicTensor@constructor
!----------------------------------------------------------------------------

INTERFACE

MODULE PURE SUBROUTINE isotropic_rank2( Obj, Lambda )
  CLASS( Rank2Tensor_ ), INTENT( INOUT ) :: Obj
  REAL( DFP ), INTENT( IN ) :: Lambda
END SUBROUTINE isotropic_rank2

END INTERFACE

INTERFACE IsotropicTensor
  MODULE PROCEDURE isotropic_rank2
END INTERFACE IsotropicTensor

PUBLIC :: IsotropicTensor

!----------------------------------------------------------------------------
!                                                       Display@constructor
!----------------------------------------------------------------------------

INTERFACE

MODULE SUBROUTINE display_obj( Obj, Msg, UnitNo )
  CLASS( Rank2Tensor_ ), INTENT( IN ) :: Obj
  CHARACTER( LEN = * ), INTENT( IN ) :: Msg
  INTEGER( I4B ), INTENT( IN ), OPTIONAL :: UnitNo
END SUBROUTINE display_obj

END INTERFACE

INTERFACE Display
  MODULE PROCEDURE display_obj
END INTERFACE Display

PUBLIC :: Display

!----------------------------------------------------------------------------
!                                                           Trace@Operation
!----------------------------------------------------------------------------

INTERFACE

MODULE PURE FUNCTION trace_obj( Obj, Power ) RESULT( Ans )
  CLASS( Rank2Tensor_ ), INTENT( IN ) :: Obj
  INTEGER( I4B ), INTENT( IN ) :: Power
  REAL( DFP ) :: Ans
END FUNCTION trace_obj

END INTERFACE

INTERFACE Trace
  MODULE PROCEDURE trace_obj
END INTERFACE Trace

PUBLIC :: Trace

!----------------------------------------------------------------------------
!                                                                J2@Operation
!----------------------------------------------------------------------------

INTERFACE

MODULE PURE FUNCTION j2_obj( Obj, isDeviatoric ) RESULT( Ans )
  CLASS( Rank2Tensor_ ), INTENT( IN ) :: Obj
  LOGICAL( LGT ), INTENT( IN ) :: isDeviatoric
  REAL( DFP ) :: Ans
END FUNCTION j2_obj

END INTERFACE

INTERFACE J2
  MODULE PROCEDURE j2_obj
END INTERFACE J2

PUBLIC :: J2

!----------------------------------------------------------------------------
!                                                                J3@Operation
!----------------------------------------------------------------------------

INTERFACE

MODULE PURE FUNCTION j3_obj( Obj, isDeviatoric ) RESULT( Ans )
  CLASS( Rank2Tensor_ ), INTENT( IN ) :: Obj
  LOGICAL( LGT ), INTENT( IN ) :: isDeviatoric
  REAL( DFP ) :: Ans
END FUNCTION j3_obj

END INTERFACE

INTERFACE J3
  MODULE PROCEDURE j3_obj
END INTERFACE J3

PUBLIC :: J3

!----------------------------------------------------------------------------
!                                                               Det@Operation
!----------------------------------------------------------------------------

INTERFACE

MODULE PURE FUNCTION det_obj( Obj ) RESULT( Ans )
  CLASS( Rank2Tensor_ ), INTENT( IN ) :: Obj
  REAL( DFP ) :: Ans
END FUNCTION det_obj

END INTERFACE

INTERFACE Det
  MODULE PROCEDURE Det_obj
END INTERFACE Det

PUBLIC :: Det

!----------------------------------------------------------------------------
!                                                         LodeAngle@Operation
!----------------------------------------------------------------------------

INTERFACE

MODULE PURE FUNCTION theta_obj( Obj, LodeType, isDeviatoric ) RESULT( Ans )
  CLASS( Rank2Tensor_ ), INTENT( IN ) :: Obj
  INTEGER( I4B ), INTENT( IN ) :: LodeType
  LOGICAL( LGT ), INTENT( IN ) :: isDeviatoric
  REAL( DFP ) :: Ans
END FUNCTION theta_obj

MODULE PURE FUNCTION theta_obj_j2j3( Obj, LodeType, J2J3 ) RESULT( Ans )
  CLASS( Rank2Tensor_ ), INTENT( IN ) :: Obj
  INTEGER( I4B ), INTENT( IN ) :: LodeType
  REAL( DFP ), INTENT( IN ), OPTIONAL :: J2J3( 2 )
  REAL( DFP ) :: Ans
END FUNCTION theta_obj_j2j3

END INTERFACE

INTERFACE LodeAngle
  MODULE PROCEDURE theta_obj, theta_obj_j2j3
END INTERFACE LodeAngle

PUBLIC :: LodeAngle

!----------------------------------------------------------------------------
!                                                               Sym@Operation
!----------------------------------------------------------------------------

INTERFACE

MODULE PURE FUNCTION sym_r2t( Obj ) RESULT( Ans )
  CLASS( Rank2Tensor_ ), INTENT( IN ) :: Obj
  TYPE( Rank2Tensor_ ) :: Ans
END FUNCTION sym_r2t

END INTERFACE

INTERFACE Sym
  MODULE PROCEDURE sym_r2t
END INTERFACE Sym

PUBLIC :: Sym

!----------------------------------------------------------------------------
!                                                            SkewSym@Operation
!----------------------------------------------------------------------------

INTERFACE

MODULE PURE FUNCTION Skewsym_r2t( Obj ) RESULT( Ans )
  CLASS( Rank2Tensor_ ), INTENT( IN ) :: Obj
  TYPE( Rank2Tensor_ ) :: Ans
END FUNCTION Skewsym_r2t

END INTERFACE

INTERFACE SkewSym
  MODULE PROCEDURE Skewsym_r2t
END INTERFACE SkewSym

PUBLIC :: SkewSym

!----------------------------------------------------------------------------
!                                                    IsotropicPart@Operation
!----------------------------------------------------------------------------

INTERFACE

MODULE PURE FUNCTION iso_part_obj( Obj ) RESULT( Ans )
  CLASS( Rank2Tensor_ ), INTENT( IN ) :: Obj
  TYPE( Rank2Tensor_ ) :: Ans
END FUNCTION iso_part_obj

END INTERFACE

INTERFACE Isotropic
  MODULE PROCEDURE iso_part_obj
END INTERFACE Isotropic

PUBLIC :: Isotropic

!----------------------------------------------------------------------------
!                                                    DeviatoricPart@Operation
!----------------------------------------------------------------------------

INTERFACE

MODULE PURE FUNCTION dev_part_obj( Obj ) RESULT( Ans )
  CLASS( Rank2Tensor_ ), INTENT( IN ) :: Obj
  TYPE( Rank2Tensor_ ) :: Ans
END FUNCTION dev_part_obj

END INTERFACE

INTERFACE Deviatoric
  MODULE PROCEDURE dev_part_obj
END INTERFACE Deviatoric

PUBLIC :: Deviatoric

!----------------------------------------------------------------------------
!                                                      Contraction@Operation
!----------------------------------------------------------------------------

INTERFACE

MODULE PURE FUNCTION r2_contract_r2( Obj1, Obj2 ) RESULT( Ans )
  CLASS( Rank2Tensor_ ), INTENT( IN ) :: Obj1, Obj2
  REAL( DFP ) :: Ans
END FUNCTION r2_contract_r2

MODULE PURE FUNCTION r2_contract_voigt_r2( Obj1, Obj2 ) RESULT( Ans )
  CLASS( Rank2Tensor_ ), INTENT( IN ) :: Obj1
  CLASS( VoigtRank2Tensor_ ), INTENT( IN ) :: Obj2
  REAL( DFP ) :: Ans
END FUNCTION r2_contract_voigt_r2

MODULE PURE FUNCTION voigt_r2_contract_r2( Obj1, Obj2 ) RESULT( Ans )
  CLASS( VoigtRank2Tensor_ ), INTENT( IN ) :: Obj1
  CLASS( Rank2Tensor_ ), INTENT( IN ) :: Obj2
  REAL( DFP ) :: Ans
END FUNCTION voigt_r2_contract_r2

MODULE PURE FUNCTION voigt_r2_contract_voigt_r2( Obj1, Obj2 ) RESULT( Ans )
  CLASS( VoigtRank2Tensor_ ), INTENT( IN ) :: Obj1
  CLASS( VoigtRank2Tensor_ ), INTENT( IN ) :: Obj2
  REAL( DFP ) :: Ans
END FUNCTION voigt_r2_contract_voigt_r2

END INTERFACE

INTERFACE Contraction
  MODULE PROCEDURE r2_contract_r2, r2_contract_voigt_r2, voigt_r2_contract_r2, &
    & voigt_r2_contract_voigt_r2
END INTERFACE Contraction

PUBLIC :: Contraction

!----------------------------------------------------------------------------
!                                                             AXPY@Operation
!----------------------------------------------------------------------------

INTERFACE

MODULE PURE SUBROUTINE axpy_a1_a2( a1, Obj1, a2, Obj2 )
  CLASS( Rank2Tensor_ ), INTENT( INOUT ) :: Obj1
  CLASS( Rank2Tensor_ ), INTENT( IN ) :: Obj2
  REAL( DFP ), INTENT( IN ) :: a1, a2
END SUBROUTINE axpy_a1_a2

END INTERFACE

INTERFACE AXPY
  MODULE PROCEDURE axpy_a1_a2
END INTERFACE AXPY

PUBLIC :: AXPY

!----------------------------------------------------------------------------
!                                                      Invariants@Operation
!----------------------------------------------------------------------------

INTERFACE

MODULE PURE FUNCTION invariants_rank2( Obj, isDeviatoric ) RESULT( Ans )
  CLASS( Rank2Tensor_ ), INTENT( IN ) :: Obj
  LOGICAL( LGT ), INTENT( IN ) :: isDeviatoric
  REAL( DFP ) :: Ans( 3 )
END FUNCTION invariants_rank2

END INTERFACE

INTERFACE Invariants
  MODULE PROCEDURE invariants_rank2
END INTERFACE Invariants

PUBLIC :: Invariants

!----------------------------------------------------------------------------
!                                                          Eigen@Operation
!----------------------------------------------------------------------------

INTERFACE

MODULE SUBROUTINE sym_spectral_r2t( Obj, Q, W )
  CLASS( Rank2Tensor_ ), INTENT( IN ) :: Obj
  REAL( DFP ), INTENT( INOUT ) :: Q( 3, 3 ), W( 3 )
END SUBROUTINE sym_spectral_r2t

MODULE SUBROUTINE spectral_r2t( Obj, Q, W )
  CLASS( Rank2Tensor_ ), INTENT( IN ) :: Obj
  COMPLEX( DFP ), INTENT( INOUT ) :: Q( 3, 3 ), W( 3 )
END SUBROUTINE spectral_r2t

END INTERFACE

INTERFACE SymEigen
  MODULE PROCEDURE sym_spectral_r2t
END INTERFACE SymEigen

INTERFACE Eigen
  MODULE PROCEDURE spectral_r2t
END INTERFACE Eigen

PUBLIC :: SymEigen, Eigen

!----------------------------------------------------------------------------
!                                                     PolarDecomp@Operation
!----------------------------------------------------------------------------

INTERFACE

MODULE SUBROUTINE right_pd_r2t( Obj, R, U )
  CLASS( Rank2Tensor_ ), INTENT( IN ) :: Obj
  CLASS( Rank2Tensor_ ), INTENT( INOUT ) :: R, U
END SUBROUTINE right_pd_r2t

MODULE SUBROUTINE left_pd_r2t( Obj, V, R )
  CLASS( Rank2Tensor_ ), INTENT( IN ) :: Obj
  CLASS( Rank2Tensor_ ), INTENT( INOUT ) :: R, V
END SUBROUTINE left_pd_r2t

END INTERFACE

INTERFACE RightPolarDecomp
  MODULE PROCEDURE right_pd_r2t
END INTERFACE RightPolarDecomp

INTERFACE LeftPolarDecomp
  MODULE PROCEDURE right_pd_r2t
END INTERFACE LeftPolarDecomp

PUBLIC :: RightPolarDecomp, LeftPolarDecomp

!----------------------------------------------------------------------------
!                                                                +@Operation
!----------------------------------------------------------------------------

INTERFACE

MODULE PURE FUNCTION obj_add_obj( Obj1, Obj2 ) RESULT( Ans )
  CLASS( Rank2Tensor_ ), INTENT( IN ) :: Obj1
  CLASS( Rank2Tensor_ ), INTENT( IN ) :: Obj2
  TYPE( Rank2Tensor_ ) :: Ans
END FUNCTION obj_add_obj

MODULE PURE FUNCTION obj_add_mat( Obj1, Obj2 ) RESULT( Ans )
  CLASS( Rank2Tensor_ ), INTENT( IN ) :: Obj1
  REAL( DFP ), INTENT( IN ) :: Obj2( 3, 3 )
  TYPE( Rank2Tensor_ ) :: Ans
END FUNCTION obj_add_mat

MODULE PURE FUNCTION mat_add_obj( Obj1, Obj2 ) RESULT( Ans )
  REAL( DFP ), INTENT( IN ) :: Obj1( 3, 3 )
  CLASS( Rank2Tensor_ ), INTENT( IN ) :: Obj2
  TYPE( Rank2Tensor_ ) :: Ans
END FUNCTION mat_add_obj

MODULE PURE FUNCTION obj_add_scalar( Obj1, Obj2 ) RESULT( Ans )
  CLASS( Rank2Tensor_ ), INTENT( IN ) :: Obj1
  REAL( DFP ), INTENT( IN ) :: Obj2
  TYPE( Rank2Tensor_ ) :: Ans
END FUNCTION obj_add_scalar

MODULE PURE FUNCTION scalar_add_obj( Obj1, Obj2 ) RESULT( Ans )
  REAL( DFP ), INTENT( IN ) :: Obj1
  CLASS( Rank2Tensor_ ), INTENT( IN ) :: Obj2
  TYPE( Rank2Tensor_ ) :: Ans
END FUNCTION scalar_add_obj

END INTERFACE

INTERFACE OPERATOR( + )
  MODULE PROCEDURE obj_add_obj, obj_add_mat, mat_add_obj, obj_add_scalar, &
    & scalar_add_obj
END INTERFACE

PUBLIC :: OPERATOR( + )

!----------------------------------------------------------------------------
!                                                                -@Operation
!----------------------------------------------------------------------------

INTERFACE

MODULE PURE FUNCTION obj_minus_obj( Obj1, Obj2 ) RESULT( Ans )
  CLASS( Rank2Tensor_ ), INTENT( IN ) :: Obj1
  CLASS( Rank2Tensor_ ), INTENT( IN ) :: Obj2
  TYPE( Rank2Tensor_ ) :: Ans
END FUNCTION obj_minus_obj

MODULE PURE FUNCTION obj_minus_mat( Obj1, Obj2 ) RESULT( Ans )
  CLASS( Rank2Tensor_ ), INTENT( IN ) :: Obj1
  REAL( DFP ), INTENT( IN ) :: Obj2( 3, 3 )
  TYPE( Rank2Tensor_ ) :: Ans
END FUNCTION obj_minus_mat

MODULE PURE FUNCTION mat_minus_obj( Obj1, Obj2 ) RESULT( Ans )
  REAL( DFP ), INTENT( IN ) :: Obj1( 3, 3 )
  CLASS( Rank2Tensor_ ), INTENT( IN ) :: Obj2
  TYPE( Rank2Tensor_ ) :: Ans
END FUNCTION mat_minus_obj

MODULE PURE FUNCTION obj_minus_scalar( Obj1, Obj2 ) RESULT( Ans )
  CLASS( Rank2Tensor_ ), INTENT( IN ) :: Obj1
  REAL( DFP ), INTENT( IN ) :: Obj2
  TYPE( Rank2Tensor_ ) :: Ans
END FUNCTION obj_minus_scalar

MODULE PURE FUNCTION scalar_minus_obj( Obj1, Obj2 ) RESULT( Ans )
  REAL( DFP ), INTENT( IN ) :: Obj1
  CLASS( Rank2Tensor_ ), INTENT( IN ) :: Obj2
  TYPE( Rank2Tensor_ ) :: Ans
END FUNCTION scalar_minus_obj

END INTERFACE

INTERFACE OPERATOR( - )
  MODULE PROCEDURE obj_minus_obj, obj_minus_mat, mat_minus_obj, &
    & obj_minus_scalar, scalar_minus_obj
END INTERFACE

PUBLIC :: OPERATOR( - )

!----------------------------------------------------------------------------
!                                                             Exp@Operation
!----------------------------------------------------------------------------

! INTERFACE

! MODULE FUNCTION exp_r2t( Obj ) RESULT( Ans )
! 	CLASS( Rank2Tensor_ ), INTENT( IN ) ::Obj
! 	TYPE( Rank2Tensor_ ) :: Ans
! END FUNCTION exp_r2t

! END INTERFACE

! INTERFACE EXP
! 	MODULE PROCEDURE exp_r2t
! END INTERFACE

! PUBLIC :: EXP

!----------------------------------------------------------------------------
!                                                                    Contains
!----------------------------------------------------------------------------

END MODULE Rank2Tensor_Method