SUBMODULE( Rank2Tensor_Method ) Operation
USE BaseMethod

#define T_11 T( 1, 1 )
#define T_12 T( 1, 2 )
#define T_13 T( 1, 3 )
#define T_21 T( 2, 1 )
#define T_22 T( 2, 2 )
#define T_23 T( 2, 3 )
#define T_31 T( 3, 1 )
#define T_32 T( 3, 2 )
#define T_33 T( 3, 3 )


IMPLICIT NONE

CONTAINS

MODULE PROCEDURE trace_obj
  ASSOCIATE( T => Obj % T )
  SELECT CASE( Power )
  CASE( 1 )
    Ans = T_11 + T_22 + T_33
  CASE( 2 )
    Ans = SUM( T( 1:3, 1:3 ) * TRANSPOSE( T( 1:3, 1:3 ) ) )
  CASE( 3 )
    Ans = SUM( MATMUL( T( 1:3, 1:3 ), T( 1:3, 1:3 ) ) * TRANSPOSE( T( 1:3, 1:3 ) ) )
  CASE( 4 )
    BLOCK
      REAL( DFP ) :: Tin( 3, 3 )
      Tin = MATMUL( T( 1:3, 1:3 ), T( 1:3, 1:3 ) )
      Ans = SUM( Tin * TRANSPOSE( Tin ) )
    END BLOCK
  END SELECT
  END ASSOCIATE
END PROCEDURE trace_obj

!----------------------------------------------------------------------------
!                                                                         J2
!----------------------------------------------------------------------------

MODULE PROCEDURE j2_obj

  ASSOCIATE( T => Obj % T )

  IF( isDeviatoric ) THEN

    Ans = 0.5_DFP * SUM( T * TRANSPOSE( T ) )

  ELSE

      Ans = ( T_11 - T_22 ) ** 2 &
        & + ( T_22 - T_33 ) ** 2 &
        & + ( T_33 - T_11 ) ** 2 &
        & + 6.0_DFP * ( T_12 * T_21 + T_23 * T_32 + T_13 * T_31 )

      Ans = Ans / 6.0_DFP
  END IF

  END ASSOCIATE

END PROCEDURE j2_obj

!----------------------------------------------------------------------------
!                                                                         J3
!----------------------------------------------------------------------------

MODULE PROCEDURE j3_obj

  IF( isDeviatoric ) THEN
    Ans = Trace( Obj, 3 ) / 3.0_DFP
  ELSE
    Ans = Trace( Deviatoric( Obj ), 3 ) / 3.0_DFP
  END IF

END PROCEDURE j3_obj

!----------------------------------------------------------------------------
!                                                                         Det
!----------------------------------------------------------------------------

MODULE PROCEDURE det_obj

  ASSOCIATE( T => Obj % T )
    Ans = T(1,1)*(T(2,2)*T(3,3)-T(2,3)*T(3,2)) &
            - T(1,2)*(T(2,1)*T(3,3)-T(2,3)*T(3,1)) &
            + T(1,3)*(T(2,1)*T(3,2)-T(3,1)*T(2,2))
  END ASSOCIATE

END PROCEDURE det_obj

!----------------------------------------------------------------------------
!                                                                   LodeAngle
!----------------------------------------------------------------------------

MODULE PROCEDURE theta_obj

  REAL( DFP ) :: J_2, J_3, Dummy

  ASSOCIATE( T => Obj % T )
    J_2 = J2( Obj, isDeviatoric )
    J_3 = J3( Obj, isDeviatoric )

    IF( J_2 .EQ. 0.0_DFP ) THEN
      Ans = 0.0_DFP
      RETURN
    END IF

    Dummy = 1.5_DFP * SQRT( 3.0_DFP ) * J_3 / ( J_2 * SQRT( J_2 ) )
    IF( Dummy .GE. 1.0_DFP ) Dummy = 1.0_DFP
    IF( Dummy .LE. -1.0_DFP ) Dummy = -1.0_DFP

    SELECT CASE( LodeType )
    CASE( SineLode )
      Ans = ASIN( -Dummy ) / 3.0_DFP
    CASE( CosineLode )
      Ans = ACOS( Dummy ) / 3.0_DFP
    END SELECT

  END ASSOCIATE

END PROCEDURE theta_obj

!----------------------------------------------------------------------------
!                                                                   LodeAngle
!----------------------------------------------------------------------------

MODULE PROCEDURE theta_obj_j2j3

  REAL( DFP ) :: J_2, J_3, Dummy

  ASSOCIATE( T => Obj % T )
    J_2 = J2J3( 1 )
    J_3 = J2J3( 2 )

    IF( J_2 .EQ. 0.0_DFP ) THEN
      Ans = 0.0_DFP
      RETURN
    END IF

    Dummy = 1.5_DFP * SQRT( 3.0_DFP ) * J_3 / ( J_2 * SQRT( J_2 ) )
    IF( Dummy .GE. 1.0_DFP ) Dummy = 1.0_DFP
    IF( Dummy .LE. -1.0_DFP ) Dummy = -1.0_DFP

    SELECT CASE( LodeType )
    CASE( SineLode )
      Ans = ASIN( -Dummy ) / 3.0_DFP
    CASE( CosineLode )
      Ans = ACOS( Dummy ) / 3.0_DFP
    END SELECT

  END ASSOCIATE

END PROCEDURE theta_obj_j2j3

!----------------------------------------------------------------------------
!                                                                        Sym
!----------------------------------------------------------------------------

MODULE PROCEDURE sym_r2t
  Ans % T = 0.5_DFP * ( Obj % T + TRANSPOSE( Obj % T ) )
END PROCEDURE sym_r2t

!----------------------------------------------------------------------------
!                                                                     SkewSym
!----------------------------------------------------------------------------

MODULE PROCEDURE skewsym_r2t
  Ans % T = 0.5_DFP * ( Obj % T - TRANSPOSE( Obj % T ) )
END PROCEDURE skewsym_r2t

!----------------------------------------------------------------------------
!                                                                   Isotropic
!----------------------------------------------------------------------------

MODULE PROCEDURE iso_part_obj

  REAL( DFP ) :: a

  ASSOCIATE( T => Ans % T )
  a = Trace( Obj, 1 ) / 3.0_DFP
  T = 0.0_DFP
  T_11 = a
  T_22 = a
  T_33 = a
  END ASSOCIATE

END PROCEDURE iso_part_obj

!----------------------------------------------------------------------------
!                                                                 Deviatoric
!----------------------------------------------------------------------------

MODULE PROCEDURE dev_part_obj

  REAL( DFP ) :: a

  ASSOCIATE( T => Ans % T )
  a = Trace( Obj, 1 ) / 3.0_DFP
  T = 0.0_DFP
  T_11 = a
  T_22 = a
  T_33 = a
  T = -T + Obj % T
  END ASSOCIATE

END PROCEDURE dev_part_obj

!----------------------------------------------------------------------------
!                                                        Contraction@Operation
!----------------------------------------------------------------------------

MODULE PROCEDURE r2_contract_r2
  Ans = SUM( Obj1 % T * Obj2 % T )
END PROCEDURE r2_contract_r2

!----------------------------------------------------------------------------
!                                                        Contraction@Operation
!----------------------------------------------------------------------------

MODULE PROCEDURE r2_contract_voigt_r2

  ASSOCIATE( T => Obj1 % T, V => Obj2 % V )

  SELECT CASE( INT( V( 2 ) , I4B ) - 2 )
  CASE( 1 )
    Ans = T( 1, 1 ) * V( 3 )
  CASE( 2 )
    Ans = T( 1, 1 ) * V( 3 ) + T( 2, 2 ) * V( 4 )
  CASE( 3 )
    Ans = T_11 * V( 3 ) + T_22 * V( 4 ) + (T_12 + T_21) * V( 5 ) * V( 9 )
  CASE( 4 )
    Ans = T_11 * V( 3 ) &
      & + T_22 * V( 4 ) &
      & + (T_12 + T_21) * V( 5 ) * V( 9 ) &
      & + T_33 * V( 6 )
  CASE( 6 )
    Ans = T_11 * V( 3 ) &
      & + T_22 * V( 4 ) &
      & + T_33 * V( 5 ) &
      & + (T_12 + T_21) * V( 6 ) * V( 9 ) &
      & + (T_23 + T_32) * V( 7 ) * V( 9 ) &
      & + (T_13 + T_31) * V( 8 ) * V( 9 )
  END SELECT

  END ASSOCIATE

END PROCEDURE r2_contract_voigt_r2

!----------------------------------------------------------------------------
!                                                        Contraction@Operation
!----------------------------------------------------------------------------

MODULE PROCEDURE voigt_r2_contract_r2
  ASSOCIATE( T => Obj2 % T, V => Obj1 % V )
  SELECT CASE( INT( V( 2 ) , I4B ) - 2 )
  CASE( 1 )
    Ans = T( 1, 1 ) * V( 3 )
  CASE( 2 )
    Ans = T( 1, 1 ) * V( 3 ) + T( 2, 2 ) * V( 4 )
  CASE( 3 )
    Ans = T_11 * V( 3 ) + T_22 * V( 4 ) + (T_12 + T_21) * V( 5 ) * V( 9 )
  CASE( 4 )
    Ans = T_11 * V( 3 ) &
      & + T_22 * V( 4 ) &
      & + (T_12 + T_21) * V( 5 ) * V( 9 ) &
      & + T_33 * V( 6 )
  CASE( 6 )
    Ans = T_11 * V( 3 ) &
      & + T_22 * V( 4 ) &
      & + T_33 * V( 5 ) &
      & + (T_12 + T_21) * V( 6 ) * V( 9 ) &
      & + (T_23 + T_32) * V( 7 ) * V( 9 ) &
      & + (T_13 + T_31) * V( 8 ) * V( 9 )
  END SELECT
  END ASSOCIATE
END PROCEDURE voigt_r2_contract_r2

!----------------------------------------------------------------------------
!                                                        Contraction@Operation
!----------------------------------------------------------------------------

MODULE PROCEDURE voigt_r2_contract_voigt_r2

  ASSOCIATE( A => Obj1 % V, B => Obj2 % V )

  SELECT CASE( INT( A( 2 ) ) - 2 ) ! size of A and B should same
  CASE( 1 )
    Ans = A( 3 ) * B( 3 )
  CASE( 2 )
    Ans = A( 3 ) * B( 3 ) + A( 4 ) * B( 4 )
  CASE( 3 )
    Ans = A( 3 ) * B( 3 ) + A( 4 ) * B( 4 ) &
      & + 2.0 * A( 9 ) * B( 9 ) * A( 5 ) * B( 5 )
  CASE( 4 )
    Ans = A( 3 ) * B( 3 ) + A( 4 ) * B( 4 ) + A( 6 ) * B( 6 ) &
      & + 2.0 * A( 9 ) * B( 9 ) * A( 5 ) * B( 5 )
  CASE( 6 )
    Ans = A( 3 ) * B( 3 ) + A( 4 ) * B( 4 ) + A( 5 ) * B( 5 ) &
      & + 2.0 * A( 9 ) * B( 9 ) * ( A( 6 ) * B( 6 ) &
      & + A( 7 ) * B( 7 ) + A( 8 ) * B( 8 ) )
  END SELECT

  END ASSOCIATE

END PROCEDURE voigt_r2_contract_voigt_r2

!----------------------------------------------------------------------------
!                                                                       AXPY
!----------------------------------------------------------------------------

MODULE PROCEDURE axpy_a1_a2

  ASSOCIATE( T1 => Obj1 % T, T2 => Obj2 % T )
    T1 = a1 * T1 + a2 * T2
  END ASSOCIATE

END PROCEDURE axpy_a1_a2

!----------------------------------------------------------------------------
!                                                                  Invariants
!----------------------------------------------------------------------------

MODULE PROCEDURE invariants_rank2

  IF( isDeviatoric ) THEN
    Ans( 1 ) = 0.0
    Ans( 2 ) = 0.5_DFP * Trace( Obj, 2 )
    Ans( 3 ) = Det( Obj )
  ELSE
    Ans( 1 ) = Trace( Obj, 1 )
    Ans( 2 ) = 0.5_DFP * ( Ans( 1 ) ** 2 -  Trace( Obj, 2 ) )
    Ans( 3 ) = Det( Obj )
  END IF

END PROCEDURE invariants_rank2

!----------------------------------------------------------------------------
!                                                                    Spectral
!----------------------------------------------------------------------------

MODULE PROCEDURE sym_spectral_r2t
  CALL DSYEVQ3( Obj % T, Q, W )
END PROCEDURE sym_spectral_r2t

!----------------------------------------------------------------------------
!                                                                    Spectral
!----------------------------------------------------------------------------

MODULE PROCEDURE spectral_r2t

  REAL( DFP ) :: EigenVal( 3, 2 ), EigenVec( 3, 3 )
  CALL call_dgeev( Obj % T, EigenVal, EigenVec )

  W = CMPLX( EigenVal( :, 1 ), EigenVal( :, 2 ) )

  ! First two eigen value is complex
  IF( ABS( EigenVal( 1, 2 )  ) .GT. Zero ) THEN
    Q( :, 1 )=  CMPLX( EigenVec( :, 1 ), EigenVec( :, 2 ) )
    Q( :, 2 )=  CMPLX( EigenVec( :, 1 ), -EigenVec( :, 2 ) )
    Q( :, 3 ) = CMPLX( EigenVec( :, 3 ), 0.0_DFP )
    RETURN
  ! Last two eigen value is complex
  ELSE IF( ABS( EigenVal( 2, 2 )  ) .GT. Zero ) THEN
    Q( :, 1 ) = CMPLX( EigenVec( :, 1 ), 0.0_DFP )
    Q( :, 2 )=  CMPLX( EigenVec( :, 2 ), EigenVec( :, 3 ) )
    Q( :, 3 )=  CMPLX( EigenVec( :, 2 ), -EigenVec( :, 3 ) )
    RETURN
  ! no complex eigen value
  ELSE
    Q( :, 1 ) = CMPLX( EigenVec( :, 1 ), 0.0_DFP )
    Q( :, 2 ) = CMPLX( EigenVec( :, 2 ), 0.0_DFP )
    Q( :, 3 ) = CMPLX( EigenVec( :, 3 ), 0.0_DFP )
  END IF

END PROCEDURE spectral_r2t

SUBROUTINE call_dgeev( Mat, EigenValues, EigenVectors )

  !.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .
  !  1. Eigen values are computed using DGEEV( ) subroutine of lapack libarary.
  !  2. EigenValues( :, 1:2 ) has two columns, the first column denotes
  !     the real value of eigen value and second column denotes the
  !     imaginary/complex value of eigenvalue. The conjugate values
  !     are put next to each other. With positive imaginary value
  !     put first.
  !  3. If j-th eigen value is imaginary then j-th and j+1 th Eigenvectors
  !     are given by
  !     v(j) = EigenVectors( :, j ) + i * EigenVectors( :, j +1 )
  !     v(j+1) = EigenVectors( :, j ) - i * EigenVectors( :, j +1 )
  !.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .

  REAL( DFP ), INTENT( IN ) :: Mat( 3, 3 )
  REAL( DFP ), INTENT( INOUT ) :: EigenValues( 3, 2 ), EigenVectors( 3, 3 )

  ! Define internal variables
  INTEGER( I4B ) :: INFO

  REAL( DFP ) :: WR( 3 ), WI( 3 ), VL( 6, 3 ), VR( 6, 3 ), WORK( 15 )


  CALL DGEEV( "N", "V", 3, Mat, 3, WR, WI, VL, 6, VR, 6, WORK, 15, INFO )

  IF( INFO .NE. 0 ) THEN

    CALL Err_Msg( &
      & "Rank2Tensor_Method@Operation.f90", &
      & "call_dgeev()", &
      & "Error in computing Eigenvalues from Lapack")

    RETURN

  END IF

  EigenValues( :, 1 ) = WR( : )
  EigenValues( :, 2 ) = WI( : )
  EigenVectors = VR( 1 : 3, 1 : 3 )

END SUBROUTINE call_dgeev

!------------------------------------------------------------------------------
!                                                             RightPolarDecomp
!------------------------------------------------------------------------------

MODULE PROCEDURE right_pd_r2t
  CALL PD( Obj % T, R % T, U % T, 1 )
END PROCEDURE right_pd_r2t

!------------------------------------------------------------------------------
!                                                             LeftPolarDecomp
!------------------------------------------------------------------------------

MODULE PROCEDURE left_pd_r2t
  CALL PD( Obj % T, R % T, V % T, 2 )
END PROCEDURE left_pd_r2t

SUBROUTINE PD( Mat, R, H, PDType )

  !.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .
  !   1. 	Ref: Higham and Noferini, 2015 Algorithm 3.1 for NSD = 3
  !   2.	PDType = 1 for F = RU; and 2 for F = VR
  !		3.	Mat = RU = VR, Therefore H denotes either U or V
  !.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .

  ! Define intent of dummy variables
  REAL( DFP ), INTENT( IN ) :: Mat( 3, 3 )
  REAL( DFP ), INTENT( INOUT ) :: R( 3, 3 ), H( 3, 3 )
  INTEGER( I4B ), INTENT( IN ) :: PDType

  ! Define internal variables
  REAL( DFP ) :: B( 4, 4 ), EigenVecs( 4, 4 ), EigenVals( 4 ), Vmax( 4 )
  INTEGER( I4B ) :: MAX_LOC( 1 )

  B( 1, 1 ) = Mat( 1, 1 ) + Mat( 2, 2 ) + Mat( 3, 3 )
  B( 1, 2 ) = Mat( 2, 3 ) - Mat( 3, 2 )
  B( 1, 3 ) = Mat( 3, 1 ) - Mat( 1, 3 )
  B( 1, 4 ) = Mat( 1, 2 ) - Mat( 2, 1 )

  B( 2, 1 ) = Mat( 1, 2 )
  B( 2, 2 ) = Mat( 1, 1 ) - Mat( 2, 2 ) -  Mat( 3, 3 )
  B( 2, 3 ) = Mat( 1, 2 ) + Mat( 2, 1 )
  B( 2, 4 ) = Mat( 1, 3 ) + Mat( 3, 1 )

  B( 3, 1 ) = B( 1, 3 )
  B( 3, 2 ) = B( 2, 3 )
  B( 3, 3 ) = - Mat( 1, 1 ) + Mat( 2, 2 ) - Mat( 3, 3 )
  B( 3, 4 ) = Mat( 2, 3 ) + Mat( 3, 2 )

  B( 4, 1 ) = B( 1, 4 )
  B( 4, 2 ) = B( 2, 4 )
  B( 4, 3 ) = B( 3, 4 )
  B( 4, 4 ) = - Mat( 1, 1 ) - Mat( 2, 2 ) + Mat( 3, 3 )

  CALL JacobiMethod( Mat = B, EigenValues  = EigenVals,  &
    & EigenVectors = EigenVecs, MaxIter = 20 )

  ! Get Dominating eigen value and corresponding eigen vectors
  MAX_LOC = MAXLOC ( ABS( EigenVals ) )
  Vmax = EigenVecs( :, MAX_LOC( 1 ) )

  ! Compute R matrix from Vmax Vector

  R( 1, 1 ) = 1.0_DFP - 2.0_DFP * ( Vmax( 3 ) * Vmax( 3 ) + Vmax( 4 ) * Vmax( 4 ) )
  R( 1, 2 ) = 2.0_DFP * ( Vmax( 2 ) * Vmax( 3 ) + Vmax( 1 ) * Vmax( 4 ) )
  R( 1, 3 ) = 2.0_DFP * ( Vmax( 2 ) * Vmax( 4 ) - Vmax( 1 ) * Vmax( 3 ) )

  R( 2, 1 ) = 2.0_DFP * ( Vmax( 2 ) * Vmax( 3 ) - Vmax( 1 ) * Vmax( 4 ) )
  R( 2, 2 ) = 1.0_DFP - 2.0_DFP * ( Vmax( 2 ) * Vmax( 2 ) + Vmax( 4 ) * Vmax( 4 ) )
  R( 2, 3 ) = 2.0_DFP * ( Vmax( 3 ) * Vmax( 4 ) + Vmax( 1 ) * Vmax( 2 ) )

  R( 3, 1 ) = 2.0_DFP * ( Vmax( 2 ) * Vmax( 4 ) + Vmax( 1 ) * Vmax( 3 ) )
  R( 3, 2 ) = 2.0_DFP * ( Vmax( 3 ) * Vmax( 4 ) - Vmax( 1 ) * Vmax( 2 ) )
  R( 3, 3 ) = 1.0_DFP - 2.0_DFP * ( Vmax( 3 ) * Vmax( 3 ) + Vmax( 2 ) * Vmax( 2 ) )

  ! Compute H matrix based upon the PDType

  SELECT CASE( PDType )
  CASE( 1 ) ! F = RU
    H = MATMUL( TRANSPOSE( R ), Mat )
  CASE( 2 ) ! F = VR
    H = MATMUL( Mat, TRANSPOSE( R ) )
  END SELECT

END SUBROUTINE PD

!------------------------------------------------------------------------------
!                                                                            +
!------------------------------------------------------------------------------

MODULE PROCEDURE Obj_add_Obj
  Ans % T = Obj1 % T + Obj2 % T
END PROCEDURE

MODULE PROCEDURE Obj_add_Mat
  Ans % T = Obj1 % T + Obj2
END PROCEDURE

MODULE PROCEDURE Mat_add_Obj
  Ans % T = Obj1 + Obj2 % T
END PROCEDURE

MODULE PROCEDURE Obj_add_Scalar
  Ans % T = Obj1 % T + Obj2
END PROCEDURE

MODULE PROCEDURE Scalar_add_Obj
  Ans % T = Obj1 + Obj2 % T
END PROCEDURE

!------------------------------------------------------------------------------
!                                                                            -
!------------------------------------------------------------------------------

MODULE PROCEDURE Obj_minus_Obj
  Ans % T = Obj1 % T - Obj2 % T
END PROCEDURE

MODULE PROCEDURE Obj_minus_Mat
  Ans % T = Obj1 % T - Obj2
END PROCEDURE

MODULE PROCEDURE Mat_minus_Obj
  Ans % T = Obj1 - Obj2 % T
END PROCEDURE

MODULE PROCEDURE Obj_minus_Scalar
  Ans % T = Obj1 % T - Obj2
END PROCEDURE

MODULE PROCEDURE Scalar_minus_Obj
  Ans % T = Obj1 - Obj2 % T
END PROCEDURE

END SUBMODULE Operation