! This program is a part of EASIFEM library
! Copyright (C) 2020-2021  Vikas Sharma, Ph.D
!
! This program is free software: you can redistribute it and/or modify
! it under the terms of the GNU General Public License as published by
! the Free Software Foundation, either version 3 of the License, or
! (at your option) any later version.
!
! This program is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.
!
! You should have received a copy of the GNU General Public License
! along with this program.  If not, see <https: //www.gnu.org/licenses/>
!

SUBMODULE (RealMatrix_Method) Constructor
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                      Shape
!----------------------------------------------------------------------------

MODULE PROCEDURE get_shape
  IF( ALLOCATED( obj%Val ) ) THEN
    Ans = SHAPE( obj%Val )
  ELSE
    Ans = 0
  END IF
END PROCEDURE get_shape

!----------------------------------------------------------------------------
!                                                                       Size
!----------------------------------------------------------------------------

MODULE PROCEDURE get_size
  !Define internal variables
  INTEGER( I4B ) :: S( 2 )
  IF( ALLOCATED( obj%Val ) ) THEN
    S = SHAPE( obj%Val )
    IF( PRESENT( Dims ) ) THEN
      Ans = S( Dims )
    ELSE
      Ans = S( 1 ) * S( 2 )
    END IF
  ELSE
    Ans = 0
  END IF
END PROCEDURE get_size

!----------------------------------------------------------------------------
!                                                         getTotalDimension
!----------------------------------------------------------------------------

MODULE PROCEDURE get_tdimension
  ans = obj%tDimension
END PROCEDURE get_tdimension

!----------------------------------------------------------------------------
!                                                         setTotalDimension
!----------------------------------------------------------------------------

MODULE PROCEDURE set_tdimension
  obj%tDimension = tDimension
END PROCEDURE set_tdimension

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE allocate_data
  CALL Reallocate( obj%Val, Dims(1), Dims(2) )
  CALL setTotalDimension( obj, 2_I4B )
END PROCEDURE allocate_data

!----------------------------------------------------------------------------
!                                                             DeallocateData
!----------------------------------------------------------------------------

MODULE PROCEDURE Deallocate_Data
  IF( ALLOCATED( obj%Val ) ) DEALLOCATE( obj%Val )
  CALL setTotalDimension( obj, 0 )
END PROCEDURE Deallocate_Data

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE realmat_initiate1
  CALL AllocateData( obj, Dims )
END PROCEDURE realmat_initiate1

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE realmat_initiate2
  CALL AllocateData( obj, [nrow, ncol] )
END PROCEDURE realmat_initiate2

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE realmat_initiate3
  INTEGER( I4B ) :: j
  DO j = 1, SIZE( obj )
    CALL AllocateData( obj( j ), Dims )
  END DO
END PROCEDURE realmat_initiate3

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE realmat_initiate4
  INTEGER( I4B ) :: j
  DO j = 1, SIZE( obj )
    CALL AllocateData( obj( j ), Dims( j, : ) )
  END DO
END PROCEDURE realmat_initiate4

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE realmat_initiate5
  obj%Val = Val
  CALL setTotalDimension( obj, 2_I4B )
END PROCEDURE realmat_initiate5

!----------------------------------------------------------------------------
!                                                                     Matrix
!----------------------------------------------------------------------------

MODULE PROCEDURE Constructor1
  CALL Initiate( obj, Dims )
END PROCEDURE Constructor1

!----------------------------------------------------------------------------
!                                                                        Eye
!----------------------------------------------------------------------------

MODULE PROCEDURE realMat_eye1
  INTEGER( I4B ) :: i
  Ans = 0_I4B
  DO i = 1, m
    Ans( i, i ) = 1
  END DO
END PROCEDURE realMat_eye1

!----------------------------------------------------------------------------
!                                                                        Eye
!----------------------------------------------------------------------------

MODULE PROCEDURE realMat_eye2
  INTEGER( I4B ) :: i
  Ans = 0.0
  DO i = 1, m
    Ans( i, i ) = 1.0
  END DO
END PROCEDURE realMat_eye2

!----------------------------------------------------------------------------
!                                                                        Eye
!----------------------------------------------------------------------------

MODULE PROCEDURE realMat_eye3
  INTEGER( I4B ) :: i
  CALL Initiate( Ans, [m,m] )
  DO i = 1, m
    Ans%Val ( i, i ) = 1.0
  END DO
END PROCEDURE realMat_eye3

!----------------------------------------------------------------------------
!                                                                        Eye
!----------------------------------------------------------------------------

MODULE PROCEDURE realMat_eye4
  INTEGER( I4B ) :: i
  Ans = 0.0
  DO i = 1, m
    Ans( i, i ) = 1.0
  END DO
END PROCEDURE realMat_eye4

!----------------------------------------------------------------------------
!                                                                        Eye
!----------------------------------------------------------------------------

MODULE PROCEDURE realMat_eye5
  INTEGER( I4B ) :: i
  Ans = 0.0
  DO i = 1, m
    Ans( i, i ) = 1.0
  END DO
END PROCEDURE realMat_eye5

!----------------------------------------------------------------------------
!                                                                    Convert
!----------------------------------------------------------------------------

MODULE PROCEDURE convert_doftonodes
  INTEGER( I4B ) :: m, inode, idof, i, j
  INTEGER( I4B ), ALLOCATABLE :: T( :, : )
  !> main
  m = nns*tdof
  ALLOCATE( T( m, m ) )
  T = Eye( m, TypeInt )
  SELECT CASE( Conversion )
  CASE( DofToNodes )
    DO inode  = 1, nns
      DO idof = 1, tdof
        j = (inode - 1)* tdof + idof
        T( j, j ) = 0
        i = (idof - 1)*nns + inode
        T( i, j ) = 1
      END DO
    END DO
  CASE( NodesToDOF )
    DO idof = 1, tdof
      DO inode  = 1, nns
        j = (idof - 1)* nns + inode
        T( j, j ) = 0
        i = (inode - 1)* tdof + idof
        T( i, j ) = 1
      END DO
    END DO
  END SELECT
  to = MATMUL( TRANSPOSE( T ), MATMUL( from, T ) )
  DEALLOCATE( T )
END PROCEDURE convert_doftonodes

!----------------------------------------------------------------------------
!                                                                 Convert
!----------------------------------------------------------------------------

MODULE PROCEDURE realmat_convert_doftonodes
  CALL Convert( From=From%val, To=To%val, Conversion=Conversion, nns=nns, &
    & tdof=tdof )
END PROCEDURE realmat_convert_doftonodes

!----------------------------------------------------------------------------
!                                                                   Convert
!----------------------------------------------------------------------------

MODULE PROCEDURE convert_mat4_to_mat2
  !   Define internal variables
  INTEGER( I4B ) :: a, b, I( 4 ), r1, r2, c1, c2
  I = SHAPE( From )
  CALL Reallocate( To, I(1)*I(3), I(2)*I(4) )
  c1 = 0; c2 = 0
  DO b = 1, I(4)
    c1 = c2 + 1
    c2 = b * I(2)
    r1 = 0; r2 = 0
    DO a = 1, I(3)
      r1 = r2 + 1;
      r2 = a * I(1)
      To( r1 : r2, c1 : c2 ) = From( :, :, a, b )
    END DO
  END DO
END PROCEDURE convert_mat4_to_mat2

!----------------------------------------------------------------------------
!                                                                       Sym
!----------------------------------------------------------------------------

MODULE PROCEDURE sym_array
  Ans = 0.5_DFP * ( obj + TRANSPOSE( obj ) )
END PROCEDURE sym_array

!----------------------------------------------------------------------------
!                                                                       Sym
!----------------------------------------------------------------------------

MODULE PROCEDURE sym_obj
  Ans%Val = 0.5_DFP * ( obj%Val + TRANSPOSE( obj%Val ) )
END PROCEDURE sym_obj

!----------------------------------------------------------------------------
!                                                                    SkewSym
!----------------------------------------------------------------------------

MODULE PROCEDURE SkewSym_array
  Ans = 0.5_DFP * ( obj - TRANSPOSE( obj ) )
END PROCEDURE SkewSym_array

!----------------------------------------------------------------------------
!                                                                    SkewSym
!----------------------------------------------------------------------------

MODULE PROCEDURE SkewSym_obj
  Ans%Val = 0.5_DFP * ( obj%Val - TRANSPOSE( obj%Val ) )
END PROCEDURE SkewSym_obj

!----------------------------------------------------------------------------
!                                                         MakeDiagonalCopies
!----------------------------------------------------------------------------

MODULE PROCEDURE realmat_make_diag_copy1
  INTEGER( I4B ) :: I, S( 2 )
  REAL( DFP ), ALLOCATABLE :: DummyMat2( :, : )

  IF( ALLOCATED( Mat ) ) THEN
    S = SHAPE( Mat )
    DummyMat2 = Mat
    DEALLOCATE( Mat )
    ALLOCATE( Mat( S( 1 )*nCopy, S( 2 )*nCopy ) )
    Mat = 0.0_DFP
    DO I = 1, nCopy
        Mat( ( I - 1 ) * S( 1 ) + 1 : I * S( 1 ), &
        & ( I - 1 ) * S( 2 ) + 1 : I * S( 2 ) ) &
        & = DummyMat2( :, : )
    END DO
    DEALLOCATE( DummyMat2 )
  END IF
END PROCEDURE realmat_make_diag_copy1

!----------------------------------------------------------------------------
!                                                         MakeDiagonalCopies
!----------------------------------------------------------------------------

MODULE PROCEDURE realmat_make_diag_copy2
  INTEGER( I4B ) :: I, S( 2 )
  S = SHAPE( From )
  CALL Reallocate( To, S( 1 )*nCopy, S( 2 )*nCopy )
  To = 0.0_DFP
  DO I = 1, nCopy
      To( ( I - 1 ) * S( 1 ) + 1 : I * S( 1 ), &
      & ( I - 1 ) * S( 2 ) + 1 : I * S( 2 ) ) &
      & = From( :, : )
  END DO
END PROCEDURE realmat_make_diag_copy2

!----------------------------------------------------------------------------
!                                                         MakeDiagonalCopies
!----------------------------------------------------------------------------

MODULE PROCEDURE realmat_make_diag_copy3
  CALL realmat_make_diag_copy1( Mat = Mat%Val, nCopy = nCopy )
END PROCEDURE realmat_make_diag_copy3

!----------------------------------------------------------------------------
!                                                         MakeDiagonalCopies
!----------------------------------------------------------------------------

MODULE PROCEDURE realmat_make_diag_copy4
  CALL realmat_make_diag_copy2( From = From%Val, To = To%Val, &
    & nCopy = nCopy )
END PROCEDURE realmat_make_diag_copy4

!----------------------------------------------------------------------------
!                                                              Random_Number
!----------------------------------------------------------------------------

MODULE PROCEDURE realmat_random_number
  IF( PRESENT( m ) .AND. PRESENT( n ) ) THEN
    CALL Reallocate( obj%Val, m, n )
    CALL RANDOM_NUMBER( obj%Val )
    RETURN
  END IF

  IF( PRESENT( m ) ) THEN
    CALL Reallocate( obj%Val, m, m )
    CALL RANDOM_NUMBER( obj%Val )
    RETURN
  END IF

  IF( PRESENT( n ) ) THEN
    CALL Reallocate( obj%Val, n, n )
    CALL RANDOM_NUMBER( obj%Val )
    RETURN
  END IF

  CALL RANDOM_NUMBER( obj%Val )

END PROCEDURE realmat_random_number

!----------------------------------------------------------------------------
!                                                                 testMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE TestMatrix
  SELECT CASE( matNo )
  CASE( 1 )
    ALLOCATE( Ans( 4, 4 ) )
    Ans( :, 1 ) = [3.0, -3.0, 6.0, -9.0]
    Ans( :, 2 ) = [-7.0, 5.0, -4.0, 5.0]
    Ans( :, 3 ) = [-2.0, 1.0, 0.0, -5.0]
    Ans( :, 4 ) = [2.0, 0.0, -5.0, 12.0]
  END SELECT
END PROCEDURE TestMatrix

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------
END SUBMODULE Constructor