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

!> authors: Vikas Sharma, Ph. D.
! date: 	17 March 2021
! summary: Methods for pull back of rank2 tensor

SUBMODULE(Rank2Tensor_Method) PushForward
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE PushForward_rank2
  TYPE( Rank2Tensor_ ) :: InvF

  SELECT CASE ( TRIM( indx1 ) )
  CASE( "NA" )
    SELECT CASE( TRIM( indx2 ) )
    CASE( "CONTRAVAR" )
      Ans = MATMUL( T, TRANSPOSE(F) )
    CASE( "COVAR" )
      CALL INV( F, InvF )
      Ans = MATMUL( T, InvF )
    CASE( "NA" )
      Ans = T
    END SELECT

  CASE( "CONTRAVAR" )
    SELECT CASE( TRIM( indx2 ) )
    CASE( "CONTRAVAR" )
      Ans = MATMUL( F, MATMUL(T, TRANSPOSE(F)) )
    CASE( "COVAR" )
      CALL INV( F, InvF )
      Ans = MATMUL( F, MATMUL ( T, InvF ) )
    CASE( "NA" )
      Ans = MATMUL( F, T )
    END SELECT

  CASE( "COVAR" )
    SELECT CASE( TRIM( indx2 ) )
    CASE( "CONTRAVAR" )
      CALL INV( F, InvF )
      Ans = MATMUL( TRANSPOSE(InvF), MATMUL(T, TRANSPOSE(F)) )
    CASE( "COVAR" )
      CALL INV( F, InvF )
      Ans = MATMUL( TRANSPOSE(InvF), MATMUL(T, InvF) )
    CASE( "NA" )
      CALL INV( F, InvF )
      Ans = MATMUL( TRANSPOSE( InvF ), T )
    END SELECT
  END SELECT

END PROCEDURE PushForward_rank2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE PushForward_vec
  TYPE( Rank2Tensor_ ) :: InvF
  SELECT CASE ( TRIM( indx1 ) )
  CASE( "NA" )
    Ans = Vec
  CASE( "CONTRAVAR" )
    Ans = MATMUL( F, Vec )
  CASE( "COVAR" )
    CALL INV( F, InvF )
    Ans = MATMUL( TRANSPOSE(InvF), Vec )
  END SELECT
END PROCEDURE PushForward_vec

END SUBMODULE PushForward