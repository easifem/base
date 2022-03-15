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

SUBMODULE(Rank2Tensor_Method) PullBackMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE pullback_rank2
  TYPE( Rank2Tensor_ ) :: InvF

  SELECT CASE ( TRIM( indx1 ) )
  CASE( "NA" )
    SELECT CASE( TRIM( indx2 ) )
    CASE( "CONTRAVAR" )
      CALL INV( F, InvF )
      Ans = MATMUL ( T, TRANSPOSE(InvF) )
    CASE( "COVAR" )
      Ans = MATMUL ( T, F )
    CASE( "NA" )
      Ans = T
    END SELECT

  CASE( "CONTRAVAR" )
    SELECT CASE( TRIM( indx2 ) )
    CASE( "CONTRAVAR" )
      CALL INV( F, InvF )
      Ans = MATMUL( InvF, MATMUL (T, TRANSPOSE(InvF)) )
    CASE( "COVAR" )
      CALL INV( F, InvF )
      Ans = MATMUL( InvF, MATMUL ( T, F ) )
    CASE( "NA" )
      CALL INV( F, InvF )
      Ans = MATMUL( InvF, T )
    END SELECT

  CASE( "COVAR" )
    SELECT CASE( TRIM( indx2 ) )
    CASE( "CONTRAVAR" )
      CALL INV( F, InvF )
      Ans = MATMUL( TRANSPOSE(F), MATMUL(T, TRANSPOSE(InvF)) )
    CASE( "COVAR" )
      Ans = MATMUL( TRANSPOSE(F), MATMUL(T, F) )
    CASE( "NA" )
      Ans = MATMUL( TRANSPOSE(F), T )
    END SELECT
  END SELECT

END PROCEDURE pullback_rank2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE pullback_vec
  TYPE( Rank2Tensor_ ) :: InvF
  SELECT CASE ( TRIM( indx1 ) )
  CASE( "NA" )
    Ans = Vec
  CASE( "CONTRAVAR" )
    CALL INV( F, InvF )
    Ans = MATMUL( InvF, Vec )
  CASE( "COVAR" )
    Ans = MATMUL( TRANSPOSE(F), Vec )
  END SELECT
END PROCEDURE pullback_vec

END SUBMODULE PullBackMethods