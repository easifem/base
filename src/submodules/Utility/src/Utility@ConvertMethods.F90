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
! date:         22 March 2021
! summary:         This submodule contains method for swaping

SUBMODULE(Utility) ConvertMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                    Convert
!----------------------------------------------------------------------------

MODULE PROCEDURE convert_1
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
END PROCEDURE convert_1

!----------------------------------------------------------------------------
!                                                                   Convert
!----------------------------------------------------------------------------

MODULE PROCEDURE convert_2
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
END PROCEDURE convert_2

!----------------------------------------------------------------------------
!                                                                 Convert
!----------------------------------------------------------------------------

MODULE PROCEDURE convert_3
  INTEGER( I4B ) :: a, b, s(6)
  REAL(DFP), ALLOCATABLE :: m2( :, : )
  !!
  s = SHAPE(from)
  CALL Reallocate( to, s(1)*s(3), s(2)*s(4), s(5), s(6) )
  !!
  DO b = 1, s(6)
    DO a = 1, s(5)
      CALL Convert(from=from(:,:,:,:, a, b), to=m2 )
      to(:,:,a,b) = m2
    END DO
  END DO
  DEALLOCATE(m2)
END PROCEDURE convert_3

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE ConvertMethods
