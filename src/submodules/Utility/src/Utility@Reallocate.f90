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
! date: 	22 March 2021
! summary: 	Methods for reallocating arrays

SUBMODULE( Utility ) Reallocate
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                 Reallocate2
!----------------------------------------------------------------------------

MODULE PROCEDURE Reallocate_Real64_R1
  IF( ALLOCATED( Mat ) ) THEN
    IF( SIZE( Mat ) .NE. row ) THEN
      DEALLOCATE( Mat )
      ALLOCATE( Mat( row ) )
    END IF
  ELSE
    ALLOCATE( Mat( row ) )
  END IF
  Mat = 0.0_DFP
END PROCEDURE Reallocate_Real64_R1

!----------------------------------------------------------------------------
!                                                                 Reallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE Reallocate_Real64_R1b
  CALL Reallocate_Real64_R1( mat, s(1) )
END PROCEDURE Reallocate_Real64_R1b

!----------------------------------------------------------------------------
!                                                                 Reallocate2
!----------------------------------------------------------------------------

MODULE PROCEDURE Reallocate_Real32_R1
  IF( ALLOCATED( Mat ) ) THEN
    IF( SIZE( Mat ) .NE. row ) THEN
      DEALLOCATE( Mat )
      ALLOCATE( Mat( row ) )
    END IF
  ELSE
    ALLOCATE( Mat( row ) )
  END IF
  Mat = 0.0
END PROCEDURE Reallocate_Real32_R1

!----------------------------------------------------------------------------
!                                                                 Reallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE Reallocate_Real32_R1b
  CALL Reallocate_Real32_R1( mat, s(1) )
END PROCEDURE Reallocate_Real32_R1b

!-----------------------------------------------------------------------------
!                                                                 Reallocate1
!-----------------------------------------------------------------------------

MODULE PROCEDURE Reallocate_Real64_R2
  IF( ALLOCATED( Mat ) ) THEN
    IF( (SIZE( Mat, 1 ) .NE. row) .OR. (SIZE( Mat, 2 ) .NE. col) ) THEN
      DEALLOCATE( Mat )
      ALLOCATE( Mat( row, col ) )
    END IF
  ELSE
    ALLOCATE( Mat( row, col ) )
  END IF
  Mat = 0.0_DFP
END PROCEDURE Reallocate_Real64_R2

!----------------------------------------------------------------------------
!                                                                 Reallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE Reallocate_Real64_R2b
  CALL Reallocate_Real64_R2( mat, s(1), s(2) )
END PROCEDURE Reallocate_Real64_R2b

!-----------------------------------------------------------------------------
!                                                                 Reallocate1
!-----------------------------------------------------------------------------

MODULE PROCEDURE Reallocate_Real32_R2
  IF( ALLOCATED( Mat ) ) THEN
    IF( (SIZE( Mat, 1 ) .NE. row) .OR. (SIZE( Mat, 2 ) .NE. col) ) THEN
      DEALLOCATE( Mat )
      ALLOCATE( Mat( row, col ) )
    END IF
  ELSE
    ALLOCATE( Mat( row, col ) )
  END IF
  Mat = 0.0
END PROCEDURE Reallocate_Real32_R2

!----------------------------------------------------------------------------
!                                                                 Reallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE Reallocate_Real32_R2b
  CALL Reallocate_Real32_R2( mat, s(1), s(2) )
END PROCEDURE Reallocate_Real32_R2b

!---------------------------------------------------------------------------
!                                                                 Reallocate
!---------------------------------------------------------------------------

MODULE PROCEDURE Reallocate_Real64_R3
  IF( ALLOCATED( Mat ) ) THEN
    IF( (SIZE( Mat, 1 ) .NE. i1) &
      & .OR. (SIZE( Mat, 2 ) .NE. i2) &
      & .OR. (SIZE( Mat, 3 ) .NE. i3) ) THEN
      DEALLOCATE( Mat )
      ALLOCATE( Mat( i1, i2, i3 ) )
    END IF
  ELSE
    ALLOCATE( Mat( i1, i2, i3 ) )
  END IF
  Mat = 0.0_DFP
END PROCEDURE Reallocate_Real64_R3

!----------------------------------------------------------------------------
!                                                                 Reallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE Reallocate_Real64_R3b
  CALL Reallocate_Real64_R3( mat, s(1), s(2), s(3) )
END PROCEDURE Reallocate_Real64_R3b

!---------------------------------------------------------------------------
!                                                                 Reallocate
!---------------------------------------------------------------------------

MODULE PROCEDURE Reallocate_Real32_R3
  IF( ALLOCATED( Mat ) ) THEN
    IF( (SIZE( Mat, 1 ) .NE. i1) &
      & .OR. (SIZE( Mat, 2 ) .NE. i2) &
      & .OR. (SIZE( Mat, 3 ) .NE. i3) ) THEN
      DEALLOCATE( Mat )
      ALLOCATE( Mat( i1, i2, i3 ) )
    END IF
  ELSE
    ALLOCATE( Mat( i1, i2, i3 ) )
  END IF
  Mat = 0.0
END PROCEDURE Reallocate_Real32_R3

!----------------------------------------------------------------------------
!                                                                 Reallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE Reallocate_Real32_R3b
  CALL Reallocate_Real32_R3( mat, s(1), s(2), s(3) )
END PROCEDURE Reallocate_Real32_R3b

!----------------------------------------------------------------------------
!                                                                 Reallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE Reallocate_Real64_R4
  IF( ALLOCATED( Mat ) ) THEN
    IF( (SIZE( Mat, 1 ) .NE. i1) &
      & .OR. (SIZE( Mat, 2 ) .NE. i2) &
      & .OR. (SIZE( Mat, 3 ) .NE. i3) &
      & .OR. (SIZE( Mat, 4 ) .NE. i4) &
      & ) THEN
      DEALLOCATE( Mat )
      ALLOCATE( Mat( i1, i2, i3, i4 ) )
    END IF
  ELSE
    ALLOCATE( Mat( i1, i2, i3, i4 ) )
  END IF
  Mat = 0.0
END PROCEDURE Reallocate_Real64_R4

!----------------------------------------------------------------------------
!                                                                 Reallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE Reallocate_Real64_R4b
  CALL Reallocate_Real64_R4( mat, s(1), s(2), s(3), s(4) )
END PROCEDURE Reallocate_Real64_R4b

!----------------------------------------------------------------------------
!                                                                 Reallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE Reallocate_Real32_R4
  IF( ALLOCATED( Mat ) ) THEN
    IF( (SIZE( Mat, 1 ) .NE. i1) &
      & .OR. (SIZE( Mat, 2 ) .NE. i2) &
      & .OR. (SIZE( Mat, 3 ) .NE. i3) &
      & .OR. (SIZE( Mat, 4 ) .NE. i4) &
      & ) THEN
      DEALLOCATE( Mat )
      ALLOCATE( Mat( i1, i2, i3, i4 ) )
    END IF
  ELSE
    ALLOCATE( Mat( i1, i2, i3, i4 ) )
  END IF
  Mat = 0.0
END PROCEDURE Reallocate_Real32_R4

!----------------------------------------------------------------------------
!                                                                 Reallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE Reallocate_Real32_R4b
  CALL Reallocate_Real32_R4( mat, s(1), s(2), s(3), s(4) )
END PROCEDURE Reallocate_Real32_R4b

!----------------------------------------------------------------------------
!                                                                 Reallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE Reallocate_Real64_R5
  IF( ALLOCATED( Mat ) ) THEN
    IF( ANY(SHAPE(Mat) .NE. [i1,i2,i3,i4,i5]) ) THEN
      DEALLOCATE( Mat )
      ALLOCATE( Mat( i1, i2, i3, i4, i5 ) )
    END IF
  ELSE
    ALLOCATE( Mat( i1, i2, i3, i4, i5 ) )
  END IF
  Mat = 0.0
END PROCEDURE Reallocate_Real64_R5

!----------------------------------------------------------------------------
!                                                                 Reallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE Reallocate_Real64_R5b
  CALL Reallocate_Real64_R5( mat, s(1), s(2), s(3), s(4), s(5) )
END PROCEDURE Reallocate_Real64_R5b

!----------------------------------------------------------------------------
!                                                                 Reallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE Reallocate_Real32_R5
  IF( ALLOCATED( Mat ) ) THEN
    IF( ANY(SHAPE(Mat) .NE. [i1,i2,i3,i4,i5]) ) THEN
      DEALLOCATE( Mat )
      ALLOCATE( Mat( i1, i2, i3, i4, i5 ) )
    END IF
  ELSE
    ALLOCATE( Mat( i1, i2, i3, i4, i5 ) )
  END IF
  Mat = 0.0
END PROCEDURE Reallocate_Real32_R5

!----------------------------------------------------------------------------
!                                                                 Reallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE Reallocate_Real32_R5b
  CALL Reallocate_Real32_R5( mat, s(1), s(2), s(3), s(4), s(5) )
END PROCEDURE Reallocate_Real32_R5b

!----------------------------------------------------------------------------
!                                                                 Reallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE Reallocate_Real64_R6
  IF( ALLOCATED( Mat ) ) THEN
    IF( ANY(SHAPE(Mat) .NE. [i1,i2,i3,i4,i5,i6]) ) THEN
      DEALLOCATE( Mat )
      ALLOCATE( Mat( i1, i2, i3, i4, i5, i6 ) )
    END IF
  ELSE
    ALLOCATE( Mat( i1, i2, i3, i4, i5, i6 ) )
  END IF
  Mat = 0.0
END PROCEDURE Reallocate_Real64_R6

!----------------------------------------------------------------------------
!                                                                 Reallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE Reallocate_Real64_R6b
  CALL Reallocate_Real64_R6( mat, s(1), s(2), s(3), s(4), s(5), s(6) )
END PROCEDURE Reallocate_Real64_R6b

!----------------------------------------------------------------------------
!                                                                 Reallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE Reallocate_Real32_R6
  IF( ALLOCATED( Mat ) ) THEN
    IF( ANY(SHAPE(Mat) .NE. [i1,i2,i3,i4,i5,i6]) ) THEN
      DEALLOCATE( Mat )
      ALLOCATE( Mat( i1, i2, i3, i4, i5, i6 ) )
    END IF
  ELSE
    ALLOCATE( Mat( i1, i2, i3, i4, i5, i6 ) )
  END IF
  Mat = 0.0
END PROCEDURE Reallocate_Real32_R6

!----------------------------------------------------------------------------
!                                                                 Reallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE Reallocate_Real32_R6b
  CALL Reallocate_Real32_R6( mat, s(1), s(2), s(3), s(4), s(5), s(6) )
END PROCEDURE Reallocate_Real32_R6b

!----------------------------------------------------------------------------
!                                                                 Reallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE Reallocate_Real64_R7
  IF( ALLOCATED( Mat ) ) THEN
    IF( ANY(SHAPE(Mat) .NE. [i1,i2,i3,i4,i5,i6,i7]) ) THEN
      DEALLOCATE( Mat )
      ALLOCATE( Mat( i1, i2, i3, i4, i5, i6, i7 ) )
    END IF
  ELSE
    ALLOCATE( Mat( i1, i2, i3, i4, i5, i6, i7 ) )
  END IF
  Mat = 0.0
END PROCEDURE Reallocate_Real64_R7

!----------------------------------------------------------------------------
!                                                                 Reallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE Reallocate_Real64_R7b
  CALL Reallocate_Real64_R7( mat, s(1), s(2), s(3), s(4), s(5), s(6), s(7) )
END PROCEDURE Reallocate_Real64_R7b

!----------------------------------------------------------------------------
!                                                                 Reallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE Reallocate_Real32_R7
  IF( ALLOCATED( Mat ) ) THEN
    IF( ANY(SHAPE(Mat) .NE. [i1,i2,i3,i4,i5,i6,i7]) ) THEN
      DEALLOCATE( Mat )
      ALLOCATE( Mat( i1, i2, i3, i4, i5, i6, i7 ) )
    END IF
  ELSE
    ALLOCATE( Mat( i1, i2, i3, i4, i5, i6, i7 ) )
  END IF
  Mat = 0.0
END PROCEDURE Reallocate_Real32_R7

!----------------------------------------------------------------------------
!                                                                 Reallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE Reallocate_Real32_R7b
  CALL Reallocate_Real32_R7( mat, s(1), s(2), s(3), s(4), s(5), s(6), s(7) )
END PROCEDURE Reallocate_Real32_R7b


!----------------------------------------------------------------------------
!                                                                 Reallocate2
!----------------------------------------------------------------------------

MODULE PROCEDURE Reallocate_Int64_R1
  IF( ALLOCATED( Mat ) ) THEN
    IF( SIZE( Mat ) .NE. row ) THEN
      DEALLOCATE( Mat )
      ALLOCATE( Mat( row ) )
    END IF
  ELSE
    ALLOCATE( Mat( row ) )
  END IF
  Mat = 0
END PROCEDURE Reallocate_Int64_R1

!----------------------------------------------------------------------------
!                                                                 Reallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE Reallocate_Int64_R1b
  CALL Reallocate_Int64_R1( mat, s(1) )
END PROCEDURE Reallocate_Int64_R1b

!----------------------------------------------------------------------------
!                                                                 Reallocate2
!----------------------------------------------------------------------------

MODULE PROCEDURE Reallocate_Int32_R1
  IF( ALLOCATED( Mat ) ) THEN
    IF( SIZE( Mat ) .NE. row ) THEN
      DEALLOCATE( Mat )
      ALLOCATE( Mat( row ) )
    END IF
  ELSE
    ALLOCATE( Mat( row ) )
  END IF
  Mat = 0
END PROCEDURE Reallocate_Int32_R1

!----------------------------------------------------------------------------
!                                                                 Reallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE Reallocate_Int32_R1b
  CALL Reallocate_Int32_R1( mat, s(1) )
END PROCEDURE Reallocate_Int32_R1b

!-----------------------------------------------------------------------------
!                                                                 Reallocate1
!-----------------------------------------------------------------------------

MODULE PROCEDURE Reallocate_Int64_R2
  IF( ALLOCATED( Mat ) ) THEN
    IF( (SIZE( Mat, 1 ) .NE. row) .OR. (SIZE( Mat, 2 ) .NE. col) ) THEN
      DEALLOCATE( Mat )
      ALLOCATE( Mat( row, col ) )
    END IF
  ELSE
    ALLOCATE( Mat( row, col ) )
  END IF
  Mat = 0_DFP
END PROCEDURE Reallocate_Int64_R2

!----------------------------------------------------------------------------
!                                                                 Reallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE Reallocate_Int64_R2b
  CALL Reallocate_Int64_R2( mat, s(1), s(2) )
END PROCEDURE Reallocate_Int64_R2b

!-----------------------------------------------------------------------------
!                                                                 Reallocate1
!-----------------------------------------------------------------------------

MODULE PROCEDURE Reallocate_Int32_R2
  IF( ALLOCATED( Mat ) ) THEN
    IF( (SIZE( Mat, 1 ) .NE. row) .OR. (SIZE( Mat, 2 ) .NE. col) ) THEN
      DEALLOCATE( Mat )
      ALLOCATE( Mat( row, col ) )
    END IF
  ELSE
    ALLOCATE( Mat( row, col ) )
  END IF
  Mat = 0
END PROCEDURE Reallocate_Int32_R2

!----------------------------------------------------------------------------
!                                                                 Reallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE Reallocate_Int32_R2b
  CALL Reallocate_Int32_R2( mat, s(1), s(2) )
END PROCEDURE Reallocate_Int32_R2b

!---------------------------------------------------------------------------
!                                                                 Reallocate
!---------------------------------------------------------------------------

MODULE PROCEDURE Reallocate_Int64_R3
  IF( ALLOCATED( Mat ) ) THEN
    IF( (SIZE( Mat, 1 ) .NE. i1) &
      & .OR. (SIZE( Mat, 2 ) .NE. i2) &
      & .OR. (SIZE( Mat, 3 ) .NE. i3) ) THEN
      DEALLOCATE( Mat )
      ALLOCATE( Mat( i1, i2, i3 ) )
    END IF
  ELSE
    ALLOCATE( Mat( i1, i2, i3 ) )
  END IF
  Mat = 0_DFP
END PROCEDURE Reallocate_Int64_R3

!----------------------------------------------------------------------------
!                                                                 Reallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE Reallocate_Int64_R3b
  CALL Reallocate_Int64_R3( mat, s(1), s(2), s(3) )
END PROCEDURE Reallocate_Int64_R3b

!---------------------------------------------------------------------------
!                                                                 Reallocate
!---------------------------------------------------------------------------

MODULE PROCEDURE Reallocate_Int32_R3
  IF( ALLOCATED( Mat ) ) THEN
    IF( (SIZE( Mat, 1 ) .NE. i1) &
      & .OR. (SIZE( Mat, 2 ) .NE. i2) &
      & .OR. (SIZE( Mat, 3 ) .NE. i3) ) THEN
      DEALLOCATE( Mat )
      ALLOCATE( Mat( i1, i2, i3 ) )
    END IF
  ELSE
    ALLOCATE( Mat( i1, i2, i3 ) )
  END IF
  Mat = 0
END PROCEDURE Reallocate_Int32_R3

!----------------------------------------------------------------------------
!                                                                 Reallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE Reallocate_Int32_R3b
  CALL Reallocate_Int32_R3( mat, s(1), s(2), s(3) )
END PROCEDURE Reallocate_Int32_R3b

!----------------------------------------------------------------------------
!                                                                 Reallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE Reallocate_Int64_R4
  IF( ALLOCATED( Mat ) ) THEN
    IF( (SIZE( Mat, 1 ) .NE. i1) &
      & .OR. (SIZE( Mat, 2 ) .NE. i2) &
      & .OR. (SIZE( Mat, 3 ) .NE. i3) &
      & .OR. (SIZE( Mat, 4 ) .NE. i4) &
      & ) THEN
      DEALLOCATE( Mat )
      ALLOCATE( Mat( i1, i2, i3, i4 ) )
    END IF
  ELSE
    ALLOCATE( Mat( i1, i2, i3, i4 ) )
  END IF
  Mat = 0
END PROCEDURE Reallocate_Int64_R4

!----------------------------------------------------------------------------
!                                                                 Reallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE Reallocate_Int64_R4b
  CALL Reallocate_Int64_R4( mat, s(1), s(2), s(3), s(4) )
END PROCEDURE Reallocate_Int64_R4b

!----------------------------------------------------------------------------
!                                                                 Reallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE Reallocate_Int32_R4
  IF( ALLOCATED( Mat ) ) THEN
    IF( (SIZE( Mat, 1 ) .NE. i1) &
      & .OR. (SIZE( Mat, 2 ) .NE. i2) &
      & .OR. (SIZE( Mat, 3 ) .NE. i3) &
      & .OR. (SIZE( Mat, 4 ) .NE. i4) &
      & ) THEN
      DEALLOCATE( Mat )
      ALLOCATE( Mat( i1, i2, i3, i4 ) )
    END IF
  ELSE
    ALLOCATE( Mat( i1, i2, i3, i4 ) )
  END IF
  Mat = 0
END PROCEDURE Reallocate_Int32_R4

!----------------------------------------------------------------------------
!                                                                 Reallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE Reallocate_Int32_R4b
  CALL Reallocate_Int32_R4( mat, s(1), s(2), s(3), s(4) )
END PROCEDURE Reallocate_Int32_R4b

!----------------------------------------------------------------------------
!                                                                 Reallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE Reallocate_Int64_R5
  IF( ALLOCATED( Mat ) ) THEN
    IF( ANY(SHAPE(Mat) .NE. [i1,i2,i3,i4,i5]) ) THEN
      DEALLOCATE( Mat )
      ALLOCATE( Mat( i1, i2, i3, i4, i5 ) )
    END IF
  ELSE
    ALLOCATE( Mat( i1, i2, i3, i4, i5 ) )
  END IF
  Mat = 0
END PROCEDURE Reallocate_Int64_R5

!----------------------------------------------------------------------------
!                                                                 Reallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE Reallocate_Int64_R5b
  CALL Reallocate_Int64_R5( mat, s(1), s(2), s(3), s(4), s(5) )
END PROCEDURE Reallocate_Int64_R5b

!----------------------------------------------------------------------------
!                                                                 Reallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE Reallocate_Int32_R5
  IF( ALLOCATED( Mat ) ) THEN
    IF( ANY(SHAPE(Mat) .NE. [i1,i2,i3,i4,i5]) ) THEN
      DEALLOCATE( Mat )
      ALLOCATE( Mat( i1, i2, i3, i4, i5 ) )
    END IF
  ELSE
    ALLOCATE( Mat( i1, i2, i3, i4, i5 ) )
  END IF
  Mat = 0
END PROCEDURE Reallocate_Int32_R5

!----------------------------------------------------------------------------
!                                                                 Reallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE Reallocate_Int32_R5b
  CALL Reallocate_Int32_R5( mat, s(1), s(2), s(3), s(4), s(5) )
END PROCEDURE Reallocate_Int32_R5b

!----------------------------------------------------------------------------
!                                                                 Reallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE Reallocate_Int64_R6
  IF( ALLOCATED( Mat ) ) THEN
    IF( ANY(SHAPE(Mat) .NE. [i1,i2,i3,i4,i5,i6]) ) THEN
      DEALLOCATE( Mat )
      ALLOCATE( Mat( i1, i2, i3, i4, i5, i6 ) )
    END IF
  ELSE
    ALLOCATE( Mat( i1, i2, i3, i4, i5, i6 ) )
  END IF
  Mat = 0
END PROCEDURE Reallocate_Int64_R6

!----------------------------------------------------------------------------
!                                                                 Reallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE Reallocate_Int64_R6b
  CALL Reallocate_Int64_R6( mat, s(1), s(2), s(3), s(4), s(5), s(6) )
END PROCEDURE Reallocate_Int64_R6b

!----------------------------------------------------------------------------
!                                                                 Reallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE Reallocate_Int32_R6
  IF( ALLOCATED( Mat ) ) THEN
    IF( ANY(SHAPE(Mat) .NE. [i1,i2,i3,i4,i5,i6]) ) THEN
      DEALLOCATE( Mat )
      ALLOCATE( Mat( i1, i2, i3, i4, i5, i6 ) )
    END IF
  ELSE
    ALLOCATE( Mat( i1, i2, i3, i4, i5, i6 ) )
  END IF
  Mat = 0
END PROCEDURE Reallocate_Int32_R6

!----------------------------------------------------------------------------
!                                                                 Reallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE Reallocate_Int32_R6b
  CALL Reallocate_Int32_R6( mat, s(1), s(2), s(3), s(4), s(5), s(6) )
END PROCEDURE Reallocate_Int32_R6b

!----------------------------------------------------------------------------
!                                                                 Reallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE Reallocate_Int64_R7
  IF( ALLOCATED( Mat ) ) THEN
    IF( ANY(SHAPE(Mat) .NE. [i1,i2,i3,i4,i5,i6,i7]) ) THEN
      DEALLOCATE( Mat )
      ALLOCATE( Mat( i1, i2, i3, i4, i5, i6, i7 ) )
    END IF
  ELSE
    ALLOCATE( Mat( i1, i2, i3, i4, i5, i6, i7 ) )
  END IF
  Mat = 0
END PROCEDURE Reallocate_Int64_R7

!----------------------------------------------------------------------------
!                                                                 Reallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE Reallocate_Int64_R7b
  CALL Reallocate_Int64_R7( mat, s(1), s(2), s(3), s(4), s(5), s(6), s(7) )
END PROCEDURE Reallocate_Int64_R7b

!----------------------------------------------------------------------------
!                                                                 Reallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE Reallocate_Int32_R7
  IF( ALLOCATED( Mat ) ) THEN
    IF( ANY(SHAPE(Mat) .NE. [i1,i2,i3,i4,i5,i6,i7]) ) THEN
      DEALLOCATE( Mat )
      ALLOCATE( Mat( i1, i2, i3, i4, i5, i6, i7 ) )
    END IF
  ELSE
    ALLOCATE( Mat( i1, i2, i3, i4, i5, i6, i7 ) )
  END IF
  Mat = 0
END PROCEDURE Reallocate_Int32_R7

!----------------------------------------------------------------------------
!                                                                 Reallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE Reallocate_Int32_R7b
  CALL Reallocate_Int32_R7( mat, s(1), s(2), s(3), s(4), s(5), s(6), s(7) )
END PROCEDURE Reallocate_Int32_R7b

!----------------------------------------------------------------------------
!                                                                 Reallocate6
!----------------------------------------------------------------------------

MODULE PROCEDURE Reallocate_Int32_R1_6
  IF( ALLOCATED( Vec1 ) ) THEN
    IF( SIZE( Vec1 ) .NE. n1 ) THEN
      DEALLOCATE( Vec1 )
      ALLOCATE( Vec1( n1 ) )
    END IF
  ELSE
    ALLOCATE( Vec1( n1 ) )
  END IF
  Vec1 = 0

  IF( ALLOCATED( Vec2 ) ) THEN
    IF( SIZE( Vec2 ) .NE. n2 ) THEN
      DEALLOCATE( Vec2 )
      ALLOCATE( Vec2( n2 ) )
    END IF
  ELSE
    ALLOCATE( Vec2( n2 ) )
  END IF
  Vec2 = 0

  IF( PRESENT( Vec3 ) ) THEN
    IF( ALLOCATED( Vec3 ) ) THEN
      IF( SIZE( Vec3 ) .NE. n3 ) THEN
        DEALLOCATE( Vec3 )
        ALLOCATE( Vec3( n3 ) )
      END IF
    ELSE
      ALLOCATE( Vec3( n3 ) )
    END IF
    Vec3 = 0
  END IF

  IF( PRESENT( Vec4 ) ) THEN
    IF( ALLOCATED( Vec4 ) ) THEN
      IF( SIZE( Vec4 ) .NE. n4 ) THEN
        DEALLOCATE( Vec4 )
        ALLOCATE( Vec4( n4 ) )
      END IF
    ELSE
      ALLOCATE( Vec4( n4 ) )
    END IF
    Vec4 = 0
  END IF

  IF( PRESENT( Vec5 ) ) THEN
    IF( ALLOCATED( Vec5 ) ) THEN
      IF( SIZE( Vec5 ) .NE. n5 ) THEN
        DEALLOCATE( Vec5 )
        ALLOCATE( Vec5( n5 ) )
      END IF
    ELSE
      ALLOCATE( Vec5( n5 ) )
    END IF
    Vec5 = 0
  END IF

  IF( PRESENT( Vec6 ) ) THEN
    IF( ALLOCATED( Vec6 ) ) THEN
      IF( SIZE( Vec6 ) .NE. n6 ) THEN
        DEALLOCATE( Vec6 )
        ALLOCATE( Vec6( n6 ) )
      END IF
    ELSE
      ALLOCATE( Vec6( n6 ) )
    END IF
    Vec6 = 0
  END IF

END PROCEDURE Reallocate_Int32_R1_6

!----------------------------------------------------------------------------
!                                                                 Reallocate7
!----------------------------------------------------------------------------

MODULE PROCEDURE Reallocate_Real64_R1_6
  IF( ALLOCATED( Vec1 ) ) THEN
    IF( SIZE( Vec1 ) .NE. n1 ) THEN
      DEALLOCATE( Vec1 )
      ALLOCATE( Vec1( n1 ) )
    END IF
  ELSE
    ALLOCATE( Vec1( n1 ) )
  END IF
  Vec1 = 0.0

  IF( ALLOCATED( Vec2 ) ) THEN
    IF( SIZE( Vec2 ) .NE. n2 ) THEN
      DEALLOCATE( Vec2 )
      ALLOCATE( Vec2( n2 ) )
    END IF
  ELSE
    ALLOCATE( Vec2( n2 ) )
  END IF
  Vec2 = 0.0

  IF( PRESENT( Vec3 ) ) THEN
    IF( ALLOCATED( Vec3 ) ) THEN
      IF( SIZE( Vec3 ) .NE. n3 ) THEN
        DEALLOCATE( Vec3 )
        ALLOCATE( Vec3( n3 ) )
      END IF
    ELSE
      ALLOCATE( Vec3( n3 ) )
    END IF
    Vec3 = 0.0
  END IF

  IF( PRESENT( Vec4 ) ) THEN
    IF( ALLOCATED( Vec4 ) ) THEN
      IF( SIZE( Vec4 ) .NE. n4 ) THEN
        DEALLOCATE( Vec4 )
        ALLOCATE( Vec4( n4 ) )
      END IF
    ELSE
      ALLOCATE( Vec4( n4 ) )
    END IF
    Vec4 = 0.0
  END IF

  IF( PRESENT( Vec5 ) ) THEN
    IF( ALLOCATED( Vec5 ) ) THEN
      IF( SIZE( Vec5 ) .NE. n5 ) THEN
        DEALLOCATE( Vec5 )
        ALLOCATE( Vec5( n5 ) )
      END IF
    ELSE
      ALLOCATE( Vec5( n5 ) )
    END IF
    Vec5 = 0.0
  END IF

  IF( PRESENT( Vec6 ) ) THEN
    IF( ALLOCATED( Vec6 ) ) THEN
      IF( SIZE( Vec6 ) .NE. n6 ) THEN
        DEALLOCATE( Vec6 )
        ALLOCATE( Vec6( n6 ) )
      END IF
    ELSE
      ALLOCATE( Vec6( n6 ) )
    END IF
    Vec6 = 0.0
  END IF
END PROCEDURE Reallocate_Real64_R1_6

!----------------------------------------------------------------------------
!                                                                 Reallocate7
!----------------------------------------------------------------------------

MODULE PROCEDURE Reallocate_Real32_R1_6
  IF( ALLOCATED( Vec1 ) ) THEN
    IF( SIZE( Vec1 ) .NE. n1 ) THEN
      DEALLOCATE( Vec1 )
      ALLOCATE( Vec1( n1 ) )
    END IF
  ELSE
    ALLOCATE( Vec1( n1 ) )
  END IF
  Vec1 = 0.0

  IF( ALLOCATED( Vec2 ) ) THEN
    IF( SIZE( Vec2 ) .NE. n2 ) THEN
      DEALLOCATE( Vec2 )
      ALLOCATE( Vec2( n2 ) )
    END IF
  ELSE
    ALLOCATE( Vec2( n2 ) )
  END IF
  Vec2 = 0.0

  IF( PRESENT( Vec3 ) ) THEN
    IF( ALLOCATED( Vec3 ) ) THEN
      IF( SIZE( Vec3 ) .NE. n3 ) THEN
        DEALLOCATE( Vec3 )
        ALLOCATE( Vec3( n3 ) )
      END IF
    ELSE
      ALLOCATE( Vec3( n3 ) )
    END IF
    Vec3 = 0.0
  END IF

  IF( PRESENT( Vec4 ) ) THEN
    IF( ALLOCATED( Vec4 ) ) THEN
      IF( SIZE( Vec4 ) .NE. n4 ) THEN
        DEALLOCATE( Vec4 )
        ALLOCATE( Vec4( n4 ) )
      END IF
    ELSE
      ALLOCATE( Vec4( n4 ) )
    END IF
    Vec4 = 0.0
  END IF

  IF( PRESENT( Vec5 ) ) THEN
    IF( ALLOCATED( Vec5 ) ) THEN
      IF( SIZE( Vec5 ) .NE. n5 ) THEN
        DEALLOCATE( Vec5 )
        ALLOCATE( Vec5( n5 ) )
      END IF
    ELSE
      ALLOCATE( Vec5( n5 ) )
    END IF
    Vec5 = 0.0
  END IF

  IF( PRESENT( Vec6 ) ) THEN
    IF( ALLOCATED( Vec6 ) ) THEN
      IF( SIZE( Vec6 ) .NE. n6 ) THEN
        DEALLOCATE( Vec6 )
        ALLOCATE( Vec6( n6 ) )
      END IF
    ELSE
      ALLOCATE( Vec6( n6 ) )
    END IF
    Vec6 = 0.0
  END IF
END PROCEDURE Reallocate_Real32_R1_6

!----------------------------------------------------------------------------
!                                                                Reallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE Reallocate_Real64_AIJ
  IF( ALLOCATED( A ) ) THEN
    IF( SIZE( A ) .NE. nA ) THEN
      DEALLOCATE( A )
      ALLOCATE( A( nA ) )
    END IF
  ELSE
    ALLOCATE( A( nA ) )
  END IF
  A = 0.0

  IF( ALLOCATED( IA ) ) THEN
    IF( SIZE( IA ) .NE. nIA ) THEN
      DEALLOCATE( IA )
      ALLOCATE( IA( nIA ) )
    END IF
  ELSE
    ALLOCATE( IA( nIA ) )
  END IF
  IA = 0

  IF( ALLOCATED( JA ) ) THEN
    IF( SIZE( JA ) .NE. nJA ) THEN
      DEALLOCATE( JA )
      ALLOCATE( JA( nJA ) )
    END IF
  ELSE
    ALLOCATE( JA( nJA ) )
  END IF
  JA = 0
END PROCEDURE Reallocate_Real64_AIJ

!----------------------------------------------------------------------------
!                                                                Reallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE Reallocate_Real32_AIJ
  IF( ALLOCATED( A ) ) THEN
    IF( SIZE( A ) .NE. nA ) THEN
      DEALLOCATE( A )
      ALLOCATE( A( nA ) )
    END IF
  ELSE
    ALLOCATE( A( nA ) )
  END IF
  A = 0.0

  IF( ALLOCATED( IA ) ) THEN
    IF( SIZE( IA ) .NE. nIA ) THEN
      DEALLOCATE( IA )
      ALLOCATE( IA( nIA ) )
    END IF
  ELSE
    ALLOCATE( IA( nIA ) )
  END IF
  IA = 0

  IF( ALLOCATED( JA ) ) THEN
    IF( SIZE( JA ) .NE. nJA ) THEN
      DEALLOCATE( JA )
      ALLOCATE( JA( nJA ) )
    END IF
  ELSE
    ALLOCATE( JA( nJA ) )
  END IF
  JA = 0
END PROCEDURE Reallocate_Real32_AIJ

!----------------------------------------------------------------------------
!                                                                Reallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE Reallocate_Real64_AI
  IF( ALLOCATED( A ) ) THEN
    IF( SIZE( A ) .NE. nA ) THEN
      DEALLOCATE( A )
      ALLOCATE( A( nA ) )
    END IF
  ELSE
    ALLOCATE( A( nA ) )
  END IF
  A = 0.0

  IF( ALLOCATED( IA ) ) THEN
    IF( SIZE( IA ) .NE. nIA ) THEN
      DEALLOCATE( IA )
      ALLOCATE( IA( nIA ) )
    END IF
  ELSE
    ALLOCATE( IA( nIA ) )
  END IF
  IA = 0
END PROCEDURE Reallocate_Real64_AI

!----------------------------------------------------------------------------
!                                                                Reallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE Reallocate_Real32_AI
  IF( ALLOCATED( A ) ) THEN
    IF( SIZE( A ) .NE. nA ) THEN
      DEALLOCATE( A )
      ALLOCATE( A( nA ) )
    END IF
  ELSE
    ALLOCATE( A( nA ) )
  END IF
  A = 0.0

  IF( ALLOCATED( IA ) ) THEN
    IF( SIZE( IA ) .NE. nIA ) THEN
      DEALLOCATE( IA )
      ALLOCATE( IA( nIA ) )
    END IF
  ELSE
    ALLOCATE( IA( nIA ) )
  END IF
  IA = 0
END PROCEDURE Reallocate_Real32_AI

END SUBMODULE Reallocate