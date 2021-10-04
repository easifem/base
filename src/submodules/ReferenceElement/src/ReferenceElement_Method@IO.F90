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
! date: 	1 March 2021
! summary: 	This submodule contains IO methods for [[ReferenceElement_]]

SUBMODULE(ReferenceElement_Method) IO
USE BaseMethod
CONTAINS

!----------------------------------------------------------------------------
!                                                                    Display
!----------------------------------------------------------------------------

MODULE PROCEDURE display_ref_topo
  INTEGER( I4B ) :: I
  CHARACTER( LEN = 120 ) :: fmt

  IF( PRESENT( UnitNo ) ) THEN
    I = UnitNo
  ELSE
    I = Stdout
  END IF
  IF( LEN_TRIM( Msg ) .NE. 0 ) THEN
    WRITE( I, "(A)" ) "#" // TRIM( Msg )
  END IF
  WRITE( I, "(A)" ) "ElemType : " // TRIM( ElementName( obj%Name ) )
  WRITE( I, "(A)" ) "XiDim : " // TRIM( INT2STR( obj%XiDimension ) )
  CALL Display( obj%Nptrs,  "Nptrs : ")
END PROCEDURE display_ref_topo

!----------------------------------------------------------------------------
!                                                                    Display
!----------------------------------------------------------------------------

MODULE PROCEDURE display_ref_elem
  ! Define internal variable
  INTEGER( I4B ) :: I, j
  CHARACTER( LEN = 120 ) :: fmt

  IF( PRESENT( UnitNo ) ) THEN
    I = UnitNo
  ELSE
    I = Stdout
  END IF
  IF( LEN_TRIM( Msg ) .NE. 0 ) THEN
    WRITE( I, "(A)" ) "#" // TRIM( Msg )
  END IF
  CALL Display( "ElemType : "// trim( ElementName( obj%Name ) ), I )
  CALL Display( obj%XiDimension, "XiDimension :: ", UnitNo = I )
  CALL Display( obj%NSD, "NSD : ", UnitNo = I )
  CALL Display( obj%Order, "Order : ", UnitNo = I )
  CALL Display( obj%EntityCounts( 1 ), "EntityCounts(0) : ", UnitNo = I )
  CALL Display( obj%EntityCounts( 2 ), "EntityCounts(1) : ", UnitNo = I )
  CALL Display( obj%EntityCounts( 3 ), "EntityCounts(2) : ", UnitNo = I )
  CALL Display( obj%EntityCounts( 4 ), "EntityCounts(3) : ", UnitNo = I )
  DO j = 1, SIZE( obj%XiJ, 2 )
    CALL Display( obj%XiJ( :, j), &
      & "Node( " // trim( str( j, .true. ) ) // " ) : " )
  END DO
  DO j = 1, SIZE( obj%Topology )
    CALL Display( obj%Topology( j ), &
      & "Topology( " // TRIM( INT2STR( j ) ) // " ) : ", I )
  END DO
END PROCEDURE display_ref_elem

END SUBMODULE IO