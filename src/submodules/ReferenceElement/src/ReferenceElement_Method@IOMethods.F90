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

SUBMODULE(ReferenceElement_Method) IOMethods
USE BaseMethod
CONTAINS

!----------------------------------------------------------------------------
!                                                                    Display
!----------------------------------------------------------------------------

MODULE PROCEDURE reftopo_Display
  CALL Display( msg, unitno=unitno)
  CALL Display( &
    & "# ElemType : " // TRIM( ElementName( obj%Name ) ), &
    & unitno=unitno )
  CALL Display( &
    & "# XiDim : " // TRIM( INT2STR( obj%XiDimension ) ), &
    & unitno=unitno )
  CALL Display( &
    & obj%Nptrs, &
    & "# Nptrs : ", &
    & unitno=unitno )
END PROCEDURE reftopo_Display

!----------------------------------------------------------------------------
!                                                                    Display
!----------------------------------------------------------------------------

MODULE PROCEDURE refelem_Display
  ! Define internal variable
  INTEGER( I4B ) :: I, j
  !!
  CALL Display( msg, unitno=unitno)
  !!
  CALL Display( "# ElemType : "// trim( ElementName( obj%Name ) ), &
    & unitno=unitno )
  !!
  CALL Display( obj%XiDimension, "# XiDimension :: ", &
    & unitno=unitno  )
  !!
  CALL Display( obj%NSD, "# NSD : ", &
    & unitno=unitno )
  !!
  CALL Display( obj%Order, "# Order : ", &
    & unitno=unitno )
  !!
  CALL Display( obj%EntityCounts( 1 ), "# EntityCounts(0) : ", &
    & unitno=unitno )
  !!
  CALL Display( obj%EntityCounts( 2 ), "# EntityCounts(1) : ", &
    & unitno=unitno )
  !!
  CALL Display( obj%EntityCounts( 3 ), "# EntityCounts(2) : ", &
    & unitno=unitno )
  !!
  CALL Display( obj%EntityCounts( 4 ), "# EntityCounts(3) : ", &
    & unitno=unitno )
  !!
  DO j = 1, SIZE( obj%XiJ, 2 )
    CALL Display( &
      & obj%XiJ( :, j), &
      & "# Node( " // trim( str( j, .true. ) ) // " ) : ", &
      & unitno=unitno )
  END DO
  !!
  DO j = 1, SIZE( obj%Topology )
    CALL Display( &
      & obj%Topology( j ), &
      & "# Topology( " // TRIM( INT2STR( j ) ) // " ) : ", &
      & unitno=unitno )
  END DO
  !!
END PROCEDURE refelem_Display

END SUBMODULE IOMethods