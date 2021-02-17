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

MODULE AbstractArray_Method
  USE GlobalData
  USE BaseType, ONLY:AbstractArray_
  IMPLICIT NONE
  PRIVATE

  INTERFACE Size
    MODULE PROCEDURE get_size
  END INTERFACE Size

  PUBLIC :: Size

  INTERFACE Shape
    MODULE PROCEDURE get_shape
  END INTERFACE Shape

  PUBLIC :: Shape

  INTERFACE TotalDimension
    MODULE PROCEDURE get_tdimension
  END INTERFACE TotalDimension

  PUBLIC :: TotalDimension

  INTERFACE setTotalDimension
    MODULE PROCEDURE set_tdimension
  END INTERFACE setTotalDimension

  PUBLIC :: setTotalDimension

  CONTAINS

  !<--------------------------------------------------------------------->|
  !
  !<--------------------------------------------------------------------->|

  PURE FUNCTION get_size( Obj, Dims ) RESULT( Ans )
    TYPE( AbstractArray_ ), INTENT( IN ) :: Obj
    INTEGER( I4B ), INTENT( IN ), OPTIONAL :: Dims
    INTEGER( I4B ) :: Ans
    Ans = 0
  END FUNCTION get_size

  !<--------------------------------------------------------------------->|
  !
  !<--------------------------------------------------------------------->|

  PURE FUNCTION get_shape( Obj ) RESULT( Ans )
    TYPE( AbstractArray_ ), INTENT( IN ) :: Obj
    INTEGER( I4B ), ALLOCATABLE :: Ans( : )
    Ans = [0]
  END FUNCTION get_shape

  !<--------------------------------------------------------------------->|
  !
  !<--------------------------------------------------------------------->|

  PURE FUNCTION get_tdimension( Obj ) RESULT( Ans )
    CLASS( AbstractArray_ ), INTENT( IN ) :: Obj
    INTEGER( I4B ) :: Ans
    Ans = Obj % tDimension
  END FUNCTION get_tdimension

  !<--------------------------------------------------------------------->|
  !
  !<--------------------------------------------------------------------->|

  PURE SUBROUTINE set_tdimension( Obj, tDimension )
    CLASS( AbstractArray_ ), INTENT( INOUT ) :: Obj
    INTEGER( I4B ), INTENT( IN ) :: tDimension
    Obj % tDimension = tDimension
  END SUBROUTINE set_tdimension

END MODULE AbstractArray_Method