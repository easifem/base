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
! date: 23 Feb 2021
! summary: [[AbstractArray_method]] module contains method for [[AbstractArray_]] data type.
!
!## Introduction
!
! [[AbstractArray_method]] module contains method for [[AbstractArray_]] data type.
!
!## Usage
!
!```fortran
!a=SIZE(obj, dims)
!a=SHAPE(obj)
!a=TotalDimension(obj)
!call setTotalDimension(obj)
!```

MODULE AbstractArray_Method
USE GlobalData, ONLY: DFP, I4B
USE BaseType, ONLY:AbstractArray_
IMPLICIT NONE
PRIVATE

!> authors: Vikas Sharma, Ph. D.
! date: 	23 Feb 2021
! summary: 	Returns the size of an array
!
INTERFACE Size
  MODULE PROCEDURE get_size
END INTERFACE Size

PUBLIC :: Size

!> authors: Vikas Sharma, Ph. D.
! date: 	23 Feb 2021
! summary: 	Returns shape of an array
!
INTERFACE Shape
  MODULE PROCEDURE get_shape
END INTERFACE Shape

PUBLIC :: Shape

!> authors: Vikas Sharma, Ph. D.
! date: 	23 Feb 2021
! summary: 	Returns total dimension of an array

INTERFACE TotalDimension
  MODULE PROCEDURE get_tdimension
END INTERFACE TotalDimension

PUBLIC :: TotalDimension

!> authors: Vikas Sharma, Ph. D.
! date: 	23 Feb 2021
! summary: 	Set total dimension of an array

INTERFACE setTotalDimension
  MODULE PROCEDURE set_tdimension
END INTERFACE setTotalDimension

PUBLIC :: setTotalDimension

CONTAINS

!----------------------------------------------------------------------------
!                                                                       size
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	23 Feb 2021
! summary: 	Returns size of an an array
!
!### Introduction
!
! `SIZE()` function returns the size of an array.

PURE FUNCTION get_size( Obj, Dims ) RESULT( Ans )
  TYPE( AbstractArray_ ), INTENT( IN ) :: Obj
  INTEGER( I4B ), INTENT( IN ), OPTIONAL :: Dims
  INTEGER( I4B ) :: Ans
  Ans = 0
END FUNCTION get_size

!----------------------------------------------------------------------------
!                                                                 shape
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	23 Feb 2021
! summary: 	Returns the shape of an array
!
!### Introduction
!
! `SHAPE()` function returns the shape of an array

PURE FUNCTION get_shape( Obj ) RESULT( Ans )
  TYPE( AbstractArray_ ), INTENT( IN ) :: Obj
  INTEGER( I4B ), ALLOCATABLE :: Ans( : )
  Ans = [0]
END FUNCTION get_shape

!----------------------------------------------------------------------------
!                                                           TotalDimension
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	23 Feb 2021
! summary: 	Returns the total dimension of an array
!
!### Introduction
!
! This function returns the total dimension (or rank) of an array,

PURE FUNCTION get_tdimension( Obj ) RESULT( Ans )
  CLASS( AbstractArray_ ), INTENT( IN ) :: Obj
  INTEGER( I4B ) :: Ans
  Ans = Obj % tDimension
END FUNCTION get_tdimension

!----------------------------------------------------------------------------
!                                                          setTotalDimension
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	23 Feb 2021
! summary: 	This subroutine set the total dimension (rank) of an array
!
!### Introduction
!
! This subroutine sets the rank(total dimension) of an array
PURE SUBROUTINE set_tdimension( Obj, tDimension )
  CLASS( AbstractArray_ ), INTENT( INOUT ) :: Obj
  INTEGER( I4B ), INTENT( IN ) :: tDimension
  Obj % tDimension = tDimension
END SUBROUTINE set_tdimension

END MODULE AbstractArray_Method