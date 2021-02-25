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
! date: 	23 Feb 2021
! summary: 	[[AbstractVector_Method]] module contains the method for data type [[AbstractVector_]] and its children.
!
!### Introduction
!
! This module contains the method for data type [[AbstractVector_]]
!
!### Usage
!
!```fortran
! a=SHAPE(obj)
! a=SIZE(obj, dims)
! a=SIZE(obj, dims)
!```
MODULE AbstractVector_Method
USE GlobalData, ONLY: DFP, I4B
USE BaseType, ONLY: AbstractVector_


PRIVATE

!> authors: Vikas Sharma, Ph. D.
! date: 	23 Feb 2021
! summary: 	This function returns the shape of [[AbstractVector_]]
!
!### Introduction
! This function returns the shape of a vector
!
!### Usage
!
!```fortran
!s=SHAPE(Obj)
!```

INTERFACE Shape
  MODULE PROCEDURE get_shape
END INTERFACE Shape

PUBLIC :: Shape

!> authors: Vikas Sharma, Ph. D.
! date: 	23 Feb 2021
! summary: 	This function returns the size of [[AbstractVector_]]
!
!### Introduction
! This function returns the size of a vector
!
!### Usage
!
!```fortran
!s=SHAPE(Obj, dims)
!```

INTERFACE Size
  MODULE PROCEDURE get_Size
END INTERFACE Size

PUBLIC :: Size

CONTAINS

!> authors: Vikas Sharma, Ph. D.
! date: 23 Feb 2021
! summary: This function returns the shape of vector
!
!### Introduction
! This function returns the shape of a vector
!
!### Usage
!
!```fortran
!s = SHAPE(Obj)
!```


PURE FUNCTION get_shape( Obj ) RESULT( Ans )
  TYPE( AbstractVector_ ), INTENT( IN ) :: Obj
  INTEGER( I4B ) :: Ans( 1 )
  Ans = 0_I4B
END FUNCTION get_shape

!<--------------------------------------------------------------------->|
!
!<--------------------------------------------------------------------->|

!> authors: Vikas Sharma, Ph. D.
! date: 23 Feb 2021
! summary: This function returns the size of a vector
!
!### Introduction
! This function returns the size of a vector
!
!### Usage
!
!```fortran
!s = SHAPE(Obj)
!```

PURE FUNCTION get_size( Obj, Dims ) RESULT( Ans )
  TYPE( AbstractVector_ ), INTENT( IN ) :: Obj
  INTEGER( I4B ), INTENT( IN ), OPTIONAL :: Dims
  INTEGER( I4B ) :: Ans
  Ans = 0
END FUNCTION get_size

END MODULE AbstractVector_Method