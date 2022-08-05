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

PUBLIC :: GetPointer
PUBLIC :: COPY
PUBLIC :: Get

!----------------------------------------------------------------------------
!                                             Get@GetValuesMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 6 March 2021
! summary: Returns the values of [[RealMatrix_]] obj in 2D array
!
!# Introduction
!
! This function returns the value stored in `obj%l` in a 2D fortran array
!
!### Usage
!
!```fortran
!	Val = Get( obj, 1.0_dfp )
!```

INTERFACE
MODULE PURE FUNCTION realmat_get1( obj, DataType ) RESULT( Ans )
  TYPE( RealMatrix_ ), INTENT( IN ) :: obj
  REAL( DFP ), INTENT( IN ) :: DataType
  REAL( DFP ), ALLOCATABLE :: Ans( :, : )
END FUNCTION realmat_get1
END INTERFACE

INTERFACE Get
  MODULE PROCEDURE realmat_get1
END INTERFACE Get

!----------------------------------------------------------------------------
!                                              Get@GetValuesMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 	6 March 2021
! summary: 	 Returns the values of [[RealMatrix_]] obj in 2D array
!
!# Introduction
!
! This function returns a section of `obj%l` in a 2D fortran array. This
! is equivalent to `Val = obj%l(RIndx, CIndx)`
!
!### Usage
!
!```fortran
! integer( i4b ) :: r( 2 ), c( 2 )
! type( RealMatrix_ ) :: obj
! call initiate( obj, [4,4] )
! call random_number( obj ); r=[1,2]; c=[2,3]
! Val = Get( obj, R, C, 1.0_dfp )
!```
!
! The above call will return `obj%Val[1:2, 2:3]`


INTERFACE
MODULE PURE FUNCTION realmat_get2( obj, RIndx, CIndx, DataType ) &
  & RESULT( Ans )
  TYPE( RealMatrix_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: RIndx( : ), CIndx( : )
  REAL( DFP ), INTENT( IN ) :: DataType
  REAL( DFP ), ALLOCATABLE :: Ans( :, : )
END FUNCTION realmat_get2
END INTERFACE

INTERFACE Get
  MODULE PROCEDURE realmat_get2
END INTERFACE Get

!----------------------------------------------------------------------------
!                                              Get@GetValuesMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 6 March 2021
! summary: Returns the values of [[RealMatrix_]] obj in 2D array
!
!# Introduction
!
! This function returns a section of `obj%l` in a 2D fortran array. This
! is equivalent to `Val = obj%l(is:ie:s, is:ie:s)`
!
!### Usage
!
!```fortran
! integer( i4b ) :: r( 2 ), c( 2 )
! type( RealMatrix_ ) :: obj
! call initiate( obj, [4,4] )
! call random_number( obj )
! Val = Get( obj, 1, 2, 1, 1.0_dfp )
!```
!
! The above call will return `obj%Val[1:2:1, 1:2:1]`

INTERFACE
MODULE PURE FUNCTION realmat_get3( obj, iStart, iEnd, Stride, &
  & DataType ) RESULT( Ans )
  TYPE( RealMatrix_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: iStart, iEnd, Stride
  REAL( DFP ), INTENT( IN ) :: DataType
  REAL( DFP ), ALLOCATABLE :: Ans( :, : )
END FUNCTION realmat_get3
END INTERFACE

INTERFACE Get
  MODULE PROCEDURE realmat_get3
END INTERFACE Get

!----------------------------------------------------------------------------
!                                               Get@GetValuesMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 	6 March 2021
! summary: Returns [[RealMatrix_]] obj from [[realmatrix_]]
!
!# Introduction
!
! This function is essentially copy method `Ans=obj`
!
!### Usage
!
!```fortran
!
!```

INTERFACE
MODULE PURE FUNCTION realmat_get4( obj, DataType ) RESULT( Ans )
  TYPE( RealMatrix_ ), INTENT( IN ) :: obj
  TYPE( RealMatrix_ ), INTENT( IN ) :: DataType
  TYPE( RealMatrix_ ) :: Ans
END FUNCTION realmat_get4
END INTERFACE

INTERFACE Get
  MODULE PROCEDURE realmat_get4
END INTERFACE Get

!----------------------------------------------------------------------------
!                                               Get@GetValuesMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 	6 March 2021
! summary: Returns [[RealMatrix_]] obj from a section of [[realmatrix_]]
!
!# Introduction
!
! This function is essentially copy method `Ans=obj(RIndx, CIndx)`
!
!### Usage
!
!```fortran
!
!```

INTERFACE
MODULE PURE FUNCTION realmat_get5( obj, RIndx, CIndx, DataType ) &
  & RESULT( Ans )
  TYPE( RealMatrix_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: RIndx( : ), CIndx( : )
  TYPE( RealMatrix_ ), INTENT( IN ) :: DataType
  TYPE( RealMatrix_ ) :: Ans
END FUNCTION realmat_get5
END INTERFACE

INTERFACE Get
  MODULE PROCEDURE realmat_get5
END INTERFACE Get

!----------------------------------------------------------------------------
!                                              Get@GetValuesMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 	6 March 2021
! summary: Returns [[RealMatrix_]] obj from a section of [[realmatrix_]]
!
!# Introduction
! 	This function is essentially copy method `Ans=obj(is:ie, is:ie)`
!
!### Usage
!
!```fortran
!
!```

INTERFACE
MODULE PURE FUNCTION realmat_get6( obj, iStart, iEnd, &
  & Stride, DataType ) RESULT( Ans )
  TYPE( RealMatrix_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: iStart, iEnd, Stride
  TYPE( RealMatrix_ ), INTENT( IN ) :: DataType
  TYPE( RealMatrix_ ) :: Ans
END FUNCTION realmat_get6
END INTERFACE

INTERFACE Get
  MODULE PROCEDURE realmat_get6
END INTERFACE Get

!----------------------------------------------------------------------------
!                                              Get@GetValuesMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 	6 March 2021
! summary: Returns values in 2D fortran array from [[realmatrix_]]
!
!# Introduction
!
! This function combines all [[realmatrix_]] value of `obj` and
! returns a 2D fortrn array
!
!### Usage
!
!```fortran
!
!```

INTERFACE
MODULE PURE FUNCTION realmat_get7( obj, DataType ) RESULT( Ans )
  TYPE( RealMatrix_ ), INTENT( IN ) :: obj( :, : )
  REAL( DFP ), INTENT( IN ) :: DataType
  REAL( DFP ), ALLOCATABLE :: Ans( :, : )
END FUNCTION realmat_get7
END INTERFACE

INTERFACE Get
  MODULE PROCEDURE realmat_get7
END INTERFACE Get

!----------------------------------------------------------------------------
!                                              Get@GetValuesMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 	6 March 2021
! summary: Returns [[realmatrix_]] object from a 2D array of [[realmatrix_]]
!
!# Introduction
!
! This function combines all [[realmatrix_]] value of `obj` and
! returns a [[realmatrix_]] object
!
!### Usage
!
!```fortran
!
!```

INTERFACE
MODULE PURE FUNCTION realmat_get8( obj, DataType ) RESULT( Ans )
  TYPE( RealMatrix_ ), INTENT( IN ) :: obj( :, : )
  TYPE( RealMatrix_ ), INTENT( IN ) :: DataType
  TYPE( RealMatrix_ ) :: Ans
END FUNCTION realmat_get8
END INTERFACE

INTERFACE Get
  MODULE PROCEDURE realmat_get8
END INTERFACE Get

!----------------------------------------------------------------------------
!                                                      Copy@GetValuesMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 	6 March 2021
! summary: Copy from [[realmatrix_]] to 2D fortran array
!
!# Introduction
!
! This subroutine copy the contents of [[realmatrix_]] object into a 2D
! fortran array
!
!### Usage
!
!```fortran
!
!```

INTERFACE
MODULE PURE SUBROUTINE realmat_copy1( From, To )
  TYPE( RealMatrix_ ), INTENT( IN ) :: From
  REAL( DFP ), ALLOCATABLE, INTENT( INOUT ) :: To( :, : )
END SUBROUTINE realmat_copy1
END INTERFACE

INTERFACE COPY
  MODULE PROCEDURE realmat_copy1
END INTERFACE COPY

INTERFACE Convert
  MODULE PROCEDURE realmat_copy1
END INTERFACE Convert

!----------------------------------------------------------------------------
!                                                      Copy@GetValuesMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 	6 March 2021
! summary: Copy from [[realmatrix_]] to another [[realmatrix_]]
!
!# Introduction
!
! This subroutine copy the contents of [[realmatrix_]] object to another
! [[realmatrix_]] object
!
!### Usage
!
!```fortran
!
!```

INTERFACE
MODULE PURE SUBROUTINE realmat_copy2( From, To )
  TYPE( RealMatrix_ ), INTENT( IN ) :: From
  TYPE( RealMatrix_ ), INTENT( INOUT ) :: To
END SUBROUTINE realmat_copy2
END INTERFACE

INTERFACE COPY
  MODULE PROCEDURE realmat_copy2
END INTERFACE COPY

INTERFACE Convert
  MODULE PROCEDURE realmat_copy2
END INTERFACE Convert

!----------------------------------------------------------------------------
!                                                      Copy@GetValuesMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  6 March 2021
! summary: Copy from 2D fortran array to [[realmatrix_]]
!
!# Introduction
!
! This subroutine copy the contents of a 2D fortran array to [[realmatrix_]]
! object
!
!### Usage
!
!```fortran
!
!```

INTERFACE
MODULE PURE SUBROUTINE realmat_copy3( From, To )
  REAL( DFP ), INTENT( IN ) :: From( :, : )
  TYPE( RealMatrix_ ), INTENT( INOUT ) :: To
END SUBROUTINE realmat_copy3
END INTERFACE

INTERFACE COPY
  MODULE PROCEDURE realmat_copy3
END INTERFACE COPY

INTERFACE Convert
  MODULE PROCEDURE realmat_copy3
END INTERFACE Convert

!----------------------------------------------------------------------------
!                                             ArrayPointer@GetValuesMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 	6 March 2021
! summary: Get pointer to the values stored inside [[realmatrix_]]
!
!# Introduction
!
! This function returns the pointer to the values stored inside the
! [[realmatrix_]]
!
!### Usage
!
!```fortran
!
!```

INTERFACE
MODULE FUNCTION realmat_getPointer( obj, DataType ) RESULT( Ans )
  TYPE( RealMatrix_ ), INTENT( IN ), TARGET :: obj
  REAL( DFP ), INTENT( IN ) :: DataType
  REAL( DFP ), POINTER :: Ans( :, : )
END FUNCTION realmat_getPointer
END INTERFACE

INTERFACE GetPointer
  MODULE PROCEDURE realmat_getPointer
END INTERFACE GetPointer
