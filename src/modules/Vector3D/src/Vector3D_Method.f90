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
! date: 	24 Feb 2021
! summary: This module contains method for [[Vector3D_]] data type
!
!### Introduction
!This module contains method for [[Vector3D_]] data type. It contains following submodules:
!
! * `Vector3D_Method@Constructor.f90`
! * `Vector3D_Method@Misc.f90`
!


MODULE Vector3D_Method
USE GlobalData, ONLY: DFP, I4B, LGT, stdout
USE BaseType, ONLY: Vector3D_
IMPLICIT NONE

PRIVATE

!----------------------------------------------------------------------------
!                                                          Shape@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	24 Feb 2021
! summary: 	This function returns the shape of [[Vector3D_]]
!
!### Introduction
! This routine returns the shape of [[Vector3D_]]
!
!### Usage
!
!```fortran
! s = SHAPE(Obj)
!```

INTERFACE
MODULE PURE FUNCTION get_shape( Obj ) RESULT( Ans )
  CLASS( Vector3D_ ), INTENT( IN ) :: Obj
  INTEGER( I4B ) :: Ans( 1 )
END FUNCTION get_shape
END INTERFACE

INTERFACE Shape
  MODULE PROCEDURE get_shape
END INTERFACE Shape

PUBLIC :: Shape

!----------------------------------------------------------------------------
!                                                           SIZE@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	24 Feb 2021
! summary: 	This routine returns the size of [[Vector3D_]]

INTERFACE
MODULE PURE FUNCTION get_size( Obj, Dims ) RESULT( Ans )
  TYPE( Vector3D_ ), INTENT( IN ) :: Obj
  INTEGER( I4B ), INTENT( IN ), OPTIONAL :: Dims
  INTEGER( I4B ) :: Ans
END FUNCTION get_size
END INTERFACE

INTERFACE SIZE
  MODULE PROCEDURE get_size
END INTERFACE SIZE

PUBLIC :: SIZE

!----------------------------------------------------------------------------
!                                                   AllocateData@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	24 Feb 2021
! summary: 	This routine allocate the data for [[Vector3D_]]
!
!### Introduction
! This subroutine reset the instance of [[Vector3D_]] to zero
!

INTERFACE
MODULE PURE SUBROUTINE Allocate_Data( Obj, Dims )
  CLASS( Vector3D_ ), INTENT( INOUT ) :: Obj
  INTEGER( I4B ), INTENT( IN ) :: Dims
END SUBROUTINE Allocate_Data
END INTERFACE

INTERFACE AllocateData
  MODULE PROCEDURE Allocate_Data
END INTERFACE AllocateData

PUBLIC :: AllocateData

!----------------------------------------------------------------------------
!                                                 DeAllocateData@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	24 Feb 2021
! summary: 	This subroutine reset the instance of [[Vector3D_]]

INTERFACE
MODULE PURE SUBROUTINE Deallocate_Data( Obj )
  CLASS( Vector3D_ ), INTENT( INOUT ) :: Obj
END SUBROUTINE Deallocate_Data
END INTERFACE

INTERFACE DeallocateData
  MODULE PROCEDURE Deallocate_Data
END INTERFACE DeallocateData

PUBLIC :: DeallocateData

!----------------------------------------------------------------------------
!                                                       Initiate@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	24 Feb 2021
! summary: 	This routine initiate the instance of [[Vector3D_]]
!
!### Introduction
!
! This routine initiate the instance of [[Vector3D_]]. If `Val` is 1D or 2D vector then the rest of the components of instance of [[Vector3D_]] will be set to zero. If `SIZE(Val)>=4` then only first 3 components are used to construct the instance.
!
!### Usage
!
!```fortran
!...
! type( Vector3D_ ) :: obj
! call initiate( obj, [1.0_DFP, 2.0_DFP, 3.0_DFP])
! call display(obj, "test1=")
! ...
!```

INTERFACE
MODULE PURE SUBROUTINE initiate_obj_from_val( Obj, Val )
  CLASS( Vector3D_ ), INTENT( INOUT ) :: Obj
  REAL( DFP ), INTENT( IN ) :: Val( : )
END SUBROUTINE initiate_obj_from_val
END INTERFACE

!----------------------------------------------------------------------------
!                                                       Initiate@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	24 Feb 2021
! summary: 	This routine initiate the instance of [[Vector3D_]] from another object, basically it is a copy operation
!
!### Introduction
! This routine initiate the instance of [[Vector3D_]] from another object, basically it is a copy operation.
!
!
!### Usage
!
!```fortran
! type( Vector3D_ ) :: obj, obj2
! call initiate( obj, [1.0_DFP, 2.0_DFP, 3.0_DFP])
! call initiate( obj2, obj )
! call display(obj2, "test2=")
!```

INTERFACE
MODULE PURE SUBROUTINE initiate_obj_from_obj( Obj, AnotherObj )
  CLASS( Vector3D_ ), INTENT( INOUT ) :: Obj
  CLASS( Vector3D_ ), INTENT( IN ) :: AnotherObj
END SUBROUTINE initiate_obj_from_obj
END INTERFACE

!----------------------------------------------------------------------------
!                                                       Initiate@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	24 Feb 2021
! summary: Generic procedure to initiate the instance of [[Vector3D_]] object

INTERFACE Initiate
  MODULE PROCEDURE initiate_obj_from_val, initiate_obj_from_obj
END INTERFACE Initiate

PUBLIC :: Initiate

!----------------------------------------------------------------------------
!                                                          COPY@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	24 Feb 2021
! summary: 	Generic procedure to copy one instance of [[Vector3D_]] into another instance

INTERFACE COPY
  MODULE PROCEDURE initiate_obj_from_obj
END INTERFACE COPY

PUBLIC :: COPY

!----------------------------------------------------------------------------
!                                                    Assignment@Constructor
!----------------------------------------------------------------------------

INTERFACE ASSIGNMENT(=)
  MODULE PROCEDURE initiate_obj_from_obj, initiate_obj_from_val
END INTERFACE

PUBLIC :: ASSIGNMENT(=)

!----------------------------------------------------------------------------
!                                                       Vector3D@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 24 Feb 2021
! summary: Function to create an instance of [[vector3d_]]
!
!### Introduction
!
! This function creates an instance of [[vector3d_]] from given fortran vector of real numbers
!
!
!### Usage
!
!```fortran
! type( Vector3D_ ) :: obj
! obj = Vector3D([1.0_DFP])
! call display( obj, "test5=")
!```

INTERFACE
MODULE PURE FUNCTION Constructor1( Val ) RESULT( Ans )
  REAL( DFP ), INTENT( IN ) :: Val( : )
  TYPE( Vector3D_ ) :: Ans
END FUNCTION Constructor1
END INTERFACE

!> authors: Vikas Sharma, Ph. D.
! date: 24 Feb 2021
! summary: Generic function to create an instance of [[Vector3D_]]

INTERFACE Vector3D
  MODULE PROCEDURE Constructor1
END INTERFACE Vector3D

PUBLIC :: Vector3D

!----------------------------------------------------------------------------
!                                              Vector3D_Pointer@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	24 Feb 2021
! summary: 	This function returns the pointer to an instance of [[Vector3D_]]
!
!### Introduction
!
! This function returns pointer to an instance of [[Vector2D_]]
!
!
!### Usage
!
!```fortran
! type( Vector3D_ ), pointer :: obj
! obj => Vector3D_Pointer([1.0_DFP])
! call display(obj, "test6=")
!```

INTERFACE
MODULE FUNCTION Constructor_1( Val ) RESULT( Ans )
  REAL( DFP ), INTENT( IN ) :: Val(:)
  CLASS( Vector3D_ ), POINTER :: Ans
END FUNCTION Constructor_1
END INTERFACE

!> authors: Vikas Sharma, Ph. D.
! date: 	24 Feb 2021
! summary: 	This function returns the pointer to an instance of [[Vector3D_]]
!
!### Usage
!
!```fortran
! type( Vector3D_ ), pointer :: obj
! obj => Vector3D_Pointer([1.0_DFP])
! call display(obj, "test6=")
!```

INTERFACE
MODULE FUNCTION Constructor_2( Obj ) RESULT( Ans )
  TYPE( Vector3D_ ), INTENT( IN ) :: Obj
  CLASS( Vector3D_ ), POINTER :: Ans
END FUNCTION Constructor_2
END INTERFACE

!> authors: Vikas Sharma, Ph. D.
! date: 	24 Feb 2021
! summary: 	This generic function returns pointer to an instance of [[Vector3D_]]

INTERFACE Vector3D_Pointer
  MODULE PROCEDURE Constructor_1, Constructor_2
END INTERFACE Vector3D_Pointer

PUBLIC :: Vector3D_Pointer

!----------------------------------------------------------------------------
!                                                       Display@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	24 Feb 2021
! summary: This subroutine display [[Vector3D_]]
!
!### Usage
!
!```fortran
! type( Vector3D_ ), pointer :: obj
! obj => Vector3D_Pointer([1.0_DFP])
! call display(obj, "test6=")
!```

INTERFACE
MODULE SUBROUTINE Display_Obj( Obj, Msg, UnitNo )
  CLASS( Vector3D_ ), INTENT( IN ) :: Obj
  CHARACTER( LEN = * ), INTENT( IN ) :: Msg
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: UnitNo
END SUBROUTINE Display_Obj
END INTERFACE

!> authors: Vikas Sharma, Ph. D.
! date: 	24 Feb 2021
! summary: 	Generic routine to display an object

INTERFACE Display
  MODULE PROCEDURE Display_Obj
END INTERFACE Display

PUBLIC :: Display

!----------------------------------------------------------------------------
!                                                          DOT_PRODUCT@Misc
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	24 Feb 2021
! summary: 	This funtion computes dot product of two [[Vector3D_]] object
!
!### Usage
!
!```fortran
! type( Vector3D_ ) :: obj1, obj2
! obj1 = [1.0_dfp, 2.0_dfp, 3.0_dfp]
! obj2 = [1.0_dfp, 0.0_dfp, 3.0_dfp]
! CALL Equalline()
! CALL Display( "test7" )
! CALL Display( obj1, "obj1 = " )
! CALL Display( obj2, "obj2 = " )
! CALL Display( DOT_PRODUCT( obj1, obj2 ), "dot_product = " )
! CALL Display( obj1 .DOT. obj2, "dot_product = " )
! CALL DotLine()
!```

INTERFACE
MODULE PURE FUNCTION dot_product_1( Obj1, Obj2 ) RESULT( Ans )
  CLASS( Vector3D_ ), INTENT( IN ) :: Obj1
  CLASS( Vector3D_ ), INTENT( IN ) :: Obj2
  REAL( DFP ) :: Ans
END FUNCTION dot_product_1
END INTERFACE

!----------------------------------------------------------------------------
!                                                           DOT_PRODUCT@Misc
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	24 Feb 2021
! summary: 	This funtion computes dot product of a [[Vector3D_]] object and fortran vector
!
!### Usage
!
!```fortran
! type( Vector3D_ ) :: obj
! real( dfp ) :: val(3)
! obj = [1.0_dfp, 2.0_dfp, 3.0_dfp]
! val = [1.0_dfp, 0.0_dfp, 3.0_dfp]
! CALL Equalline()
! CALL Display( "test8" )
! CALL Display( obj, "obj1 = " )
! CALL Display( val, "val = " )
! CALL Display( DOT_PRODUCT( obj=obj, val=val ), "dot_product = " )
! CALL Display( obj.DOT. val, "dot_product = " )
! CALL DotLine()
!```

INTERFACE
MODULE PURE FUNCTION dot_product_2( Obj, Val ) RESULT( Ans )
  CLASS( Vector3D_ ), INTENT( IN ) :: Obj
  REAL( DFP ), INTENT( IN ) :: Val( : )
  REAL( DFP ) :: Ans
END FUNCTION dot_product_2
END INTERFACE

!----------------------------------------------------------------------------
!                                                          DOT_PRODUCT@Misc
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	24 Feb 2021
! summary: 	This funtion computes dot product of a [[Vector3D_]] object and fortran vector
!
!### Usage
!
!```fortran
! type( Vector3D_ ) :: obj
! real( dfp ) :: val(3)
! obj = [1.0_dfp, 2.0_dfp, 3.0_dfp]
! val = [1.0_dfp, 0.0_dfp, 3.0_dfp]
! CALL Equalline()
! CALL Display( "test8" )
! CALL Display( obj, "obj1 = " )
! CALL Display( val, "val = " )
! CALL Display( DOT_PRODUCT( obj=obj, val=val ), "dot_product = " )
! CALL Display( obj.DOT. val, "dot_product = " )
! CALL DotLine()
!```

INTERFACE
MODULE PURE FUNCTION dot_product_3( Val, Obj ) RESULT( Ans )
  CLASS( Vector3D_ ), INTENT( IN ) :: Obj
  REAL( DFP ), INTENT( IN ) :: Val( : )
  REAL( DFP ) :: Ans
END FUNCTION dot_product_3
END INTERFACE

!----------------------------------------------------------------------------
!                                                          DOT_PRODUCT@Misc
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	24 Feb 2021
! summary: 	This Generic function computes dot product of [[Vector3D_]] object.

INTERFACE DOT_PRODUCT
  MODULE PROCEDURE dot_product_1, dot_product_2
END INTERFACE DOT_PRODUCT

PUBLIC :: DOT_PRODUCT

INTERFACE OPERATOR( .DOT. )
  MODULE PROCEDURE dot_product_1, dot_product_2, dot_product_3
END INTERFACE

PUBLIC :: OPERATOR(.DOT. )

!----------------------------------------------------------------------------
!                                                       Vector_Product@Misc
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	24 Feb 2021
! summary: 	This function computes the vector product
!
!### Introduction
!
! Ths function computes the vector product of two [[Vector3D_]] object and returns another [[Vector3D_]] object.
!
!### Usage
!
!```fortran
! type( Vector3D_ ) :: obj1, obj2, obj3
! obj1 = [1.0_dfp, 2.0_dfp, 3.0_dfp]
! obj2 = [1.0_dfp, 0.0_dfp, 3.0_dfp]
! CALL Equalline()
! CALL Display( "test9" )
! CALL Display( obj1, "obj1 = " )
! CALL Display( obj2, "obj2 = " )
! CALL Display( VECTOR_PRODUCT( obj1, obj2 ), "vector_product = " )
! CALL Display( obj1 .X. obj2, "vector_product = " )
! CALL DotLine()
!```

INTERFACE
MODULE PURE FUNCTION vector_product_1( Obj1, Obj2 ) RESULT( Ans )
  CLASS( Vector3D_ ), INTENT( IN ) :: Obj1
  CLASS( Vector3D_ ), INTENT( IN ) :: Obj2
  TYPE( Vector3D_ ) :: Ans
END FUNCTION vector_product_1
END INTERFACE

!----------------------------------------------------------------------------
!                                                       Vector_Product@Misc
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	24 Feb 2021
! summary: 	This function computes the vector product
!
!### Introduction
!
! Ths function computes the vector product of a [[Vector3D_]] object and a fortran vector, and returns another [[Vector3D_]] object.
!
!### Usage
!
!```fortran
! type( Vector3D_ ) :: obj
! real( dfp ) :: val(3)
! obj = [1.0_dfp, 2.0_dfp, 3.0_dfp]
! val = [1.0_dfp, 0.0_dfp, 3.0_dfp]
! CALL Equalline()
! CALL Display( "test10" )
! CALL Display( obj, "obj1 = " )
! CALL Display( val, "val = " )
! CALL Display( Vector_PRODUCT( obj=obj, val=val ), "vector_product = " )
! CALL Display( Obj .X. val, "vector_product = " )
! CALL DotLine()
!```

INTERFACE
MODULE PURE FUNCTION vector_product_2( Obj, Val ) RESULT( Ans )
  CLASS( Vector3D_ ), INTENT( IN ) :: Obj
  REAL( DFP ), INTENT( IN ) :: Val( : )
  TYPE( Vector3D_ ) :: Ans
END FUNCTION vector_product_2
END INTERFACE

!----------------------------------------------------------------------------
!                                                       Vector_Product@Misc
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	24 Feb 2021
! summary: 	This function computes the vector product
!
!### Introduction
!
! Ths function computes the vector product of a [[Vector3D_]] object and a fortran vector, and returns another [[Vector3D_]] object.
!
!### Usage
!
!```fortran
! type( Vector3D_ ) :: obj
! real( dfp ) :: val(3)
! obj = [1.0_dfp, 2.0_dfp, 3.0_dfp]
! val = [1.0_dfp, 0.0_dfp, 3.0_dfp]
! CALL Equalline()
! CALL Display( "test10" )
! CALL Display( obj, "obj1 = " )
! CALL Display( val, "val = " )
! CALL Display( Vector_PRODUCT( obj=obj, val=val ), "vector_product = " )
! CALL Display( Obj .X. val, "vector_product = " )
! CALL DotLine()
!```

INTERFACE
MODULE PURE FUNCTION vector_product_3( Val, Obj ) RESULT( Ans )
  CLASS( Vector3D_ ), INTENT( IN ) :: Obj
  REAL( DFP ), INTENT( IN ) :: Val( : )
  TYPE( Vector3D_ ) :: Ans
END FUNCTION vector_product_3
END INTERFACE

!----------------------------------------------------------------------------
!                                                       Vector_Product@Misc
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	25 Feb 2021
! summary: 	This
INTERFACE
MODULE PURE FUNCTION vector_product_4( u, v, w ) RESULT( Ans )
  CLASS( Vector3D_ ), INTENT( IN ) :: u
  CLASS( Vector3D_ ), INTENT( IN ) :: v
  CLASS( Vector3D_ ), INTENT( IN ) :: w
  TYPE( Vector3D_ ) :: Ans
END FUNCTION vector_product_4
END INTERFACE

!----------------------------------------------------------------------------
!                                                       Vector_Product@Misc
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 24 Feb 2021
! summary: This generic function computes the vector product

INTERFACE Vector_Product
  MODULE PROCEDURE vector_product_1, vector_product_2
END INTERFACE Vector_Product

PUBLIC :: Vector_Product

!> authors: Vikas Sharma, Ph. D.
! date: 	25 Feb 2021
! summary: 	A generic procedure to perform vector product

INTERFACE OPERATOR(.X.)
  MODULE PROCEDURE  vector_product_1, vector_product_2, vector_product_3
END INTERFACE OPERATOR(.X.)

PUBLIC :: OPERATOR(.X.)

!----------------------------------------------------------------------------
!                                                                 Vector_
!----------------------------------------------------------------------------


END MODULE Vector3D_Method