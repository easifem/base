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
! s = SHAPE(obj)
!```

INTERFACE
MODULE PURE FUNCTION get_shape( obj ) RESULT( Ans )
  CLASS( Vector3D_ ), INTENT( IN ) :: obj
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
MODULE PURE FUNCTION get_size( obj, Dims ) RESULT( Ans )
  TYPE( Vector3D_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ), OPTIONAL :: Dims
  INTEGER( I4B ) :: Ans
END FUNCTION get_size
END INTERFACE

INTERFACE SIZE
  MODULE PROCEDURE get_size
END INTERFACE SIZE

PUBLIC :: SIZE

!----------------------------------------------------------------------------
!                                                TotalDimension@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	23 Feb 2021
! summary: 	Returns the total dimension of an array
!
!### Introduction
!
! This function returns the total dimension (or rank) of an array,

INTERFACE
MODULE PURE FUNCTION Vec3D_getTotalDimension( obj ) RESULT( Ans )
  TYPE( Vector3D_ ), INTENT( IN ) :: obj
  INTEGER( I4B ) :: ans
END FUNCTION Vec3D_getTotalDimension
END INTERFACE

INTERFACE getTotalDimension
  MODULE PROCEDURE Vec3D_getTotalDimension
END INTERFACE getTotalDimension

PUBLIC :: getTotalDimension

!----------------------------------------------------------------------------
!                                             setTotalDimension@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	23 Feb 2021
! summary: 	This subroutine set the total dimension (rank) of an array
!
!### Introduction
!
! This subroutine sets the rank(total dimension) of an array

INTERFACE
MODULE PURE SUBROUTINE Vec3D_setTotalDimension( obj, tDimension )
  CLASS( Vector3D_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: tDimension
END SUBROUTINE Vec3D_setTotalDimension
END INTERFACE

INTERFACE setTotalDimension
  MODULE PROCEDURE Vec3D_setTotalDimension
END INTERFACE setTotalDimension

PUBLIC :: setTotalDimension

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
MODULE PURE SUBROUTINE Allocate_Data( obj, Dims )
  CLASS( Vector3D_ ), INTENT( INOUT ) :: obj
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
MODULE PURE SUBROUTINE Deallocate_Data( obj )
  CLASS( Vector3D_ ), INTENT( INOUT ) :: obj
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
MODULE PURE SUBROUTINE initiate_obj_from_val( obj, Val )
  CLASS( Vector3D_ ), INTENT( INOUT ) :: obj
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
MODULE PURE SUBROUTINE initiate_obj_from_obj( obj, Anotherobj )
  CLASS( Vector3D_ ), INTENT( INOUT ) :: obj
  CLASS( Vector3D_ ), INTENT( IN ) :: Anotherobj
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
! This function returns pointer to an instance of [[Vector3D_]]
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
MODULE FUNCTION Constructor_2( obj ) RESULT( Ans )
  TYPE( Vector3D_ ), INTENT( IN ) :: obj
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
MODULE SUBROUTINE Display_obj( obj, Msg, UnitNo )
  CLASS( Vector3D_ ), INTENT( IN ) :: obj
  CHARACTER( LEN = * ), INTENT( IN ) :: Msg
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: UnitNo
END SUBROUTINE Display_obj
END INTERFACE

!> authors: Vikas Sharma, Ph. D.
! date: 	24 Feb 2021
! summary: 	Generic routine to display an object

INTERFACE Display
  MODULE PROCEDURE Display_obj
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
MODULE PURE FUNCTION dot_product_1( obj1, obj2 ) RESULT( Ans )
  CLASS( Vector3D_ ), INTENT( IN ) :: obj1
  CLASS( Vector3D_ ), INTENT( IN ) :: obj2
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
MODULE PURE FUNCTION dot_product_2( obj, Val ) RESULT( Ans )
  CLASS( Vector3D_ ), INTENT( IN ) :: obj
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
MODULE PURE FUNCTION dot_product_3( Val, obj ) RESULT( Ans )
  CLASS( Vector3D_ ), INTENT( IN ) :: obj
  REAL( DFP ), INTENT( IN ) :: Val( : )
  REAL( DFP ) :: Ans
END FUNCTION dot_product_3
END INTERFACE

!----------------------------------------------------------------------------
!                                                          DOT_PRODUCT@Misc
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	25 Feb 2021
! summary: 	This function returns the box product (triple scalar product) of three vector.
!
!### Introduction
!
! Box product or triple scalar product is defined as:
!
! $$\mathbf{u} \cdot (\mathbf{v} \times \mathbf{w})=[\mathbf{u}, \mathbf{v}, \mathbf{w}]$$
!
! This function computes the box product.
!
!### Usage
!
!```fortran
! type( Vector3D_ ) :: obj1, obj2, obj3
! obj1 = [1.0_dfp, 0.0_dfp, 0.0_dfp]
! obj2 = [0.0_dfp, 1.0_dfp, 0.0_dfp]
! obj3 = [1.0_dfp, 2.0_dfp, 1.0_dfp]
! CALL Equalline()
! CALL Display( "test12" )
! CALL Display( DOT_PRODUCT( obj1, obj2, obj3 ), "dot_product = " )
! CALL DotLine()
!```

INTERFACE
MODULE PURE FUNCTION dot_product_4( u, v, w ) RESULT( Ans )
  CLASS( Vector3D_ ), INTENT( IN ) :: u
  CLASS( Vector3D_ ), INTENT( IN ) :: v
  CLASS( Vector3D_ ), INTENT( IN ) :: w
  REAL( DFP ) :: Ans
END FUNCTION dot_product_4
END INTERFACE


!----------------------------------------------------------------------------
!                                                          DOT_PRODUCT@Misc
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	24 Feb 2021
! summary: 	This Generic function computes dot product of [[Vector3D_]] object.

INTERFACE DOT_PRODUCT
  MODULE PROCEDURE dot_product_1, dot_product_2, dot_product_4
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
MODULE PURE FUNCTION vector_product_1( obj1, obj2 ) RESULT( Ans )
  CLASS( Vector3D_ ), INTENT( IN ) :: obj1
  CLASS( Vector3D_ ), INTENT( IN ) :: obj2
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
! CALL Display( obj .X. val, "vector_product = " )
! CALL DotLine()
!```

INTERFACE
MODULE PURE FUNCTION vector_product_2( obj, Val ) RESULT( Ans )
  CLASS( Vector3D_ ), INTENT( IN ) :: obj
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
! CALL Display( obj .X. val, "vector_product = " )
! CALL DotLine()
!```

INTERFACE
MODULE PURE FUNCTION vector_product_3( Val, obj ) RESULT( Ans )
  CLASS( Vector3D_ ), INTENT( IN ) :: obj
  REAL( DFP ), INTENT( IN ) :: Val( : )
  TYPE( Vector3D_ ) :: Ans
END FUNCTION vector_product_3
END INTERFACE

!----------------------------------------------------------------------------
!                                                       Vector_Product@Misc
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	25 Feb 2021
! summary: 	This function performs vector triple product
!
!### Introduction
!
! This function performs vector triple product, and returns the resultant [[Vector3D_]] object;
!
! $$\mathbf{u} \times (\mathbf{v} \times \mathbf{w}) = (\mathbf{u} \cdot \mathbf{w}) \mathbf{v} - (\mathbf{u} \cdot \mathbf{v}) \mathbf{w}$$
!
!
!### Usage
!
!```fortran
! type( Vector3D_ ) :: obj1, obj2, obj3
! obj1 = [1.0_dfp, 0.0_dfp, 0.0_dfp]
! obj2 = [0.0_dfp, 1.0_dfp, 0.0_dfp]
! obj3 = [1.0_dfp, 2.0_dfp, 1.0_dfp]
!
! CALL Equalline()
! CALL Display( "test11" )
! CALL Display( Vector_PRODUCT( obj1, obj2, obj3 ), "vector_product = " )
! CALL Display( obj1 .X. (obj2 .X. obj3), "vector_product = " )
! CALL DotLine()
!```

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
  MODULE PROCEDURE vector_product_1, vector_product_2, vector_product_4
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
!                                                                 NORM2@Misc
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 Feb 2021
! summary: This function creates Norm2 of a vector
!
!### Introduction
! This function computes second norm of [[vector3d_]] object.
!
! $$\vert \mathbf(u) \vert = \sqrt{\matbf{u} \cdot \mathbf{v}}$$
!
!
!### Usage
!
!```fortran
! type( Vector3D_ ) :: obj
! obj = [1.0_dfp, 2.0_dfp, 3.0_dfp]
! CALL Equalline()
! CALL Display( "test13" )
! CALL Display( NORM2( obj ), "NORM2 = " )
! CALL Display( .NORM. obj, ".Norm. obj = ")
! CALL DotLine()
!```

INTERFACE
MODULE PURE FUNCTION Norm2_obj( obj ) RESULT( Ans )
  CLASS( Vector3D_ ), INTENT( IN ) :: obj
  REAL( DFP ) :: Ans
END FUNCTION Norm2_obj
END INTERFACE

INTERFACE OPERATOR( .NORM.)
  MODULE PROCEDURE Norm2_obj
END INTERFACE OPERATOR( .NORM.)

PUBLIC :: OPERATOR( .NORM.)

INTERFACE Norm2
  MODULE PROCEDURE Norm2_obj
END INTERFACE Norm2

PUBLIC :: Norm2

!----------------------------------------------------------------------------
!                                                            UnitVector@Misc
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	25 Feb 2021
! summary: 	Returnt the unit vector from a given vector
!
!### Usage
!
!```fortran
!
!```

INTERFACE
MODULE PURE FUNCTION get_unitVector( obj ) RESULT( Ans )
  CLASS( Vector3D_ ), INTENT( IN ) :: obj
  TYPE( Vector3D_ ) :: Ans
END FUNCTION get_unitVector
END INTERFACE

INTERFACE UnitVector
  MODULE PROCEDURE get_unitVector
END INTERFACE UnitVector

PUBLIC :: UnitVector

INTERFACE Hat
  MODULE PROCEDURE get_unitVector
END INTERFACE Hat

PUBLIC :: Hat

INTERFACE OPERATOR( .HAT. )
  MODULE PROCEDURE get_unitVector
END INTERFACE OPERATOR( .HAT. )

PUBLIC :: OPERATOR( .HAT. )

!----------------------------------------------------------------------------
!                                                                 Angle@Misc
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date:  25 Feb 2021
! summary: 	This function returns the angle beteen two vectors
!
!### Introduction
! Angle between two vectors $\mathbf{u}$ and $\mathbf{v}$ is given by:
!
! $$\cos \theta = \frac{\mathbf{u} \cdot \mathbf{v}}{\vert \mathbf{u} \vert \cdot \vert \mathbf{v} \vert}$$
!
! This function computes the angle between the two vectors and returnt the result in radians.
!
!
!### Usage
!
!```fortran
! type( Vector3D_ ) :: obj1, obj2
! obj1 = [0.0_dfp, 2.0_dfp, 3.0_dfp]
! obj2 = [1.0_dfp, 0.0_dfp]
! CALL Equalline()
! CALL Display( "test14" )
! CALL Display( ANGLE( obj1, obj2), "Angle = " )
! CALL Display( obj1 .ANGLE. obj2, ".Angle. = ")
! CALL Display( DEGREES( obj1 .ANGLE. obj2 ), "In degrees :: ")
! CALL DotLine()
!```

INTERFACE
MODULE PURE FUNCTION get_angle( u, v ) RESULT( Ans )
  CLASS( Vector3D_ ), INTENT( IN ) :: u
  CLASS( Vector3D_ ), INTENT( IN ) :: v
  REAL( DFP ):: Ans
END FUNCTION get_angle
END INTERFACE

INTERFACE OPERATOR( .Angle. )
  MODULE PROCEDURE get_angle
END INTERFACE OPERATOR( .Angle. )

PUBLIC :: OPERATOR( .Angle. )

INTERFACE Angle
  MODULE PROCEDURE get_angle
END INTERFACE Angle

PUBLIC :: Angle

!----------------------------------------------------------------------------
!                                                     ProjectionVector@Misc
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	25 Feb 2021
! summary: 	Returns the vector of projection from u onto v
!
!### Introduction
! Projetion of a vector $\mathbf{u}$ on \mathbf{v} is given by
!
! $$\mathbf{p} =\left( \frac{\bf{u}\cdot \bf{v}}{\bf{v} \cdot \bf{v}} \right) \bf{v}$$
!
! This function computes $\mathbf{p}$.
!
!@note
! `.PARALLEL.` operator is alias of `.ProjectionVector.`
!@endnote
!
!
!
!### Usage
!
!```fortran
! type( Vector3D_ ) :: obj1, obj2
! obj1 = [4.0_dfp, 2.0_dfp, 3.0_dfp]
! obj2 = [2.0_dfp, 0.0_dfp]
! CALL Equalline()
! CALL Display( "test15" )
! CALL Display( ProjectionVector( obj1, obj2), "PROJECTIONVector = " )
! CALL Display( obj1 .PROJECTIONVector. obj2, ".PROJECTIONVector. = ")
! CALL Display( PROJECTION(obj1, obj2), "PROJECTION = " )
! CALL Display( obj1 .PROJECTION. obj2, ".PROJECTION. = ")
! CALL DotLine()
!```

INTERFACE
MODULE PURE FUNCTION get_projection_vector_obj( u, v ) RESULT( Ans )
  CLASS( Vector3D_ ), INTENT( IN ) :: u
  CLASS( Vector3D_ ), INTENT( IN ) :: v
  TYPE( Vector3D_ ) ::  Ans
END FUNCTION get_projection_vector_obj
END INTERFACE

INTERFACE ProjectionVector
  MODULE PROCEDURE get_projection_vector_obj
END INTERFACE ProjectionVector

PUBLIC :: ProjectionVector

INTERFACE OPERATOR( .ProjectionVector. )
  MODULE PROCEDURE get_projection_vector_obj
END INTERFACE OPERATOR( .ProjectionVector. )

PUBLIC :: OPERATOR( .ProjectionVector. )

INTERFACE OPERATOR( .Parallel. )
  MODULE PROCEDURE get_projection_vector_obj
END INTERFACE OPERATOR( .Parallel. )

PUBLIC :: OPERATOR( .Parallel. )

!----------------------------------------------------------------------------
!                                                                Normal@Misc
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	25 Feb 2021
! summary: Returns the component of u normal to v.
!
!### Introduction
!
! The component of u normal to v is given by:
!
! $$\bf{n} =\bf{u} -(\bf{u} \cdot \hat{\bf{v} } )\hat{\bf{v} }$$
!
! This subroutine return the component of u normal to v
!
!### Usage
!
!```fortran
!
!```

INTERFACE
MODULE PURE FUNCTION getNormal_Vector( u, v ) RESULT( Ans )
  CLASS( Vector3D_ ), INTENT( IN ) :: u
  CLASS( Vector3D_ ), INTENT( IN ) :: v
  TYPE( Vector3D_ ) :: Ans
END FUNCTION getNormal_Vector
END INTERFACE

INTERFACE OPERATOR( .Normal. )
  MODULE PROCEDURE getNormal_Vector
END INTERFACE OPERATOR( .Normal. )

PUBLIC :: OPERATOR( .Normal. )

INTERFACE Normal
  MODULE PROCEDURE getNormal_Vector
END INTERFACE Normal

PUBLIC :: Normal

!----------------------------------------------------------------------------
!                                                           Projection@Misc
!----------------------------------------------------------------------------


!> authors: Vikas Sharma, Ph. D.
! date: 	25 Feb 2021
! summary: 	Returns the projection from u onto v
!
!### Introduction
! Projetion of a vector $\mathbf{u}$ on \mathbf{v} is given by
!
! $$p = \mathbf{u} \cdot \hat{\mathbf{v}}$$
!
! This function computes $p$
!
!
!### Usage
!
!```fortran
! type( Vector3D_ ) :: obj1, obj2
! obj1 = [4.0_dfp, 2.0_dfp, 3.0_dfp]
! obj2 = [2.0_dfp, 0.0_dfp]
! CALL Equalline()
! CALL Display( "test15" )
! CALL Display( ProjectionVector( obj1, obj2), "PROJECTIONVector = " )
! CALL Display( obj1 .PROJECTIONVector. obj2, ".PROJECTIONVector. = ")
! CALL Display( PROJECTION(obj1, obj2), "PROJECTION = " )
! CALL Display( obj1 .PROJECTION. obj2, ".PROJECTION. = ")
! CALL DotLine()
!```

INTERFACE
MODULE PURE FUNCTION get_projection_obj( u, v ) RESULT( Ans )
  CLASS( Vector3D_ ), INTENT( IN ) :: u
  CLASS( Vector3D_ ), INTENT( IN ) :: v
  REAL( DFP ) :: Ans
END FUNCTION get_projection_obj
END INTERFACE

INTERFACE Projection
  MODULE PROCEDURE get_projection_obj
END INTERFACE Projection

PUBLIC :: Projection

INTERFACE OPERATOR( .Projection. )
  MODULE PROCEDURE get_projection_obj
END INTERFACE OPERATOR( .Projection. )

PUBLIC :: OPERATOR( .Projection. )

END MODULE Vector3D_Method