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
! date: 7 March 2021
! summary: This module contains BLAS1 methods for [[RealVector_]] and [[RealMatrix_]] data type.
!

MODULE BLAS1_Method
USE GlobalData
USE BaseType
IMPLICIT NONE
PRIVATE

!----------------------------------------------------------------------------
!                                                                 DOT@BLAS1
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	25 Feb 2021
! summary: 	This routine returns dot product of two [[RealVector_]]
!
!@todo
! type(_obj_) :: obj1, obj2
! call RANDOM_NUMBER( obj1, 100 )
! call RANDOM_NUMBER( obj2, 100 )
! CALL Display( DOT(obj1, obj2), "dot 1=" )
!@endtodo

INTERFACE
MODULE PURE FUNCTION scalarDOTscalar( Obj1, Obj2 ) RESULT( Ans )
  CLASS( RealVector_ ), INTENT( IN ) :: Obj1, Obj2
  REAL( DFP ) :: Ans
END FUNCTION scalarDOTscalar
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 DOT@BLAS1
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	25 Feb 2021
! summary: 	This routines returns the dot product of vector of [[RealVector_]] data type.
!
!@todo
! type(_obj_) :: obj1(2), obj2(2)
! call RANDOM_NUMBER( obj1(1), 100 )
! call RANDOM_NUMBER( obj1(2), 100 )
! obj2 = obj1
! CALL Display( DOT(obj1, obj2), "dot =" )
!@endtodo

INTERFACE
MODULE PURE FUNCTION vectorDOTvector( Obj1, Obj2 ) RESULT( Ans )
  CLASS( RealVector_ ), INTENT( IN ) :: Obj1( : ), Obj2( : )
  REAL( DFP ) :: Ans
END FUNCTION vectorDOTvector
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 DOT@BLAS1
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	25 Feb 2021
! summary: 	This routine computes dot product of a vector of [[RealVector_]] and scalar of [[RealVector_]]
!
!@todo
! type(_obj_) :: obj1(2), obj2(2)
! call RANDOM_NUMBER( obj1(1), 100 )
! call RANDOM_NUMBER( obj1(2), 100 )
! obj2 = obj1
! CALL Display( DOT(obj1, obj2), "dot =" )
!@endtodo

INTERFACE
MODULE PURE FUNCTION vectorDOTscalar( Obj1, Obj2 ) RESULT( Ans )
  CLASS( RealVector_ ), INTENT( IN ) :: Obj1( : ), Obj2
  REAL( DFP ) :: Ans
END FUNCTION vectorDOTscalar
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 DOT@BLAS1
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	25 Feb 2021
! summary: 	This routine computes dot product of a scalar of [[RealVector_]] and vector of [[RealVector_]]
!
!### Usage
!
!```fortran
! type(_obj_) :: obj1
! real( dfp ) :: val( 100 )
! call RANDOM_NUMBER( obj1, 100 )
! call RANDOM_NUMBER( val )
! CALL Display( DOT(obj1, val), "dot =" )
!```

INTERFACE
MODULE PURE FUNCTION scalarDOTvector( Obj1, Obj2 ) RESULT( Ans )
  CLASS( RealVector_ ), INTENT( IN ) :: Obj1, Obj2( : )
  REAL( DFP ) :: Ans
END FUNCTION scalarDOTvector
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 DOT@BLAS1
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	25 Feb 2021
! summary: 	This routine computes dot product of a fortran vector and a scalar instance of [[RealVector_]]
!
!@todo
! type(_obj_) :: obj1
! real( dfp ) :: val( 100 )
! call RANDOM_NUMBER( obj1, 100 )
! call RANDOM_NUMBER( val )
! CALL Display( DOT(obj1, val), "dot =" )
!@endtodo

INTERFACE
MODULE PURE FUNCTION intrinsicDOTscalar( Val, Obj ) RESULT( Ans )
  REAL ( DFP ), INTENT( IN ) :: Val( : )
  CLASS( RealVector_ ), INTENT( IN ) :: Obj
  REAL( DFP ) :: Ans
END FUNCTION intrinsicDOTscalar
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 DOT@BLAS1
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	25 Feb 2021
! summary: 	This routine computes dot product of a fortran array and scalar instance of [[RealVector_]]
!
!@todo
! type(_obj_) :: obj1
! real( dfp ) :: val( 100 )
! call RANDOM_NUMBER( obj1, 100 )
! call RANDOM_NUMBER( val )
! CALL Display( DOT(obj1, val), "dot =" )
!@endtodo

INTERFACE
MODULE PURE FUNCTION scalarDOTintrinsic( Obj, Val ) RESULT( Ans )
  REAL ( DFP ), INTENT( IN ) :: Val( : )
  CLASS( RealVector_ ), INTENT( IN ) :: Obj
  REAL( DFP ) :: Ans
END FUNCTION scalarDOTintrinsic
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 DOT@BLAS1
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	25 Feb 2021
! summary: 	This generic routine computes dot product

INTERFACE DOT
  MODULE PROCEDURE scalarDOTscalar, vectorDOTvector, vectorDOTscalar, &
    & scalarDOTvector, scalarDOTintrinsic
END INTERFACE DOT

PUBLIC :: DOT

INTERFACE OPERATOR(.DOT.)
  MODULE PROCEDURE scalarDOTscalar, vectorDOTvector, vectorDOTscalar, &
    & scalarDOTvector, scalarDOTintrinsic, intrinsicDOTscalar
END INTERFACE OPERATOR(.DOT.)

PUBLIC :: OPERATOR(.DOT.)

!----------------------------------------------------------------------------
!                                                                Norm2@BLAS1
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	25 Feb 2021
! summary: 	This function computes Euclidean norm of [[RealVector_]]
!
!### Introduction
!
! L2 norm of a vector is give by
!
! $$\left| \left| \bf{V} \right|  \right|  =\sqrt{\bf{V} \cdot \bf{V} }$$
!
!@note
! 	This subroutine uses DOT function.
!@endnote
!
!### Usage
!
!```fortran
!	s = NORM2(Obj)
!```

INTERFACE
MODULE PURE FUNCTION NRM2scalar( Obj ) RESULT( Ans )
  CLASS( RealVector_ ), INTENT( IN ) :: Obj
  REAL( DFP ) :: Ans
END FUNCTION NRM2scalar
END INTERFACE

!----------------------------------------------------------------------------
!                                                                Norm2@BLAS1
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	25 Feb 2021
! summary: 	This routine computes the L2 norm of [[RealVector_]]
!
!### Introduction
!
! This routine computes L2 norm of a vector of [[RealVector_]].
!
!@note
! 	This function employs DOT function.
!@endnote
!
!@todo
! type(_obj_) :: obj1
! real( dfp ) :: val( 100 )
! call RANDOM_NUMBER( obj1, 100 )
! call RANDOM_NUMBER( val )
! CALL Display( DOT(obj1, val), "dot =" )
!@endtodo

INTERFACE
MODULE PURE FUNCTION NRM2vector( Obj ) RESULT( Ans )
  CLASS( RealVector_ ), INTENT( IN ) :: Obj( : )
  REAL( DFP ) :: Ans
END FUNCTION NRM2vector
END INTERFACE

!----------------------------------------------------------------------------
!                                                                Norm2@BLAS1
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	25 Feb 2021
! summary: 	This routine computes norm2
!
!@note
! type(_obj_) :: obj1
! real( dfp ) :: val( 100 )
! call RANDOM_NUMBER( obj1, 100 )
! call RANDOM_NUMBER( val )
! CALL Display( DOT(obj1, val), "dot =" )
!@endnote

INTERFACE NORM2
  MODULE PROCEDURE NRM2scalar, NRM2vector
END INTERFACE NORM2

PUBLIC :: NORM2

!----------------------------------------------------------------------------
!                                                             Norm2SQR@BLAS1
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	25 Feb 2021
! summary: 	This routine computes square of L2 norm of [[RealVector_]]
!
!@note
! 	This function internally calls DOT function
!@endnote
!
!@todo
! type(_obj_) :: obj1
! real( dfp ) :: val( 100 )
! call RANDOM_NUMBER( obj1, 100 )
! call RANDOM_NUMBER( val )
! CALL Display( DOT(obj1, val), "dot =" )
!@endtodo

INTERFACE
MODULE PURE FUNCTION NRM2SQRscalar( Obj ) RESULT( Ans )
  CLASS( RealVector_ ), INTENT( IN ) :: Obj
  REAL( DFP ) :: Ans
END FUNCTION NRM2SQRscalar
END INTERFACE

!----------------------------------------------------------------------------
!                                                             Norm2SQR@BLAS1
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	25 Feb 2021
! summary: 	This routine computes square of L2 norm of [[RealVector_]]
!
!### Introduction
!
! $$\text{NRM2SQR}=(\left| \left| \bf{V} \right|  \right|_{2}  )^{2}=DOT\left( V,V\right)$$
!
!@note
! 	This function internally calls DOT function
!@endnote
!
!@todo
! 	usage
!@endtodo

INTERFACE
MODULE PURE FUNCTION NRM2SQRvector( Obj ) RESULT( Ans )
  CLASS( RealVector_ ), INTENT( IN ) :: Obj( : )
  REAL( DFP ) :: Ans
END FUNCTION NRM2SQRvector
END INTERFACE

!----------------------------------------------------------------------------
!                                                             Norm2SQR@BLAS1
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	25 Feb 2021
! summary: 	This routine computes square of L2 norm of fortran vector
!
!### Introduction
!
! $$\text{NRM2SQR}=(\left| \left| \bf{V} \right|  \right|_{2}  )^{2}=DOT\left( V,V\right)$$
!
!@note
! 	This function internally calls DOT function
!@endnote
!
!@todo
! type(_obj_) :: obj, obj1( 2 )
! obj = RealVector( [1,2,3] )
! call display( NORM2( obj ), "norm2 = " )
! call display( NORM2SQR( obj ), "norm2sqr = " )
! obj1(1) = RealVector( [1,2,3] )
! obj1(2) = RealVector( [1,2,3] )
! call display( NORM2( obj1 ), "norm2 = " )
! call display( NORM2SQR( obj1 ), "norm2sqr = " )
!@endtodo

INTERFACE
MODULE PURE FUNCTION NRM2SQRintrinsic( Val ) RESULT( Ans )
  REAL( DFP ), INTENT( IN ) :: Val( : )
  REAL( DFP ) :: Ans
END FUNCTION NRM2SQRintrinsic
END INTERFACE

!----------------------------------------------------------------------------
!                                                             Norm2SQR@BLAS1
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	25 Feb 2021
! summary: 	This is a generic function which computers square of L2 norm of a vector.
!
!### Introduction
!
! $$\text{NRM2SQR}=(\left| \left| \bf{V} \right|  \right|_{2}  )^{2}=DOT\left( V,V\right)$$
!
!@note
! 	This function internally calls DOT function
!@endnote
!
!@todo
! type(_obj_) :: obj, obj1( 2 )
! obj = RealVector( [1,2,3] )
! call display( NORM2( obj ), "norm2 = " )
! call display( NORM2SQR( obj ), "norm2sqr = " )
! obj1(1) = RealVector( [1,2,3] )
! obj1(2) = RealVector( [1,2,3] )
! call display( NORM2( obj1 ), "norm2 = " )
! call display( NORM2SQR( obj1 ), "norm2sqr = " )
!@endtodoe

INTERFACE NORM2SQR
  MODULE PROCEDURE NRM2SQRscalar, NRM2SQRvector, NRM2SQRintrinsic
END INTERFACE NORM2SQR

PUBLIC :: NORM2SQR

!----------------------------------------------------------------------------
!                                                                 ASUM@BLAS1
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	25 Feb 2021
! summary: 	This function computes the absolute sum of a vector
!### Introduction
!
! This function computes the absolute sum of a vector.
!
! $$\left| \left| V\right|  \right|_{1}  =\sum^{N}_{i=1} \left( \  \left| V_{i}\right|  \right)$$
!
!@note
! 	This function calls BLAS function ASUM.
!@endnote
!
!@todo
! 	usage
!@endtodo

INTERFACE
MODULE PURE FUNCTION ASUMScalar( Obj ) RESULT( Ans )
  CLASS( RealVector_ ), INTENT( IN ) :: Obj
  REAL( DFP ) :: Ans
END FUNCTION ASUMScalar
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 ASUM@BLAS1
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	25 Feb 2021
! summary: 	This function computes the absolute sum of a vector
!### Introduction
!
! This function computes the absolute sum of a vector.
!
! $$\left| \left| V\right|  \right|_{1}  =\sum^{N}_{i=1} \left( \  \left| V_{i}\right|  \right)$$
!
!@note
! 	This function calls BLAS function ASUM.
!@endnote
!
!@todo
! 	usage
!@endtodo

INTERFACE
MODULE PURE FUNCTION ASUMvector( Obj ) RESULT( Ans )
  CLASS( RealVector_ ), INTENT( IN ) :: Obj( : )
  REAL( DFP ) :: Ans
END FUNCTION ASUMvector
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 ASUM@BLAS1
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	25 Feb 2021
! summary: 	This function computes the absolute sum of a vector
!### Introduction
!
! This function computes the absolute sum of a vector.
!
! $$\left| \left| V\right|  \right|_{1}  =\sum^{N}_{i=1} \left( \  \left| V_{i}\right|  \right)$$
!
!@note
! 	This function calls BLAS function ASUM.
!@endnote
!
!@todo
! 	usage
!@endtodo

INTERFACE ASUM
  MODULE PROCEDURE ASUMScalar, ASUMvector
END INTERFACE ASUM

PUBLIC :: ASUM

!----------------------------------------------------------------------------
!                                                          SHALLOWCOPY@BLAS1
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE intrinsicSHALLOWCOPYintrinsic( Y, X )
  REAL( DFP ), ALLOCATABLE, INTENT( INOUT ) :: Y( : )
  REAL( DFP ), INTENT( IN ) :: X( : )
END SUBROUTINE intrinsicSHALLOWCOPYintrinsic
END INTERFACE

!----------------------------------------------------------------------------
!                                                          SHALLOWCOPY@BLAS1
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE scalarSHALLOWCOPYscalar( Y, X )
  TYPE( RealVector_ ), INTENT( INOUT ) :: Y
  CLASS( RealVector_ ), INTENT( IN ) :: X
END SUBROUTINE scalarSHALLOWCOPYscalar
END INTERFACE

!----------------------------------------------------------------------------
!                                                          SHALLOWCOPY@BLAS1
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE vectorSHALLOWCOPYvector( Y, X )
  TYPE( RealVector_ ), INTENT( INOUT ), ALLOCATABLE :: Y( : )
  CLASS( RealVector_ ), INTENT( IN ) :: X( : )
END SUBROUTINE vectorSHALLOWCOPYvector
END INTERFACE

!----------------------------------------------------------------------------
!                                                          SHALLOWCOPY@BLAS1
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE scalarSHALLOWCOPYvector( Y, X )
  TYPE( RealVector_ ), INTENT( INOUT ) :: Y
  CLASS( RealVector_ ), INTENT( IN ) :: X( : )
END SUBROUTINE scalarSHALLOWCOPYvector
END INTERFACE

!----------------------------------------------------------------------------
!                                                          SHALLOWCOPY@BLAS1
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE scalarSHALLOWCOPYintrinsic( Y, X )
  CLASS( RealVector_ ), INTENT( INOUT ) :: Y
  REAL( DFP ), INTENT( IN ) :: X( : )
END SUBROUTINE scalarSHALLOWCOPYintrinsic
END INTERFACE

!----------------------------------------------------------------------------
!                                                          SHALLOWCOPY@BLAS1
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE intrinsicSHALLOWCOPYscalar( Y, X )
  REAL( DFP ), ALLOCATABLE, INTENT( INOUT ) :: Y( : )
  CLASS( RealVector_ ), INTENT( IN ) :: X
END SUBROUTINE intrinsicSHALLOWCOPYscalar
END INTERFACE

!----------------------------------------------------------------------------
!                                                          SHALLOWCOPY@BLAS1
!----------------------------------------------------------------------------

INTERFACE SHALLOWCOPY
  MODULE PROCEDURE intrinsicSHALLOWCOPYintrinsic, scalarSHALLOWCOPYscalar, &
    & vectorSHALLOWCOPYvector, scalarSHALLOWCOPYvector, &
    & scalarSHALLOWCOPYintrinsic
END INTERFACE SHALLOWCOPY

PUBLIC :: SHALLOWCOPY

!----------------------------------------------------------------------------
!                                                                 COPY@BLAS1
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	25 Feb 2021
! summary: 	This routine copies one vector into another
!
!### Introduction
! This subroutine copies one [[RealVector_]] object into another object, i.e. `Obj1=Obj2`. See figure given below:
!
!
! <img src=|media|/scalar_copy_scalar.jpg alt="drawing" style="max-width:500px;"/>
!
!@note
! 	This subroutine internally uses [[intrinsicCOPYintrinsic]] routine.
!@endnote
!
!@todo
! usage
!@endtodo

INTERFACE
MODULE PURE SUBROUTINE scalarCOPYscalar( Y, X )
  TYPE( RealVector_ ), INTENT( INOUT ) :: Y
  CLASS( RealVector_ ), INTENT( IN ) :: X
END SUBROUTINE scalarCOPYscalar
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 COPY@BLAS1
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	25 Feb 2021
! summary: 	This routine copies one vector into another
!
!### Introduction
! This subroutine copy a vector of [[RealVector_]] into another vector, i.e. `Obj1=Obj2` see the figure below:
!
! <img src=|media|/vector_copy_vector.jpg alt="drawing" style="max-width:500px;"/>
!
!@note
! 	This subroutine internally uses [[intrinsicCOPYintrinsic]] routine. Also note that `Obj1` and `Obj2` are vectors of [[RealVector_]] data type.
!@endnote
!
!@todo
! usage
!@endtodo

INTERFACE
MODULE PURE SUBROUTINE vectorCOPYvector( Y, X )
  TYPE( RealVector_ ), INTENT( INOUT ), ALLOCATABLE :: Y( : )
  CLASS( RealVector_ ), INTENT( IN ) :: X( : )
END SUBROUTINE vectorCOPYvector
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 COPY@BLAS1
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	25 Feb 2021
! summary: 	This routine copies one vector into another
!
!### Introduction
! This subroutine copies a vector of [[RealVector_]] into a scalar instance of [[RealVector_]]. See Figure below:
!
! <img src=|media|/scalar_copy_vector.jpg alt="drawing" style="max-width:500px;"/>
!
!@note
! 	This subroutine internally uses [[intrinsicCOPYintrinsic]] routine.
!
!@todo
! usage
!@endtodo

INTERFACE
MODULE PURE SUBROUTINE scalarCOPYvector( Y, X )
  TYPE( RealVector_ ), INTENT( INOUT ) :: Y
  CLASS( RealVector_ ), INTENT( IN ) :: X( : )
END SUBROUTINE scalarCOPYvector
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 COPY@BLAS1
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	25 Feb 2021
! summary: 	This routine copies one vector into another
!
!### Introduction
! This subroutine copy a fortran vector into [[RealVector_]] obj, i.e. `Obj=Val`
!
!@note
! 	This subroutine internally uses [[intrinsicCOPYintrinsic]] routine.
!@endnote
!
!@todo
! usage
!@endtodo

INTERFACE
MODULE PURE SUBROUTINE scalarCOPYintrinsic( Y, X )
  CLASS( RealVector_ ), INTENT( INOUT ) :: Y
  REAL( DFP ), INTENT( IN ) :: X( : )
END SUBROUTINE scalarCOPYintrinsic
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 COPY@BLAS1
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	25 Feb 2021
! summary: 	This routine copies one vector into another
!
!### Introduction
! This subroutine copy an instance of [[RealVector_]] in another fortran vector, i.e. `Val=Obj`
!
!@note
! 	This subroutine internally calls [[intrinsicCOPYintrinsic]]. Also `Val` is allocatable.
!@endnote
!
!@todo
! usage
!@endtodo

INTERFACE
MODULE PURE SUBROUTINE intrinsicCOPYscalar( Y, X )
  REAL( DFP ), ALLOCATABLE, INTENT( INOUT ) :: Y( : )
  CLASS( RealVector_ ), INTENT( IN ) :: X
END SUBROUTINE intrinsicCOPYscalar
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 COPY@BLAS1
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	26 Feb 2021
! summary: 	This is generic subroutine for copying.
!
!### Introduction
! See
! * [[intrinsicCOPYintrinsic]]
! * [[scalarCOPYscalar]]
! * [[vectorCOPYvector]]
! * [[scalarCOPYvector]]
! * [[scalarCOPYintrinsic]]

INTERFACE COPY
  MODULE PROCEDURE scalarCOPYscalar, &
    & vectorCOPYvector, scalarCOPYvector, scalarCOPYintrinsic
END INTERFACE COPY

PUBLIC :: COPY

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE ASSIGNMENT( = )
  MODULE PROCEDURE scalarCOPYscalar, &
    & vectorCOPYvector, scalarCOPYvector, scalarCOPYintrinsic
END INTERFACE ASSIGNMENT( = )

PUBLIC :: ASSIGNMENT( = )

!----------------------------------------------------------------------------
!                                                                 SWAP@BLAS1
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE scalarSWAPscalar( X, Y )
  CLASS( RealVector_ ), INTENT( INOUT ) :: X
  CLASS( RealVector_ ), INTENT( INOUT ) :: Y
END SUBROUTINE scalarSWAPscalar

MODULE PURE SUBROUTINE vectorSWAPvector( X, Y )
  CLASS( RealVector_ ), INTENT( INOUT ) :: X( : )
  CLASS( RealVector_ ), INTENT( INOUT ) :: Y( : )
END SUBROUTINE vectorSWAPvector

MODULE PURE SUBROUTINE scalarSWAPintrinsic( X, Y )
  CLASS( RealVector_ ), INTENT( INOUT ) :: X
  REAL( DFP ), INTENT( INOUT ) :: Y( : )
END SUBROUTINE scalarSWAPintrinsic
END INTERFACE

INTERFACE SWAP
  MODULE PROCEDURE scalarSWAPscalar, &
    & vectorSWAPvector, scalarSWAPintrinsic
END INTERFACE SWAP

PUBLIC :: SWAP

!----------------------------------------------------------------------------
!                                                                SCALE@BLAS1
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE SCALscalar( X, A )
  CLASS ( RealVector_ ), INTENT( INOUT ) :: X
  REAL( DFP ), INTENT( IN ) :: A
END SUBROUTINE SCALscalar

MODULE PURE SUBROUTINE SCALvector( X, A )
  CLASS ( RealVector_ ), INTENT( INOUT ) :: X( : )
  REAL( DFP ), INTENT( IN ) :: A
END SUBROUTINE SCALvector
END INTERFACE

INTERFACE SCAL
  MODULE PROCEDURE SCALscalar, SCALvector
END INTERFACE SCAL

PUBLIC :: SCAL

!----------------------------------------------------------------------------
!                                                                  AXPY@BLAS1
!----------------------------------------------------------------------------

INTERFACE

! Y = Y + A*X
MODULE PURE SUBROUTINE scalarAXPYscalar( X, Y, A )
  CLASS( RealVector_ ), INTENT( IN ) :: X
  CLASS( RealVector_ ), INTENT( INOUT ) :: Y
  REAL( DFP ), INTENT( IN ) :: A
END SUBROUTINE scalarAXPYscalar
END INTERFACE

INTERFACE
MODULE PURE SUBROUTINE vectorAXPYvector( X, Y, A )
  CLASS( RealVector_ ), INTENT( IN ) :: X( : )
  CLASS( RealVector_ ), INTENT( INOUT ) :: Y( : )
  REAL( DFP ), INTENT( IN ) :: A
END SUBROUTINE vectorAXPYvector
END INTERFACE

INTERFACE
MODULE PURE SUBROUTINE scalarAXPYintrinsic( X, Y, A )
  REAL( DFP ), INTENT( IN ) :: X( : )
  CLASS( RealVector_ ), INTENT( INOUT ) :: Y
  REAL( DFP ), INTENT( IN ) :: A
END SUBROUTINE scalarAXPYintrinsic
END INTERFACE

INTERFACE AXPY
  MODULE PROCEDURE scalarAXPYscalar, vectorAXPYvector, &
    & scalarAXPYintrinsic
END INTERFACE AXPY

PUBLIC :: AXPY

END MODULE BLAS1_Method