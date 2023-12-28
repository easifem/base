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

!> author: Vikas Sharma, Ph. D.
! date: 7 March 2021
! summary: This module contains BLAS1 methods

MODULE RealVector_Blas1Methods
USE GlobalData
USE BaseType
IMPLICIT NONE
PRIVATE
PUBLIC :: ASUM
PUBLIC :: AXPY
PUBLIC :: COPY
PUBLIC :: Compact
PUBLIC :: DOT_PRODUCT
PUBLIC :: NORM2
PUBLIC :: NORM1
PUBLIC :: NORMi
PUBLIC :: SWAP
PUBLIC :: SCAL
PUBLIC :: PMUL
PUBLIC :: PDIV
PUBLIC :: Reciprocal

!----------------------------------------------------------------------------
!                                                                 ASUM@BLAS1
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         25 Feb 2021
! summary:         This function computes the absolute sum of a vector
!
!# Introduction
!
! This function computes the absolute sum of a vector.
!
! $$\left| \left| V\right|  \right|_{1}  =\sum^{N}_{i=1} \left( \
! \left| V_{i}\right|  \right)$$
!
!@note
! This function calls BLAS function ASUM.
!@endnote
!
!@todo
! subroutine test1
!   type( RealVector_ ) :: obj
!   real( dfp ) :: ans
!   obj = RealVector(arange(1,1000,1))
!   ans = asum(obj)
!   call display( ans-sum(obj%val), "test1: 0 if correct : " )
! end
!@endtodo

INTERFACE asum
  MODULE FUNCTION ASUMScalar(obj) RESULT(ans)
    CLASS(RealVector_), INTENT(IN) :: obj
    REAL(DFP) :: ans
  END FUNCTION ASUMScalar
END INTERFACE asum

!----------------------------------------------------------------------------
!                                                                 ASUM@BLAS1
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         25 Feb 2021
! summary:         This function computes the absolute sum of a vector
!# Introduction
!
! This function computes the absolute sum of a vector.
!
! $$\left| \left| V\right|  \right|_{1}  =\sum^{N}_{i=1} \left( \
! \left| V_{i}\right|  \right)$$
!
!@note
!         This function calls [[ASUMScalar]] method
!@endnote
!
!### Usage
!
!```fortran
!   integer( i4b ), parameter :: n = 10, m=5
!   integer( i4b ) :: i
!   type( RealVector_ ) :: obj( m )
!   real( dfp ) :: ans_l(m), ans
!   do i = 1, m
!     obj( i ) = RealVector(arange(1,n,1))
!   end do
!   ans = 0.0
!   !$omp parallel default(shared) private( i ) reduction(+:ans)
!   CALL OMP_INITIATE
!   !$omp do
!   do i = 1, m
!     ans = ans + asum(obj(i)) !! no parallel
!   enddo
!   !$omp enddo
!   CALL OMP_FINALIZE
!   !$omp end parallel
!   call display( ans - (m*sum(obj(1)%val)), "test2: 0 if correct : " )
!```
!
! Another example
!
!### Usage
!
!```fortran
! integer( i4b ), parameter :: n = 100, m=5
! integer( i4b ) :: i
! type( RealVector_ ) :: obj( m )
! real( dfp ) :: ans_l(m), ans
! do i = 1, m
!   obj( i ) = RealVector(arange(1,n,1))
! end do
! ans = ASUM(obj)
! call display( ans - (m*sum(obj(1)%val)), "test3: 0 if correct : " )
!```

INTERFACE asum
  MODULE FUNCTION ASUMvector(obj) RESULT(ans)
    CLASS(RealVector_), INTENT(IN) :: obj(:)
    REAL(DFP) :: ans
  END FUNCTION ASUMvector
END INTERFACE asum

!----------------------------------------------------------------------------
!                                                                  AXPY@BLAS1
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         8 March 2021
! summary: This subroutine computes AXPY
!
!# Introduction
!
! This subroutine performs following task
!
! $$Y=Y+A*X$$
!
! Y = Y + A*X
!
! Here A is an scalar
!
!@note
! In joined state this subroutine creates new threads and share the work.
! If this subroutine is called within parallel block (i.e., forked state)
! then it does not create any new threads. Each thread will call this
! subroutine while X, Y, A treated as shared type.
!@endnote
!
!### Usage
!
!```fortran
! integer( i4b ), parameter :: n = 100
! integer( i4b ) :: i
! real( dfp ) :: a = 1.0_DFP
! type( RealVector_ ) :: x, y, z
! call random_number( x, n )
! call random_number( y, n )
! z%val = y%val + a * x%val
! call AXPY( x = x, y = y, A = a  )
! call display( asum(y%val - z%val), "test4: 0 if correct : " )
!```

INTERFACE AXPY
  MODULE SUBROUTINE scalarAXPYscalar(X, Y, A)
    CLASS(RealVector_), INTENT(IN) :: X
    CLASS(RealVector_), INTENT(INOUT) :: Y
    REAL(DFP), INTENT(IN) :: A
  END SUBROUTINE scalarAXPYscalar
END INTERFACE AXPY

!----------------------------------------------------------------------------
!                                                                  AXPY@BLAS1
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 8 March 2021
! summary: This subroutine computes AXPY
!
!# Introduction
!         This subroutine performs following task
!
! $$Y=Y+A*X$$
!
! Here A is an scalar
!
!@note
! In joined state this subroutine creates new threads and share the work.
! If this subroutine is called within parallel block (i.e., forked state)
! then it does not create any new threads. Each thread will call this
! subroutine while X, Y, A treated as shared type.
!@endnote
!

INTERFACE AXPY
  MODULE SUBROUTINE scalarAXPYintrinsic(X, Y, A)
    REAL(DFP), INTENT(IN) :: X(:)
    CLASS(RealVector_), INTENT(INOUT) :: Y
    REAL(DFP), INTENT(IN) :: A
  END SUBROUTINE scalarAXPYintrinsic
END INTERFACE AXPY

!----------------------------------------------------------------------------
!                                                                  AXPY@BLAS1
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 8 March 2021
! summary: This subroutine computes AXPY
!
!# Introduction
! This subroutine performs `AXPY` operation. It performs the following task.
!
! $$Y(i)=Y(i)+A(i)*X(i)$$
!
! Here A is an vector of length same as size of `X` or `Y`.
!
!@note
! In joined state this subroutine creates new threads and share the work.
! If this subroutine is called within parallel block (i.e., forked state)
! then it does not create any new threads. Each thread will call this
! subroutine while X, Y, A treated as shared type.
!@endnote
!
!### Usage
!
!```fortran
! integer( i4b ), parameter :: n = 100, m = 4
! integer( i4b ) :: i, tsize(m)
! real( dfp ) :: a( m ), ans
! type( RealVector_ ), allocatable :: x( : ), y( : ), z( : )
! tsize = m; a = 1.0
! call random_number( x, tsize )
! call random_number( y, tsize )
! call initiate( z, tsize )
! do i = 1, m
!   z(i)%val = y(i)%val + a( i ) * x(i)%val
! end do
! call AXPY( x = x, y = y, A = a )
! ans = 0.0
! do i = 1, m
!   ans = ans + asum( y(i)%val - z(i)%val )
! end do
! call display( ans, "test5: 0 if correct : " )
!```

INTERFACE AXPY
  MODULE SUBROUTINE vectorAXPYvector(X, Y, A)
    CLASS(RealVector_), INTENT(IN) :: X(:)
    CLASS(RealVector_), INTENT(INOUT) :: Y(:)
    REAL(DFP), INTENT(IN) :: A(:)
  END SUBROUTINE vectorAXPYvector
END INTERFACE AXPY

!----------------------------------------------------------------------------
!                                                                 COPY@BLAS1
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 25 Feb 2021
! summary: This routine copies one vector into another
!
!# Introduction
! This subroutine copies one [[RealVector_]] object into another object, i.e.
! `Y=X`. See figure given below:
!
! <img src=|media|/scalar_copy_scalar.jpg alt="drawing"
! style="max-width:500px;"/>
!
!@note
! This subroutine internally uses [[intrinsicCOPYintrinsic]] routine.
!@endnote
!
!### Usage
!
!```fortran
! integer( i4b ), parameter :: n = 10000
! type( RealVector_ ) :: x, y
! real( dfp ), allocatable :: z( : )
! call random_number( x, n )
! call copy( x = x, y = y )
! call display( asum( x%val - y%val ), "test6: 0 if correct : " )
! call copy( y=z, x=x )
! call display( asum( z - x%val ), "test6: 0 if correct : " )
! call copy( y=x, x=z )
! call display( asum( z - x%val ), "test6: 0 if correct : " )
!```

INTERFACE copy
  MODULE SUBROUTINE scalarCOPYscalar(Y, X)
    TYPE(RealVector_), INTENT(INOUT) :: Y
    CLASS(RealVector_), INTENT(IN) :: X
  END SUBROUTINE scalarCOPYscalar
END INTERFACE copy

!----------------------------------------------------------------------------
!                                                                 COPY@BLAS1
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 25 Feb 2021
! summary: This routine copies one vector into another
!
!# Introduction
! This subroutine copy a fortran vector into [[RealVector_]] obj, i.e. `Y=X`
!
!@note
! This subroutine internally uses [[intrinsicCOPYintrinsic]] routine.
!@endnote
!
!### Usage
!
!```fortran
! integer( i4b ), parameter :: n = 10000
! type( RealVector_ ) :: x, y
! real( dfp ), allocatable :: z( : )
! call random_number( x, n )
! call copy( x = x, y = y )
! call display( asum( x%val - y%val ), "test6: 0 if correct : " )
! call copy( y=z, x=x )
! call display( asum( z - x%val ), "test6: 0 if correct : " )
! call copy( y=x, x=z )
! call display( asum( z - x%val ), "test6: 0 if correct : " )
!```

INTERFACE copy
  MODULE SUBROUTINE scalarCOPYintrinsic_1a(Y, X)
    CLASS(RealVector_), INTENT(INOUT) :: Y
    REAL(REAL32), INTENT(IN) :: X(:)
  END SUBROUTINE scalarCOPYintrinsic_1a
END INTERFACE copy

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE copy
  MODULE SUBROUTINE scalarCOPYintrinsic_1b(Y, X)
    CLASS(RealVector_), INTENT(INOUT) :: Y
    REAL(REAL64), INTENT(IN) :: X(:)
  END SUBROUTINE scalarCOPYintrinsic_1b
END INTERFACE copy

!----------------------------------------------------------------------------
!                                                                 COPY@BLAS1
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 25 Feb 2021
! summary: This routine copies one vector into another
!
!# Introduction
! This subroutine copy an instance of [[RealVector_]] in another fortran
! vector, i.e. `Val=obj`
!
!@note
! This subroutine internally calls [[intrinsicCOPYintrinsic]]. Also `Val`
! is allocatable.
!@endnote
!
!
!### Usage
!
!```fortran
! integer( i4b ), parameter :: n = 10000
! type( RealVector_ ) :: x, y
! real( dfp ), allocatable :: z( : )
! call random_number( x, n )
! call copy( x = x, y = y )
! call display( asum( x%val - y%val ), "test6: 0 if correct : " )
! call copy( y=z, x=x )
! call display( asum( z - x%val ), "test6: 0 if correct : " )
! call copy( y=x, x=z )
! call display( asum( z - x%val ), "test6: 0 if correct : " )
!```

INTERFACE copy
  MODULE SUBROUTINE intrinsicCOPYscalar_1a(Y, X)
    REAL(REAL32), ALLOCATABLE, INTENT(INOUT) :: Y(:)
    CLASS(RealVector_), INTENT(IN) :: X
  END SUBROUTINE intrinsicCOPYscalar_1a
END INTERFACE copy

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE copy
  MODULE SUBROUTINE intrinsicCOPYscalar_1b(Y, X)
    REAL(REAL64), ALLOCATABLE, INTENT(INOUT) :: Y(:)
    CLASS(RealVector_), INTENT(IN) :: X
  END SUBROUTINE intrinsicCOPYscalar_1b
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 COPY@BLAS1
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 25 Feb 2021
! summary: This routine copies one vector into another
!
!# Introduction
! This subroutine copy a vector of [[RealVector_]] into another vector, i.e.
! `obj1=obj2` see the figure below:
!
! <img src=|media|/vector_copy_vector.jpg alt="drawing"
! style="max-width:500px;"/>
!
!@note
!This subroutine internally uses [[intrinsicCOPYintrinsic]] routine. Also
! note that `obj1` and `obj2` are vectors of [[RealVector_]] data type.
!@endnote
!
!
!### Usage
!
!```fortran
! integer( i4b ), parameter :: n = 10000, m = 5
! type( RealVector_ ), allocatable :: x( : ), y( : )
! integer( i4b ) :: tsize( m ), i
! real( dfp ), allocatable :: z( : )
! real( dfp ) :: ans
! tsize = n
! call random_number(x, tsize)
! call copy( x = x, y = y )
! ans = 0.0
! do i = 1, size( x )
!   ans = ans + asum( x(i)%val - y(i)%val )
! end do
! call display( ans, "test7: 0 if correct : " )
!```

INTERFACE copy
  MODULE SUBROUTINE vectorCOPYvector(Y, X)
    TYPE(RealVector_), INTENT(INOUT), ALLOCATABLE :: Y(:)
    CLASS(RealVector_), INTENT(IN) :: X(:)
  END SUBROUTINE vectorCOPYvector
END INTERFACE copy

!----------------------------------------------------------------------------
!                                                                 COPY@BLAS1
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 25 Feb 2021
! summary: This routine copies one vector into another
!
!# Introduction
! This subroutine copies a vector of [[RealVector_]] into a scalar instance
! of [[RealVector_]]. See Figure below:
!
! <img src=|media|/scalar_copy_vector.jpg alt="drawing"
! style="max-width:500px;"/>
!
!@note
! This subroutine internally uses [[intrinsicCOPYintrinsic]] routine.
!@endnote
!
!@todo
! need parallel
!@endtodo

INTERFACE copy
  MODULE SUBROUTINE scalarCOPYvector(Y, X)
    TYPE(RealVector_), INTENT(INOUT) :: Y
    CLASS(RealVector_), INTENT(IN) :: X(:)
  END SUBROUTINE scalarCOPYvector
END INTERFACE copy

!----------------------------------------------------------------------------
!                                                            Compact@BLAS1V
!----------------------------------------------------------------------------

INTERFACE Compact
  MODULE SUBROUTINE Compact_real_1(Val, row)
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: Val(:)
    INTEGER(I4B), INTENT(IN) :: row
  END SUBROUTINE
END INTERFACE Compact

!----------------------------------------------------------------------------
!                                                            Compact@BLAS1V
!----------------------------------------------------------------------------

INTERFACE Compact
  MODULE SUBROUTINE Compact_int_1(Val, row)
    INTEGER(I4B), ALLOCATABLE, INTENT(INOUT) :: Val(:)
    INTEGER(I4B), INTENT(IN) :: row
  END SUBROUTINE
END INTERFACE Compact

!----------------------------------------------------------------------------
!                                                                 DOT@BLAS1
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 25 Feb 2021
! summary: This routine returns dot product of two [[RealVector_]]
!
!@todo
! type(_obj_) :: obj1, obj2
! call RANDOM_NUMBER( obj1, 100 )
! call RANDOM_NUMBER( obj2, 100 )
! CALL Display( DOT(obj1, obj2), "dot 1=" )
!@endtodo

INTERFACE DOT_PRODUCT
  MODULE PURE FUNCTION scalarDOTscalar(obj1, obj2) RESULT(ans)
    CLASS(RealVector_), INTENT(IN) :: obj1, obj2
    REAL(DFP) :: ans
  END FUNCTION scalarDOTscalar
END INTERFACE DOT_PRODUCT

!----------------------------------------------------------------------------
!                                                                 DOT@BLAS1
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 25 Feb 2021
! summary: This routine computes dot product of a fortran array and scalar
! instance of [[RealVector_]]
!
!@todo
! type(_obj_) :: obj1
! real( dfp ) :: val( 100 )
! call RANDOM_NUMBER( obj1, 100 )
! call RANDOM_NUMBER( val )
! CALL Display( DOT(obj1, val), "dot =" )
!@endtodo

INTERFACE DOT_PRODUCT
  MODULE PURE FUNCTION scalarDOTintrinsic(obj, Val) RESULT(ans)
    REAL(DFP), INTENT(IN) :: Val(:)
    CLASS(RealVector_), INTENT(IN) :: obj
    REAL(DFP) :: ans
  END FUNCTION scalarDOTintrinsic
END INTERFACE DOT_PRODUCT

!----------------------------------------------------------------------------
!                                                                 DOT@BLAS1
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  25 Feb 2021
! summary:   This routines returns the dot product of vector of
! [[RealVector_]] data type.
!
!@todo
! type(_obj_) :: obj1(2), obj2(2)
! call RANDOM_NUMBER( obj1(1), 100 )
! call RANDOM_NUMBER( obj1(2), 100 )
! obj2 = obj1
! CALL Display( DOT(obj1, obj2), "dot =" )
!@endtodo

INTERFACE DOT_PRODUCT
  MODULE PURE FUNCTION vectorDOTvector(obj1, obj2) RESULT(ans)
    CLASS(RealVector_), INTENT(IN) :: obj1(:), obj2(:)
    REAL(DFP) :: ans
  END FUNCTION vectorDOTvector
END INTERFACE DOT_PRODUCT

!----------------------------------------------------------------------------
!                                                                 DOT@BLAS1
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:    25 Feb 2021
! summary: This routine computes dot product of a vector of [[RealVector_]]
! and scalar of [[RealVector_]]
!
!@todo
! type(_obj_) :: obj1(2), obj2(2)
! call RANDOM_NUMBER( obj1(1), 100 )
! call RANDOM_NUMBER( obj1(2), 100 )
! obj2 = obj1
! CALL Display( DOT(obj1, obj2), "dot =" )
!@endtodo

INTERFACE DOT_PRODUCT
  MODULE PURE FUNCTION vectorDOTscalar(obj1, obj2) RESULT(ans)
    CLASS(RealVector_), INTENT(IN) :: obj1(:), obj2
    REAL(DFP) :: ans
  END FUNCTION vectorDOTscalar
END INTERFACE DOT_PRODUCT

!----------------------------------------------------------------------------
!                                                                 DOT@BLAS1
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:    25 Feb 2021
! summary:  This routine computes dot product of a scalar of [[RealVector_]]
! and vector of [[RealVector_]]
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

INTERFACE DOT_PRODUCT
  MODULE PURE FUNCTION scalarDOTvector(obj1, obj2) RESULT(ans)
    CLASS(RealVector_), INTENT(IN) :: obj1, obj2(:)
    REAL(DFP) :: ans
  END FUNCTION scalarDOTvector
END INTERFACE DOT_PRODUCT

!----------------------------------------------------------------------------
!                                                                Norm2@BLAS1
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:    25 Feb 2021
! summary: This function computes Euclidean norm of [[RealVector_]]
!
!# Introduction
!
! L2 norm of a vector is give by
!
! $$\left| \left| \bf{V} \right|  \right|  =\sqrt{\bf{V} \cdot \bf{V} }$$
!
!@note
! This subroutine uses DOT function.
!@endnote
!
!### Usage
!
!```fortran
!s = NORM2(obj)
!```

INTERFACE NORM2
  MODULE PURE FUNCTION NRM2scalar(obj) RESULT(ans)
    CLASS(RealVector_), INTENT(IN) :: obj
    REAL(DFP) :: ans
  END FUNCTION NRM2scalar
END INTERFACE NORM2

!----------------------------------------------------------------------------
!                                                                Norm2@BLAS1
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 25 Feb 2021
! summary:   This routine computes the L2 norm of [[RealVector_]]
!
!# Introduction
!
! This routine computes L2 norm of a vector of [[RealVector_]].
!
!@note
! This function employs DOT function.
!@endnote
!
!@todo
! type(_obj_) :: obj1
! real( dfp ) :: val( 100 )
! call RANDOM_NUMBER( obj1, 100 )
! call RANDOM_NUMBER( val )
! CALL Display( DOT(obj1, val), "dot =" )
!@endtodo

INTERFACE NORM2
  MODULE PURE FUNCTION NRM2vector(obj) RESULT(ans)
    CLASS(RealVector_), INTENT(IN) :: obj(:)
    REAL(DFP) :: ans
  END FUNCTION NRM2vector
END INTERFACE NORM2

!----------------------------------------------------------------------------
!                                                                Norm2@BLAS1
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:    25 Feb 2021
! summary: This function computes the l1 norm

INTERFACE NORM1
  MODULE FUNCTION obj_NORM1(obj) RESULT(ans)
    CLASS(RealVector_), INTENT(IN) :: obj
    REAL(DFP) :: ans
  END FUNCTION obj_NORM1
END INTERFACE NORM1

!----------------------------------------------------------------------------
!                                                                Norm2@BLAS1
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:    25 Feb 2021
! summary: This function computes the l1 norm

INTERFACE NORMi
  MODULE FUNCTION obj_NORMi(obj) RESULT(ans)
    CLASS(RealVector_), INTENT(IN) :: obj
    REAL(DFP) :: ans
  END FUNCTION obj_NORMi
END INTERFACE NORMi

!----------------------------------------------------------------------------
!                                                                 SWAP@BLAS1
!----------------------------------------------------------------------------

INTERFACE SWAP
  MODULE PURE SUBROUTINE scalarSWAPscalar(X, Y)
    CLASS(RealVector_), INTENT(INOUT) :: X
    CLASS(RealVector_), INTENT(INOUT) :: Y
  END SUBROUTINE scalarSWAPscalar

  MODULE PURE SUBROUTINE vectorSWAPvector(X, Y)
    CLASS(RealVector_), INTENT(INOUT) :: X(:)
    CLASS(RealVector_), INTENT(INOUT) :: Y(:)
  END SUBROUTINE vectorSWAPvector

  MODULE PURE SUBROUTINE scalarSWAPintrinsic(X, Y)
    CLASS(RealVector_), INTENT(INOUT) :: X
    REAL(DFP), INTENT(INOUT) :: Y(:)
  END SUBROUTINE scalarSWAPintrinsic
END INTERFACE SWAP

!----------------------------------------------------------------------------
!                                                                SCALE@BLAS1
!----------------------------------------------------------------------------

INTERFACE SCAL
  MODULE PURE SUBROUTINE SCALscalar(X, A)
    CLASS(RealVector_), INTENT(INOUT) :: X
    REAL(DFP), INTENT(IN) :: A
  END SUBROUTINE SCALscalar

  MODULE PURE SUBROUTINE SCALvector(X, A)
    CLASS(RealVector_), INTENT(INOUT) :: X(:)
    REAL(DFP), INTENT(IN) :: A
  END SUBROUTINE SCALvector
END INTERFACE SCAL

!----------------------------------------------------------------------------
!                                                                 PMUL@BLAS1
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-12-28
! summary:  obj = obj1 * obj2

INTERFACE PMUL
  MODULE SUBROUTINE obj_PMUL1(obj, obj1, obj2)
    CLASS(RealVector_), INTENT(INOUT) :: obj
    CLASS(RealVector_), INTENT(IN) :: obj1
    CLASS(RealVector_), INTENT(IN) :: obj2
  END SUBROUTINE obj_PMUL1
END INTERFACE PMUL

!----------------------------------------------------------------------------
!                                                                 PMUL@BLAS1
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-12-28
! summary:  obj = obj1 / obj2

INTERFACE PDIV
  MODULE SUBROUTINE obj_PDIV1(obj, obj1, obj2, check_divide_by_zero)
    CLASS(RealVector_), INTENT(INOUT) :: obj
    CLASS(RealVector_), INTENT(IN) :: obj1
    CLASS(RealVector_), INTENT(IN) :: obj2
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: check_divide_by_zero
  END SUBROUTINE obj_PDIV1
END INTERFACE PDIV

!----------------------------------------------------------------------------
!                                                                 PMUL@BLAS1
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-12-28
! summary: Reciprocal obj1 = 1.0/obj2

INTERFACE Reciprocal
  MODULE SUBROUTINE obj_Reciprocal1(obj1, obj2, check_divide_by_zero)
    CLASS(RealVector_), INTENT(INOUT) :: obj1
    CLASS(RealVector_), INTENT(IN) :: obj2
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: check_divide_by_zero
  END SUBROUTINE obj_Reciprocal1
END INTERFACE Reciprocal

END MODULE RealVector_Blas1Methods
