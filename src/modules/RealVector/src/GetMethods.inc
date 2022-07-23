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

PUBLIC :: getIndex
PUBLIC :: get
PUBLIC :: isPresent
PUBLIC :: getPointer

!----------------------------------------------------------------------------
!                                                    getPointer@getMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	28 Feb 2021
! summary: Returns the pointer to vector of real numbers in [[RealVector_]]
!
!# Introduction
!
! This function returns the pointer to vector of real numbers stored
! inside [[RealVector_]]

INTERFACE
MODULE FUNCTION realVec_getPointer1( obj ) RESULT( val )
  CLASS( RealVector_ ), INTENT( IN ), TARGET :: obj
  REAL( DFP ), POINTER :: val( : )
END FUNCTION realVec_getPointer1
END INTERFACE

INTERFACE getPointer
  MODULE PROCEDURE realVec_getPointer1
END INTERFACE

!----------------------------------------------------------------------------
!                                                    getPointer@getMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	28 Feb 2021
! summary: Returns the pointer to vector of real numbers
!
!# Introduction
! This function returns the pointer to vector of real numbers stored
! inside [[RealVector_]] for a given degree of freedom

INTERFACE
MODULE FUNCTION realVec_getPointer2( obj, dofobj, idof ) RESULT( val )
  CLASS( RealVector_ ), INTENT( IN ), TARGET :: obj
  TYPE( DOF_ ), INTENT( IN ) :: dofobj
  INTEGER( I4B ), INTENT( IN ) :: idof
  REAL( DFP ), POINTER :: val( : )
END FUNCTION realVec_getPointer2
END INTERFACE

INTERFACE getPointer
  MODULE PROCEDURE realVec_getPointer2
END INTERFACE

!----------------------------------------------------------------------------
!                                                         getIndex@getMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	28 Feb 2021
! summary: This function finds location of value inside the [[RealVector_]]
!
!# Introduction
!
! This function finds the location of `value` inside the instance of
! [[RealVector_]]

INTERFACE
MODULE PURE FUNCTION realVec_getIndex1( obj, value, tol ) RESULT( Ans )
  CLASS( RealVector_ ), INTENT( IN ) :: obj
  REAL( DFP ), INTENT( IN ) :: value
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: tol
  INTEGER( I4B ) :: Ans
END FUNCTION realVec_getIndex1
END INTERFACE

INTERFACE getIndex
  MODULE PROCEDURE realVec_getIndex1
END INTERFACE getIndex

!----------------------------------------------------------------------------
!                                                           IndexOf@getMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	28 Feb 2021
! summary:  Returns location of values inside the [[RealVector_]]
!
!# Introduction
!
! This function returns the nearest location of values inside the
! [[RealVector_]]

INTERFACE
MODULE PURE FUNCTION realVec_getIndex2( obj, value, tol ) RESULT( Ans )
  CLASS( RealVector_ ), INTENT( IN ) :: obj
  REAL( DFP ), INTENT( IN ) :: value( : )
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: tol
  INTEGER( I4B ), ALLOCATABLE :: Ans( : )
END FUNCTION realVec_getIndex2
END INTERFACE

INTERFACE getIndex
  MODULE PROCEDURE realVec_getIndex2
END INTERFACE getIndex

!----------------------------------------------------------------------------
!                                                       isPresent@getMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	28 Feb 2021
! summary: 	Returns true if value is present inside [[RealVector_]]

INTERFACE
MODULE PURE FUNCTION realVec_isPresent1( obj, value, tol ) RESULT( Ans )
  CLASS( RealVector_ ), INTENT( IN ) :: obj
  REAL( DFP ), INTENT( IN ) :: value
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: tol
  LOGICAL( LGT ) :: Ans
END FUNCTION realVec_isPresent1
END INTERFACE

INTERFACE isPresent
  MODULE PROCEDURE realVec_isPresent1
END INTERFACE isPresent

!----------------------------------------------------------------------------
!                                                         isPresentgetMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	28 Feb 2021
! summary:  Returns true if value is present inside [[RealVector_]]

INTERFACE
MODULE PURE FUNCTION realVec_isPresent2( obj, value, tol ) RESULT( Ans )
  CLASS( RealVector_ ), INTENT( IN ) :: obj
  REAL( DFP ), INTENT( IN ) :: value( : )
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: tol
  LOGICAL( LGT ), ALLOCATABLE :: Ans( : )
END FUNCTION realVec_isPresent2
END INTERFACE

INTERFACE isPresent
  MODULE PROCEDURE realVec_isPresent2
END INTERFACE isPresent

!----------------------------------------------------------------------------
!                                                             get@GetMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	28 Feb 2021
! summary: 	This function returns a vector of Integer from [[RealVector_]]

INTERFACE
MODULE PURE FUNCTION realVec_get1( obj, DataType ) RESULT( ans )
  CLASS( RealVector_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: DataType
  INTEGER( I4B ), ALLOCATABLE :: ans( : )
END FUNCTION realVec_get1
END INTERFACE

INTERFACE get
  MODULE PROCEDURE realVec_get1
END INTERFACE get

!----------------------------------------------------------------------------
!                                                              get@GetMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	28 Feb 2021
! summary: 	This function returns a vector of integer from [[RealVector_]]

INTERFACE
MODULE PURE FUNCTION realVec_get2( obj, nodenum, DataType ) RESULT( ans )
  CLASS( RealVector_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: nodenum( : )
  INTEGER( I4B ), INTENT( IN ):: DataType
  INTEGER( I4B ), ALLOCATABLE :: ans( : )
END FUNCTION realVec_get2
END INTERFACE

INTERFACE get
  MODULE PROCEDURE realVec_get2
END INTERFACE get

!----------------------------------------------------------------------------
!                                                             get@GetMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	28 Feb 2021
! summary: 	This function returns a vector of integer from [[RealVector_]]

INTERFACE
MODULE PURE FUNCTION realVec_get3( obj, istart, iend, stride, &
  & DataType ) RESULT( ans )
  CLASS( RealVector_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: istart, iend, stride
  INTEGER( I4B ), INTENT( IN ):: DataType
  INTEGER( I4B ), ALLOCATABLE :: ans( : )
END FUNCTION realVec_get3
END INTERFACE

INTERFACE get
  MODULE PROCEDURE realVec_get3
END INTERFACE get

!----------------------------------------------------------------------------
!                                                              get@GetMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	28 Feb 2021
! summary: 	This function returns a vector of real from [[RealVector_]]

INTERFACE
MODULE PURE FUNCTION realVec_get4a( obj, DataType ) RESULT( ans )
  CLASS( RealVector_ ), INTENT( IN ) :: obj
  REAL( Real32 ), INTENT( IN ) :: DataType
  REAL( Real32 ), ALLOCATABLE :: ans( : )
END FUNCTION realVec_get4a
MODULE PURE FUNCTION realVec_get4b( obj, DataType ) RESULT( ans )
  CLASS( RealVector_ ), INTENT( IN ) :: obj
  REAL( Real64 ), INTENT( IN ) :: DataType
  REAL( Real64 ), ALLOCATABLE :: ans( : )
END FUNCTION realVec_get4b
END INTERFACE

INTERFACE get
  MODULE PROCEDURE realVec_get4a, realVec_get4b
END INTERFACE get

!----------------------------------------------------------------------------
!                                                             get@GetMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	28 Feb 2021
! summary: 	This function returns a vector of real from [[RealVector_]]

INTERFACE
MODULE PURE FUNCTION realVec_get5a( obj, nodenum, DataType ) RESULT( ans )
  CLASS( RealVector_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: nodenum( : )
  REAL( Real32 ), INTENT( IN ):: DataType
  REAL( Real32 ), ALLOCATABLE :: ans( : )
END FUNCTION realVec_get5a
MODULE PURE FUNCTION realVec_get5b( obj, nodenum, DataType ) RESULT( ans )
  CLASS( RealVector_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: nodenum( : )
  REAL( Real64 ), INTENT( IN ):: DataType
  REAL( Real64 ), ALLOCATABLE :: ans( : )
END FUNCTION realVec_get5b
END INTERFACE

INTERFACE get
  MODULE PROCEDURE realVec_get5a, realVec_get5b
END INTERFACE get

!----------------------------------------------------------------------------
!                                                              get@GetMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	28 Feb 2021
! summary: 	This function returns a vector of real from [[RealVector_]]

INTERFACE
MODULE PURE FUNCTION realVec_get6( obj, istart, iend, stride, &
  & DataType ) RESULT( ans )
  CLASS( RealVector_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: istart, iend, stride
  REAL( DFP ), INTENT( IN ):: DataType
  REAL( DFP ), ALLOCATABLE :: ans( : )
END FUNCTION realVec_get6
END INTERFACE

INTERFACE get
  MODULE PROCEDURE realVec_get6
END INTERFACE get

!----------------------------------------------------------------------------
!                                                     get@GetMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	28 Feb 2021
! summary: This function returns the vector of integer from [[RealVector_]]

INTERFACE
MODULE PURE FUNCTION realVec_get7( obj, DataType ) RESULT( val )
  CLASS( RealVector_ ), INTENT( IN ) :: obj( : )
  INTEGER( I4B ), INTENT( IN ) :: DataType
  INTEGER( I4B ), ALLOCATABLE :: val( : )
END FUNCTION realVec_get7
END INTERFACE

INTERFACE get
  MODULE PROCEDURE realVec_get7
END INTERFACE get

!----------------------------------------------------------------------------
!                                                             get@GetMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	28 Feb 2021
! summary: 	This function returns a vector of integer from [[RealVector_]]

INTERFACE
MODULE PURE FUNCTION realVec_get8( obj, nodenum, DataType ) RESULT( val )
  CLASS( RealVector_ ), INTENT( IN ) :: obj( : )
  INTEGER( I4B ), INTENT( IN ) :: nodenum( : )
  INTEGER( I4B ), INTENT( IN ) :: DataType
  INTEGER( I4B ), ALLOCATABLE :: val( : )
END FUNCTION realVec_get8
END INTERFACE

INTERFACE get
  MODULE PROCEDURE realVec_get8
END INTERFACE get

!----------------------------------------------------------------------------
!                                                             get@GetMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	28 Feb 2021
! summary: 	This function returns an integer vector from [[RealVector_]]

INTERFACE
MODULE PURE FUNCTION realVec_get9( obj, istart, iend, &
  & stride, DataType ) RESULT( val )
  CLASS( RealVector_ ), INTENT( IN ) :: obj( : )
  INTEGER( I4B ), INTENT( IN ) :: istart
  INTEGER( I4B ), INTENT( IN ) :: iend
  INTEGER( I4B ), INTENT( IN ) :: stride
  INTEGER( I4B ), INTENT( IN ):: DataType
  INTEGER( I4B ), ALLOCATABLE :: val( : )
END FUNCTION realVec_get9
END INTERFACE

INTERFACE get
  MODULE PROCEDURE realVec_get9
END INTERFACE get

!----------------------------------------------------------------------------
!                                                             get@GetMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	28 Feb 2021
! summary: 	This function returns a vector of real from [[RealVector_]]

INTERFACE
MODULE PURE FUNCTION realVec_get10a( obj, DataType ) RESULT( val )
  CLASS( RealVector_ ), INTENT( IN ) :: obj( : )
  REAL( Real32 ), INTENT( IN ) :: DataType
  REAL( Real32 ), ALLOCATABLE :: val( : )
END FUNCTION realVec_get10a
MODULE PURE FUNCTION realVec_get10b( obj, DataType ) RESULT( val )
  CLASS( RealVector_ ), INTENT( IN ) :: obj( : )
  REAL( Real64 ), INTENT( IN ) :: DataType
  REAL( Real64 ), ALLOCATABLE :: val( : )
END FUNCTION realVec_get10b
END INTERFACE

INTERFACE get
  MODULE PROCEDURE realVec_get10a, realVec_get10b
END INTERFACE get

!----------------------------------------------------------------------------
!                                                     get@GetMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	28 Feb 2021
! summary: 	This function returns a vector of real from [[RealVector_]]

INTERFACE
MODULE PURE FUNCTION realVec_get11a( obj, nodenum, DataType ) RESULT( val )
  CLASS( RealVector_ ), INTENT( IN ) :: obj( : )
  REAL( Real32 ), INTENT( IN ) :: DataType
  INTEGER( I4B ), INTENT( IN ) :: nodenum( : )
  REAL( Real32 ), ALLOCATABLE :: val( : )
END FUNCTION realVec_get11a
MODULE PURE FUNCTION realVec_get11b( obj, nodenum, DataType ) RESULT( val )
  CLASS( RealVector_ ), INTENT( IN ) :: obj( : )
  REAL( Real64 ), INTENT( IN ) :: DataType
  INTEGER( I4B ), INTENT( IN ) :: nodenum( : )
  REAL( Real64 ), ALLOCATABLE :: val( : )
END FUNCTION realVec_get11b
END INTERFACE

INTERFACE get
  MODULE PROCEDURE realVec_get11a, realVec_get11b
END INTERFACE get

!----------------------------------------------------------------------------
!                                                             get@GetMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	28 Feb 2021
! summary: 	This function returns a vector of real from [[RealVector_]]

INTERFACE
MODULE PURE FUNCTION realVec_get12a( obj, istart, iend, stride, &
  & DataType ) RESULT( val )
  CLASS( RealVector_ ), INTENT( IN ) :: obj( : )
  INTEGER( I4B ), INTENT( IN ) :: istart, iend, stride
  REAL( Real32 ), INTENT( IN ):: DataType
  REAL( Real32 ), ALLOCATABLE :: val( : )
END FUNCTION realVec_get12a
MODULE PURE FUNCTION realVec_get12b( obj, istart, iend, stride, &
  & DataType ) RESULT( val )
  CLASS( RealVector_ ), INTENT( IN ) :: obj( : )
  INTEGER( I4B ), INTENT( IN ) :: istart, iend, stride
  REAL( Real64 ), INTENT( IN ):: DataType
  REAL( Real64 ), ALLOCATABLE :: val( : )
END FUNCTION realVec_get12b
END INTERFACE

INTERFACE get
  MODULE PROCEDURE realVec_get12a, realVec_get12b
END INTERFACE get

!----------------------------------------------------------------------------
!                                                             get@GetMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	28 Feb 2021
! summary: 	This function returns the instance of [[RealVector_]]
!
!# Introduction
! This function returns an scalar instance of [[RealVector_]] by
! combining different entries of a vector of [[RealVector_]]

INTERFACE
MODULE PURE FUNCTION realVec_get13( obj, DataType ) RESULT( val )
  CLASS( RealVector_ ), INTENT( IN ) :: obj( : )
  TYPE( RealVector_ ), INTENT( IN ) :: DataType
  TYPE( RealVector_ ) :: val
END FUNCTION realVec_get13
END INTERFACE

INTERFACE get
  MODULE PROCEDURE realVec_get13
END INTERFACE get

!----------------------------------------------------------------------------
!                                                             get@GetMethod
!----------------------------------------------------------------------------

INTERFACE
!> authors: Vikas Sharma, Ph. D.
! date: 	28 Feb 2021
! summary: 	This function returns the instance of [[RealVector_]]
!
!# Introduction
! This function returns the instance of [[RealVector_]] from the vector of
! [[RealVector_]].

MODULE PURE FUNCTION realVec_get14( obj, nodenum, DataType ) RESULT( val )
  CLASS( RealVector_ ), INTENT( IN ) :: obj( : )
  INTEGER( I4B ), INTENT( IN ) :: nodenum( : )
  TYPE( RealVector_ ), INTENT( IN ) :: DataType
  TYPE( RealVector_ ) :: val
END FUNCTION realVec_get14
END INTERFACE

INTERFACE get
  MODULE PROCEDURE realVec_get14
END INTERFACE get

!----------------------------------------------------------------------------
!                                                             get@GetMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	28 Feb 2021
! summary: 	This function returns the instance of [[RealVector_]]
!
!# Introduction
! This function returns the instance of [[RealVector_]] from the vector of
! [[RealVector_]].

INTERFACE
MODULE PURE FUNCTION realVec_get15( obj, istart, iend, stride, &
  & DataType ) RESULT( val )
  CLASS( RealVector_ ), INTENT( IN ) :: obj( : )
  INTEGER( I4B ), INTENT( IN ) :: istart, iend, stride
  TYPE( RealVector_ ), INTENT( IN ) :: DataType
  TYPE( RealVector_ ) :: val
END FUNCTION realVec_get15
END INTERFACE

INTERFACE get
  MODULE PROCEDURE realVec_get15
END INTERFACE get

!----------------------------------------------------------------------------
!                                                             get@GetMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	28 Feb 2021
! summary: 	This function returns an instance of [[RealVector_]]
!
!# Introduction
!
! This function returns an instance of [[RealVector_]] by using selective
! from `obj`

INTERFACE
MODULE PURE FUNCTION realVec_get16( obj, nodenum, DataType ) RESULT( val )
  CLASS( RealVector_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: nodenum( : )
  TYPE( RealVector_ ), INTENT( IN ) :: DataType
  TYPE( RealVector_ ) :: val
END FUNCTION realVec_get16
END INTERFACE

INTERFACE get
  MODULE PROCEDURE realVec_get16
END INTERFACE get

!----------------------------------------------------------------------------
!                                                             get@GetMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	28 Feb 2021
! summary: 	This function returns the instance of [[RealVector_]]
!
!# Introduction
! This function returns the instance of [[RealVector_]] using istart, iend,
! stride values

INTERFACE
MODULE PURE FUNCTION realVec_get17( obj, istart, iend, stride, &
  & DataType ) RESULT( val )
  CLASS( RealVector_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: istart, iend, stride
  TYPE( RealVector_ ), INTENT( IN ) :: DataType
  TYPE( RealVector_ ) :: val
END FUNCTION realVec_get17
END INTERFACE

INTERFACE get
  MODULE PROCEDURE realVec_get17
END INTERFACE get

!----------------------------------------------------------------------------
!                                                             get@GetMethod
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION realVec_get18a( obj, nodenum, DataType ) RESULT( val )
  CLASS( RealVector_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: nodenum
  REAL( Real32 ), INTENT( IN ):: DataType
  REAL( Real32 ) :: val
END FUNCTION realVec_get18a
MODULE PURE FUNCTION realVec_get18b( obj, nodenum, DataType ) RESULT( val )
  CLASS( RealVector_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: nodenum
  REAL( Real64 ), INTENT( IN ):: DataType
  REAL( Real64 ) :: val
END FUNCTION realVec_get18b
END INTERFACE

INTERFACE get
  MODULE PROCEDURE realVec_get18a, realVec_get18b
END INTERFACE get

!----------------------------------------------------------------------------
!                                                              get@GetMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	28 Feb 2021
! summary: 	This function returns a vector of real from [[RealVector_]]

INTERFACE
MODULE PURE FUNCTION realVec_get19( obj ) RESULT( ans )
  CLASS( RealVector_ ), INTENT( IN ) :: obj
  REAL( DFP ), ALLOCATABLE :: ans( : )
END FUNCTION realVec_get19
END INTERFACE

INTERFACE get
  MODULE PROCEDURE realVec_get19
END INTERFACE get

!----------------------------------------------------------------------------
!                                                             get@GetMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	28 Feb 2021
! summary: 	This function returns a vector of real from [[RealVector_]]

INTERFACE
MODULE PURE FUNCTION realVec_get20( obj, nodenum ) RESULT( ans )
  CLASS( RealVector_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: nodenum( : )
  REAL( DFP ), ALLOCATABLE :: ans( : )
END FUNCTION realVec_get20
END INTERFACE

INTERFACE get
  MODULE PROCEDURE realVec_get20
END INTERFACE get

!----------------------------------------------------------------------------
!                                                              get@GetMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	28 Feb 2021
! summary: 	This function returns a vector of real from [[RealVector_]]

INTERFACE
MODULE PURE FUNCTION realVec_get21( obj, istart, iend, stride ) &
  & RESULT( ans )
  CLASS( RealVector_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: istart, iend, stride
  REAL( DFP ), ALLOCATABLE :: ans( : )
END FUNCTION realVec_get21
END INTERFACE

INTERFACE get
  MODULE PROCEDURE realVec_get21
END INTERFACE get

!----------------------------------------------------------------------------
!                                                             get@GetMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	28 Feb 2021
! summary: 	This function returns a vector of real from [[RealVector_]]

INTERFACE
MODULE PURE FUNCTION realVec_get22( obj ) RESULT( val )
  CLASS( RealVector_ ), INTENT( IN ) :: obj( : )
  REAL( DFP ), ALLOCATABLE :: val( : )
END FUNCTION realVec_get22
END INTERFACE

INTERFACE get
  MODULE PROCEDURE realVec_get22
END INTERFACE get

!----------------------------------------------------------------------------
!                                                     get@GetMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	28 Feb 2021
! summary: 	This function returns a vector of real from [[RealVector_]]

INTERFACE
MODULE PURE FUNCTION realVec_get23( obj, nodenum ) RESULT( val )
  CLASS( RealVector_ ), INTENT( IN ) :: obj( : )
  INTEGER( I4B ), INTENT( IN ) :: nodenum( : )
  REAL( DFP ), ALLOCATABLE :: val( : )
END FUNCTION realVec_get23
END INTERFACE

INTERFACE get
  MODULE PROCEDURE realVec_get23
END INTERFACE get

!----------------------------------------------------------------------------
!                                                             get@GetMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	28 Feb 2021
! summary: 	This function returns a vector of real from [[RealVector_]]

INTERFACE
MODULE PURE FUNCTION realVec_get24( obj, istart, iend, stride) &
  & RESULT( val )
  CLASS( RealVector_ ), INTENT( IN ) :: obj( : )
  INTEGER( I4B ), INTENT( IN ) :: istart, iend, stride
  REAL( DFP ), ALLOCATABLE :: val( : )
END FUNCTION realVec_get24
END INTERFACE

INTERFACE get
  MODULE PROCEDURE realVec_get24
END INTERFACE get

!----------------------------------------------------------------------------
!                                                             get@GetMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	28 Feb 2021
! summary: 	This function returns a vector of real from [[RealVector_]]

INTERFACE
MODULE PURE FUNCTION realVec_get25( obj, dofobj, nodenum, &
  & ivar, idof ) RESULT( ans )
  CLASS( RealVector_ ), INTENT( IN ) :: obj
  TYPE(DOF_), INTENT( IN ) :: dofobj
  INTEGER( I4B ), INTENT( IN ) :: nodenum
  INTEGER( I4B ), INTENT( IN ) :: ivar
  INTEGER( I4B ), INTENT( IN ) :: idof
  REAL( DFP ) :: ans
END FUNCTION realVec_get25
END INTERFACE

INTERFACE get
  MODULE PROCEDURE realVec_get25
END INTERFACE get

!----------------------------------------------------------------------------
!                                                             get@GetMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	28 Feb 2021
! summary: 	This function returns a vector of real from [[RealVector_]]

INTERFACE
MODULE PURE FUNCTION realVec_get26( obj, dofobj, nodenum, &
  & ivar, idof ) RESULT( ans )
  CLASS( RealVector_ ), INTENT( IN ) :: obj
  TYPE(DOF_), INTENT( IN ) :: dofobj
  INTEGER( I4B ), INTENT( IN ) :: nodenum(:)
  INTEGER( I4B ), INTENT( IN ) :: ivar
  INTEGER( I4B ), INTENT( IN ) :: idof
  REAL( DFP ) :: ans( size(nodenum) )
END FUNCTION realVec_get26
END INTERFACE

INTERFACE get
  MODULE PROCEDURE realVec_get26
END INTERFACE get

!----------------------------------------------------------------------------
!                                                             get@GetMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	28 Feb 2021
! summary: 	This function returns a vector of real from [[RealVector_]]

INTERFACE
MODULE PURE FUNCTION realVec_get27( obj, dofobj, nodenum, &
  & ivar ) RESULT( ans )
  CLASS( RealVector_ ), INTENT( IN ) :: obj
  TYPE(DOF_), INTENT( IN ) :: dofobj
  INTEGER( I4B ), INTENT( IN ) :: nodenum(:)
  INTEGER( I4B ), INTENT( IN ) :: ivar
  REAL( DFP ), ALLOCATABLE :: ans( : )
END FUNCTION realVec_get27
END INTERFACE

INTERFACE get
  MODULE PROCEDURE realVec_get27
END INTERFACE get

!----------------------------------------------------------------------------
!                                                             get@GetMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	28 Feb 2021
! summary: 	This function returns a vector of real from [[RealVector_]]

INTERFACE
MODULE PURE FUNCTION realVec_get28( obj, dofobj, nodenum, &
  & ivar, spacecompo, timecompo ) RESULT( ans )
  CLASS( RealVector_ ), INTENT( IN ) :: obj
  TYPE(DOF_), INTENT( IN ) :: dofobj
  INTEGER( I4B ), INTENT( IN ) :: nodenum(:)
  INTEGER( I4B ), INTENT( IN ) :: ivar
  INTEGER( I4B ), INTENT( IN ) :: spacecompo
  INTEGER( I4B ), INTENT( IN ) :: timecompo
  REAL( DFP ), ALLOCATABLE :: ans( : )
END FUNCTION realVec_get28
END INTERFACE

INTERFACE get
  MODULE PROCEDURE realVec_get28
END INTERFACE get
