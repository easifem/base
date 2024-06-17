#define _AXB_( a, x, b ) _AXB1_( a, x, b )
#define _AXB1_( a, x, b ) a ## x ## b
#define _AX_( a, x ) _AX1_( a, x )
#define _AX1_( a, x ) a ## x

TYPE, EXTENDS( AbstractVector_ ) :: _AX_( _KIND_, Vector_ )
  _TYPE_ ( _KIND_ ), ALLOCATABLE :: Val( : )
END TYPE

PUBLIC :: _AX_( _KIND_, Vector_ )

TYPE( _AX_( _KIND_, Vector_ ) ), PUBLIC, PARAMETER :: _AXB_( Type, _KIND_, Vector ) = &
  & _AX_( _KIND_, Vector_ )( tDimension = 1_I4B, Val = NULL( ) )

TYPE :: _AX_( _KIND_, VectorPointer )
  CLASS( _AX_( _KIND_, Vector_ ) ), POINTER :: Ptr => NULL( )
END TYPE

PUBLIC :: _AX_( _KIND_, VectorPointer )


#undef _TYPE_
#undef _KIND_
#undef _AXB_
#undef _AXB1_
#undef _AX_
#undef _AX1_


!<-- example --->|
! to use this macro define _TYPE_ INTEGER
!                   define _KIND_ Int8
!
! TYPE, EXTENDS( AbstractVector_ ) :: Int8Vector_
! 	INTEGER( Int8 ), ALLOCATABLE :: Val( : )
! END TYPE Int8Vector_

! PUBLIC :: Int8Vector_

! TYPE(Int8Vector_), PUBLIC, PARAMETER :: TypeInt8Vector = Int8Vector_( &
! 	tDimension = 1_I4B, Val = NULL( ) )

! TYPE :: Int8VectorPointer
! 	CLASS( Int8Vector_ ), POINTER :: Ptr => NULL( )
! END TYPE Int8VectorPointer
!<-- example --->|