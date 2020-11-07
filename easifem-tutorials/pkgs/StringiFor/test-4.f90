!> authors: Dr. Vikas Sharma
!
! This program test str() method of easifem

program main
use easifem
implicit none

CALL Display( str( FReal64, 1.0_Real64) )
CALL Display( str( FReal32, 1.0_Real32) )
CALL Display( str( FDFP, 1.0_DFP) )
CALL Display( str( FI4B, 1_I4B) )
CALL Display( str( FInt32, 1_Int32) )
CALL Display( str( FInt16, 1_Int16) )

CALL Display( str( pi, no_sign=.true., compact=.true.), "no_sign, compact :: " )
CALL Display( str( Pi, no_sign=.true. ), "no_sign :: " )
CALL Display( str( Pi ) )

CALL Display( str( 1, no_sign=.true. ), "int no_sign :: " )
CALL Display( str( 1 ), "int :: " )

CALL Display( str(n=[1._DFP, -2._DFP]), "Default :: " )
CALL Display( str(n=[1._DFP, -2._DFP], separator=";" ), "Separator ; :: " )
CALL Display( str(n=[1._DFP, -2._DFP], separator=",", delimiters= ["(", ")"]), "Separator and delimiters :: " )

CALL Display( str(n=[1, -2]), "Default :: " )
CALL Display( str(n=[1, -2], separator=";" ), "Separator ; :: " )
CALL Display( str(n=[1, -2], separator=",", delimiters= ["(", ")"]), "Separator and delimiters :: " )

WRITE( *, * ) strz( 111 )
WRITE( *, * ) strz( 111, nz_pad=3 )
WRITE( *, * ) strz( 111, nz_pad=4 )
WRITE( *, * ) strz( 111, nz_pad=5 )
! WRITE( *, * )e

WRITE( *, * ) CTON( "-1", 1.0_DFP )
WRITE( *, * ) CTON( "-1", 1_I4B )

WRITE( *, * ) BSTR( 1.0_DFP )
WRITE( *, * ) BSTR( 1.0_Real32 )
WRITE( *, * ) BSTR( 1.0_Real64 )
WRITE( *, * ) BSTR( 1_I4B )
WRITE( *, * ) BSTR( 1_Int32 )

end program main