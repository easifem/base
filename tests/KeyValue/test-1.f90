program main
use easifem
implicit none

type( keyvalue_ ) :: obj
real( dfp ) :: vec( 3 ), mat( 3, 3 )

call random_number( vec )
call random_number( mat )

obj = keyvalue( 'real-rank-0', 1.0_dfp )
call display( obj, 'before' )
obj = 2.0_dfp
call display( obj, 'after' )

obj = keyvalue( 'real-rank-1', vec )
call display( obj, 'obj' )
obj = [2.0_dfp, 3.0_dfp, 4.0_dfp]
call display( obj, 'after' )

end program main
