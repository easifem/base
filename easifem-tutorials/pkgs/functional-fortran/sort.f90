program main
use mod_functional
implicit none

INTEGER, allocatable :: intvec( : )

allocate( intvec( 5 ) )
intvec = [100, 12, 34, 56, 33]
intvec = sort( intvec )
write( *, * ) intvec
end program main