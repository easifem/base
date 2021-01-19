program main
use easifem

real( dfp ), allocatable :: mat_dof( :, : ), mat_node( :, : ), dummy( :, : )
integer( i4b ), allocatable :: T( :, : )
integer( I4B ) :: nns, spacecompo, tdof, idof, inode, timecompo, m, n, i, j

nns = 3
timecompo = 2
spacecompo = 2
tdof = timecompo * spacecompo
m = nns * tdof
n = m

allocate( mat_dof( m, n ), mat_node( m, n ), dummy( nns, nns ), T( m, n ) )

do j = 1, tdof
  do i = 1, tdof
    call RANDOM_NUMBER( dummy )
    call display( dummy, "dummy")
    mat_dof( ( i - 1 ) * nns + 1 : i*nns, ( j - 1 ) * nns + 1 : j*nns ) = dummy
  end do
end do

! call display( mat_dof, "mat in dof format")

T = Eye( m, TypeInt )
do inode  = 1, nns
  do idof = 1, tdof
    j = (inode - 1)* tdof + idof
    T( j, j ) = 0
    i = (idof - 1)*nns + inode
    T( i, j ) = 1
  end do
end do

! call display( T, "T", stdout, .true. )

mat_node = MATMUL( TRANSPOSE( T ), MATMUL( mat_dof, T ) )

! call display( mat_node, "mat_node", stdout, .true. )
end program main