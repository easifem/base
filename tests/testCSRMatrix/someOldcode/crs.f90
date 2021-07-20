! convert data to crs format

program main
use easifem
implicit none

  type( sparsematrix_ ) :: obj
  real( dfp ), allocatable :: mat( :, : )

  call initiate( obj = obj, tdof = 1, tnodes = [8] )
  call setsparsity( obj = obj, row = 1, col = [1,2,7] )
  call setsparsity( obj = obj, row = 2, col = [2,1,3,6,7,8] )
  call setsparsity( obj = obj, row = 3, col = [3, 2, 4, 8] )
  call setsparsity( obj = obj, row = 4, col = [4,3,5,8] )
  call setsparsity( obj = obj, row = 5, col = [5,4,6,8] )
  call setsparsity( obj = obj, row = 6, col = [6,2,5,7,8] )
  call setsparsity( obj = obj, row = 7, col = [7,1,2,6] )
  call setsparsity( obj = obj, row = 8, col = [8,2,3,4,5,6] )

  allocate( mat( 3, 3 ) )
  call RANDOM_NUMBER( mat ); call addcontribution( obj, [1,2,7], mat, 1.0_dfp, na)
  call RANDOM_NUMBER( mat ); call addcontribution( obj, [2,3,8], mat, 1.0_dfp, na)
  call RANDOM_NUMBER( mat ); call addcontribution( obj, [3,4,8], mat, 1.0_dfp, na)
  call RANDOM_NUMBER( mat ); call addcontribution( obj, [4,5,8], mat, 1.0_dfp, na)
  call RANDOM_NUMBER( mat ); call addcontribution( obj, [5,6,8], mat, 1.0_dfp, na)
  call RANDOM_NUMBER( mat ); call addcontribution( obj, [2,6,7], mat, 1.0_dfp, na)
  call RANDOM_NUMBER( mat ); call addcontribution( obj, [8,6,2], mat, 1.0_dfp, na)

  ! call display( obj, "obj")

  ! block
  !   real( dfp ), allocatable :: val( : )
  !   INTEGER( I4B ), allocatable :: Row( : ), Col( : )
  !   integer( I4B ) :: n, nnz, i, indx, m, j

  !   nnz = getNNZ( obj )
  !   n = SIZE( obj, 1 )
  !   allocate( row( n + 1 ), col( nnz ), val( nnz ) )
  !   row = 0; row( 1 ) = 1
  !   val = 0.0
  !   indx = 0
  !   DO i = 1, n
  !     m = SIZE( obj%Row( i )%Val )
  !     row( i + 1 ) = row( i ) + m
  !     IF( m .eq. 0 ) CYCLE
  !     DO j = 1, m
  !       indx = indx + 1
  !       Col( indx ) = obj%Row( i )%Val( j )
  !       Val( indx ) = obj%Val( i )%Val( j )
  !     END DO
  !   END DO

  !   call display( row, "row", stdout, .true. )
  !   call display( col, "col", stdout, .true. )
  !   call display( val, "val", stdout, .true. )
  ! end block

  ! block
  !   type( CRS_ ) :: crsobj
  !   call convert( from = obj, to = crsobj )
  !   call display( crsobj, "crsobj", [1,2,2] )
  ! end block


  ! block
  !   type( CRS_ ) :: crsobj
  !   call convert( from = obj, to = crsobj )
  !   call sort( crsobj, .true. )
  !   call display( crsobj, "crsobj" )
  ! end block

  ! block
  !   type( CRS_ ) :: crsobj
  !   call convert( from = obj, to = crsobj )
  !   call removeduplicates( crsobj, .true. )
  !   call display( crsobj, "crsobj" )
  ! end block

  ! block
  !   type( CRS_ ) :: crsobj
  !   integer( i4b  ), allocatable :: indu( : )
  !   call convert( from = obj, to = crsobj )
  !   call clean( crsobj, .true., 3, indu )
  !   call display( crsobj, "crsobj" )
  !   call display( indu, "indu")
  ! end block

  ! block
  !   type( CRS_ ) :: crsobj, crsobj2
  !   call convert( from = obj, to = crsobj )
  !   call copy( crsobj, crsobj2 )
  !   call display( crsobj2, "crsobj2" )
  ! end block

  ! block
  !   type( CRS_ ) :: crsobj
  !   real( dfp ) :: aij
  !   INTEGER( I4B ):: i, j

  !   call convert( from = obj, to = crsobj )
  !   call display( obj, "obj" )
  !   read( *, * ) i, j
  !   aij = ArrayValues( crsobj, i, j, .false. )
  !   call display( aij, "ArrayValues( crsobj, 1, 1, .false. )" )
  ! end block

  ! block
  !   type( CRS_ ) :: crsobj
  !   real( dfp ) :: aij
  !   INTEGER( I4B ):: i, j, nrow, ncol, lines( 2 )
  !   type( File_ ) :: afile

  !   call convert( from = obj, to = crsobj )
  !   call display( obj, "obj" )
  !   nrow = SIZE( obj, 1 ); ncol = SIZE( obj, 2 )
  !   call openFileToWrite( aFile, "./", "delme", ".ps" )
  !   call pspltm( nrow, ncol, 0, crsobj%JA, crsobj%IA, "title", &
  !     & 0, 1, 'cm', 0, lines, aFile%UnitNo )
  !   close( aFile%UnitNo )

  ! end block

  ! ! test crs to dense
  ! block
  !   type( CRS_ ) :: crsobj
  !   real( dfp ), allocatable :: x( :, : )

  !   call convert( from = obj, to = crsobj )
  !   call convert( from = crsobj, to = x )
  !   call display( x, "x" )
  ! end block


  ! ! test matvec with AX subroutine
  ! block
  !   type( CRS_ ) :: crsobj
  !   real( dfp ), allocatable :: x( : ), y( : ), mat( :, : )
  !   INTEGER( I4B ):: nrow, ncol

  !   call convert( from = obj, to = crsobj )
  !   call display( obj, "obj" )
  !   nrow = SIZE( obj, 1 ); ncol = SIZE( obj, 2 )
  !   allocate( x( ncol ), y( ncol ) )
  !   call RANDOM_NUMBER( x )
  !   call matvec( obj = crsobj, x = x, y = y, matvectype = 'AX' )
  !   call display( y, "y" )
  !   call convert( from = crsobj, to = mat )
  !   call display( y - matmul( mat, x ), "error" )
  ! end block

  ! ! test matvec with ATX subroutine
  ! block
  !   type( CRS_ ) :: crsobj
  !   real( dfp ), allocatable :: x( : ), y( : ), mat( :, : )
  !   INTEGER( I4B ):: nrow, ncol

  !   call convert( from = obj, to = crsobj )
  !   call display( obj, "obj" )
  !   nrow = SIZE( obj, 1 ); ncol = SIZE( obj, 2 )
  !   allocate( x( ncol ), y( ncol ) )
  !   call RANDOM_NUMBER( x )
  !   call matvec( obj = crsobj, x = x, y = y, matvectype = 'ATX' )
  !   call display( y, "y" )
  !   call convert( from = crsobj, to = mat )
  !   call display( y - matmul( transpose(mat), x ), "error" )
  ! end block

  ! ! testing matmul function
  ! block
  !   type( CRS_ ) :: crsobj
  !   real( dfp ), allocatable :: x( : ), y( : ), mat( :, : )
  !   INTEGER( I4B ):: nrow, ncol

  !   call convert( from = obj, to = crsobj )
  !   call display( obj, "obj" )
  !   nrow = SIZE( obj, 1 ); ncol = SIZE( obj, 2 )
  !   allocate( x( ncol ) )
  !   call RANDOM_NUMBER( x )
  !   y = matmul( crsobj, x, 'AX')
  !   call display( y, "y" )
  !   call convert( from = crsobj, to = mat )
  !   call display( y - matmul( mat, x ), "error" )
  ! end block


end program main