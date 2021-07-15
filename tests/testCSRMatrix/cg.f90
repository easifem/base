! incomplete lu factorization
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
call setsparsity( obj ) !<-- some final touch up
allocate( mat( 3, 3 ) )
call RANDOM_NUMBER( mat ); call addcontribution( obj, [1,2,7], mat + transpose( mat ), 1.0_dfp, na)
call RANDOM_NUMBER( mat ); call addcontribution( obj, [2,3,8], mat + transpose( mat ), 1.0_dfp, na)
call RANDOM_NUMBER( mat ); call addcontribution( obj, [3,4,8], mat + transpose( mat ), 1.0_dfp, na)
call RANDOM_NUMBER( mat ); call addcontribution( obj, [4,5,8], mat + transpose( mat ), 1.0_dfp, na)
call RANDOM_NUMBER( mat ); call addcontribution( obj, [5,6,8], mat + transpose( mat ), 1.0_dfp, na)
call RANDOM_NUMBER( mat ); call addcontribution( obj, [2,6,7], mat + transpose( mat ), 1.0_dfp, na)
call RANDOM_NUMBER( mat ); call addcontribution( obj, [8,6,2], mat + transpose( mat ), 1.0_dfp, na)
call display( obj, "obj" )

block
  type( csr_ ) :: csrobj
  real( dfp ), allocatable :: rhs( : ), sol( : ), w( : ), alu( : )
  integer( i4b ) :: ipar( 16 ), nrow, fac, ierr, lfil
  real( dfp ) :: fpar( 16 ), droptol
  integer( i4b ), allocatable :: jlu( : ), ju( : )

  call convert( from = obj, to = csrobj )

  droptol = 1.0E-5; lfil = 5
  call sparsekit_ilut( csrobj, alu, jlu, ju, ierr, droptol, lfil )
  call display( ierr, "ierr" )
  if( ierr .ne. 0 ) stop
  !
  nrow = size( obj, 1 )
  fac = 5
  call reallocate( rhs, nrow )
  call reallocate( sol, nrow )
  call reallocate( w, fac * nrow )
  !
  call convert( from = csrobj, to = mat )
  call display( mat, "mat", stdout, .true. )
  call RANDOM_NUMBER( sol ); call display( sol, "sol", stdout, .true. )
  rhs = matmul( mat, sol ); call display( rhs, "rhs", stdout, .true. )
  sol = 0.0

  ipar( 2 ) = 1 !left-precondition, 2 ---> right, 3 ---> left-right (dont use this)
  ipar( 3 ) = 0 !---> let solver decide 1( res ) --> 2 (res) ---> -2 (error) ---> -1 (error) ---> 999 --->
  ipar( 4 ) = SIZE( w ) ! size of w
  ipar( 5 ) = 5 ! size of Krylov subspace used in GMRES, FGMRES, DQGMRES
  ipar( 6 ) = 10 ! max number of mat-vec allowed
  ipar( 7 ) = 0 ! on return current number of mat-vec
  ipar( 8 ) = 0 ! on return pointer to the input vector requested in mat-vec step
  ipar( 9 ) = 0 ! on return pointer to the output vector requested in mat-vec step
  ipar( 10 ) = 0 ! NA
  ipar( 11 ) = 0 ! contains the convergence status 1 --> covergence else 0
  ipar( 12 ) = 0 ! NA
  ipar( 13 ) = 0  ! Number of initialization. During each initilization
                  ! residual norm is computed directly from M_l( b- Ax )
  ipar( 14 ) = 0 ! NA
  ipar( 15 ) = 0 ! NA
  ipar( 16 ) = 0 ! NA

  fpar( 1 ) = 1.0E-5 ! Relative tolerance
  fpar( 2 ) = 1.0E-5 ! absolute tolerance
  fpar( 3 ) = 0.0 ! initial residual/error norm
  fpar( 4 ) = 0.0 ! target residual/error norm
  fpar( 5 ) = 0.0 ! current residual/error norm
  fpar( 6 ) = 0.0 ! current residual/error norm
  fpar( 7 ) = 0.0 ! convergence rate
  fpar( 8:16 ) = 0.0 ! NA

10 call cg( nrow, rhs, sol, ipar, fpar, w )

  select case( ipar( 1 ) )
  case( 1  )
    ! matvec with A
    write( *, * ) "matvec:: Ax"
    call amux( nrow, w( ipar( 8 ) ), w( ipar( 9 ) ), csrobj%A, csrobj%ja, csrobj%ia )
    goto 10
  case( 2 )
    write( *, * ) "matvec:: A^Tx"
    call atmux( nrow, w( ipar( 8 ) ), w( ipar( 9 ) ), csrobj%A, csrobj%ja, csrobj%ia )
    goto 10
  case( 3, 5 )
    write( *, * ) "solve:: ML^-1 x"
    call lusol( nrow, w( ipar( 8 ) ), w( ipar( 9 ) ), alu, jlu, ju )
    goto 10
  case( 4, 6 )
    write( *, * ) "solve:: ML^-T x"
    call lutsol( nrow, w( ipar( 8 ) ), w( ipar( 9 ) ), alu, jlu, ju )
    goto 10
  case( 10 )
    write( *, * ) "call stoping test"
    goto 10
  case( 0 )
    write( *, * ) "success"
  case( -1 )
    write( *, * ) "error: iter number is greater than the present lim"
  case( -2 )
    write( *, * ) "error: insufficient work space"
    ! in this case appropriatet size of w is returned in IPAR( 4 )
  case( -3 )
    write( *, * ) "error: breakdown of algorithm"
  case( -4 )
    write( *, * ) "error: error in fpar( 1 ) and fpar( 2 )"
  case( -9 )
    write( *, * ) "error: absnormal number detected"
  case( -10 )
    write( *, * ) "error: invalid floating point number is detected"
  end select

  call display( sol, "sol", stdout, .true. )
  call display( fpar( 3 ), "initial residue/error residue norm" )
  call display( fpar( 4 ), "targer residue/error residue norm" )
  call display( fpar( 5 ), "current residue norm" )
  call display( fpar( 6 ), "current residue/error norm" )

end block

end program main