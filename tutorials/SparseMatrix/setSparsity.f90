! incomplete lu factorization
program main
  use easifem
  implicit none

  type( sparsematrix_ ) :: tanmat
  real( dfp ), allocatable :: mat( :, : )

  call display( 'flag-1')
  call initiate( obj = tanmat, tdof = 1, tnodes = [10] )
  call setsparsity( obj = tanmat, row = 1, col = [1,2] )
  call setsparsity( obj = tanmat, row = 2, col = [1,2,3] )
  call setsparsity( obj = tanmat, row = 3, col = [2,3,4] )
  call setsparsity( obj = tanmat, row = 4, col = [3,4,5] )
  call setsparsity( obj = tanmat, row = 5, col = [4,5,6] )
  call setsparsity( obj = tanmat, row = 6, col = [5,6,7] )
  call setsparsity( obj = tanmat, row = 7, col = [6,7,8] )
  call setsparsity( obj = tanmat, row = 8, col = [7,8,9] )
  call setsparsity( obj = tanmat, row = 9, col = [8,9,10] )
  call setsparsity( obj = tanmat, row = 10, col = [9,10] )
  call display( 'flag-2')
  call setsparsity( tanmat ) !<-- some final touch up
  call display( 'flag-3')

  allocate( mat( 2, 2 ) )
  mat = reshape( [1,-1,-1,1], [2,2])
  call addcontribution( tanmat, [1,2], mat, 1.0_dfp, NONE )
  call addcontribution( tanmat, [2,3], mat, 1.0_dfp, NONE )
  call addcontribution( tanmat, [3,4], mat, 1.0_dfp, NONE )
  call addcontribution( tanmat, [4,5], mat, 1.0_dfp, NONE )
  call addcontribution( tanmat, [5,6], mat, 1.0_dfp, NONE )
  call addcontribution( tanmat, [6,7], mat, 1.0_dfp, NONE )
  call addcontribution( tanmat, [7,8], mat, 1.0_dfp, NONE )
  call addcontribution( tanmat, [8,9], mat, 1.0_dfp, NONE )
  call addcontribution( tanmat, [9,10], mat, 1.0_dfp, NONE )
  call display( 'flag-4')
  ! call spy( tanmat, [String( './' ), String( 'tanmat' ), String( '.txt' )], &
  !   & 'gnuplot', .false. )
  call display( tanmat, 'tanamt' )
  call display( 'flag-5')
  call convert( from = tanmat, to = mat )
  call display( 'flag-6')
  ! call spy( tanmat, [String( './' ), String( 'tanmat' ), String( '.txt' )] &
  !   & , 'gnuplot', .false. )
  call display( mat, 'mat' )

  end program main

