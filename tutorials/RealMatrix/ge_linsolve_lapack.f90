! This tutorial shows how to solve linear equation
!
! test01
! How to solve A(:,:) x(:) = b(:) using GESV or solve
! test02
! Solve A(:,:) x(:,:) = b(:,:) using GESV or solve
! test03
! Solve a least square problem, A(m,n)*x(n) = b(m), m>n
! test04
! Solve a least square problem, A(m,n)*x(n) = b(m), m<n
! test05
! Solve a least square problem, A(m,n)*x(n,:) = b(m,:), m>n
! test06
! Solves a least square problem using DGELSD which uses SVD decomposition
! test07
! Solves a least square problem using DGELSS which uses SVD decomposition


!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

module test
  use baseType
  use baseMethod
  implicit none
  contains

!----------------------------------------------------------------------------
!                                                                   test01
!----------------------------------------------------------------------------

subroutine test01()
  integer( i4b ), parameter :: n = 4
  real( dfp ) :: mat( n, n ), b( n ), x( n ), x0(n)
  write( *, * ) "hello world..."

  CALL Blanklines( NOL = 2 )
  call DISPLAY( "TEST-01" )
  CALL EqualLine( )
  call RANDOM_NUMBER( mat )
  call RANDOM_NUMBER( x ); x0 = x
  call DISPLAY( x, msg="    Exact solutions are :: ")
  b = MATMUL( mat, x )
  x = 0.0_dfp
  x = Solve( mat, b )
  call DISPLAY( val = x, msg= "    Solution from solve = " )

  if( NORM2( x - x0 ) .le. 1.0E-9 ) then
    call display( "TEST-01: SUCCESS" )
  else
    call display( "TEST-01: FAILED" )
  end if

  CALL Blanklines( NOL = 2 )
end subroutine test01

!----------------------------------------------------------------------------
!                                                                   test02
!----------------------------------------------------------------------------


subroutine test02()
  integer( i4b ), parameter :: n = 4, m=3
  real( dfp ) :: mat( n, n ), b( n, m ), x( n, m), x0( n, m )

  CALL Blanklines( NOL = 2 )
  call DISPLAY( "TEST-02" )
  CALL EqualLine( )
  call RANDOM_NUMBER( mat )
  call RANDOM_NUMBER( x ); x0 = x
  call DISPLAY( x, msg="    Exact solutions are :: ")
  b = MATMUL( mat, x )
  x = 0.0_dfp
  x = Solve( mat, b )
  call DISPLAY( val = x, msg= "    Solution from solve = " )

  if( NORM2( x - x0 ) .le. 1.0E-9 ) then
    call display( "TEST-02: SUCCESS" )
  else
    call display( "TEST-02: FAILED" )
  end if
  CALL Blanklines( NOL = 2 )
end subroutine test02

!----------------------------------------------------------------------------
!                                                                   test03
!----------------------------------------------------------------------------

subroutine test03()
  integer( i4b ), parameter :: m = 5, n = 3
  real( dfp ) :: mat( m, n ), b( m ), x( n ), x0(n)

  CALL Blanklines( NOL = 2 )
  call DISPLAY( "TEST-03" )
  CALL EqualLine( )
  call RANDOM_NUMBER( mat )
  call RANDOM_NUMBER( x ); x0 = x
  call DISPLAY( x, msg="    Exact solutions are :: ")
  b = MATMUL( mat, x )
  x = 0.0_dfp
  CALL GELS( A=mat, b=b, x = x )
  call DISPLAY( val = x, msg= "    Solution from solve = " )

  if( NORM2( x - x0 ) .le. 1.0E-9 ) then
    call display( "TEST-03: SUCCESS" )
  else
    call display( "TEST-03: FAILED" )
  end if
  CALL Blanklines( NOL = 2 )
end subroutine test03

!----------------------------------------------------------------------------
!                                                                   test04
!----------------------------------------------------------------------------

subroutine test04
  integer( i4b ), parameter :: m = 3, n = 5
  real( dfp ) :: mat( m, n ), b( m ), x( n ), x0(n)

  CALL Blanklines( NOL = 2 )
  call DISPLAY( "TEST-04" )
  CALL EqualLine( )
  call RANDOM_NUMBER( mat )
  call RANDOM_NUMBER( x ); x0 = x
  call DISPLAY( x, msg="    Exact solutions are :: ")
  b = MATMUL( mat, x )
  x = 0.0_dfp
  CALL GELS( A=mat, b=b, x = x )
  call DISPLAY( val = x, msg= "    Solution from solve = " )

  if( NORM2( x - x0 ) .le. 1.0E-9 ) then
    call display( "TEST-04: SUCCESS" )
  else
    call display( "TEST-04: FAILED" )
  end if
  CALL Blanklines( NOL = 2 )
end subroutine test04

!----------------------------------------------------------------------------
!                                                                   test05
!----------------------------------------------------------------------------

subroutine test05
  integer( i4b ), parameter :: m = 5, n = 3, nrhs=3
  real( dfp ) :: mat( m, n ), b( m, nrhs ), x( n, nrhs ), x0(n, nrhs)

  CALL Blanklines( NOL = 2 )
  call DISPLAY( "TEST-05" )
  CALL EqualLine( )
  call RANDOM_NUMBER( mat )
  call RANDOM_NUMBER( x ); x0 = x
  call DISPLAY( x, msg="    Exact solutions are :: ")
  b = MATMUL( mat, x )
  x = 0.0_dfp
  CALL GELS( A=mat, b=b, x = x )
  call DISPLAY( val = x, msg= "    Solution from solve = " )

  if( NORM2( x - x0 ) .le. 1.0E-9 ) then
    call display( "TEST-05: SUCCESS" )
  else
    call display( "TEST-05: FAILED" )
  end if
  CALL Blanklines( NOL = 2 )
end subroutine test05

!----------------------------------------------------------------------------
!                                                                   test06
!----------------------------------------------------------------------------

subroutine test06()
  integer( i4b ), parameter :: m = 5, n = 3
  real( dfp ) :: mat( m, n ), b( m ), x( n ), x0(n)

  CALL Blanklines( NOL = 2 )
  call DISPLAY( "TEST-06" )
  CALL EqualLine( )
  call RANDOM_NUMBER( mat )
  call RANDOM_NUMBER( x ); x0 = x
  call DISPLAY( x, msg="    Exact solutions are :: ")
  b = MATMUL( mat, x )
  x = 0.0_dfp
  CALL GELSD( A=mat, b=b, x = x )
  call DISPLAY( val = x, msg= "    Solution from solve = " )

  if( NORM2( x - x0 ) .le. 1.0E-9 ) then
    call display( "TEST-06: SUCCESS" )
  else
    call display( "TEST-06: FAILED" )
  end if
  CALL Blanklines( NOL = 2 )
end subroutine test06

!----------------------------------------------------------------------------
!                                                                   test07
!----------------------------------------------------------------------------

subroutine test07()
  integer( i4b ), parameter :: m = 5, n = 3
  real( dfp ) :: mat( m, n ), b( m ), x( n ), x0(n)

  CALL Blanklines( NOL = 2 )
  call DISPLAY( "TEST-07" )
  CALL EqualLine( )
  call RANDOM_NUMBER( mat )
  call RANDOM_NUMBER( x ); x0 = x
  call DISPLAY( x, msg="    Exact solutions are :: ")
  b = MATMUL( mat, x )
  x = 0.0_dfp
  CALL GELSS( A=mat, b=b, x = x )
  call DISPLAY( val = x, msg= "    Solution from solve = " )

  if( NORM2( x - x0 ) .le. 1.0E-9 ) then
    call display( "TEST-07: SUCCESS" )
  else
    call display( "TEST-07: FAILED" )
  end if
  CALL Blanklines( NOL = 2 )
end subroutine test07

end module test

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

program main
use test
call test01
call test02
call test03
call test04
call test05
call test06
call test07

end program main
