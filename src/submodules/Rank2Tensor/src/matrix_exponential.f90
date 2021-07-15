subroutine c8mat_expm1 ( n, a, e )

!*****************************************************************************80
!
!! C8MAT_EXPM1 is essentially MATLAB's built-in matrix exponential algorithm.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 March 2013
!
!  Author:
!
!    FORTRAN90 version by John Burkardt
!
!  Reference:
!
!    Cleve Moler, Charles VanLoan,
!    Nineteen Dubious Ways to Compute the Exponential of a Matrix,
!    Twenty-Five Years Later,
!    SIAM Review,
!    Volume 45, Number 1, March 2003, pages 3-49.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the dimension of the matrix.
!
!    Input, complex ( kind = 8 ) A(N,N), the matrix.
!
!    Output, complex ( kind = 8 ) E(N,N), the estimate for exp(A).
!
  implicit none

  integer ( kind = 4 ) n

  complex ( kind = 8 ) a(n,n)
  complex ( kind = 8 ) a2(n,n)
  real ( kind = 8 ) a_norm
  real ( kind = 8 ) c
  real ( kind = 8 ) c8mat_norm_li
  complex ( kind = 8 ) d(n,n)
  complex ( kind = 8 ) e(n,n)
  integer ( kind = 4 ) ee
  integer ( kind = 4 ) k
  logical p
  integer ( kind = 4 ) , parameter :: q = 6
  real ( kind = 8 ) r8_log_2
  integer ( kind = 4 ) s
  complex ( kind = 8 ) x(n,n)
!
!  Make a copy of the matrix.
!
  a2(1:n,1:n) = a(1:n,1:n)
!
!  Compute the L-infinity norm.
!
  a_norm = c8mat_norm_li ( n, n, a2 )
!
!  Determine a scaling factor for the matrix.
!
  ee = int ( r8_log_2 ( a_norm ) ) + 1

  s = max ( 0, ee + 1 )

  a2(1:n,1:n) = a2(1:n,1:n) / 2.0D+00 ** s

  x(1:n,1:n) = a2(1:n,1:n)

  c = 0.5D+00

  call c8mat_identity ( n, e )
  e(1:n,1:n) = e(1:n,1:n) + c * a2(1:n,1:n)

  call c8mat_identity ( n, d )
  d(1:n,1:n) = d(1:n,1:n) - c * a2(1:n,1:n)

  p = .true.

  do k = 2, q

    c = c * real ( q - k + 1, kind = 8 ) &
      / real ( k * ( 2 * q - k + 1 ), kind = 8 )

    x(1:n,1:n) = matmul ( a2(1:n,1:n), x(1:n,1:n) )

    e(1:n,1:n) = e(1:n,1:n) + c * x(1:n,1:n)

    if ( p ) then
      d(1:n,1:n) = d(1:n,1:n) + c * x(1:n,1:n)
    else
      d(1:n,1:n) = d(1:n,1:n) - c * x(1:n,1:n)
    end if

    p = .not. p

  end do
!
!  E -> inverse(D) * E
!
  call c8mat_minvm ( n, n, d, e, e )
!
!  E -> E^(2*S)
!
  do k = 1, s
    e(1:n,1:n) = matmul ( e(1:n,1:n), e(1:n,1:n) )
  end do

  return
end

subroutine r8mat_expm1 ( n, a, e )

!*****************************************************************************80
!
!! R8MAT_EXPM1 is essentially MATLAB's built-in matrix exponential algorithm.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    26 November 2011
!
!  Author:
!
!    Cleve Moler, Charles Van Loan
!
!  Reference:
!
!    Cleve Moler, Charles VanLoan,
!    Nineteen Dubious Ways to Compute the Exponential of a Matrix,
!    Twenty-Five Years Later,
!    SIAM Review,
!    Volume 45, Number 1, March 2003, pages 3-49.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the dimension of the matrix.
!
!    Input, real ( kind = 8 ) A(N,N), the matrix.
!
!    Output, real ( kind = 8 ) E(N,N), the estimate for exp(A).
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n,n)
  real ( kind = 8 ) a2(n,n)
  real ( kind = 8 ) a_norm
  real ( kind = 8 ) c
  real ( kind = 8 ) d(n,n)
  real ( kind = 8 ) e(n,n)
  integer ( kind = 4 ) ee
  integer ( kind = 4 ) k
  logical p
  integer ( kind = 4 ) , parameter :: q = 6
  real ( kind = 8 ) r8_log_2
  real ( kind = 8 ) r8mat_norm_li
  integer ( kind = 4 ) s
  real ( kind = 8 ) x(n,n)

  a2(1:n,1:n) = a(1:n,1:n)

  a_norm = r8mat_norm_li ( n, n, a2 )

  ee = int ( r8_log_2 ( a_norm ) ) + 1

  s = max ( 0, ee + 1 )

  a2(1:n,1:n) = a2(1:n,1:n) / 2.0D+00**s

  x(1:n,1:n) = a2(1:n,1:n)

  c = 0.5D+00

  call r8mat_identity ( n, e )
  e(1:n,1:n) = e(1:n,1:n) + c * a2(1:n,1:n)

  call r8mat_identity ( n, d )
  d(1:n,1:n) = d(1:n,1:n) - c * a2(1:n,1:n)

  p = .true.

  do k = 2, q

    c = c * real ( q - k + 1, kind = 8 ) &
      / real ( k * ( 2 * q - k + 1 ), kind = 8 )

    x(1:n,1:n) = matmul ( a2(1:n,1:n), x(1:n,1:n) )

    e(1:n,1:n) = e(1:n,1:n) + c * x(1:n,1:n)

    if ( p ) then
      d(1:n,1:n) = d(1:n,1:n) + c * x(1:n,1:n)
    else
      d(1:n,1:n) = d(1:n,1:n) - c * x(1:n,1:n)
    end if

    p = .not. p

  end do
!
!  E -> inverse(D) * E
!
  call r8mat_minvm ( n, n, d, e, e )
!
!  E -> E^(2*S)
!
  do k = 1, s
    e(1:n,1:n) = matmul ( e(1:n,1:n), e(1:n,1:n) )
  end do

  return
end

subroutine r8mat_expm2 ( n, a, e )

!*****************************************************************************80
!
!! R8MAT_EXPM2 uses the Taylor series for the matrix exponential.
!
!  Discussion:
!
!    Formally,
!
!      exp ( A ) = I + A + 1/2 A^2 + 1/3! A^3 + ...
!
!    This function sums the series until a tolerance is satisfied.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    26 November 2011
!
!  Author:
!
!    Cleve Moler, Charles Van Loan
!
!  Reference:
!
!    Cleve Moler, Charles VanLoan,
!    Nineteen Dubious Ways to Compute the Exponential of a Matrix,
!    Twenty-Five Years Later,
!    SIAM Review,
!    Volume 45, Number 1, March 2003, pages 3-49.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the dimension of the matrix.
!
!    Input, real ( kind = 8 ) A(N,N), the matrix.
!
!    Output, real ( kind = 8 ) E(N,N), the estimate for exp(A).
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n,n)
  real ( kind = 8 ) e(n,n)
  real ( kind = 8 ) f(n,n)
  real ( kind = 8 ) g(n,n)
  integer ( kind = 4 ) k
  logical r8mat_insignificant

  e(1:n,1:n) = 0.0D+00

  call r8mat_identity ( n, f )

  k = 1

  do

    if ( r8mat_insignificant ( n, n, e, f ) ) then
      exit
    end if

    e(1:n,1:n) = e(1:n,1:n) + f(1:n,1:n)

    f(1:n,1:n) = matmul ( a(1:n,1:n), f(1:n,1:n) ) / real ( k, kind = 8 )
    k = k + 1

  end do

  return
end

subroutine r8mat_expm3 ( n, a, e )

!*****************************************************************************80
!
!! R8MAT_EXPM3 approximates the matrix exponential using an eigenvalue approach.
!
!  Discussion:
!
!    exp(A) = V * D * V
!
!    where V is the matrix of eigenvectors of A, and D is the diagonal matrix
!    whose i-th diagonal entry is exp(lambda(i)), for lambda(i) an eigenvalue
!    of A.
!
!    This function is accurate for matrices which are symmetric, orthogonal,
!    or normal.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    26 November 2011
!
!  Author:
!
!    Cleve Moler, Charles Van Loan
!
!  Reference:
!
!    Cleve Moler, Charles VanLoan,
!    Nineteen Dubious Ways to Compute the Exponential of a Matrix,
!    Twenty-Five Years Later,
!    SIAM Review,
!    Volume 45, Number 1, March 2003, pages 3-49.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the dimension of the matrix.
!
!    Input, real ( kind = 8 ) A(N,N), the matrix.
!
!    Output, real ( kind = 8 ) E(N,N), the estimate for exp(A).
!
! [ V, D ] = eig ( A );
! E = V * diag ( exp ( diag ( D ) ) ) / V;

  return
end


!
!------------------------------------------------------------------------------
!                   Author      :   Vikas sharma
!                   Position    :   Doctral Student
!                   Institute   :   Kyoto Univeristy, Japan
!                   Program name:   TensorFunctions.part
!                   Last Update :   Dec-16-2017
!
!------------------------------------------------------------------------------
!                       Details of Program
!==============================================================================
!
!   TYPE    :: Part of module
!
!   DESCRIPTION ::
!       -   This part contains subroutines for computing various
!           Tensor valued Tensor functions
!
!   HOSTING FILE
!       -  Rank2Tensor_Class.f90
!==============================================================================

!------------------------------------------------------------------------------
!                                                                   TensorPower
!------------------------------------------------------------------------------

!  RECURSIVE FUNCTION TensorPower( N, T, TSquare, I1, I2, I3 ) RESULT( TP )

! !   Description
! !.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .
! !       1. -    T^n is computed using Cayley-Hamilton theorem
! !.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .

!     USE Utility, ONLY : Eye

!     ! Define Intent of dummy variables
!     REAL( DFP ), DIMENSION( :, : ), INTENT( IN ) :: T, TSquare
!     REAL( DFP ), DIMENSION( SIZE( T, 1 ), SIZE( T, 2 ) ) :: TP
!     REAL( DFP ), INTENT( IN ) :: I1, I2, I3
!     INTEGER( I4B ), INTENT( IN ) :: N

!     ! Define internal variables
!     INTEGER( I4B ) :: I
!     Error_Flag = .FALSE.

!     SELECT CASE( N )

!     CASE( 0 )
!         TP = Eye( SIZE( T, 1 ) )
!     CASE( 1 )
!         TP = T
!     CASE( 2 )
!         TP = TSquare
!     CASE DEFAULT
!         TP  = I1 * TensorPower( N-1, T, TSquare, I1, I2, I3 ) &
!             - I2 * TensorPower( N-2, T, TSquare, I1, I2, I3 ) &
!             + I3 * TensorPower( N-3, T, TSquare, I1, I2, I3 )
!     END SELECT
! !
!  END FUNCTION TensorPower

!------------------------------------------------------------------------------
!                                                               f_TensorEXP_1
!------------------------------------------------------------------------------

 FUNCTION f_TensorEXP_1( Mat, t, m )

!   Description
!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .
!       1. -    compute exp( T ) using time-series
!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .

    USE Utility, ONLY: Factorial, Eye, INT2STR

    ! Define Intent of dummy variables

    REAL( DFP ), DIMENSION( :, : ), INTENT( IN ) :: Mat
    REAL( DFP ), DIMENSION( SIZE( Mat, 1 ), SIZE( Mat, 2 ) ) :: f_TensorEXP_1
    INTEGER( I4B ), INTENT( IN ), OPTIONAL :: m
    REAL( DFP ), INTENT( IN ) :: t

    ! Define internal variables
    REAL( DFP ), ALLOCATABLE :: Dummy( :, : )
    INTEGER( I4B ) :: N = 20, I

    IF( PRESENT( m ) ) N = m

    IF( N .GE. 40 ) THEN

        CALL Err_Msg( "Rank2Tensor_Class.f90>>TensorFunctions.part", &
        "Tensor_Exp()", &
        "m is too large to compute the factorial; Program Stopped!")
        STOP

    END IF

    Dummy = Eye( SIZE( Mat, 1 ) )
    f_TensorEXP_1 = Dummy

    DO I = 1, N


        Dummy = MATMUL( Mat, Dummy )
        f_TensorEXP_1 = f_TensorEXP_1 + ( t**I ) * Dummy / REAL( Factorial( I ), KIND = DFP )

    END DO


    DEALLOCATE( Dummy )

 END FUNCTION f_TensorEXP_1

!------------------------------------------------------------------------------
!                                                               m_TensorEXP_1
!------------------------------------------------------------------------------

 FUNCTION m_TensorEXP_1( obj, t, m )

!   Description
!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .
!       1. -    compute exp( T ) using time-series
!.  .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .   .

    USE Utility, ONLY: Factorial, Eye

    ! Define Intent of dummy variables

    CLASS( Rank2Tensor_ ), INTENT( IN ) ::  obj
    REAL( DFP ), DIMENSION( 3, 3 ) :: m_TensorEXP_1
    INTEGER( I4B ), INTENT( IN ), OPTIONAL :: m
    REAL( DFP ), INTENT( IN ) :: t

    ! Define internal variables
    REAL( DFP ), ALLOCATABLE :: Mat( :, : )

    Mat = obj

    IF( PRESENT( m ) ) THEN

        m_TensorEXP_1 = f_TensorEXP_1( Mat, t, m )

    ELSE

        m_TensorEXP_1 = f_TensorEXP_1( Mat, t )

    END IF

    DEALLOCATE( Mat )

 END FUNCTION m_TensorEXP_1

!------------------------------------------------------------------------------
!
!------------------------------------------------------------------------------

