SUBMODULE( RealMatrix_Method ) LAPACK
  !! This submodule contains linear algebra related methods for [[RealMatrix_]]
USE BaseMethod
IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                              JacobiMethod
!----------------------------------------------------------------------------

MODULE PROCEDURE eig_jacobi_method
  INTEGER( I4B ) :: i,ip,iq,n, ii, jj, tRot
  REAL( DFP ) :: c,g,h,s,sm,t,tau,theta,tresh
  REAL( DFP ), DIMENSION( SIZE( EigenValues ) ) :: b, z

  n = SIZE( Mat, 1 )

  !---- Initialize v to the identity matrix.
  CALL unit_matrix( EigenVectors( :, : ) )

  !---- Initialize b and d to the diagonal of A
  !---- This vector will accumulate terms of the
  !---- form tapq as in eq. (11.1.14).
  b(:) = get_diag( Mat( :, : ) )
  EigenValues( : ) = b(:)
  z(:) = 0.0_DFP
  tRot = 0

  DO i = 1, MaxIter

    !---- SUM off-diagonal elements.
    sm = SUM( ABS( Mat ), mask = upper_triangle( n, n ) )
    IF ( sm .EQ. 0.0_DFP ) THEN

      !---- Restore the upper diagonal  part
      DO jj = 1, n
          DO ii = 1, jj - 1
            Mat( ii, jj ) = Mat( jj, ii )
          END DO
      END DO

      !---- Sort the Eigen vector and Eigen values in decreasing order
      CALL SortEigenValues( EigenValues = EigenValues, &
        & EigenVectors = EigenVectors )
      RETURN

    END IF

    !---- The normal return, which relies on quadratic convergence
    !---- to machine underflow.
    tresh = MERGE( 0.2_DFP*sm / n**2, 0.0_DFP, i < 4 )

    !---- On the first three sweeps, we will rotate only IF tresh exceeded.
    DO ip = 1, n-1
      DO iq = ip+1, n
        g = 100.0_DFP * ABS( Mat( ip, iq ) )
        !---- After four sweeps, skip the rotation IF the off-diagonal
        !---- element is small.
        IF( (i > 4) .AND. ( ABS( EigenValues(  ip ) ) + g .EQ. &
          & ABS( EigenValues(  ip ) ) ) .AND. ( ABS( EigenValues(  iq ) ) &
          & + g .EQ. ABS( EigenValues(  iq ) ) ) ) THEN

          Mat( ip, iq ) = 0.0_DFP

        ELSE IF ( ABS( Mat( ip, iq ) ) .GT. tresh ) THEN

          h = EigenValues(  iq ) - EigenValues(  ip )

          IF ( ABS( h ) + g .EQ. ABS( h ) ) THEN
              t = Mat( ip, iq ) / h
          ELSE
              theta = 0.5_DFP * h / Mat( ip, iq )
              t = 1.0_DFP / ( ABS( theta )+SQRT( 1.0_DFP + theta**2))
              IF (theta .LT. 0.0_DFP) t = -t
          END IF

          c = 1.0_DFP / SQRT( 1 + t**2 )
          s = t * c
          tau = s / ( 1.0_DFP + c )
          h = t * Mat( ip, iq )
          z( ip ) = z( ip ) - h
          z( iq ) = z( iq ) + h
          EigenValues( ip ) = EigenValues( ip ) - h
          EigenValues( iq ) = EigenValues( iq ) + h
          Mat( ip, iq ) = 0.0_DFP

          CALL jrotate( Mat( 1 : ip-1, ip ), Mat( 1 : ip-1, iq ) )
          ! Case of rotations 1 ≤ j < p.
          CALL jrotate( Mat( ip, ip+1 : iq-1 ), Mat( ip+1 : iq-1,iq ))
          ! Case of rotations p < j < q.
          CALL jrotate( Mat( ip, iq+1 : n ), Mat( iq, iq+1 : n ) )
          ! Case of rotations q < j ≤ n.
          CALL jrotate( EigenVectors( :, ip ), EigenVectors( :, iq ) )
          tRot = tRot + 1
        END IF
      END DO
    END DO

    b( : ) = b( : ) + z( : )
    ! Update d with the SUM of tapq, and reinitialize z.
    EigenValues( : ) = b( : )
    z( : ) = 0.0_DFP
  END DO

  CONTAINS

  PURE SUBROUTINE jrotate(a1,a2)
      REAL(DFP), DIMENSION(:), INTENT(INOUT) :: a1,a2
      REAL(DFP), DIMENSION(SIZE(a1)) :: wk1
      wk1(:)=a1(:)
      a1(:)=a1(:)-s*(a2(:)+a1(:)*tau)
      a2(:)=a2(:)+s*(wk1(:)-a2(:)*tau)
  END SUBROUTINE jrotate

  PURE FUNCTION get_diag(mat)
    REAL( DFP ), DIMENSION( :, : ), INTENT( IN ) :: mat
    REAL( DFP ), DIMENSION( SIZE( mat, 1 ) ) :: get_diag
    INTEGER(I4B) :: j
    j=size(mat,1)
    DO j=1,size(mat,1)
      get_diag(j)=mat(j,j)
    END DO
  END FUNCTION get_diag

  PURE SUBROUTINE unit_matrix(mat)
    REAL(DFP), DIMENSION(:,:), INTENT(OUT) :: mat
    INTEGER(I4B) :: i,n
    n=min(size(mat,1),size(mat,2))
    mat(:,:)=0.0_sp
    DO i=1,n
      mat(i,i)=1.0_sp
    END DO
  END SUBROUTINE unit_matrix

  PURE SUBROUTINE SortEigenValues( EigenValues , EigenVectors )
    REAL( DFP ), DIMENSION(:), INTENT(INOUT) :: EigenValues
    REAL( DFP ), DIMENSION(:,:), INTENT(INOUT) :: EigenVectors
    INTEGER(I4B) :: i, j, n

    n = SIZE( EigenValues )

    DO i = 1, n-1
      j = ImaxLoc( EigenValues( i : n ) )
      j = j + i - 1
      IF ( j .NE. i ) THEN
        CALL SWAP( EigenValues( i ), EigenValues( j ) )
        CALL SWAP( EigenVectors( :, i ), EigenVectors( :, j ) )
      END IF
    END DO
  END SUBROUTINE SortEigenValues

  PURE FUNCTION upper_triangle(j,k,extra)
    INTEGER(I4B), INTENT(IN) :: j,k
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: extra
    LOGICAL(LGT), DIMENSION(j,k) :: upper_triangle
    INTEGER(I4B) :: n
    n=0
    IF (PRESENT(extra)) n=extra
    upper_triangle=(outerdIFf(arth(1,1,j),arth(1,1,k)) < n)
  END FUNCTION upper_triangle

  ! PURE FUNCTION lower_triangle(j,k,extra)
  !   INTEGER(I4B), INTENT(IN) :: j,k
  !   INTEGER(I4B), OPTIONAL, INTENT(IN) :: extra
  !   LOGICAL(LGT), DIMENSION(j,k) :: lower_triangle
  !   INTEGER(I4B) :: n
  !   n=0
  !   IF (PRESENT(extra)) n=extra
  !   lower_triangle=(outerdIFf(arth_i(1,1,j),arth_i(1,1,k)) > -n)
  ! END FUNCTION lower_triangle
END PROCEDURE eig_jacobi_method

! !------------------------------------------------------------------------------
! !                                                                  PowerMethod
! !------------------------------------------------------------------------------
! !
!  SUBROUTINE PowerMethod( Mat, Y, Lambda, Tol, MaxIter )
! !
! !   Description
! !------------------------------------------------------------------------------
! !       1.  -   POWER_METHOD applies the power method for a real eigenvalue.
! !           -   For a given (NxN) matrix "A" and an (N) vector "Y",
! !               the power method produces a series of estimates for "LAMBDA",
! !               the largest eigenvalue, and Y, the eigenvector corresponding
! !               to LAMBDA.
! !           -   IF the matrix A has a single real eigenvalue of maximum modulus,
! !               THEN this iteration will generally produce a good estimate for
! !               that eigenvalue and its corresponding eigenvector.
! !           -   IF there are multiple distinct eigenvalues of the same modulus,
! !               perhaps two values of opposite sign, or complex eigenvalues,
! !               THEN the situation is more complicated.
! !           -   When estimating the value of LAMBDA, we use the Rayleigh
! !               quotient, LAMBDA = ( y' * A * y ) / ( y' * y ).
! !               Since we normalize Y, the bottom of the fraction is 1.
! !               Using this estimate allows us to easily capture the sign of
! !               LAMDBA.  Using the euclidean norm instead, for instance,
! !               would always give a positive value.
! !           -   IF the DOminant eigenvalue is negative, THEN the iteration
! !               as given will produce eigenvector iterates that alternate
! !               in sign.
! !           -   It is worth knowing whether the successive eigenvector estimates
! !               are tENDing to some value.  Since an eigenvector is really a
! !               direction, we need to normalize the vectors, and we need to
! !               somehow treat both a vector and its negative as holding the
! !               same information.  This means that the proper way of measuring
! !               the dIFference between two eigenvector estimates is to normalize
! !               them both, and THEN compute the cosine between them as y1'y2,
! !               followed by the sine, which is sqrt ( 1 - ( y1'y2)^2 ).
! !               IF this sine is small, the vectors y1 and y2 are "close" in the
! !               sense of direction.
! !
! ! Ref -   https://people.sc.fsu.edu/~jburkardt/f_src/
! !                       power_method/power_method.html
! !   Author  -   John Burkardt
! !
! !------------------------------------------------------------------------------
! !
!     ! USE Utility, ONLY : assert_eq
! ! Define Intent of dummy arguments
!     REAL( DFP ), DIMENSION( :, : ), INTENT( IN ) :: Mat
!     REAL( DFP ), DIMENSION( : ), INTENT( INOUT ) :: Y
!     INTEGER( I4B ), INTENT( IN ) :: MaxIter
!     REAL( DFP ), INTENT( IN ) :: Tol
!     REAL( DFP ), INTENT(  OUT ) :: Lambda

!     INTEGER( I4B ) :: n , Counter
!     REAL( DFP ) :: cos_y1y2, lambda_old, sin_y1y2, val_dIF
!     REAL( DFP ), ALLOCATABLE, DIMENSION( : ) :: ay, y_old
!     LOGICAL( LGT ), PARAMETER :: debug = .FALSE.

! ! get N
! ! Flag-1
!     n = assert_eq( &
!     (/SIZE( Mat, 1 ), SIZE( Mat, 2 ), SIZE( Y )/) , &
!     "LinearAlgebra.f90>>EigenSystem.part>>PowerMethod, Flag-1 " )
! ! Allocate arrays
!     ALLOCATE( ay( n ), y_old( n ) )
! !
! !  Force Y to be a vector of unit norm.
!     y( 1 : n ) = y( 1 : n ) / SQRT ( SUM ( y( 1 : n )**2 ) )
!     Counter = 0
!     y_old( 1 : n ) = y( 1 : n )
! !
! !  Compute AY = A*Y.
!     ay( 1 : n ) = MATMUL ( Mat( 1 : n, 1 : n ), y( 1 : n ) )
! !
! !  Estimate LAMBDA = (AY,Y)/(Y,Y).
!     Lambda = DOT_PRODUCT ( y( 1 : n ), ay( 1 : n ) )
! !
! !  Force AY to have unit norm and Replace Y by AY.
!     y( 1 : n ) = ay( 1 : n ) / SQRT ( SUM ( ay( 1 : n )**2 ) )
! !
! !  The sign of Y is optional.  IF LAMBDA is probably negative,
! !  switch sign of new Y to match old one.
!     IF ( lambda < 0.0_DFP ) THEN
!       y( 1 : n ) = - y( 1 : n )
!     END IF
! !
!     val_dIF = 0.0_DFP
!     cos_y1y2 = DOT_PRODUCT ( y(1:n), y_old(1:n) )
!     sin_y1y2 = SQRT( ( 1.0_DFP - cos_y1y2 ) * ( 1.0_DFP + cos_y1y2 ) )
! !
! !  Now repeat these steps in an iteration.
! DO Counter = 1, MaxIter
! !
!     lambda_old = lambda
!     y_old( 1 : n ) = y( 1 : n )
!     ay( 1 : n ) = MATMUL ( Mat( 1 : n, 1 : n ), y( 1 : n ) )
!     Lambda = DOT_PRODUCT ( y( 1 : n ), ay( 1 : n ) )
! !
!     y( 1 : n ) = ay( 1 : n ) / SQRT( SUM ( ay( 1 : n )**2 ) )
!     IF ( lambda < 0.0_DFP ) THEN
!         y( 1 : n ) = - y( 1 : n )
!     END IF
! !
!     val_dif = ABS ( Lambda - lambda_old )
!     cos_y1y2 = DOT_PRODUCT ( y(1:n), y_old(1:n) )
!     sin_y1y2 = SQRT ( ( 1.0_DFP - cos_y1y2 ) * ( 1.0_DFP + cos_y1y2 ) )
! !
!     IF ( val_dIF .LE. tol ) THEN
!         EXIT
!     END IF
! END DO
!     y(1:n) = ay(1:n) / lambda
! !
!     DEALLOCATE( ay, y_old )
! END SUBROUTINE PowerMethod
! !
! !------------------------------------------------------------------------------
! !                                                                  PowerMethod2
! !------------------------------------------------------------------------------
! !
!  SUBROUTINE PowerMethod2( Mat, V, MaxIter, tol, Lambda )
! !
! !   Description
! !------------------------------------------------------------------------------
! !       1   -   POWER_METHOD2 applies the power method for possibly
! !               complex eigenvalues.
! !           -   V is vector of "Complex" number; V contains the Initial Guess
! !               for eigenvectors in its real components
! !------------------------------------------------------------------------------
! ! Define Intent of dummy arguments
! !------------------------------------------------------------------------------
! !
! !  USE Utility, ONLY : Assert_Eq
! !
!     REAL( DFP ), DIMENSION( :, : ), INTENT( IN ) :: Mat
!     !REAL( DFP ), DIMENSION( : ), INTENT( IN ) :: X0
!     COMPLEX ( DFPC ), DIMENSION( : ), INTENT( INOUT ) :: V
!     COMPLEX ( DFPC ), INTENT( OUT ) :: Lambda
!     INTEGER( I4B ), INTENT( IN ) :: MaxIter
!     REAL( DFP ), INTENT( IN ) :: Tol
! !
! ! Define Internal varibles
!     INTEGER(  I4B ) :: N, Iter, IterNum
!     REAL( DFP ), ALLOCATABLE, DIMENSION( : ) :: X, Y, Z
!     REAL( DFP ) :: Alpha, Beta, Gamma, Lambda_Imag, Lambda_Real, &
!     Pi_xx, Pi_xy, Pi_xz, Pi_yy, Pi_yz, Pi_zz
! !
!     IterNum = 0
!     N = Assert_Eq( SIZE( Mat, 1 ), SIZE( Mat, 2 ), SIZE( V ),&
!     "PowerMethod2" )
! !
!     ALLOCATE( X( N ), Y( N ), Z( N ) )
! ! Initiate Data
!     X ( : ) = REAL( V( : ) , KIND = DFP ) !X( : ) = X0( : )
!     Pi_xx = DOT_PRODUCT(  X, X )
!     X = X / Pi_xx
!     Y = MATMUL( Mat, X )
!     Pi_xy = DOT_PRODUCT( X, Y )
!     Pi_yy = DOT_PRODUCT( Y, Y )
! !
!     DO Iter = 1, MaxIter
!         IF( Pi_yy - Pi_xy * Pi_xy .LT. Tol * Tol * Pi_yy ) THEN
!             Lambda = Pi_xy
!             V( : ) = Y( : ) / SQRT( Pi_yy )
!             IF( ALLOCATED( X ) ) DEALLOCATE( X )
!             IF( ALLOCATED( Y ) ) DEALLOCATE( Y )
!             IF( ALLOCATED( Z ) ) DEALLOCATE( Z )
!             RETURN
!         END IF
! !
!         Z( : ) = MATMUL( Mat, Y )
!         Pi_xz = DOT_PRODUCT( X, Z )
!         Pi_yz = DOT_PRODUCT( Y, Z )
!         Pi_zz = DOT_PRODUCT( Z, Z )
! !
!         Alpha = - ( Pi_yz - Pi_xy * Pi_xz ) / ( Pi_yy - Pi_xy * Pi_xy )
!         Beta = ( Pi_xy * Pi_yz - Pi_yy * Pi_xz ) / ( Pi_yy - Pi_xy * Pi_xy )
!         Gamma = Pi_zz + Alpha * Alpha * Pi_yy + Beta * Beta &
!         + 2.0_DFP * ( Alpha * Pi_yz + Beta * Pi_xz + Alpha * Beta * Pi_xy )
! !
!         IF( Gamma .LT. Tol * Tol * Pi_zz &
!         .AND. Alpha * Alpha .LT. 4.0_DFP * Beta ) THEN
!             Lambda_Real = - Alpha / 2.0_DFP
!             Lambda_Imag = SQRT( 4.0_DFP * Beta - Alpha * Alpha ) / 2.0_DFP
!             Lambda = CMPLX( Lambda_Real, Lambda_Imag )
!             V = ( Lambda * Y - Z ) / SQRT ( Beta * Pi_yy+Alpha * Pi_yz+Pi_zz )
!             RETURN
!         END IF
! !
!         X = Y / SQRT( Pi_yy )
!         Y = Z / SQRT( Pi_yy )
!         Pi_xy = Pi_yz / Pi_yy
!         Pi_yy = Pi_zz / Pi_yy
!         IterNum = Iter
!     END DO
! !
!   WRITE ( *, '(a)' ) ' '
!   WRITE ( *, '(a)' ) 'PowerMethod2 - Fatal error!'
!   WRITE ( *, '(a)' ) '  Convergence was not reached.'
! !
!   STOP
! !
! END SUBROUTINE PowerMethod2

END SUBMODULE LAPACK