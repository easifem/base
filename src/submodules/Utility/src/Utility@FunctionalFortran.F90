! This program is a part of EASIFEM library
! Copyright (C) 2020-2021  Vikas Sharma, Ph.D
!
! This program is free software: you can redistribute it and/or modify
! it under the terms of the GNU General Public License as published by
! the Free Software Foundation, either version 3 of the License, or
! (at your option) any later version.
!
! This program is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.
!
! You should have received a copy of the GNU General Public License
! along with this program.  If not, see <https: //www.gnu.org/licenses/>
!

SUBMODULE(Utility ) FunctionalFortran
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                 ExpMesh
!----------------------------------------------------------------------------

MODULE PROCEDURE ExpMesh_Real64
  INTEGER( I4B ) :: i
  REAL( DFP ) :: alpha, beta
  !>
  IF( ABS(a - 1) .LT. TINY(1.0_DFP) ) THEN
    alpha = (rmax - rmin) / N
    DO i = 1, N+1
      ans(i) = alpha * (i-1.0_DFP) + rmin
    END DO
  ELSE
    IF( N .GT. 1 ) THEN
      beta = LOG(a) / (N-1)
      alpha = (rmax - rmin) / (EXP(beta*N) - 1)
      DO i = 1, N+1
          ans(i) = alpha * (exp(beta*(i-1)) - 1) + rmin
      END DO
    ELSE IF (N .EQ. 1) THEN
      ans(1) = rmin
      ans(2) = rmax
    END IF
  END IF
END PROCEDURE ExpMesh_Real64

!----------------------------------------------------------------------------
!                                                                 ExpMesh
!----------------------------------------------------------------------------

MODULE PROCEDURE ExpMesh_Real32
  INTEGER( I4B ) :: i
  REAL( DFP ) :: alpha, beta
  !>
  IF( ABS(a - 1) .LT. TINY(1.0_DFP) ) THEN
    alpha = (rmax - rmin) / N
    DO i = 1, N+1
      ans(i) = alpha * (i-1.0_DFP) + rmin
    END DO
  ELSE
    IF( N .GT. 1 ) THEN
      beta = LOG(a) / (N-1)
      alpha = (rmax - rmin) / (EXP(beta*N) - 1)
      DO i = 1, N+1
          ans(i) = alpha * (exp(beta*(i-1)) - 1) + rmin
      END DO
    ELSE IF (N .EQ. 1) THEN
      ans(1) = rmin
      ans(2) = rmax
    END IF
  END IF
END PROCEDURE ExpMesh_Real32

!----------------------------------------------------------------------------
!                                                               Linspace
!----------------------------------------------------------------------------

MODULE PROCEDURE LinSpace_Real32
  !   Local vars
  REAL( Real32 ) :: dx
  INTEGER( I4B ) :: i
  INTEGER( I4B ) :: nn
  !> main
  nn = INPUT( option=n, default=100 )
  ALLOCATE( ans( nn ) )
  dx=(b-a)/REAL((n-1),Real32)
  ans=[(i*dx+a, i=0,nn-1)]
END PROCEDURE LinSpace_Real32

!----------------------------------------------------------------------------
!                                                                 LinSpace
!----------------------------------------------------------------------------

MODULE PROCEDURE LinSpace_Real64
  !   Local vars
  REAL( Real64 ) :: dx
  INTEGER( I4B ) :: i
  INTEGER( I4B ) :: nn
  !> main
  nn = INPUT( option=n, default=100 )
  ALLOCATE( ans( nn ) )
  dx=(b-a)/REAL((n-1),Real64)
  ans=[(i*dx+a, i=0,nn-1)]
END PROCEDURE LinSpace_Real64

!----------------------------------------------------------------------------
!                                                                 MeshGrid
!----------------------------------------------------------------------------

MODULE PROCEDURE MeshGrid2D_Real64
  ! Local variables
  INTEGER( I4B ) :: nx
  INTEGER( I4B ) :: ny
  ! Initial setting
  nx=SIZE(xgv, dim=1)
  ny = SIZE(ygv, dim=1)
  CALL Reallocate( x, ny, nx )
  CALL Reallocate( y, ny, nx )
  x(:,:) = SPREAD( xgv, dim=2, ncopies=ny )
  y(:,:) = SPREAD( ygv, dim=1, ncopies=nx )
END PROCEDURE MeshGrid2D_Real64

!----------------------------------------------------------------------------
!                                                                 MeshGrid
!----------------------------------------------------------------------------

MODULE PROCEDURE MeshGrid2D_Real32
  ! Local variables
  INTEGER( I4B ) :: nx
  INTEGER( I4B ) :: ny
  ! Initial setting
  nx=SIZE(xgv, dim=1)
  ny = SIZE(ygv, dim=1)
  CALL Reallocate( x, ny, nx )
  CALL Reallocate( y, ny, nx )
  x(:,:) = SPREAD( xgv, dim=2, ncopies=ny )
  y(:,:) = SPREAD( ygv, dim=1, ncopies=nx )
END PROCEDURE MeshGrid2D_Real32

!----------------------------------------------------------------------------
!                                                              MeshGrid
!----------------------------------------------------------------------------

MODULE PROCEDURE MeshGrid3D_Real64
  integer :: nx, ny, nz, i
  nx = size(xgv) ; ny = size(ygv) ; nz = size(zgv)
  CALL Reallocate( x, nx, ny, nz )
  CALL Reallocate( y, nx, ny, nz )
  CALL Reallocate( z, nx, ny, nz )
  DO i=1,nz
    X(:,:,i) = SPREAD( xgv, dim=2, ncopies=ny )
    Y(:,:,i) = SPREAD( ygv, dim=1, ncopies=nx )
  END DO
  DO i=1,nx
    Z(i,:,:) = SPREAD( zgv, dim=1, ncopies=ny )
  END DO
END PROCEDURE MeshGrid3D_Real64

!----------------------------------------------------------------------------
!                                                              MeshGrid
!----------------------------------------------------------------------------

MODULE PROCEDURE MeshGrid3D_Real32
  integer :: nx, ny, nz, i
  nx = size(xgv) ; ny = size(ygv) ; nz = size(zgv)
  CALL Reallocate( x, ny, nx, nz )
  CALL Reallocate( y, ny, nx, nz )
  CALL Reallocate( z, ny, nx, nz )
  DO i=1,nz
    X(:,:,i) = SPREAD( xgv, dim=2, ncopies=ny )
    Y(:,:,i) = SPREAD( ygv, dim=1, ncopies=nx )
  END DO
  DO i=1,nx
    Z(i,:,:) = SPREAD( zgv, dim=1, ncopies=ny )
  END DO
END PROCEDURE MeshGrid3D_Real32

!----------------------------------------------------------------------------
!                                                                    arange
!----------------------------------------------------------------------------

MODULE PROCEDURE arange_int
  ! Internal var
  integer(i4b) :: incr
  integer(i4b) :: i
  integer(i4b) :: n
  incr = INPUT( default = 1, option=increment )
  n = ( iend - istart ) / incr+1
  ALLOCATE( Ans(n) )
  DO CONCURRENT( i = 1:n )
    Ans(i) = istart + ( i - 1 ) * incr
  enddo
END PROCEDURE arange_int

!----------------------------------------------------------------------------
!                                                                    arange
!----------------------------------------------------------------------------

MODULE PROCEDURE arange_real64
  ! internal var
  REAL( Real64 ) :: incr
  INTEGER( I4B ) :: i
  INTEGER( I4B ) :: n

  incr = INPUT( Default = 1.0_Real64, Option=increment )

  n = ( iend - istart + 0.5_Real64 * incr ) / incr + 1
  ALLOCATE( Ans( n ) )
  DO CONCURRENT( i = 1:n )
    Ans( i ) = istart + ( i - 1 ) * incr
  ENDDO
END PROCEDURE arange_real64

!----------------------------------------------------------------------------
!                                                                     arange
!----------------------------------------------------------------------------

MODULE PROCEDURE arange_real32
  ! internal var
  REAL( Real32 ) :: incr
  INTEGER( I4B ) :: i
  INTEGER( I4B ) :: n

  incr = INPUT( Default = 1.0_Real32, Option=increment )

  n = ( iend - istart + 0.5_Real32 * incr ) / incr + 1
  ALLOCATE( Ans( n ) )
  DO CONCURRENT( i = 1:n )
    Ans( i ) = istart + ( i - 1 ) * incr
  ENDDO
END PROCEDURE arange_real32

!----------------------------------------------------------------------------
!                                                                     Head
!----------------------------------------------------------------------------

MODULE PROCEDURE head_int
  ans = x( 1 )
END PROCEDURE head_int

!----------------------------------------------------------------------------
!                                                                     Head
!----------------------------------------------------------------------------

MODULE PROCEDURE head_real
  ans = x( 1 )
END PROCEDURE head_real

!----------------------------------------------------------------------------
!                                                                     Head
!----------------------------------------------------------------------------

MODULE PROCEDURE head_char
  ans( 1:1 ) = x( 1:1 )
END PROCEDURE head_char

!----------------------------------------------------------------------------
!                                                                 Tail
!----------------------------------------------------------------------------

MODULE PROCEDURE tail_int
  Ans = x(2:)
END PROCEDURE tail_int

!----------------------------------------------------------------------------
!                                                                 Tail
!----------------------------------------------------------------------------

MODULE PROCEDURE tail_real
  Ans = x(2:)
END PROCEDURE tail_real

!----------------------------------------------------------------------------
!                                                                 Tail
!----------------------------------------------------------------------------

MODULE PROCEDURE tail_char
  Ans = x(2:)
END PROCEDURE tail_char

!----------------------------------------------------------------------------
!                                                                      SPLIT
!----------------------------------------------------------------------------

MODULE PROCEDURE split_int
  if(section == 1)then
    Ans = x(1:size(x)/2)
  elseif(section == 2)then
    Ans = x(size(x)/2+1:)
  endif
END PROCEDURE

!----------------------------------------------------------------------------
!                                                                      SPLIT
!----------------------------------------------------------------------------

MODULE PROCEDURE split_real
  if(section == 1)then
    Ans = x(1:size(x)/2)
  elseif(section == 2)then
    Ans = x(size(x)/2+1:)
  endif
END PROCEDURE split_real

!----------------------------------------------------------------------------
!                                                                      SPLIT
!----------------------------------------------------------------------------

MODULE PROCEDURE split_char
  if (section == 1) then
    Ans = x(1:len(x) / 2)
  else if (section == 2) then
    Ans = x(len(x) / 2 + 1:)
  else
    Ans = ''
  end if
END PROCEDURE split_char


END SUBMODULE FunctionalFortran