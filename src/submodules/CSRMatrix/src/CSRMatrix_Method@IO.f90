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

!> authors: Vikas Sharma, Ph. D.
! date: 14 July 2021
! summary: This submodule contains the methods for sparse matrix

SUBMODULE( CSRMatrix_Method ) IO
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                    Display
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_Display
  INTEGER( I4B ) :: I
  I = INPUT( Option = UnitNo, Default = stdout )
  CALL Display( msg, unitNo = I )
  CALL Display( obj%csrOwnership, "CSR OWNERSHIP : " )
  CALL Display( obj%tDimension, "TOTAL DIMENSION : " )
  CALL Display( obj%MatrixProp, "MATRIX PROPERTY : " )
  IF( ASSOCIATED( obj%csr ) ) THEN
    CALL Display( obj=obj%csr, msg="CSR SPARSITY : ", unitNo=I )
  ELSE
    CALL Display( "CSR SPARSITY PATTERN obj%csr is not associated!" )
  END IF
  IF( ALLOCATED( obj%A ) ) THEN
    CALL DUMP(1, obj%csr%nrow, .true., obj%A, obj%csr%JA, obj%csr%IA, I )
  ELSE
    CALL DUMP(1, obj%csr%nrow, .false., obj%A, obj%csr%JA, obj%csr%IA, I )
  END IF
END PROCEDURE csrMat_Display

!----------------------------------------------------------------------------
!                                                                       Spy
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_SPY
  INTEGER( I4B ) :: nrow, ncol, mode, ptitle, nlines
  INTEGER( I4B ), ALLOCATABLE :: lines( : )
  REAL( DFP ) :: size
  CHARACTER( LEN = 2 ) :: munt
  nrow = obj%csr%nrow
  ncol = obj%csr%ncol
  mode = 0
  ptitle = 0
  size = 6
  munt = 'in'
  IF( obj%csr%dof%storageFMT .EQ. NODES_FMT ) THEN
    nlines = 1
    ALLOCATE( lines( nlines ) )
    lines = 1
  ELSE
    nlines = .tdof. obj%csr%dof
    ALLOCATE( lines( nlines ) )
    lines( 1 ) = obj%csr%dof%valmap( 1 )
    lines( 2:nlines ) = obj%csr%dof%valmap( 2:nlines ) - 1
  END IF
  CALL PSPLTM(nrow,ncol,mode,obj%csr%ja,obj%csr%ia,msg,ptitle,size,munt, &
    & nlines,lines,unitNo)
END PROCEDURE csrMat_SPY

END SUBMODULE IO