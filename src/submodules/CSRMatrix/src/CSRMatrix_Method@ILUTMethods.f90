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

SUBMODULE( CSRMatrix_Method ) ILUTMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                  getILUT
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_getILUT1
  INTEGER( I4B ) :: nnz, s( 2 ), ierr, IWK
  INTEGER( I4B ), ALLOCATABLE :: JW( : )
  REAL( DFP ), ALLOCATABLE :: W( : )

  s = SHAPE( obj )
  nnz = getNNZ( obj )
  ALLOCATE( JW( 2*s(1) ), W( s(1) + 1 ) )
  CALL Reallocate( JU, s(1) )
  IWK = 2*nnz
  DO
    CALL Reallocate( ALU, IWK, JLU, IWK )
    CALL ILUT( s(1), obj%A, obj%csr%JA, obj%csr%IA, lfil, droptol, &
      & ALU, JLU, JU, IWK, W, JW, ierr )
    IF( ierr .EQ. -2 .OR. ierr .EQ. -3 ) THEN
      IWK = 2*IWK
    ELSE
      EXIT
    END IF
  END DO
  !
  SELECT CASE( ierr )
  CASE( -1 )
    CALL ErrorMSG( &
      & "Input matrix may be wrong. (The elimination process has generated a &
        & row in L or U whose length is .gt.  n.)", &
      & "CSRMatrix_Method@ILUTMethods.f90", &
      & "csrMat_getILUT1()", &
      & __LINE__, stderr )
    STOP
  CASE( -2 )
    CALL ErrorMSG( &
      & "The matrix L overflows the array AL", &
      & "CSRMatrix_Method@ILUTMethods.f90", &
      & "csrMat_getILUT1()", &
      & __LINE__, stderr )
    STOP
  CASE( -3 )
    CALL ErrorMSG( &
      & "The matrix U overflows the array ALU", &
      & "CSRMatrix_Method@ILUTMethods.f90", &
      & "csrMat_getILUT1()", &
      & __LINE__, stderr )
    STOP
  CASE( -4 )
    CALL ErrorMSG( &
      & "Illegal value for lfil.", &
      & "CSRMatrix_Method@ILUTMethods.f90", &
      & "csrMat_getILUT1()", &
      & __LINE__, stderr )
    STOP
  CASE( -5 )
    CALL ErrorMSG( &
      & "zero row encountered", &
      & "CSRMatrix_Method@ILUTMethods.f90", &
      & "csrMat_getILUT1()", &
      & __LINE__, stderr )
    STOP
  END SELECT
  DEALLOCATE( JW, W )
END PROCEDURE csrMat_getILUT1

!----------------------------------------------------------------------------
!                                                                 getILUT
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_getILUT2
  REAL( DFP ), ALLOCATABLE :: ALU( : ), A( : ), WK( : )
  INTEGER( I4B ), ALLOCATABLE :: JLU( : ), JU( : ), JA( : ), IA( : ), IWK( : )
  INTEGER( I4B ) :: s( 2 ), ii, nnz

  CALL csrMat_getILUT1( obj=obj, ALU=ALU, JLU=JLU, JU=JU, lfil=lfil, &
    & droptol=droptol )
  s = SHAPE( obj )
  DO ii = 1, s( 1 )
    IF( ALU( ii ) .APPROXEQ. 0.0_DFP ) CYCLE
    ALU( ii ) = 1.0 / ALU( ii )
  END DO
  nnz = JLU( s( 1 ) + 1 )
  CALL Reallocate( WK, s(1), IWK, s(1)+1 )
  CALL Reallocate( A, nnz, JA, nnz, IA, s(1)+1 )
  CALL MSRCSR(s(1), ALU, JLU, A, JA, IA, WK, IWK )
  nnz = IA(s(1)+1)-1
  CALL Initiate( obj=Pmat, A=A(1:nnz), IA=IA, JA=JA(1:nnz) )
  DEALLOCATE( ALU, A, JLU, JU, JA, IA, WK, IWK )
END PROCEDURE csrMat_getILUT2

!----------------------------------------------------------------------------
!                                                                  getILUTP
!----------------------------------------------------------------------------

! subroutine ilutp(n,a,ja,ia,lfil,droptol,permtol,mbloc,alu,
! jlu,ju,iwk,w,jw,iperm,ierr)

MODULE PROCEDURE csrMat_getILUTP1
  INTEGER( I4B ) :: nnz, s( 2 ), ierr, IWK, k
  INTEGER( I4B ), ALLOCATABLE :: JW( : )
  REAL( DFP ), ALLOCATABLE :: W( : )

  s = SHAPE( obj )
  nnz = getNNZ( obj )
  ALLOCATE( JW( 2*s(1) ), W( s(1) ) )
  CALL Reallocate( JU, s(1), IPERM, 2*s(1) )
  IWK = 2*nnz
  !
  DO
    CALL Reallocate( ALU, IWK, JLU, IWK )
    CALL ILUTP( s(1), obj%A, obj%csr%JA, obj%csr%IA, lfil, droptol, &
      & permtol, mbloc, ALU, JLU, JU, IWK, W, JW, IPERM, ierr )
    IF( ierr .EQ. -2 .OR. ierr .EQ. -3 ) THEN
      IWK = 2*IWK
    ELSE
      EXIT
    END IF
  END DO
  !
  SELECT CASE( ierr )
  CASE( -1 )
    CALL ErrorMSG( &
      & "Input matrix may be wrong. (The elimination process has generated a &
      & row in L or U whose length is .gt.  n.)", &
      & "CSRMatrix_Method@ILUTMethods.f90", &
      & "csrMat_getILUTP1()", &
      & __LINE__, stderr )
    STOP
  CASE( -2 )
    CALL ErrorMSG( &
      & "The matrix L overflows the array AL", &
      & "CSRMatrix_Method@ILUTMethods.f90", &
      & "csrMat_getILUTP1()", &
      & __LINE__, stderr )
    STOP
  CASE( -3 )
    CALL ErrorMSG( &
      & "The matrix U overflows the array ALU", &
      & "CSRMatrix_Method@ILUTMethods.f90", &
      & "csrMat_getILUTP1()", &
      & __LINE__, stderr )
    STOP
  CASE( -4 )
    CALL ErrorMSG( &
      & "Illegal value for lfil.", &
      & "CSRMatrix_Method@ILUTMethods.f90", &
      & "csrMat_getILUTP1()", &
      & __LINE__, stderr )
    STOP
  CASE( -5 )
    CALL ErrorMSG( &
      & "zero row encountered", &
      & "CSRMatrix_Method@ILUTMethods.f90", &
      & "csrMat_getILUTP1()", &
      & __LINE__, stderr )
    STOP
  END SELECT
  !
  ! DO k=obj%csr%IA(1), obj%csr%IA(s(1)+1)-1
  !   obj%csr%JA(k) = IPERM(obj%csr%JA(k))
  ! END DO
  !
  DEALLOCATE( JW, W )
END PROCEDURE csrMat_getILUTP1

!----------------------------------------------------------------------------
!                                                                   getILUTP
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_getILUTP2
  REAL( DFP ), ALLOCATABLE :: ALU( : ), A( : ), WK( : )
  INTEGER( I4B ), ALLOCATABLE :: JLU( : ), JU( : ), JA( : ), IA( : ), IWK( : )
  INTEGER( I4B ) :: s( 2 ), ii, nnz

  CALL csrMat_getILUTP1( obj, ALU, JLU, JU, lfil, droptol, permtol, mbloc, &
    & IPERM )
  s = SHAPE( obj )
  DO ii = 1, s( 1 )
    IF( ALU( ii ) .APPROXEQ. 0.0_DFP ) CYCLE
    ALU( ii ) = 1.0 / ALU( ii )
  END DO
  nnz = JLU( s( 1 ) + 1 )
  CALL Reallocate( WK, s(1), IWK, s(1)+1 )
  CALL Reallocate( A, nnz, JA, nnz, IA, s(1)+1 )
  CALL MSRCSR( s(1), ALU, JLU, A, JA, IA, WK, IWK )
  nnz = IA(s(1)+1)-1
  CALL Initiate( obj=Pmat, A=A(1:nnz), IA=IA, JA=JA(1:nnz) )
  DEALLOCATE( ALU, A, JLU, JU, JA, IA, WK, IWK )
END PROCEDURE csrMat_getILUTP2

!----------------------------------------------------------------------------
!                                                                    getILUD
!----------------------------------------------------------------------------

!> subroutine ilud(n,a,ja,ia,alph,tol,alu,jlu,ju,iwk,w,jw,ierr)

MODULE PROCEDURE csrMat_getILUD1
  INTEGER( I4B ) :: nnz, s( 2 ), ierr, IWK, k
  INTEGER( I4B ), ALLOCATABLE :: JW( : )
  REAL( DFP ), ALLOCATABLE :: W( : )
  !
  s = SHAPE( obj )
  nnz = getNNZ( obj )
  ALLOCATE( JW( 2*s(1) ), W( 2*s(1) ) )
  CALL Reallocate( JU, s(1) )
  IWK = 2*nnz
  !
  DO
    CALL Reallocate( ALU, IWK, JLU, IWK )
    CALL ILUD(s(1),obj%A,obj%csr%JA,obj%csr%IA,alpha,droptol,ALU,JLU,JU,&
      & IWK,W,JW,ierr)
    IF( ierr .EQ. -2 .OR. ierr .EQ. -3 ) THEN
      IWK = 2*IWK
    ELSE
      EXIT
    END IF
  END DO
  !
  SELECT CASE( ierr )
  CASE( -1 )
    CALL ErrorMSG( &
      & "Input matrix may be wrong. (The elimination process has generated a &
      & row in L or U whose length is .gt.  n.)", &
      & "CSRMatrix_Method@ILUTMethods.f90", &
      & "csrMat_getILUD1()", &
      & __LINE__, stderr )
    STOP
  CASE( -2 )
    CALL ErrorMSG( &
      & "The matrix L overflows the array AL", &
      & "CSRMatrix_Method@ILUTMethods.f90", &
      & "csrMat_getILUD1()", &
      & __LINE__, stderr )
    STOP
  CASE( -3 )
    CALL ErrorMSG( &
      & "zero row encountered", &
      & "CSRMatrix_Method@ILUTMethods.f90", &
      & "csrMat_getILUD1()", &
      & __LINE__, stderr )
    STOP
  END SELECT
  DEALLOCATE( JW, W )
END PROCEDURE csrMat_getILUD1

!----------------------------------------------------------------------------
!                                                                  getILUD
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_getILUD2
  REAL( DFP ), ALLOCATABLE :: ALU( : ), A( : ), WK( : )
  INTEGER( I4B ), ALLOCATABLE :: JLU( : ), JU( : ), JA( : ), IA( : ), IWK( : )
  INTEGER( I4B ) :: s( 2 ), ii, nnz

  CALL csrMat_getILUD1( obj, ALU, JLU, JU, alpha, droptol )
  s = SHAPE( obj )
  DO ii = 1, s( 1 )
    IF( ALU( ii ) .APPROXEQ. 0.0_DFP ) CYCLE
    ALU( ii ) = 1.0 / ALU( ii )
  END DO
  nnz = JLU( s( 1 ) + 1 )
  CALL Reallocate( WK, s(1), IWK, s(1)+1 )
  CALL Reallocate( A, nnz, JA, nnz, IA, s(1)+1 )
  CALL MSRCSR( s(1), ALU, JLU, A, JA, IA, WK, IWK )
  nnz = IA(s(1)+1)-1
  CALL Initiate( obj=Pmat, A=A(1:nnz), IA=IA, JA=JA(1:nnz) )
  DEALLOCATE( ALU, A, JLU, JU, JA, IA, WK, IWK )
END PROCEDURE csrMat_getILUD2

!----------------------------------------------------------------------------
!                                                                  getILUDP
!----------------------------------------------------------------------------

!> subroutine ilud(n,a,ja,ia,alph,tol,alu,jlu,ju,iwk,w,jw,ierr)

MODULE PROCEDURE csrMat_getILUDP1
  INTEGER( I4B ) :: nnz, s( 2 ), ierr, IWK, k
  INTEGER( I4B ), ALLOCATABLE :: JW( : )
  REAL( DFP ), ALLOCATABLE :: W( : )
  !
  s = SHAPE( obj )
  nnz = getNNZ( obj )
  ALLOCATE( JW( 2*s(1) ), W( 2*s(1) ) )
  CALL Reallocate( JU, s(1), IPERM, 2*s(1) )
  IWK = 2*nnz
  !
  DO
    CALL Reallocate( ALU, IWK, JLU, IWK )
    CALL ILUDP(s(1),obj%A,obj%csr%JA,obj%csr%IA,alpha,droptol,permtol,&
      & mbloc,ALU,JLU,JU,IWK,W,JW,IPERM,ierr)
    IF( ierr .EQ. -2 .OR. ierr .EQ. -3 ) THEN
      IWK = 2*IWK
    ELSE
      EXIT
    END IF
  END DO
  !
  SELECT CASE( ierr )
  CASE( -1 )
    CALL ErrorMSG( &
      & "Input matrix may be wrong. (The elimination process has generated a &
      & row in L or U whose length is .gt.  n.)", &
      & "CSRMatrix_Method@ILUTMethods.f90", &
      & "csrMat_getILUDP1()", &
      & __LINE__, stderr )
    STOP
  CASE( -2 )
    CALL ErrorMSG( &
      & "The L/U matrix overflows the arrays ALU,JLU", &
      & "CSRMatrix_Method@ILUTMethods.f90", &
      & "csrMat_getILUDP1()", &
      & __LINE__, stderr )
    STOP
  CASE( -3 )
    CALL ErrorMSG( &
      & "zero row encountered", &
      & "CSRMatrix_Method@ILUTMethods.f90", &
      & "csrMat_getILUDP1()", &
      & __LINE__, stderr )
    STOP
  END SELECT
  DEALLOCATE( JW, W )
END PROCEDURE csrMat_getILUDP1

!----------------------------------------------------------------------------
!                                                                 getILUDP
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_getILUDP2
  REAL( DFP ), ALLOCATABLE :: ALU( : ), A( : ), WK( : )
  INTEGER( I4B ), ALLOCATABLE :: JLU( : ), JU( : ), JA( : ), IA( : ), IWK( : )
  INTEGER( I4B ) :: s( 2 ), ii, nnz

  CALL csrMat_getILUDP1( obj, ALU, JLU, JU, alpha, droptol, permtol, mbloc, &
    & IPERM )
  s = SHAPE( obj )
  DO ii = 1, s( 1 )
    IF( ALU( ii ) .APPROXEQ. 0.0_DFP ) CYCLE
    ALU( ii ) = 1.0 / ALU( ii )
  END DO
  nnz = JLU( s( 1 ) + 1 )
  CALL Reallocate( WK, s(1), IWK, s(1)+1 )
  CALL Reallocate( A, nnz, JA, nnz, IA, s(1)+1 )
  CALL MSRCSR( s(1), ALU, JLU, A, JA, IA, WK, IWK )
  nnz = IA(s(1)+1)-1
  CALL Initiate( obj=Pmat, A=A(1:nnz), IA=IA, JA=JA(1:nnz) )
  DEALLOCATE( ALU, A, JLU, JU, JA, IA, WK, IWK )
END PROCEDURE csrMat_getILUDP2

!----------------------------------------------------------------------------
!                                                                  getILUDP
!----------------------------------------------------------------------------

!> subroutine ilud(n,a,ja,ia,alph,tol,alu,jlu,ju,iwk,w,jw,ierr)
! iluk(n,a,ja,ia,lfil,alu,jlu,ju,levs,iwk,w,jw,ierr)

MODULE PROCEDURE csrMat_getILUK1
  INTEGER( I4B ) :: nnz, s( 2 ), ierr, IWK, k
  INTEGER( I4B ), ALLOCATABLE :: JW( : )
  REAL( DFP ), ALLOCATABLE :: W( : )
  !
  s = SHAPE( obj )
  nnz = getNNZ( obj )
  ALLOCATE( JW( 3*s(1) ), W( s(1) ) )
  CALL Reallocate( JU, s(1) )
  IWK = 2*nnz
  !
  DO
    CALL Reallocate( ALU, IWK, JLU, IWK, LEVS, IWK )
    CALL ILUK(s(1),obj%A,obj%csr%JA,obj%csr%IA,lfil,ALU,JLU,JU,LEVS,&
      & IWK,W,JW,ierr)
    IF( ierr .EQ. -2 .OR. ierr .EQ. -3 ) THEN
      IWK = 2*IWK
    ELSE
      EXIT
    END IF
  END DO
  !
  SELECT CASE( ierr )
  CASE( -1 )
    CALL ErrorMSG( &
      & "Input matrix may be wrong. (The elimination process has generated a &
      & row in L or U whose length is .gt.  n.)", &
      & "CSRMatrix_Method@ILUTMethods.f90", &
      & "csrMat_getILUK1()", &
      & __LINE__, stderr )
    STOP
  CASE( -2 )
    CALL ErrorMSG( &
      & "The matrix L overflows the array AL ", &
      & "CSRMatrix_Method@ILUTMethods.f90", &
      & "csrMat_getILUK1()", &
      & __LINE__, stderr )
    STOP
  CASE( -3 )
    CALL ErrorMSG( &
      & "The matrix U overflows the array ALU", &
      & "CSRMatrix_Method@ILUTMethods.f90", &
      & "csrMat_getILUK1()", &
      & __LINE__, stderr )
    STOP
  CASE( -4 )
    CALL ErrorMSG( &
      & "Illegal value for lfil.", &
      & "CSRMatrix_Method@ILUTMethods.f90", &
      & "csrMat_getILUK1()", &
      & __LINE__, stderr )
    STOP
  CASE( -5 )
    CALL ErrorMSG( &
      & "zero row encountered", &
      & "CSRMatrix_Method@ILUTMethods.f90", &
      & "csrMat_getILUK1()", &
      & __LINE__, stderr )
    STOP
  END SELECT
  DEALLOCATE( JW, W )
END PROCEDURE csrMat_getILUK1

!----------------------------------------------------------------------------
!                                                                 getILUK
!----------------------------------------------------------------------------

MODULE PROCEDURE csrMat_getILUK2
  REAL( DFP ), ALLOCATABLE :: ALU( : ), A( : ), WK( : )
  INTEGER( I4B ), ALLOCATABLE :: JLU( : ), JU( : ), JA( : ), IA( : ), IWK( : )
  INTEGER( I4B ) :: s( 2 ), ii, nnz

  CALL csrMat_getILUK1( obj, ALU, JLU, JU, lfil, LEVS )
  s = SHAPE( obj )
  DO ii = 1, s( 1 )
    IF( ALU( ii ) .APPROXEQ. 0.0_DFP ) CYCLE
    ALU( ii ) = 1.0 / ALU( ii )
  END DO
  nnz = JLU( s( 1 ) + 1 )
  CALL Reallocate( WK, s(1), IWK, s(1)+1 )
  CALL Reallocate( A, nnz, JA, nnz, IA, s(1)+1 )
  CALL MSRCSR( s(1), ALU, JLU, A, JA, IA, WK, IWK )
  nnz = IA(s(1)+1)-1
  CALL Initiate( obj=Pmat, A=A(1:nnz), IA=IA, JA=JA(1:nnz) )
  DEALLOCATE( ALU, A, JLU, JU, JA, IA, WK, IWK )
END PROCEDURE csrMat_getILUK2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE ILUTMethods