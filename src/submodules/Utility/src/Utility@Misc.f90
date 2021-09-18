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

SUBMODULE( Utility ) Misc
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                     Radian
!----------------------------------------------------------------------------

MODULE PROCEDURE radian_dfp
  Ans = deg / 180.0_DFP * 3.1415926535_DFP
END PROCEDURE

!----------------------------------------------------------------------------
!                                                                     Radian
!----------------------------------------------------------------------------

MODULE PROCEDURE radian_int
  Ans = REAL( deg, KIND=DFP ) / 180.0_DFP * 3.1415926535_DFP
END PROCEDURE

!----------------------------------------------------------------------------
!                                                                    Degrees
!----------------------------------------------------------------------------

MODULE PROCEDURE degrees_dfp
  Ans = rad / 3.1415926535_DFP * 180.0_DFP
END PROCEDURE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE Loc_Nearest_Point
  ! Define internal variables
  REAL( DFP ) :: xr( 3 )
  INTEGER( I4B ) :: i, n,m,norm,tr_norm

  n = SIZE( Array, 1 )
  m = SIZE( Array, 2 )
  IF( n .NE. SIZE(x) ) THEN
    CALL ErrorMSG(&
      & Msg="SearchNearestCoord >> size(Array,1) should be =size(x)", &
      & File= __FILE__, &
      & Line = __LINE__, &
      & Routine = "Loc_Nearest_Point(Array, x)" )
    STOP
  ENDIF
  !
  DO i = 1, m
    xr( 1:n ) = Array( 1:n, i )
    tr_norm = NORM2( xr(1:n) - x(1:n) )
    IF( i .EQ. 1 ) THEN
      norm = tr_norm
      id  = i
    ELSE
      IF( norm .GT. tr_norm ) THEN
        norm = tr_norm
        id  = i
      ELSE
        CYCLE
      END IF
    END IF
  END DO
END PROCEDURE

!----------------------------------------------------------------------------
!                                                            ExecuteCommand
!----------------------------------------------------------------------------

MODULE PROCEDURE exe_cmd
  ! Define internal variables
  INTEGER( I4B ) :: CMDSTAT, EXITSTAT
  LOGICAL( LGT ) :: WAIT = .TRUE.
  CHARACTER( LEN = 300 ) :: CMDMSG = ""

  CALL EXECUTE_COMMAND_LINE( TRIM(CMD), CMDSTAT = CMDSTAT, &
    & EXITSTAT = EXITSTAT, WAIT = WAIT, CMDMSG = CMDMSG )

  IF( CMDSTAT .NE. 0 ) THEN
    IF( CMDSTAT .EQ. -1 ) THEN
      CALL ErrorMsg( &
        & File = __FILE__, &
        & Routine = "exe_cmd()", &
        & Line = __LINE__, &
        & MSG = "following command failed " // TRIM( CMDMSG ) )
    END IF

    CALL ErrorMsg( &
      & File = __FILE__, &
      & Routine = "exe_cmd()", &
      & Line = __LINE__, &
      & MSG = "following command failed " // TRIM( CMDMSG ) )
    STOP
  END IF
END PROCEDURE

!----------------------------------------------------------------------------
!                                                                 getUnitNo
!----------------------------------------------------------------------------

MODULE PROCEDURE getUnitNo_1
  ! Define internal variables
  LOGICAL( LGT ) :: isOpen, isExist
  INTEGER( I4B ) :: Imin, Imax, I

  Imin = 10
  Imax = 1000

  DO I = Imin, Imax, 1
    INQUIRE( UNIT = I, OPENED = isOpen, EXIST = isExist )
    IF( isExist .AND. .NOT. isOpen ) EXIT
  END DO
  IF( isOpen .OR. .NOT. isExist ) THEN
    CALL ErrorMsg( &
      & File = __FILE__, &
      & Routine = "getUnitNo_1()", &
      & Line = __LINE__, &
      & MSG = " cannot find a valid unit number; Program Stopped" )
    STOP
  END IF
  ans = I
END PROCEDURE getUnitNo_1

!----------------------------------------------------------------------------
!                                                                 Factorial
!----------------------------------------------------------------------------

MODULE PROCEDURE Factorial
  IF ( N .EQ. 0 ) THEN
    Ans = 1
  ELSE
    Ans = N * Factorial( N - 1 )
  END IF
END PROCEDURE

!----------------------------------------------------------------------------
!                                                                    Int2Str
!----------------------------------------------------------------------------

MODULE PROCEDURE Int2Str
  CHARACTER( LEN = 15 ) :: Str
  WRITE( Str, "(I15)" ) I
  Int2Str = TRIM( ADJUSTL( Str ) )
END PROCEDURE

!----------------------------------------------------------------------------
!                                                                  Real2Str
!----------------------------------------------------------------------------

MODULE PROCEDURE SP2Str
  CHARACTER( LEN = 20 ) :: Str
  WRITE( Str, "(G17.7)" ) I
  SP2Str = TRIM( ADJUSTL( Str ) )
END PROCEDURE

!----------------------------------------------------------------------------
!                                                                  Real2Str
!----------------------------------------------------------------------------

MODULE PROCEDURE DP2Str
  CHARACTER( LEN = 20 ) :: Str
  WRITE( Str, "(G17.7)" ) I
  DP2Str = TRIM( ADJUSTL( Str ) )
END PROCEDURE

!----------------------------------------------------------------------------
!                                                                      ARTH
!----------------------------------------------------------------------------

MODULE PROCEDURE arth_r
  INTEGER(I4B) :: k,k2
  REAL(SP) :: temp
  IF (n > 0) arth_r(1)=first
  IF (n <= NPAR_ARTH) THEN
    DO k=2,n
      arth_r(k)=arth_r(k-1)+increment
    END DO
  ELSE
    DO k=2,NPAR2_ARTH
      arth_r(k)=arth_r(k-1)+increment
    END DO
    temp=increment*NPAR2_ARTH
    k=NPAR2_ARTH
    DO
      IF (k >= n) exit
      k2=k+k
      arth_r(k+1:min(k2,n))=temp+arth_r(1:min(k,n-k))
      temp=temp+temp
      k=k2
    END DO
  END IF
END PROCEDURE arth_r

!----------------------------------------------------------------------------
!                                                                      ARTH
!----------------------------------------------------------------------------

MODULE PROCEDURE arth_d
  INTEGER(I4B) :: k,k2
  REAL(DP) :: temp
  IF (n > 0) arth_d(1)=first
  IF (n <= NPAR_ARTH) THEN
    DO k=2,n
      arth_d(k)=arth_d(k-1)+increment
    END DO
  ELSE
    DO k=2,NPAR2_ARTH
      arth_d(k)=arth_d(k-1)+increment
    END DO
    temp=increment*NPAR2_ARTH
    k=NPAR2_ARTH
    DO
      IF (k >= n) exit
      k2=k+k
      arth_d(k+1:min(k2,n))=temp+arth_d(1:min(k,n-k))
      temp=temp+temp
      k=k2
    END DO
  END IF
END PROCEDURE arth_d

!----------------------------------------------------------------------------
!                                                                       ARTH
!----------------------------------------------------------------------------

MODULE PROCEDURE arth_i
  INTEGER(I4B) :: k,k2,temp
  IF (n > 0) arth_i(1)=first
  IF (n <= NPAR_ARTH) THEN
    DO k=2,n
      arth_i(k)=arth_i(k-1)+increment
    END DO
  ELSE
    DO k=2,NPAR2_ARTH
      arth_i(k)=arth_i(k-1)+increment
    END DO
    temp=increment*NPAR2_ARTH
    k=NPAR2_ARTH
    DO
      IF (k >= n) exit
      k2=k+k
      arth_i(k+1:min(k2,n))=temp+arth_i(1:min(k,n-k))
      temp=temp+temp
      k=k2
    END DO
  END IF
END PROCEDURE arth_i

!----------------------------------------------------------------------------
!                                                                 OuterDiff
!----------------------------------------------------------------------------

MODULE PROCEDURE outerdIFf_r
  outerdIFf_r = SPREAD(a,dim=2,ncopies=size(b)) - &
    SPREAD(b,dim=1,ncopies=size(a))
END PROCEDURE outerdIFf_r

MODULE PROCEDURE outerdIFf_d
  outerdIFf_d = SPREAD(a,dim=2,ncopies=size(b)) - &
    SPREAD(b,dim=1,ncopies=size(a))
END PROCEDURE outerdIFf_d

MODULE PROCEDURE outerdIFf_i
  outerdIFf_i = SPREAD(a,dim=2,ncopies=size(b)) - &
    SPREAD(b,dim=1,ncopies=size(a))
END PROCEDURE outerdIFf_i

!----------------------------------------------------------------------------
!                                                                   IMAXLOC
!----------------------------------------------------------------------------

MODULE PROCEDURE imaxloc_r
  INTEGER( I4B ), DIMENSION(1) :: imax
  imax = MAXLOC( arr(:) )
  imaxloc_r = imax(1)
END PROCEDURE imaxloc_r

!----------------------------------------------------------------------------
!                                                                   IMAXLOC
!----------------------------------------------------------------------------

MODULE PROCEDURE imaxloc_i
  INTEGER( I4B ), DIMENSION(1) :: imax
  imax = MAXLOC( iarr( : ) )
  imaxloc_i = imax(1)
END PROCEDURE imaxloc_i

!----------------------------------------------------------------------------
!                                                                    IMINLOC
!----------------------------------------------------------------------------

MODULE PROCEDURE iminloc_r
  INTEGER(I4B), DIMENSION(1) :: imin
  imin=MINLOC(arr(:))
  iminloc_r=imin(1)
END PROCEDURE iminloc_r

END SUBMODULE Misc