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

SUBMODULE(MiscUtility) Methods
USE BaseMethod
IMPLICIT NONE
INTEGER(I4B), PARAMETER :: NPAR_ARTH = 16, NPAR2_ARTH = 8
INTEGER(I4B), PARAMETER :: NPAR_GEOP = 4, NPAR2_GEOP = 2
INTEGER(I4B), PARAMETER :: NPAR_CUMSUM = 16
INTEGER(I4B), PARAMETER :: NPAR_CUMPROD = 8
INTEGER(I4B), PARAMETER :: NPAR_POLY = 8
INTEGER(I4B), PARAMETER :: NPAR_POLYTERM = 8
! pi to the full precision of DFP
REAL(DFP), PARAMETER :: PI_DFP = ACOS(-1.0_DFP)
CONTAINS

!----------------------------------------------------------------------------
!                                                                     Radian
!----------------------------------------------------------------------------

MODULE PROCEDURE radian_dfp
Ans = deg / 180.0_DFP * PI_DFP
END PROCEDURE

!----------------------------------------------------------------------------
!                                                                     Radian
!----------------------------------------------------------------------------

MODULE PROCEDURE radian_int
Ans = REAL(deg, KIND=DFP) / 180.0_DFP * PI_DFP
END PROCEDURE

!----------------------------------------------------------------------------
!                                                                    Degrees
!----------------------------------------------------------------------------

MODULE PROCEDURE degrees_dfp
Ans = rad / PI_DFP * 180.0_DFP
END PROCEDURE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE Loc_Nearest_Point
! Define internal variables
INTEGER(I4B) :: i, n, m
REAL(DFP) :: norm, tr_norm

n = SIZE(Array, 1)
m = SIZE(Array, 2)
IF (n .NE. SIZE(x)) THEN
  CALL ErrorMSG(&
    & Msg="SearchNearestCoord >> size(Array,1) should be =size(x)", &
    & File=__FILE__, &
    & Line=__LINE__, &
    & Routine="Loc_Nearest_Point(Array, x)")
  STOP
END IF
!
DO i = 1, m
  tr_norm = NORM2(Array(1:n, i) - x(1:n))
  IF (i .EQ. 1) THEN
    norm = tr_norm
    id = i
  ELSE
    IF (norm .GT. tr_norm) THEN
      norm = tr_norm
      id = i
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
INTEGER(I4B) :: CMDSTAT, EXITSTAT0
CHARACTER(LEN=300) :: CMDMSG

CMDMSG = ""
EXITSTAT0 = 0

CALL EXECUTE_COMMAND_LINE(TRIM(CMD), WAIT=.TRUE., CMDSTAT=CMDSTAT, &
                          EXITSTAT=EXITSTAT0, CMDMSG=CMDMSG)

IF (CMDSTAT .NE. 0) THEN
  CALL ErrorMsg( &
    & File=__FILE__, &
    & Routine="ExecuteCommand()", &
    & Line=__LINE__, &
    & MSG="the command could not be run: "//TRIM(CMD)//" "// &
    & TRIM(CMDMSG)//" "//TRIM(Str))
  STOP
END IF

IF (PRESENT(exitStat)) THEN
  exitStat = EXITSTAT0
  RETURN
END IF

IF (EXITSTAT0 .NE. 0) THEN
  CALL ErrorMsg( &
    & File=__FILE__, &
    & Routine="ExecuteCommand()", &
    & Line=__LINE__, &
    & MSG="the command failed with exit status "// &
    & TRIM(Int2Str(EXITSTAT0))//": "//TRIM(CMD)//" "//TRIM(Str))
  STOP
END IF
END PROCEDURE exe_cmd

!----------------------------------------------------------------------------
!                                                                 getUnitNo
!----------------------------------------------------------------------------

MODULE PROCEDURE getUnitNo_1
! Define internal variables
LOGICAL(LGT) :: isOpen, isExist
INTEGER(I4B) :: Imin, Imax, I

Imin = 10
Imax = 1000

DO I = Imin, Imax, 1
  INQUIRE (UNIT=I, OPENED=isOpen, EXIST=isExist)
  IF (isExist .AND. .NOT. isOpen) EXIT
END DO
IF (isOpen .OR. .NOT. isExist) THEN
  CALL ErrorMsg( &
    & File=__FILE__, &
    & Routine="getUnitNo_1()", &
    & Line=__LINE__, &
    & MSG=" cannot find a valid unit number; Program Stopped")
  STOP
END IF
ans = I
END PROCEDURE getUnitNo_1

!----------------------------------------------------------------------------
!                                                                 Factorial
!----------------------------------------------------------------------------

MODULE PROCEDURE Factorial
INTEGER(I4B) :: k

IF (N .LT. 0) THEN
  Ans = 0
  RETURN
END IF

Ans = 1
DO k = 2, N
  Ans = Ans * k
END DO
END PROCEDURE Factorial

!----------------------------------------------------------------------------
!                                                                    Int2Str
!----------------------------------------------------------------------------

MODULE PROCEDURE Int2Str
CHARACTER(LEN=15) :: Str
WRITE (Str, "(I15)") I
Int2Str = TRIM(ADJUSTL(Str))
END PROCEDURE

!----------------------------------------------------------------------------
!                                                                  Real2Str
!----------------------------------------------------------------------------

MODULE PROCEDURE SP2Str
CHARACTER(LEN=20) :: Str
WRITE (Str, "(G17.7)") I
SP2Str = TRIM(ADJUSTL(Str))
END PROCEDURE

!----------------------------------------------------------------------------
!                                                                  Real2Str
!----------------------------------------------------------------------------

MODULE PROCEDURE DP2Str
CHARACTER(LEN=20) :: Str
WRITE (Str, "(G17.7)") I
DP2Str = TRIM(ADJUSTL(Str))
END PROCEDURE

!----------------------------------------------------------------------------
!                                                                      ARTH
!----------------------------------------------------------------------------

MODULE PROCEDURE arth_r
INTEGER(I4B) :: k, k2
REAL(SP) :: temp
IF (n > 0) arth_r(1) = first
IF (n <= NPAR_ARTH) THEN
  DO k = 2, n
    arth_r(k) = arth_r(k - 1) + increment
  END DO
ELSE
  DO k = 2, NPAR2_ARTH
    arth_r(k) = arth_r(k - 1) + increment
  END DO
  temp = increment * NPAR2_ARTH
  k = NPAR2_ARTH
  DO
    IF (k >= n) exit
    k2 = k + k
    arth_r(k + 1:min(k2, n)) = temp + arth_r(1:min(k, n - k))
    temp = temp + temp
    k = k2
  END DO
END IF
END PROCEDURE arth_r

!----------------------------------------------------------------------------
!                                                                      ARTH
!----------------------------------------------------------------------------

MODULE PROCEDURE arth_d
INTEGER(I4B) :: k, k2
REAL(DP) :: temp
IF (n > 0) arth_d(1) = first
IF (n <= NPAR_ARTH) THEN
  DO k = 2, n
    arth_d(k) = arth_d(k - 1) + increment
  END DO
ELSE
  DO k = 2, NPAR2_ARTH
    arth_d(k) = arth_d(k - 1) + increment
  END DO
  temp = increment * NPAR2_ARTH
  k = NPAR2_ARTH
  DO
    IF (k >= n) exit
    k2 = k + k
    arth_d(k + 1:min(k2, n)) = temp + arth_d(1:min(k, n - k))
    temp = temp + temp
    k = k2
  END DO
END IF
END PROCEDURE arth_d

!----------------------------------------------------------------------------
!                                                                       ARTH
!----------------------------------------------------------------------------

MODULE PROCEDURE arth_i
INTEGER(I4B) :: k, k2, temp
IF (n > 0) arth_i(1) = first
IF (n <= NPAR_ARTH) THEN
  DO k = 2, n
    arth_i(k) = arth_i(k - 1) + increment
  END DO
ELSE
  DO k = 2, NPAR2_ARTH
    arth_i(k) = arth_i(k - 1) + increment
  END DO
  temp = increment * NPAR2_ARTH
  k = NPAR2_ARTH
  DO
    IF (k >= n) exit
    k2 = k + k
    arth_i(k + 1:min(k2, n)) = temp + arth_i(1:min(k, n - k))
    temp = temp + temp
    k = k2
  END DO
END IF
END PROCEDURE arth_i

!----------------------------------------------------------------------------
!                                                                 OuterDiff
!----------------------------------------------------------------------------

MODULE PROCEDURE outerdIFf_r
outerdIFf_r = SPREAD(a, dim=2, ncopies=size(b)) - &
              SPREAD(b, dim=1, ncopies=size(a))
END PROCEDURE outerdIFf_r

MODULE PROCEDURE outerdIFf_d
outerdIFf_d = SPREAD(a, dim=2, ncopies=size(b)) - &
              SPREAD(b, dim=1, ncopies=size(a))
END PROCEDURE outerdIFf_d

MODULE PROCEDURE outerdIFf_i
outerdIFf_i = SPREAD(a, dim=2, ncopies=size(b)) - &
              SPREAD(b, dim=1, ncopies=size(a))
END PROCEDURE outerdIFf_i

!----------------------------------------------------------------------------
!                                                                   IMAXLOC
!----------------------------------------------------------------------------

MODULE PROCEDURE imaxloc_r
INTEGER(I4B), DIMENSION(1) :: imax
imax = MAXLOC(arr(:))
imaxloc_r = imax(1)
END PROCEDURE imaxloc_r

!----------------------------------------------------------------------------
!                                                                   IMAXLOC
!----------------------------------------------------------------------------

MODULE PROCEDURE imaxloc_i
INTEGER(I4B), DIMENSION(1) :: imax
imax = MAXLOC(iarr(:))
imaxloc_i = imax(1)
END PROCEDURE imaxloc_i

!----------------------------------------------------------------------------
!                                                                    IMINLOC
!----------------------------------------------------------------------------

MODULE PROCEDURE iminloc_r
INTEGER(I4B), DIMENSION(1) :: imin
imin = MINLOC(arr(:))
iminloc_r = imin(1)
END PROCEDURE iminloc_r

!----------------------------------------------------------------------------
!                                                                    IMINLOC
!----------------------------------------------------------------------------

MODULE PROCEDURE iminloc_i
INTEGER(I4B), DIMENSION(1) :: imin
imin = MINLOC(iarr(:))
iminloc_i = imin(1)
END PROCEDURE iminloc_i

!----------------------------------------------------------------------------
!                                                                       IMG
!----------------------------------------------------------------------------

MODULE PROCEDURE IMG_1
ans = AIMAG(x)
END PROCEDURE IMG_1

!----------------------------------------------------------------------------
!                                                                       IMG
!----------------------------------------------------------------------------

MODULE PROCEDURE IMG_2
ans = AIMAG(x)
END PROCEDURE IMG_2

!----------------------------------------------------------------------------
!                                                                 Safe_ACOS
!----------------------------------------------------------------------------

MODULE PROCEDURE safe_ACOS
REAL(DFP) :: c2
!!
c2 = c
c2 = MAX(c2, -1.0_DFP)
c2 = MIN(c2, +1.0_DFP)
!!
ans = acos(c2)
END PROCEDURE safe_ACOS

!----------------------------------------------------------------------------
!                                                                 Safe_ASIN
!----------------------------------------------------------------------------

MODULE PROCEDURE safe_ASIN
REAL(DFP) :: s2
s2 = s
s2 = MAX(s2, -1.0_DFP)
s2 = MIN(s2, +1.0_DFP)
ans = asin(s2)
END PROCEDURE safe_ASIN

END SUBMODULE Methods
