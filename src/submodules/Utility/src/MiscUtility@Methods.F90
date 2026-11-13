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

#define NPAR_ARTH 16
#define NPAR2_ARTH 8
#define NPAR_GEOP 4
#define NPAR2_GEOP 2
#define NPAR_CUMSUM 16
#define NPAR_CUMPROD 8
#define NPAR_POLY 8
#define NPAR_POLYTERM 8

SUBMODULE(MiscUtility) Methods
USE ErrorHandling, ONLY: ErrorMsg
USE BaseType, ONLY: math => TypeMathOpt
IMPLICIT NONE
! INTEGER(I4B), PARAMETER :: NPAR_ARTH = 16
! INTEGER(I4B), PARAMETER :: NPAR2_ARTH = 8
! INTEGER(I4B), PARAMETER :: NPAR_GEOP = 4
! INTEGER(I4B), PARAMETER :: NPAR2_GEOP = 2
! INTEGER(I4B), PARAMETER :: NPAR_CUMSUM = 16
! INTEGER(I4B), PARAMETER :: NPAR_CUMPROD = 8
! INTEGER(I4B), PARAMETER :: NPAR_POLY = 8
! INTEGER(I4B), PARAMETER :: NPAR_POLYTERM = 8

CONTAINS

!----------------------------------------------------------------------------
!                                                                     Radian
!----------------------------------------------------------------------------

MODULE PROCEDURE radian_dfp
ans = deg / 180.0_DFP * math%pi
END PROCEDURE radian_dfp

!----------------------------------------------------------------------------
!                                                                     Radian
!----------------------------------------------------------------------------

MODULE PROCEDURE radian_int
ans = REAL(deg, KIND=DFP) / 180.0_DFP * math%pi
END PROCEDURE radian_int

!----------------------------------------------------------------------------
!                                                                    Degrees
!----------------------------------------------------------------------------

MODULE PROCEDURE degrees_dfp
ans = rad / math%pi * 180.0_DFP
END PROCEDURE degrees_dfp

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
  CALL ErrorMSG( &
    Msg="SearchNearestCoord >> size(Array,1) should be =size(x)", &
    File=__FILE__, &
    Line=__LINE__, &
    Routine="Loc_Nearest_Point(Array, x)")
  STOP
END IF
!
mainloop: DO i = 1, m
  tr_norm = NORM2(Array(1:n, i) - x(1:n))
  IF (i .EQ. 1) THEN
    norm = tr_norm
    id = i
  ELSE
    IF (norm .GT. tr_norm) THEN
      norm = tr_norm
      id = i
    ELSE
      CYCLE mainloop
    END IF
  END IF
END DO mainloop
END PROCEDURE Loc_Nearest_Point

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
    File=__FILE__, &
    Routine="ExecuteCommand()", &
    Line=__LINE__, &
    MSG="the command could not be run: "//TRIM(CMD)//" "// &
    TRIM(CMDMSG)//" "//TRIM(Str))
  STOP
END IF

IF (PRESENT(exitStat)) THEN
  exitStat = EXITSTAT0
  RETURN
END IF

IF (EXITSTAT0 .NE. 0) THEN
  CALL ErrorMsg( &
    File=__FILE__, &
    Routine="ExecuteCommand()", &
    Line=__LINE__, &
    MSG="the command failed with exit status "// &
    TRIM(Int2Str(EXITSTAT0))//": "//TRIM(CMD)//" "//TRIM(Str))
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

mainloop: DO I = Imin, Imax, 1
  INQUIRE (UNIT=I, OPENED=isOpen, EXIST=isExist)
  IF (isExist .AND. .NOT. isOpen) EXIT mainloop
END DO mainloop

IF (isOpen .OR. .NOT. isExist) THEN
  CALL ErrorMsg(File=__FILE__, &
                Routine="getUnitNo_1()", &
                Line=__LINE__, &
                MSG=" cannot find a valid unit number; Program Stopped")
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
CHARACTER(LEN=15) :: astr
WRITE (astr, "(I15)") I
ans = TRIM(ADJUSTL(astr))
END PROCEDURE Int2Str

!----------------------------------------------------------------------------
!                                                                  Real2Str
!----------------------------------------------------------------------------

MODULE PROCEDURE SP2Str
CHARACTER(LEN=20) :: astr
WRITE (astr, "(G17.7)") I
ans = TRIM(ADJUSTL(astr))
END PROCEDURE SP2Str

!----------------------------------------------------------------------------
!                                                                  Real2Str
!----------------------------------------------------------------------------

MODULE PROCEDURE DP2Str
CHARACTER(LEN=20) :: astr
WRITE (astr, "(G17.7)") I
ans = TRIM(ADJUSTL(astr))
END PROCEDURE DP2Str

!----------------------------------------------------------------------------
!                                                                      ARTH
!----------------------------------------------------------------------------

MODULE PROCEDURE arth_r
INTEGER(I4B) :: k, k2
REAL(REAL32) :: temp
IF (n > 0) ans(1) = first
IF (n <= NPAR_ARTH) THEN
  loop1: DO k = 2, n
    ans(k) = ans(k - 1) + increment
  END DO loop1
ELSE
  loop2: DO k = 2, NPAR2_ARTH
    ans(k) = ans(k - 1) + increment
  END DO loop2
  temp = increment * NPAR2_ARTH
  k = NPAR2_ARTH
  loop3: DO
    IF (k >= n) EXIT loop3
    k2 = k + k
    ans(k + 1:MIN(k2, n)) = temp + ans(1:MIN(k, n - k))
    temp = temp + temp
    k = k2
  END DO loop3
END IF
END PROCEDURE arth_r

!----------------------------------------------------------------------------
!                                                                      ARTH
!----------------------------------------------------------------------------

MODULE PROCEDURE arth_d
INTEGER(I4B) :: k, k2
REAL(REAL64) :: temp
IF (n > 0) ans(1) = first
IF (n <= NPAR_ARTH) THEN
  DO k = 2, n
    ans(k) = ans(k - 1) + increment
  END DO
ELSE
  DO k = 2, NPAR2_ARTH
    ans(k) = ans(k - 1) + increment
  END DO
  temp = increment * NPAR2_ARTH
  k = NPAR2_ARTH
  loop3: DO
    IF (k >= n) EXIT loop3
    k2 = k + k
    ans(k + 1:MIN(k2, n)) = temp + ans(1:MIN(k, n - k))
    temp = temp + temp
    k = k2
  END DO loop3
END IF
END PROCEDURE arth_d

!----------------------------------------------------------------------------
!                                                                       ARTH
!----------------------------------------------------------------------------

MODULE PROCEDURE arth_i
INTEGER(I4B) :: k, k2, temp
IF (n > 0) ans(1) = first
IF (n <= NPAR_ARTH) THEN
  DO k = 2, n
    ans(k) = ans(k - 1) + increment
  END DO
ELSE
  DO k = 2, NPAR2_ARTH
    ans(k) = ans(k - 1) + increment
  END DO
  temp = increment * NPAR2_ARTH
  k = NPAR2_ARTH
  loop3: DO
    IF (k >= n) EXIT loop3
    k2 = k + k
    ans(k + 1:MIN(k2, n)) = temp + ans(1:MIN(k, n - k))
    temp = temp + temp
    k = k2
  END DO loop3
END IF
END PROCEDURE arth_i

!----------------------------------------------------------------------------
!                                                                 OuterDiff
!----------------------------------------------------------------------------

MODULE PROCEDURE OUTERDIFF_R
ans = SPREAD(a, dim=2, ncopies=SIZE(b)) - &
      SPREAD(b, dim=1, ncopies=SIZE(a))
END PROCEDURE OUTERDIFF_R

MODULE PROCEDURE OUTERDIFF_D
ans = SPREAD(a, dim=2, ncopies=SIZE(b)) - &
      SPREAD(b, dim=1, ncopies=SIZE(a))
END PROCEDURE OUTERDIFF_D

MODULE PROCEDURE OUTERDIFF_I
ans = SPREAD(a, dim=2, ncopies=SIZE(b)) - &
      SPREAD(b, dim=1, ncopies=SIZE(a))
END PROCEDURE OUTERDIFF_I

!----------------------------------------------------------------------------
!                                                                   IMAXLOC
!----------------------------------------------------------------------------

MODULE PROCEDURE IMAXLOC_R
INTEGER(I4B), DIMENSION(1) :: imax
imax = MAXLOC(arr(:))
ans = imax(1)
END PROCEDURE IMAXLOC_R

!----------------------------------------------------------------------------
!                                                                   IMAXLOC
!----------------------------------------------------------------------------

MODULE PROCEDURE IMAXLOC_I
INTEGER(I4B), DIMENSION(1) :: imax
imax = MAXLOC(iarr(:))
ans = imax(1)
END PROCEDURE IMAXLOC_I

!----------------------------------------------------------------------------
!                                                                    IMINLOC
!----------------------------------------------------------------------------

MODULE PROCEDURE IMINLOC_R
INTEGER(I4B), DIMENSION(1) :: imin
imin = MINLOC(arr(:))
ans = imin(1)
END PROCEDURE IMINLOC_R

!----------------------------------------------------------------------------
!                                                                    IMINLOC
!----------------------------------------------------------------------------

MODULE PROCEDURE IMINLOC_I
INTEGER(I4B), DIMENSION(1) :: imin
imin = MINLOC(iarr(:))
ans = imin(1)
END PROCEDURE IMINLOC_I

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

MODULE PROCEDURE SAFE_ACOS
REAL(DFP) :: c2
!!
c2 = c
c2 = MAX(c2, -1.0_DFP)
c2 = MIN(c2, +1.0_DFP)
!!
ans = ACOS(c2)
END PROCEDURE SAFE_ACOS

!----------------------------------------------------------------------------
!                                                                 Safe_ASIN
!----------------------------------------------------------------------------

MODULE PROCEDURE SAFE_ASIN
REAL(DFP) :: s2
s2 = s
s2 = MAX(s2, -1.0_DFP)
s2 = MIN(s2, +1.0_DFP)
ans = ASIN(s2)
END PROCEDURE SAFE_ASIN

END SUBMODULE Methods

#undef NPAR_ARTH 16
#undef NPAR2_ARTH 8
#undef NPAR_GEOP 4
#undef NPAR2_GEOP 2
#undef NPAR_CUMSUM 16
#undef NPAR_CUMPROD 8
#undef NPAR_POLY 8
#undef NPAR_POLYTERM 8
