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


!----------------------------------------------------------------------------
!                                                               Radian@MISC
!----------------------------------------------------------------------------

!> authors: Dr. Vikas Sharma
!
! Convert degrees into radian

INTERFACE
  MODULE PURE FUNCTION radian_dfp(deg) RESULT(Ans)
    REAL(DFP), INTENT(IN) :: deg
    REAL(DFP) :: Ans
  END FUNCTION
END INTERFACE

!----------------------------------------------------------------------------
!                                                               Radian@MISC
!----------------------------------------------------------------------------

!> authors: Dr. Vikas Sharma
!
! Converts degrees into radian

INTERFACE
  MODULE PURE FUNCTION radian_int(deg) RESULT(Ans)
    INTEGER(I4B), INTENT(IN) :: deg
    REAL(DFP) :: Ans
  END FUNCTION
END INTERFACE

INTERFACE radian
  MODULE PROCEDURE radian_dfp, radian_int
END INTERFACE

PUBLIC :: radian

!----------------------------------------------------------------------------
!                                                              Degrees@MISC
!----------------------------------------------------------------------------

!> authors: Dr. Vikas Sharma
!
! This function converts radian into degrees
! Belongs to `Degrees`

INTERFACE
  MODULE PURE FUNCTION degrees_dfp(rad) RESULT(Ans)
    REAL(DFP), INTENT(IN) :: rad
    REAL(DFP) :: Ans
  END FUNCTION
END INTERFACE

INTERFACE Degrees
  MODULE PROCEDURE degrees_dfp
END INTERFACE Degrees

PUBLIC :: Degrees

!----------------------------------------------------------------------------
!                                                                 @MISC
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date:         3 April 2021
! summary:         This subroutine search the location of nearest point to x in the array of coordinates; Array
!
!# Introduction
!
! This subroutine search the location of nearest point to x in the
! array of coordinates; Array
!
! ## Usage
! ```fortran
! real( dfp ) :: xij( 2, 20 ), x( 2 )
! integer( i4b ) :: id
!
! call random_number( xij )
! x = [11.0, 100.0]
! xij( 1:2, 15 ) = x
! id = searchNearestCoord(Array=xij, x=x)
! call display( id==15, "test4:: " )
!```

INTERFACE
  MODULE FUNCTION Loc_Nearest_Point(Array, x) RESULT(id)
    REAL(DFP), INTENT(IN) :: Array(:, :)
  !! Nodal coordinates in XiJ format
    REAL(DFP), INTENT(IN) :: x(:)
    INTEGER(I4B) :: id
  END FUNCTION
END INTERFACE

INTERFACE LOC_NearestPoint
  MODULE PROCEDURE Loc_Nearest_Point
END INTERFACE LOC_NearestPoint

PUBLIC :: LOC_NearestPoint

INTERFACE SearchNearestCoord
  MODULE PROCEDURE Loc_Nearest_Point
END INTERFACE SearchNearestCoord

PUBLIC :: SearchNearestCoord

!----------------------------------------------------------------------------
!                                                                     @MISC
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date:         3 April 2021
! summary:         This subroutine run a system commoand on terminal

INTERFACE
  MODULE SUBROUTINE exe_cmd(CMD, Str)
    CHARACTER(LEN=*), INTENT(IN) :: CMD, Str
  END SUBROUTINE
END INTERFACE

INTERFACE ExecuteCommand
  MODULE PROCEDURE exe_cmd
END INTERFACE ExecuteCommand

PUBLIC :: ExecuteCommand

!----------------------------------------------------------------------------
!                                                             getUnitNo@MISC
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION getUnitNo_1() RESULT(ans)
    INTEGER(I4B) :: ans
  END FUNCTION getUnitNo_1
END INTERFACE

INTERFACE getUnitNo
  MODULE PROCEDURE getUnitNo_1
END INTERFACE getUnitNo

PUBLIC :: getUnitNo

!----------------------------------------------------------------------------
!                                                             Factorial@MISC
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date:         3 April 2021
! summary: This FUNCTION computes the factorial of an INTEGER

INTERFACE
  MODULE RECURSIVE FUNCTION Factorial(N) RESULT(Ans)
    INTEGER(I4B), INTENT(IN) :: N
    INTEGER(I4B) :: Ans
  END FUNCTION
END INTERFACE

PUBLIC :: Factorial

!----------------------------------------------------------------------------
!                                                               Int2STR@MISC
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date:         3 April 2021
! summary:         Convert INTEGER  to  string

INTERFACE
  MODULE PURE FUNCTION Int2Str(I)
    INTEGER(I4B), INTENT(IN) :: I
    CHARACTER(LEN=15) :: Int2Str
  END FUNCTION
END INTERFACE

PUBLIC :: Int2Str

!----------------------------------------------------------------------------
!                                                              Real2Str@MISC
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION SP2Str(I)
    REAL(SP), INTENT(IN) :: I
    CHARACTER(LEN=20) :: SP2Str
  END FUNCTION
END INTERFACE

INTERFACE
  MODULE FUNCTION DP2Str(I)
    REAL(DP), INTENT(IN) :: I
    CHARACTER(LEN=20) :: DP2Str
  END FUNCTION
END INTERFACE

INTERFACE Real2Str
  MODULE PROCEDURE SP2Str, DP2Str
END INTERFACE Real2Str

PUBLIC :: Real2Str

!----------------------------------------------------------------------------
!                                                                       ARTH
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION arth_r(first, increment, n)
    REAL(SP), INTENT(IN) :: first, increment
    INTEGER(I4B), INTENT(IN) :: n
    REAL(SP) :: arth_r(n)
  END FUNCTION
END INTERFACE

INTERFACE
  MODULE PURE FUNCTION arth_d(first, increment, n)
    REAL(DP), INTENT(IN) :: first, increment
    INTEGER(I4B), INTENT(IN) :: n
    REAL(DP) :: arth_d(n)
  END FUNCTION
END INTERFACE

INTERFACE
  MODULE PURE FUNCTION arth_i(first, increment, n)
    INTEGER(I4B), INTENT(IN) :: first, increment, n
    INTEGER(I4B) :: arth_i(n)
  END FUNCTION
END INTERFACE

INTERFACE ARTH
  MODULE PROCEDURE arth_d, arth_i, arth_r
END INTERFACE ARTH

PUBLIC :: ARTH

!----------------------------------------------------------------------------
!                                                                 outerDIFF
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION outerdIFf_r(a, b)
    REAL(SP), DIMENSION(:), INTENT(IN) :: a, b
    REAL(SP), DIMENSION(size(a), size(b)) :: outerdIFf_r
  END FUNCTION
END INTERFACE

INTERFACE
  MODULE PURE FUNCTION outerdIFf_d(a, b)
    REAL(DP), DIMENSION(:), INTENT(IN) :: a, b
    REAL(DP), DIMENSION(size(a), size(b)) :: outerdIFf_d
  END FUNCTION
END INTERFACE

INTERFACE
  MODULE PURE FUNCTION outerdIFf_i(a, b)
    INTEGER(I4B), DIMENSION(:), INTENT(IN) :: a, b
    INTEGER(I4B), DIMENSION(size(a), size(b)) :: outerdIFf_i
  END FUNCTION
END INTERFACE

INTERFACE outerDIFF
  MODULE PROCEDURE outerdIFf_r, outerdIFf_i, outerdIFf_d
END INTERFACE

PUBLIC :: outerDIFF

!----------------------------------------------------------------------------
!                                                              IMAXLOC@MISC
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION imaxloc_r(arr)
    REAL(DFP), INTENT(IN) :: arr(:)
    INTEGER(I4B) :: imaxloc_r
  END FUNCTION
END INTERFACE

INTERFACE
  MODULE PURE FUNCTION imaxloc_i(iarr)
    INTEGER(I4B), INTENT(IN) :: iarr(:)
    INTEGER(I4B) :: imaxloc_i
  END FUNCTION
END INTERFACE

INTERFACE IMAXLOC
  MODULE PROCEDURE imaxloc_r, imaxloc_i
END INTERFACE

PUBLIC :: IMAXLOC

!----------------------------------------------------------------------------
!                                                                 IMIN@MISC
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION iminloc_r(arr)
    REAL(DFP), INTENT(IN) :: arr(:)
    INTEGER(I4B) :: iminloc_r
  END FUNCTION
END INTERFACE

INTERFACE IMINLOC
  MODULE PROCEDURE iminloc_r
END INTERFACE IMINLOC

PUBLIC :: IMINLOC