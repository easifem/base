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

SUBMODULE(Sym_Lapack_Method) LinearSolveMethods
USE String_Class
USE BaseMethod, ONLY: UpperCase, ErrorMsg
USE F95_LAPACK, ONLY: LACPY, SYSV
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                   SymSolve
!----------------------------------------------------------------------------

MODULE PROCEDURE SymLinSolve_1
REAL(DFP), ALLOCATABLE :: LocalA(:, :)
INTEGER(I4B) :: n
TYPE(String) :: LSolveName
!!
n = SIZE(A, 1)
ALLOCATE (LocalA(n, n))
!!
CALL LACPY(A=A, B=LocalA, UPLO=UPLO)
X = B
!!
IF (PRESENT(SolverName)) THEN
  LSolveName = UpperCase(SolverName)
ELSE
  LSolveName = "SYSV"
END IF
!!
SELECT CASE (LSolveName%chars())
CASE ("SYSV")
  CALL SYSV(A=LocalA, B=X, UPLO=UPLO, IPIV=IPIV, INFO=INFO)
CASE DEFAULT
  CALL ErrorMsg( &
    & msg="NO CASE FOUND FOR "//LSolveName%chars(), &
    & file=__FILE__, &
    & line=__LINE__, &
    & routine="SymLinSolve_1", &
    & unitno=stderr)
  DEALLOCATE (LocalA)
  STOP
END SELECT
DEALLOCATE (LocalA)
END PROCEDURE SymLinSolve_1

!----------------------------------------------------------------------------
!                                                                  SymSolve
!----------------------------------------------------------------------------

MODULE PROCEDURE SymLinSolve_2
REAL(DFP), ALLOCATABLE :: LocalA(:, :)
INTEGER(I4B) :: n
TYPE(String) :: LSolveName
!!
n = SIZE(A, 1)
ALLOCATE (LocalA(n, n))
!!
CALL LACPY(A=A, B=LocalA, UPLO=UPLO)
X = B
!!
IF (PRESENT(SolverName)) THEN
  LSolveName = UpperCase(SolverName)
ELSE
  LSolveName = "SYSV"
END IF
!!
SELECT CASE (LSolveName%chars())
CASE ("SYSV")
  CALL SYSV(A=LocalA, B=X, UPLO=UPLO, IPIV=IPIV, INFO=INFO)
CASE DEFAULT
  CALL ErrorMsg( &
    & msg="NO CASE FOUND FOR "//LSolveName%chars(), &
    & file=__FILE__, &
    & line=__LINE__, &
    & routine="SymLinSolve_2", &
    & unitno=stderr)
  DEALLOCATE (LocalA)
  STOP
END SELECT

DEALLOCATE (LocalA)
END PROCEDURE SymLinSolve_2

!----------------------------------------------------------------------------
!                                                                   SymSolve
!----------------------------------------------------------------------------

MODULE PROCEDURE SymLinSolve_3
TYPE(String) :: LSolveName
!!
X = B
!!
IF (PRESENT(SolverName)) THEN
  LSolveName = UpperCase(SolverName)
ELSE
  LSolveName = "SYSV"
END IF
!!
SELECT CASE (LSolveName%chars())
CASE ("SYSV")
  CALL SYSV(A=A, B=X, UPLO=UPLO, IPIV=IPIV, INFO=INFO)
CASE DEFAULT
  CALL ErrorMsg( &
    & msg="NO CASE FOUND FOR "//LSolveName%chars(), &
    & file=__FILE__, &
    & line=__LINE__, &
    & routine="SymLinSolve_3", &
    & unitno=stderr)
  STOP
END SELECT
END PROCEDURE SymLinSolve_3

!----------------------------------------------------------------------------
!                                                                   SymSolve
!----------------------------------------------------------------------------

MODULE PROCEDURE SymLinSolve_4
TYPE(String) :: LSolveName
!!
X = B
!!
IF (PRESENT(SolverName)) THEN
  LSolveName = UpperCase(SolverName)
ELSE
  LSolveName = "SYSV"
END IF
!!
SELECT CASE (LSolveName%chars())
CASE ("SYSV")
  CALL SYSV(A=A, B=X, UPLO=UPLO, IPIV=IPIV, INFO=INFO)
CASE DEFAULT
  CALL ErrorMsg( &
    & msg="NO CASE FOUND FOR "//LSolveName%chars(), &
    & file=__FILE__, &
    & line=__LINE__, &
    & routine="SymLinSolve_4", &
    & unitno=stderr)
  STOP
END SELECT
END PROCEDURE SymLinSolve_4

!----------------------------------------------------------------------------
!                                                                   SymSolve
!----------------------------------------------------------------------------

MODULE PROCEDURE SymLinSolve_5
TYPE(String) :: LSolveName
!!
IF (PRESENT(SolverName)) THEN
  LSolveName = UpperCase(SolverName)
ELSE
  LSolveName = "SYSV"
END IF
!!
SELECT CASE (LSolveName%chars())
CASE ("SYSV")
  CALL SYSV(A=A, B=B, UPLO=UPLO, IPIV=IPIV, INFO=INFO)
CASE DEFAULT
  CALL ErrorMsg( &
    & msg="NO CASE FOUND FOR "//LSolveName%chars(), &
    & file=__FILE__, &
    & line=__LINE__, &
    & routine="SymLinSolve_5", &
    & unitno=stderr)
  STOP
END SELECT
END PROCEDURE SymLinSolve_5

!----------------------------------------------------------------------------
!                                                                   SymSolve
!----------------------------------------------------------------------------

MODULE PROCEDURE SymLinSolve_6
TYPE(String) :: LSolveName
!!
IF (PRESENT(SolverName)) THEN
  LSolveName = UpperCase(SolverName)
ELSE
  LSolveName = "SYSV"
END IF
!!
SELECT CASE (LSolveName%chars())
CASE ("SYSV")
  CALL SYSV(A=A, B=B, UPLO=UPLO, IPIV=IPIV, INFO=INFO)
CASE DEFAULT
  CALL ErrorMsg( &
    & msg="NO CASE FOUND FOR "//LSolveName%chars(), &
    & file=__FILE__, &
    & line=__LINE__, &
    & routine="SymLinSolve_6", &
    & unitno=stderr)
  STOP
END SELECT
END PROCEDURE SymLinSolve_6

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE LinearSolveMethods
