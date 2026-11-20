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

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-10
! summary: Input and output methods of FEVariable_

MODULE FEVariable_IOMethod
USE BaseType, ONLY: FEVariable_, &
                    FEVariableScalar_, &
                    FEVariableVector_, &
                    FEVariableMatrix_, &
                    FEVariableConstant_, &
                    FEVariableSpace_, &
                    FEVariableTime_, &
                    FEVariableSpaceTime_, &
                    TypeFEVariableOpt

USE GlobalData, ONLY: I4B, DFP, LGT

IMPLICIT NONE
PRIVATE

PUBLIC :: Display

!----------------------------------------------------------------------------
!                                                          Display@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-10
! summary: Displays the content of [[FEVariable_]]
!
! Writes `msg`, the rank, the variable type and the values (as returned
! by [[Get]]), followed by `s`, `tshape`, `defineOn`, `len`, `capacity`,
! `isInit` and the size of `obj%val`. Output goes to `unitNo`, or to the
! default output unit when `unitNo` is absent.
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: FEVariable_
! USE BaseType, ONLY: Space => TypeFEVariableSpace
! USE BaseType, ONLY: Vector => TypeFEVariableVector
! USE FEVariable_Method, ONLY: Display
! USE FEVariable_Method, ONLY: NodalVariable
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: LGT
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! ! Test 1: Display writes the message, rank, varType and sizes to a unit
! BLOCK
!   TYPE(FEVariable_) :: obj
!   INTEGER(I4B) :: unitNo, ios
!   CHARACTER(256) :: line, msg
!   LOGICAL(LGT) :: hasMsg, hasRank, hasVarType, hasDefineOn, hasLen
!
!   obj = NodalVariable(RESHAPE([1.0_DFP, 2.0_DFP, 3.0_DFP, 4.0_DFP], &
!                               [2, 2]), Vector, Space)
!
!   OPEN (newunit=unitNo, status="SCRATCH", action="READWRITE")
!   CALL Display(obj, "my variable", unitNo=unitNo)
!   REWIND (unitNo)
!
!   hasMsg = .FALSE.
!   hasRank = .FALSE.
!   hasVarType = .FALSE.
!   hasDefineOn = .FALSE.
!   hasLen = .FALSE.
!   readLoop: DO
!     READ (unitNo, '(A)', iostat=ios, iomsg=msg) line
!     IF (ios .NE. 0) EXIT readLoop
!     hasMsg = hasMsg .OR. INDEX(line, "my variable") .GT. 0
!     hasRank = hasRank .OR. INDEX(line, "rank: 1 (Vector)") .GT. 0
!     hasVarType = hasVarType .OR. INDEX(line, "varType: Space") .GT. 0
!     hasDefineOn = hasDefineOn .OR. INDEX(line, "defineOn: nodal") .GT. 0
!     hasLen = hasLen .OR. INDEX(line, "len:") .GT. 0
!   END DO readLoop
!   CLOSE (unitNo)
!
!   CALL OK(hasMsg, "message is written")
!   CALL OK(hasRank, "rank is written")
!   CALL OK(hasVarType, "varType is written")
!   CALL OK(hasDefineOn, "defineOn is written")
!   CALL OK(hasLen, "len is written")
! END BLOCK
!
! END PROGRAM main
!```

INTERFACE Display
  MODULE SUBROUTINE fevar_Display(obj, Msg, UnitNo)
    TYPE(FEVariable_), INTENT(IN) :: obj
    !! finite element variable
    CHARACTER(*), INTENT(IN) :: Msg
    !! message written first
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: UnitNo
    !! output unit; default is the standard output
  END SUBROUTINE fevar_Display
END INTERFACE Display

END MODULE FEVariable_IOMethod
