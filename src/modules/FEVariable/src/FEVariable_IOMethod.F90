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
! Writes a readable report of `obj` to a unit.
!
! The report has these parts, in order:
!
! 1. The text `Msg`.
! 2. The rank: scalar, vector or matrix.
! 3. The variable type: constant, space, time or space-time.
! 4. The values stored in `obj`.
! 5. The components `s` and `tshape`.
! 6. The `defineOn` setting: nodal or quadrature. Any other value
!    writes no line.
! 7. The components `len`, `capacity` and `isInit`.
! 8. The size of `obj%val`.
!
! If `UnitNo` is absent, the output goes to the standard output.
!
! For an unknown rank or variable type, it writes `UNKNOWN` and skips
! the values.
!
! Examples
!
! - [test 1](|base|FEVariable/examples/Display_test_1.F90)

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
