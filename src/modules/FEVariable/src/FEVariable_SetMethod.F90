! This program is a part of EASIFEM library
! Expandable And Scalable Infrastructure for Finite Element Methods
! htttps://www.easifem.com
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

MODULE FEVariable_SetMethod
USE BaseType, ONLY: FEVariable_
USE BaseType, ONLY: TypeFEVariableOpt
USE BaseType, ONLY: FEVariableScalar_
USE BaseType, ONLY: FEVariableVector_
USE BaseType, ONLY: FEVariableMatrix_
USE BaseType, ONLY: FEVariableConstant_
USE BaseType, ONLY: FEVariableSpace_
USE BaseType, ONLY: FEVariableTime_
USE BaseType, ONLY: FEVariableSpaceTime_
USE GlobalData, ONLY: I4B, DFP, LGT
IMPLICIT NONE

PRIVATE
PUBLIC :: Set

!----------------------------------------------------------------------------
!                                                                        Set
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-13
! summary:         Set the constant value of scalar fevariable
!
!# Set
!
! This method sets the constant value of scalar fevariable.
! After calling this method obj will become constant.
!
INTERFACE Set
  MODULE PURE SUBROUTINE obj_Set1( &
    obj, val, rank, vartype, scale, addContribution)
    TYPE(FEVariable_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: val
    TYPE(FEVariableScalar_), INTENT(IN) :: rank
    TYPE(FEVariableConstant_), INTENT(IN) :: vartype
    REAL(DFP), INTENT(IN) :: scale
    LOGICAL(LGT), INTENT(IN) :: addContribution
  END SUBROUTINE obj_Set1
END INTERFACE Set

!----------------------------------------------------------------------------
!                                                                        Set
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-13
! summary: Set the space value of scalar variable
!
!# Set
!
! This method sets the space value of scalar variable.
! After calling this method obj%varType is set to space
!
INTERFACE Set
  MODULE PURE SUBROUTINE obj_Set2( &
    obj, val, rank, vartype, scale, addContribution)
    TYPE(FEVariable_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: val(:)
    TYPE(FEVariableScalar_), INTENT(IN) :: rank
    TYPE(FEVariableSpace_), INTENT(IN) :: vartype
    REAL(DFP), INTENT(IN) :: scale
    LOGICAL(LGT), INTENT(IN) :: addContribution
  END SUBROUTINE obj_Set2
END INTERFACE Set

!----------------------------------------------------------------------------
!                                                                        Set
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-13
! summary: Set the space time value of scalar variable
!
!# Set
!
! This method sets the space time value of scalar variable.
! After calling this method obj%varType is set to spacetime
!
INTERFACE Set
  MODULE PURE SUBROUTINE obj_Set3( &
    obj, val, rank, vartype, scale, addContribution)
    TYPE(FEVariable_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: val(:, :)
    TYPE(FEVariableScalar_), INTENT(IN) :: rank
    TYPE(FEVariableSpaceTime_), INTENT(IN) :: vartype
    REAL(DFP), INTENT(IN) :: scale
    LOGICAL(LGT), INTENT(IN) :: addContribution
  END SUBROUTINE obj_Set3
END INTERFACE Set

!----------------------------------------------------------------------------
!                                                                        Set
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-13
! summary: Set the time value of scalar variable
!
!# Set
!
! This method sets the time value of scalar variable.
! After calling this method obj%varType is set to time
!
INTERFACE Set
  MODULE PURE SUBROUTINE obj_Set10(obj, val, rank, vartype, scale, &
                                   addContribution)
    TYPE(FEVariable_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: val(:)
    TYPE(FEVariableScalar_), INTENT(IN) :: rank
    TYPE(FEVariableTime_), INTENT(IN) :: vartype
    REAL(DFP), INTENT(IN) :: scale
    LOGICAL(LGT), INTENT(IN) :: addContribution
  END SUBROUTINE obj_Set10
END INTERFACE Set

!----------------------------------------------------------------------------
!                                                                        Set
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-13
! summary: Set the constant value of vector variable
!
!# Set
!
! This method sets the constant value of vector variable.
! After calling this method obj%varType is set to constant
!
INTERFACE Set
  MODULE PURE SUBROUTINE obj_Set4(obj, val, rank, vartype, scale, &
                                  addContribution)
    TYPE(FEVariable_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: val(:)
    TYPE(FEVariableVector_), INTENT(IN) :: rank
    TYPE(FEVariableConstant_), INTENT(IN) :: vartype
    REAL(DFP), INTENT(IN) :: scale
    LOGICAL(LGT), INTENT(IN) :: addContribution
  END SUBROUTINE obj_Set4
END INTERFACE Set

!----------------------------------------------------------------------------
!                                                                        Set
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-13
! summary: Set the space value of vector variable
!
!# Set
!
! This method sets the space value of vector variable.
! After calling this method obj%varType is set to space
!
INTERFACE Set
  MODULE PURE SUBROUTINE obj_Set5(obj, val, rank, vartype, scale, &
                                  addContribution)
    TYPE(FEVariable_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: val(:, :)
    TYPE(FEVariableVector_), INTENT(IN) :: rank
    TYPE(FEVariableSpace_), INTENT(IN) :: vartype
    REAL(DFP), INTENT(IN) :: scale
    LOGICAL(LGT), INTENT(IN) :: addContribution
  END SUBROUTINE obj_Set5
END INTERFACE Set

!----------------------------------------------------------------------------
!                                                                        Set
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-13
! summary: Set the space-time value of vector variable
!
!# Set
!
! This method sets the space-time value of vector variable.
! After calling this method obj%varType is set to spaceTime
!
INTERFACE Set
  MODULE PURE SUBROUTINE obj_Set6(obj, val, rank, vartype, scale, &
                                  addContribution)
    TYPE(FEVariable_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: val(:, :, :)
    TYPE(FEVariableVector_), INTENT(IN) :: rank
    TYPE(FEVariableSpaceTime_), INTENT(IN) :: vartype
    REAL(DFP), INTENT(IN) :: scale
    LOGICAL(LGT), INTENT(IN) :: addContribution
  END SUBROUTINE obj_Set6
END INTERFACE Set

!----------------------------------------------------------------------------
!                                                                        Set
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-13
! summary: Set the time value of vector variable
!
!# Set
!
! This method sets the time value of vector variable.
! After calling this method obj%varType is set to time
!
INTERFACE Set
  MODULE PURE SUBROUTINE obj_Set11(obj, val, rank, vartype, scale, &
                                   addContribution)
    TYPE(FEVariable_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: val(:, :)
    TYPE(FEVariableVector_), INTENT(IN) :: rank
    TYPE(FEVariableTime_), INTENT(IN) :: vartype
    REAL(DFP), INTENT(IN) :: scale
    LOGICAL(LGT), INTENT(IN) :: addContribution
  END SUBROUTINE obj_Set11
END INTERFACE Set

!----------------------------------------------------------------------------
!                                                                        Set
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-13
! summary: Set the constant value of matrix variable
!
!# Set
!
! This method sets the constant value of matrix variable.
! After calling this method obj%varType is set to constant
!
INTERFACE Set
  MODULE PURE SUBROUTINE obj_Set7(obj, val, rank, vartype, scale, &
                                  addContribution)
    TYPE(FEVariable_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: val(:, :)
    TYPE(FEVariableMatrix_), INTENT(IN) :: rank
    TYPE(FEVariableConstant_), INTENT(IN) :: vartype
    REAL(DFP), INTENT(IN) :: scale
    LOGICAL(LGT), INTENT(IN) :: addContribution
  END SUBROUTINE obj_Set7
END INTERFACE Set

!----------------------------------------------------------------------------
!                                                                        Set
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-13
! summary: Set the space value of matrix variable
!
!# Set
!
! This method sets the space value of matrix variable.
! After calling this method obj%varType is set to space
!
INTERFACE Set
  MODULE PURE SUBROUTINE obj_Set8(obj, val, rank, vartype, scale, &
                                  addContribution)
    TYPE(FEVariable_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: val(:, :, :)
    TYPE(FEVariableMatrix_), INTENT(IN) :: rank
    TYPE(FEVariableSpace_), INTENT(IN) :: vartype
    REAL(DFP), INTENT(IN) :: scale
    LOGICAL(LGT), INTENT(IN) :: addContribution
  END SUBROUTINE obj_Set8
END INTERFACE Set

!----------------------------------------------------------------------------
!                                                                        Set
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-13
! summary: Set the spaceTime value of matrix variable
!
!# Set
!
! This method sets the spaceTime value of matrix variable.
! After calling this method obj%varType is set to spaceTime
!
INTERFACE Set
  MODULE PURE SUBROUTINE obj_Set9(obj, val, rank, vartype, scale, &
                                  addContribution)
    TYPE(FEVariable_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: val(:, :, :, :)
    TYPE(FEVariableMatrix_), INTENT(IN) :: rank
    TYPE(FEVariableSpaceTime_), INTENT(IN) :: vartype
    REAL(DFP), INTENT(IN) :: scale
    LOGICAL(LGT), INTENT(IN) :: addContribution
  END SUBROUTINE obj_Set9
END INTERFACE Set

!----------------------------------------------------------------------------
!                                                                        Set
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-13
! summary: Set the time value of matrix variable
!
!# Set
!
! This method sets the time value of matrix variable.
! After calling this method obj%varType is set to time
!
INTERFACE Set
  MODULE PURE SUBROUTINE obj_Set12(obj, val, rank, vartype, scale, &
                                   addContribution)
    TYPE(FEVariable_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: val(:, :, :)
    TYPE(FEVariableMatrix_), INTENT(IN) :: rank
    TYPE(FEVariableTime_), INTENT(IN) :: vartype
    REAL(DFP), INTENT(IN) :: scale
    LOGICAL(LGT), INTENT(IN) :: addContribution
  END SUBROUTINE obj_Set12
END INTERFACE Set

END MODULE FEVariable_SetMethod
