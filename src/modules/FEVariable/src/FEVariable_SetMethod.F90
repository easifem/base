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
USE BaseType, ONLY: FEVariable_, &
                    TypeFEVariableOpt, &
                    FEVariableScalar_, &
                    FEVariableVector_, &
                    FEVariableMatrix_, &
                    FEVariableConstant_, &
                    FEVariableSpace_, &
                    FEVariableTime_, &
                    FEVariableSpaceTime_

USE GlobalData, ONLY: I4B, DFP, LGT

IMPLICIT NONE

PRIVATE

PUBLIC :: Set

!----------------------------------------------------------------------------
!                                                                        Set
!----------------------------------------------------------------------------

INTERFACE Set
  MODULE PURE SUBROUTINE obj_Set1(obj, val, rank, vartype, scale, &
                                  addContribution)
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

INTERFACE Set
  MODULE PURE SUBROUTINE obj_Set2(obj, val, rank, vartype, scale, &
                                  addContribution)
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

INTERFACE Set
  MODULE PURE SUBROUTINE obj_Set3(obj, val, rank, vartype, scale, &
                                  addContribution)
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

END MODULE FEVariable_SetMethod
