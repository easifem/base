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

MODULE DOF_AddMethods
USE GlobalData, ONLY: DFP, I4B, LGT
USE BaseType, ONLY: RealVector_, DOF_

IMPLICIT NONE
PRIVATE

PUBLIC :: Add

!----------------------------------------------------------------------------
!                                                            Add@addMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 27 June 2021
! summary:          Add values in a vector of real numbers
!
!# Introduction
!
! This subroutine is designed to Add values in a vector of real number
! - [[DOF_]] object `obj` contains the storage pattern of degrees of freedom
! inside `vec`. This storage pattern can be `FMT_Nodes` or `FMT_DOF`
! - `value` denotes the nodal values of all dof defined inside `obj`. Once
! storage pattern in `value` can be `FMT_DOF` or `FMT_Nodes`.
! - To tackle this `conversion`  can be Add to `DOFToNodes`, `NodesToDOF`
! or `NONE`.
!
! This subroutine effectivily performes
! `vec( nptrs ) = vec(nptrs) + scale * value`

INTERFACE Add
  MODULE SUBROUTINE obj_Add1(vec, obj, nodenum, VALUE, scale, &
                             conversion)
    REAL(DFP), INTENT(INOUT) :: vec(:)
    !! Vector to set values in
    TYPE(DOF_), INTENT(IN) :: obj
    !! degree of freedom object
    INTEGER(I4B), INTENT(IN) :: nodenum(:)
    !! Node number
    REAL(DFP), INTENT(IN) :: VALUE(:)
    !! Value
    REAL(DFP), INTENT(IN) :: scale
    !! scale
    INTEGER(I4B), INTENT(IN) :: conversion(1)
    !! conversion
  END SUBROUTINE obj_Add1
END INTERFACE Add

!----------------------------------------------------------------------------
!                                                            Add@addMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 27 June 2021
! summary:          Add values in a vector of real numbers
!
!# Introduction
!
! This subroutine is designed to Add values in a vector of real number
! - [[DOF_]] object `obj` contains the storage pattern of degrees of freedom
! inside `vec`. This storage pattern can be `FMT_Nodes` or `FMT_DOF`
! - `value` denotes the nodal values of all dof defined inside `obj`. Once
! storage pattern in `value` can be `FMT_DOF` or `FMT_Nodes`.
! - To tackle this `conversion`  can be Add to `DOFToNodes`, `NodesToDOF`
! or `NONE`.
!
! This subroutine effectivily performes
! `vec( nptrs ) = vec(nptrs) + scale * value`

INTERFACE Add
  MODULE SUBROUTINE obj_Add2(vec, obj, nodenum, VALUE, scale)
    REAL(DFP), INTENT(INOUT) :: vec(:)
    !! vector to set values in
    TYPE(DOF_), INTENT(IN) :: obj
    !! degree of freedom object
    INTEGER(I4B), INTENT(IN) :: nodenum(:)
    !! node number
    REAL(DFP), INTENT(IN) :: VALUE
    !! scalar value
    REAL(DFP), INTENT(IN) :: scale
    !! scale
  END SUBROUTINE obj_Add2
END INTERFACE Add

!----------------------------------------------------------------------------
!                                                            Add@addMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         27 June 2021
! summary:         Add values in a vector of real numbers
!
!# Introduction
!
! This subroutine is designed to Add values in a vector of real number
! - [[DOF_]] object `obj` contains the storage pattern of degrees of freedom
! inside `vec`. This storage pattern can be `FMT_Nodes` or `FMT_DOF`
! - `value` denotes the nodal values of dof `dofno`.
!
! This subroutine effectivily performes
! `vec( nptrs ) = vec(nptrs) + scale * value`

INTERFACE Add
  MODULE SUBROUTINE obj_Add3(vec, obj, nodenum, VALUE, scale, idof)
    REAL(DFP), INTENT(INOUT) :: vec(:)
    !! vector to set values in
    TYPE(DOF_), INTENT(IN) :: obj
    !! degree of freedom object
    INTEGER(I4B), INTENT(IN) :: nodenum(:)
    !! node number
    REAL(DFP), INTENT(IN) :: VALUE(:)
    !! vec = values, size of value should be equal to the size of nodenum
    REAL(DFP), INTENT(IN) :: scale
    !! scale
    INTEGER(I4B), INTENT(IN) :: idof
    !! global degree of freedom number
  END SUBROUTINE obj_Add3
END INTERFACE Add

!----------------------------------------------------------------------------
!                                                             Add@addMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 27 June 2021
! summary: Add values in a vector of real numbers
!
! This subroutine calls obj_Add3

INTERFACE Add
  MODULE SUBROUTINE obj_Add4(vec, obj, nodenum, VALUE, scale, ivar, idof)
    REAL(DFP), INTENT(INOUT) :: vec(:)
    !! vector to set values in
    TYPE(DOF_), INTENT(IN) :: obj
    !! Object `obj` contains the storage pattern of degrees of freedom
    !! inside `vec`.
    !! This storage pattern can be `FMT_Nodes` or `FMT_DOF`
    INTEGER(I4B), INTENT(IN) :: nodenum(:)
    !! node number
    REAL(DFP), INTENT(IN) :: VALUE(:)
    !! `value` denotes the nodal values of dof `idof`.
    REAL(DFP), INTENT(IN) :: scale
    !! scale
    INTEGER(I4B), INTENT(IN) :: ivar
    !! physical variable
    INTEGER(I4B), INTENT(IN) :: idof
  END SUBROUTINE obj_Add4
END INTERFACE Add

!----------------------------------------------------------------------------
!                                                             Add@addMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 27 June 2021
! summary: Add values in a vector of real numbers
!
!@note
!   this routine calls obj_Add3
!@endnote

INTERFACE Add
  MODULE SUBROUTINE obj_Add5(vec, obj, nodenum, VALUE, scale, ivar, &
                             spacecompo, timecompo)
    REAL(DFP), INTENT(INOUT) :: vec(:)
    TYPE(DOF_), INTENT(IN) :: obj
    !! Object `obj` contains the storage pattern of degrees of freedom
    !! inside `vec`.
    !! This storage pattern can be `FMT_Nodes` or `FMT_DOF`
    INTEGER(I4B), INTENT(IN) :: nodenum(:)
    !! node number
    REAL(DFP), INTENT(IN) :: VALUE(:)
    !! `value` denotes the nodal values of dof `idof`.
    !! the size of value should be same as nodenum
    REAL(DFP), INTENT(IN) :: scale
    !! scale
    INTEGER(I4B), INTENT(IN) :: ivar
    !! physical variable
    INTEGER(I4B), INTENT(IN) :: spacecompo
    !! space components
    INTEGER(I4B), INTENT(IN) :: timecompo
    !! time components
  END SUBROUTINE obj_Add5
END INTERFACE Add

!----------------------------------------------------------------------------
!                                                             Add@addMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 27 June 2021
! summary: Add values in a vector of real numbers

INTERFACE Add
  MODULE SUBROUTINE obj_Add6(vec, obj, nodenum, VALUE, scale, ivar, &
                             spacecompo, timecompo)
    REAL(DFP), INTENT(INOUT) :: vec(:)
    TYPE(DOF_), INTENT(IN) :: obj
    !! Object `obj` contains the storage pattern of degrees of freedom
    !! inside `vec`.
    !! This storage pattern can be `FMT_Nodes` or `FMT_DOF`
    INTEGER(I4B), INTENT(IN) :: nodenum(:)
    !! node number
    REAL(DFP), INTENT(IN) :: VALUE(:)
    !! `value` denotes the nodal values of dof `idof`.
    REAL(DFP), INTENT(IN) :: scale
    !! scale
    INTEGER(I4B), INTENT(IN) :: ivar
    !! physical variable
    INTEGER(I4B), INTENT(IN) :: spacecompo
    !! space components
    INTEGER(I4B), INTENT(IN) :: timecompo(:)
    !! time components
  END SUBROUTINE obj_Add6
END INTERFACE Add

!----------------------------------------------------------------------------
!                                                             Add@addMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 27 June 2021
! summary: Add values in a vector of real numbers

INTERFACE Add
  MODULE SUBROUTINE obj_Add7(vec, obj, nodenum, VALUE, scale, ivar, &
                             spacecompo, timecompo)
    REAL(DFP), INTENT(INOUT) :: vec(:)
    TYPE(DOF_), INTENT(IN) :: obj
    !! Object `obj` contains the storage pattern of degrees of freedom
    !! inside `vec`.
    !! This storage pattern can be `FMT_Nodes` or `FMT_DOF`
    INTEGER(I4B), INTENT(IN) :: nodenum(:)
    !! node number
    REAL(DFP), INTENT(IN) :: VALUE(:)
    !! `value` denotes the nodal values of dof `idof`.
    REAL(DFP), INTENT(IN) :: scale
    !! scale
    INTEGER(I4B), INTENT(IN) :: ivar
    !! physical variable
    INTEGER(I4B), INTENT(IN) :: spacecompo(:)
    !! space components
    INTEGER(I4B), INTENT(IN) :: timecompo
    !! time components
  END SUBROUTINE obj_Add7
END INTERFACE Add

!----------------------------------------------------------------------------
!                                                            Add@addMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 27 June 2021
! summary:          Add values in a vector of real numbers
!
!# Introduction
!
! This subroutine is designed to Add values in a vector of real number
! - [[DOF_]] object `obj` contains the storage pattern of degrees of freedom
! inside `vec`. This storage pattern can be `FMT_Nodes` or `FMT_DOF`
! - `value` denotes the nodal values of all dof defined inside `obj`. Once
! storage pattern in `value` can be `FMT_DOF` or `FMT_Nodes`.
! - To tackle this `conversion`  can be Add to `DOFToNodes`, `NodesToDOF`
! or `NONE`.
!
! This subroutine effectivily performes
! `vec( nptrs ) = vec(nptrs) + scale * value`

INTERFACE Add
  MODULE SUBROUTINE obj_Add8(vec, obj, nodenum, VALUE, scale)
    REAL(DFP), INTENT(INOUT) :: vec(:)
    TYPE(DOF_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: nodenum
    REAL(DFP), INTENT(IN) :: VALUE
    REAL(DFP), INTENT(IN) :: scale
  END SUBROUTINE obj_Add8
END INTERFACE Add

!----------------------------------------------------------------------------
!                                                            Add@addMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         27 June 2021
! summary:         Add values in a vector of real numbers
!
!# Introduction
!
! This subroutine is designed to Add values in a vector of real number
! - [[DOF_]] object `obj` contains the storage pattern of degrees of freedom
! inside `vec`. This storage pattern can be `FMT_Nodes` or `FMT_DOF`
! - `value` denotes the nodal values of dof `dofno`.
!
! This subroutine effectivily performes
! `vec( nptrs ) = vec(nptrs) + scale * value`

INTERFACE Add
  MODULE SUBROUTINE obj_Add9(vec, obj, nodenum, VALUE, scale, idof)
    REAL(DFP), INTENT(INOUT) :: vec(:)
    TYPE(DOF_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: nodenum
    REAL(DFP), INTENT(IN) :: VALUE
    REAL(DFP), INTENT(IN) :: scale
    INTEGER(I4B), INTENT(IN) :: idof
  END SUBROUTINE obj_Add9
END INTERFACE Add

!----------------------------------------------------------------------------
!                                                            Add@addMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         27 June 2021
! summary:         Add values in a vector of real numbers
!
!# Introduction
!
! This subroutine is designed to Add values in a vector of real number
! - [[DOF_]] object `obj` contains the storage pattern of degrees of freedom
! inside `vec`. This storage pattern can be `FMT_Nodes` or `FMT_DOF`
! - `value` denotes the nodal values of dof `dofno`.
!
! This subroutine effectivily performes
! `vec( nptrs ) = vec(nptrs) + scale * value`

INTERFACE Add
  MODULE SUBROUTINE obj_Add10(vec, obj, nodenum, VALUE, scale, ivar, idof)
    REAL(DFP), INTENT(INOUT) :: vec(:)
    TYPE(DOF_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: nodenum
    REAL(DFP), INTENT(IN) :: VALUE
    REAL(DFP), INTENT(IN) :: scale
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: idof
  END SUBROUTINE obj_Add10
END INTERFACE Add

!----------------------------------------------------------------------------
!                                                            Add@addMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         27 June 2021
! summary:         Add values in a vector of real numbers
!
!# Introduction
!
! This subroutine is designed to Add values in a vector of real number
! - [[DOF_]] object `obj` contains the storage pattern of degrees of freedom
! inside `vec`. This storage pattern can be `FMT_Nodes` or `FMT_DOF`
! - `value` denotes the nodal values of dof `dofno`.
!
! This subroutine effectivily performes
! `vec( nptrs ) = vec(nptrs) + scale * value`

INTERFACE Add
  MODULE SUBROUTINE obj_Add11(vec, obj, nodenum, VALUE, scale, &
                              ivar, spacecompo, timecompo)
    REAL(DFP), INTENT(INOUT) :: vec(:)
    TYPE(DOF_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: nodenum
    REAL(DFP), INTENT(IN) :: VALUE
    REAL(DFP), INTENT(IN) :: scale
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: spacecompo
    INTEGER(I4B), INTENT(IN) :: timecompo
  END SUBROUTINE obj_Add11
END INTERFACE Add

!----------------------------------------------------------------------------
!                                                            Add@addMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         27 June 2021
! summary:         Add values in a vector of real numbers
!
!# Introduction
!
! This subroutine is designed to Add values in a vector of real number
! - [[DOF_]] object `obj` contains the storage pattern of degrees of freedom
! inside `vec`. This storage pattern can be `FMT_Nodes` or `FMT_DOF`
! - `value` denotes the nodal values of dof `dofno`.
!
! This subroutine effectivily performes
! `vec( nptrs ) = vec(nptrs) + scale * value`

INTERFACE Add
  MODULE SUBROUTINE obj_Add12(vec, obj, nodenum, VALUE, scale, &
                              ivar, spacecompo, timecompo)
    REAL(DFP), INTENT(INOUT) :: vec(:)
    TYPE(DOF_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: nodenum
    REAL(DFP), INTENT(IN) :: VALUE
    REAL(DFP), INTENT(IN) :: scale
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: spacecompo
    INTEGER(I4B), INTENT(IN) :: timecompo(:)
  END SUBROUTINE obj_Add12
END INTERFACE Add

!----------------------------------------------------------------------------
!                                                            Add@addMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         27 June 2021
! summary:         Add values in a vector of real numbers
!
!# Introduction
!
! This subroutine is designed to Add values in a vector of real number
! - [[DOF_]] object `obj` contains the storage pattern of degrees of freedom
! inside `vec`. This storage pattern can be `FMT_Nodes` or `FMT_DOF`
! - `value` denotes the nodal values of dof `dofno`.
!
! This subroutine effectivily performes
! `vec( nptrs ) = vec(nptrs) + scale * value`

INTERFACE Add
  MODULE SUBROUTINE obj_Add13(vec, obj, nodenum, VALUE, scale, &
                              ivar, spacecompo, timecompo)
    REAL(DFP), INTENT(INOUT) :: vec(:)
    TYPE(DOF_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: nodenum
    REAL(DFP), INTENT(IN) :: VALUE
    REAL(DFP), INTENT(IN) :: scale
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: spacecompo(:)
    INTEGER(I4B), INTENT(IN) :: timecompo
  END SUBROUTINE obj_Add13
END INTERFACE Add

END MODULE DOF_AddMethods
