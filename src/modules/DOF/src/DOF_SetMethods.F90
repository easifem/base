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

MODULE DOF_SetMethods
USE GlobalData, ONLY: DFP, I4B, LGT
USE BaseType, ONLY: DOF_

IMPLICIT NONE
PRIVATE

PUBLIC :: Set

!----------------------------------------------------------------------------
!                                                             Set@SetMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 26 June 2021
! summary: Set values in a vector of real numbers
!
!# Introduction
!
! - This subroutine is designed to Set the values in a vector of real number
! - This subroutine effectivily performes `vec( nptrs ) = value`
! - If `SIZE(value)==1` then all values are Set to `value(1)`
! - If `SIZE(value) .EQ. SIZE(nptrs)` then, each dof is Set to value
! - If `SIZE(value)=tDOF*Size(nptrs)` then each dof is Set to appropriate
! value from value

INTERFACE Set
  MODULE PURE SUBROUTINE obj_Set1(vec, obj, nodenum, VALUE, conversion)
    REAL(DFP), INTENT(INOUT) :: vec(:)
    CLASS(DOF_), INTENT(IN) :: obj
    !! `obj` contains the storage pattern of degrees of freedom
    !! inside `vec`.
    !! This storage pattern can be `FMT_Nodes` or `FMT_DOF`
    INTEGER(I4B), INTENT(IN) :: nodenum(:)
    !! node number to set
    INTEGER(I4B), INTENT(IN) :: conversion(1)
    !! DOFToNodes
    !! NodesTODOF
    !! None
    REAL(DFP), INTENT(IN) :: VALUE(:)
    !! `value` denotes the nodal values of all dof defined inside `obj`.
    !! The storage pattern in `value` can be `FMT_DOF` or `FMT_Nodes`.
  END SUBROUTINE obj_Set1
END INTERFACE Set

!----------------------------------------------------------------------------
!                                                             Set@SetMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 26 June 2021
! summary: Set values in a vector of real numbers

INTERFACE Set
  MODULE PURE SUBROUTINE obj_Set2(vec, obj, nodenum, VALUE)
    REAL(DFP), INTENT(INOUT) :: vec(:)
    CLASS(DOF_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: nodenum(:)
    !! node number to set
    REAL(DFP), INTENT(IN) :: VALUE
    !! scalar value
  END SUBROUTINE obj_Set2
END INTERFACE Set

!----------------------------------------------------------------------------
!                                                             Set@SetMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 27 June 2021
! summary: Set values in a vector of real numbers
!
!# Introduction
!
! - This subroutine is designed to Set the values in a array of real number.
! - This subroutine handles only those entries which belongs to the
! dofno.
! - This subroutine effectivily performes `vec( nptrs ) = value`
! - If the size of value is not equal to 1, then the size of nptrs should be
! same as the size of value
!
!@note
! In [[DOF_]], dofno are continuously numbered, so if there are two
! or more physical variables, then dofno of the second or later physical
! variables will not start from 1.
!@endnote

INTERFACE Set
  MODULE PURE SUBROUTINE obj_Set3(vec, obj, nodenum, VALUE, idof)
    REAL(DFP), INTENT(INOUT) :: vec(:)
    CLASS(DOF_), INTENT(IN) :: obj
    !! Object `obj` contains the storage pattern of degrees of freedom
    !! inside `vec`.
    !! This storage pattern can be `FMT_Nodes` or `FMT_DOF`
    INTEGER(I4B), INTENT(IN) :: nodenum(:)
    !! node number
    REAL(DFP), INTENT(IN) :: VALUE(:)
    !! `value` denotes the nodal values of dof `idof`.
    INTEGER(I4B), INTENT(IN) :: idof
    !! global degree of freedom in obj
  END SUBROUTINE obj_Set3
END INTERFACE Set

!----------------------------------------------------------------------------
!                                                             Set@SetMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 27 June 2021
! summary: Set values in a vector of real numbers

INTERFACE Set
  MODULE PURE SUBROUTINE obj_Set4(vec, obj, nodenum, VALUE, ivar, idof)
    REAL(DFP), INTENT(INOUT) :: vec(:)
    CLASS(DOF_), INTENT(IN) :: obj
    !! Object `obj` contains the storage pattern of degrees of freedom
    !! inside `vec`.
    !! This storage pattern can be `FMT_Nodes` or `FMT_DOF`
    INTEGER(I4B), INTENT(IN) :: nodenum(:)
    !! node number
    REAL(DFP), INTENT(IN) :: VALUE(:)
    !! `value` denotes the nodal values of dof `idof`.
    INTEGER(I4B), INTENT(IN) :: ivar
    !! physical variable
    INTEGER(I4B), INTENT(IN) :: idof
  END SUBROUTINE obj_Set4
END INTERFACE Set

!----------------------------------------------------------------------------
!                                                             Set@SetMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 27 June 2021
! summary: Set values in a vector of real numbers

INTERFACE Set
  MODULE PURE SUBROUTINE obj_Set5(vec, obj, nodenum, VALUE, ivar, &
                                  spacecompo, timecompo)
    REAL(DFP), INTENT(INOUT) :: vec(:)
    CLASS(DOF_), INTENT(IN) :: obj
    !! Object `obj` contains the storage pattern of degrees of freedom
    !! inside `vec`.
    !! This storage pattern can be `FMT_Nodes` or `FMT_DOF`
    INTEGER(I4B), INTENT(IN) :: nodenum(:)
    !! node number
    REAL(DFP), INTENT(IN) :: VALUE(:)
    !! `value` denotes the nodal values of dof `idof`.
    INTEGER(I4B), INTENT(IN) :: ivar
    !! physical variable
    INTEGER(I4B), INTENT(IN) :: spacecompo
    !! space component of physical variable
    INTEGER(I4B), INTENT(IN) :: timecompo
    !! time component of physical  variable
  END SUBROUTINE obj_Set5
END INTERFACE Set

!----------------------------------------------------------------------------
!                                                             Set@SetMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 27 June 2021
! summary: Set values in a vector of real numbers

INTERFACE Set
  MODULE PURE SUBROUTINE obj_Set6(vec, obj, nodenum, VALUE, ivar, &
                                  spacecompo, timecompo)
    REAL(DFP), INTENT(INOUT) :: vec(:)
    CLASS(DOF_), INTENT(IN) :: obj
    !! Object `obj` contains the storage pattern of degrees of freedom
    !! inside `vec`.
    !! This storage pattern can be `FMT_Nodes` or `FMT_DOF`
    INTEGER(I4B), INTENT(IN) :: nodenum(:)
    !! node number
    REAL(DFP), INTENT(IN) :: VALUE(:)
    !! `value` denotes the nodal values of dof `idof`.
    INTEGER(I4B), INTENT(IN) :: ivar
    !! physical variable
    INTEGER(I4B), INTENT(IN) :: spacecompo
    !! space component of physical variable
    INTEGER(I4B), INTENT(IN) :: timecompo(:)
    !! time components of physical variables
  END SUBROUTINE obj_Set6
END INTERFACE Set

!----------------------------------------------------------------------------
!                                                             Set@SetMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 27 June 2021
! summary: Set values in a vector of real numbers

INTERFACE Set
  MODULE PURE SUBROUTINE obj_Set7(vec, obj, nodenum, VALUE, ivar, &
                                  spacecompo, timecompo)
    REAL(DFP), INTENT(INOUT) :: vec(:)
    CLASS(DOF_), INTENT(IN) :: obj
    !! Object `obj` contains the storage pattern of degrees of freedom
    !! inside `vec`.
    !! This storage pattern can be `FMT_Nodes` or `FMT_DOF`
    INTEGER(I4B), INTENT(IN) :: nodenum(:)
    !! node number
    REAL(DFP), INTENT(IN) :: VALUE(:)
    !! `value` denotes the nodal values of dof `idof`.
    INTEGER(I4B), INTENT(IN) :: ivar
    !! physical variable
    INTEGER(I4B), INTENT(IN) :: spacecompo(:)
    !! space components of physical variables
    INTEGER(I4B), INTENT(IN) :: timecompo
    !! time component of physical variable
  END SUBROUTINE obj_Set7
END INTERFACE Set

!----------------------------------------------------------------------------
!                                                             Set@SetMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 26 June 2021
! summary: Set values in a vector of real numbers

INTERFACE Set
  MODULE PURE SUBROUTINE obj_Set8(vec, obj, nodenum, VALUE)
    REAL(DFP), INTENT(INOUT) :: vec(:)
    CLASS(DOF_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: nodenum
    !! node to set
    REAL(DFP), INTENT(IN) :: VALUE
    !! scalar value
  END SUBROUTINE obj_Set8
END INTERFACE Set

!----------------------------------------------------------------------------
!                                                             Set@SetMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 27 June 2021
! summary: Set values in a vector of real numbers
!
!# Introduction
!
! - This subroutine is designed to Set the values in a array of real number.
! - This subroutine handles only those entries which belongs to the
! dofno.
! - This subroutine effectivily performes `vec( nptrs ) = value`
! - If the size of value is not equal to 1, then the size of nptrs should be
! same as the size of value
!
!@note
! In [[DOF_]], dofno are continuously numbered, so if there are two
! or more physical variables, then dofno of the second or later physical
! variables will not start from 1.
!@endnote

INTERFACE Set
  MODULE PURE SUBROUTINE obj_Set9(vec, obj, nodenum, VALUE, idof)
    REAL(DFP), INTENT(INOUT) :: vec(:)
    CLASS(DOF_), INTENT(IN) :: obj
    !! Object `obj` contains the storage pattern of degrees of freedom
    !! inside `vec`.
    !! This storage pattern can be `FMT_Nodes` or `FMT_DOF`
    INTEGER(I4B), INTENT(IN) :: nodenum
    !! node number
    REAL(DFP), INTENT(IN) :: VALUE
    !! `value` denotes the nodal values of dof `idof`.
    INTEGER(I4B), INTENT(IN) :: idof
    !! global degree of freedom in obj
  END SUBROUTINE obj_Set9
END INTERFACE Set

!----------------------------------------------------------------------------
!                                                             Set@SetMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 27 June 2021
! summary: Set values in a vector of real numbers
!
!# Introduction
!
! - This subroutine is designed to Set the values in a array of real number.
! - This subroutine handles only those entries which belongs to the
! dofno.
! - This subroutine effectivily performes `vec( nptrs ) = value`
! - If the size of value is not equal to 1, then the size of nptrs should be
! same as the size of value
!
!@note
! In [[DOF_]], dofno are continuously numbered, so if there are two
! or more physical variables, then dofno of the second or later physical
! variables will not start from 1.
!@endnote

INTERFACE Set
  MODULE PURE SUBROUTINE obj_Set10(vec, obj, nodenum, VALUE, ivar, idof)
    REAL(DFP), INTENT(INOUT) :: vec(:)
    CLASS(DOF_), INTENT(IN) :: obj
    !! object `obj` contains the storage pattern of degrees of freedom
    !! inside `vec`.
    !! This storage pattern can be `FMT_Nodes` or `FMT_DOF`
    INTEGER(I4B), INTENT(IN) :: nodenum
    !! node number
    REAL(DFP), INTENT(IN) :: VALUE
    !! `value` denotes the nodal values of dof `idof`.
    INTEGER(I4B), INTENT(IN) :: ivar
    !! physical variable
    INTEGER(I4B), INTENT(IN) :: idof
    !! local degree of freedom in physical variable
  END SUBROUTINE obj_Set10
END INTERFACE Set

!----------------------------------------------------------------------------
!                                                             Set@SetMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 27 June 2021
! summary: Set values in a vector of real numbers
!
!# Introduction
!
! - This subroutine is designed to Set the values in a array of real number.
! - This subroutine handles only those entries which belongs to the
! dofno.
! - This subroutine effectivily performes `vec( nptrs ) = value`
! - If the size of value is not equal to 1, then the size of nptrs should be
! same as the size of value
!
!@note
! In [[DOF_]], dofno are continuously numbered, so if there are two
! or more physical variables, then dofno of the second or later physical
! variables will not start from 1.
!@endnote

INTERFACE Set
  MODULE PURE SUBROUTINE obj_Set11(vec, obj, nodenum, VALUE, ivar, &
                                   spacecompo, timecompo)
    REAL(DFP), INTENT(INOUT) :: vec(:)
    CLASS(DOF_), INTENT(IN) :: obj
    !! object `obj` contains the storage pattern of degrees of freedom
    !! inside `vec`.
    !! This storage pattern can be `FMT_Nodes` or `FMT_DOF`
    INTEGER(I4B), INTENT(IN) :: nodenum
    !! node number
    REAL(DFP), INTENT(IN) :: VALUE
    !! `value` denotes the nodal values of dof `idof`.
    INTEGER(I4B), INTENT(IN) :: ivar
    !! physical variable
    INTEGER(I4B), INTENT(IN) :: spacecompo
    !! space component of physical variable
    INTEGER(I4B), INTENT(IN) :: timecompo
    !! time component of physical variable
  END SUBROUTINE obj_Set11
END INTERFACE Set

!----------------------------------------------------------------------------
!                                                             Set@SetMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 27 June 2021
! summary: Set values in a vector of real numbers
!
!# Introduction
!
! - This subroutine is designed to Set the values in a array of real number.
! - This subroutine handles only those entries which belongs to the
! dofno.
! - This subroutine effectivily performes `vec( nptrs ) = value`
! - If the size of value is not equal to 1, then the size of nptrs should be
! same as the size of value
!
!@note
! In [[DOF_]], dofno are continuously numbered, so if there are two
! or more physical variables, then dofno of the second or later physical
! variables will not start from 1.
!@endnote

INTERFACE Set
  MODULE PURE SUBROUTINE obj_Set12(vec, obj, nodenum, VALUE, ivar, &
                                   spacecompo, timecompo)
    REAL(DFP), INTENT(INOUT) :: vec(:)
    CLASS(DOF_), INTENT(IN) :: obj
    !! object `obj` contains the storage pattern of degrees of freedom
    !! inside `vec`.
    !! This storage pattern can be `FMT_Nodes` or `FMT_DOF`
    INTEGER(I4B), INTENT(IN) :: nodenum
    !! node number
    REAL(DFP), INTENT(IN) :: VALUE
    !! `value` denotes the nodal values of dof `idof`.
    INTEGER(I4B), INTENT(IN) :: ivar
    !! physical variable
    INTEGER(I4B), INTENT(IN) :: spacecompo
    !! space component of physical variable
    INTEGER(I4B), INTENT(IN) :: timecompo(:)
    !! time components of physical variables
  END SUBROUTINE obj_Set12
END INTERFACE Set

!----------------------------------------------------------------------------
!                                                             Set@SetMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 27 June 2021
! summary: Set values in a vector of real numbers
!
!# Introduction
!
! - This subroutine is designed to Set the values in a array of real number.
! - This subroutine handles only those entries which belongs to the
! dofno.
! - This subroutine effectivily performes `vec( nptrs ) = value`
! - If the size of value is not equal to 1, then the size of nptrs should be
! same as the size of value
!
!@note
! In [[DOF_]], dofno are continuously numbered, so if there are two
! or more physical variables, then dofno of the second or later physical
! variables will not start from 1.
!@endnote

INTERFACE Set
  MODULE PURE SUBROUTINE obj_Set13(vec, obj, nodenum, VALUE, ivar, &
                                   spacecompo, timecompo)
    REAL(DFP), INTENT(INOUT) :: vec(:)
    CLASS(DOF_), INTENT(IN) :: obj
    !! object `obj` contains the storage pattern of degrees of freedom
    !! inside `vec`.
    !! This storage pattern can be `FMT_Nodes` or `FMT_DOF`
    INTEGER(I4B), INTENT(IN) :: nodenum
    !! node number
    REAL(DFP), INTENT(IN) :: VALUE
    !! `value` denotes the nodal values of dof `idof`.
    INTEGER(I4B), INTENT(IN) :: ivar
    !! physical variable
    INTEGER(I4B), INTENT(IN) :: spacecompo(:)
    !! space components of physical variables
    INTEGER(I4B), INTENT(IN) :: timecompo
    !! time component of physical variable
  END SUBROUTINE obj_Set13
END INTERFACE Set

END MODULE DOF_SetMethods
