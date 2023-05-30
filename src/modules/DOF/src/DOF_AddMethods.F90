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
USE GlobalData
USE BaseType
IMPLICIT NONE
PRIVATE

PUBLIC :: add

!----------------------------------------------------------------------------
!                                                            add@addMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 27 June 2021
! summary:          add values in a vector of real numbers
!
!# Introduction
!
! This subroutine is designed to add values in a vector of real number
! - [[DOF_]] object `obj` contains the storage pattern of degrees of freedom
! inside `vec`. This storage pattern can be `FMT_Nodes` or `FMT_DOF`
! - `value` denotes the nodal values of all dof defined inside `obj`. Once
! storage pattern in `value` can be `FMT_DOF` or `FMT_Nodes`.
! - To tackle this `conversion`  can be add to `DOFToNodes`, `NodesToDOF`
! or `NONE`.
!
! This subroutine effectivily performes
! `vec( nptrs ) = vec(nptrs) + scale * value`

INTERFACE
  MODULE PURE SUBROUTINE dof_add1(vec, obj, nodenum, VALUE, scale, &
    & conversion)
    REAL(DFP), INTENT(INOUT) :: vec(:)
    CLASS(DOF_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: nodenum(:)
    REAL(DFP), INTENT(IN) :: VALUE(:)
    REAL(DFP), INTENT(IN) :: scale
    INTEGER(I4B), INTENT(IN) :: conversion(1)
  END SUBROUTINE dof_add1
END INTERFACE

INTERFACE add
  MODULE PROCEDURE dof_add1
END INTERFACE add

!----------------------------------------------------------------------------
!                                                            add@addMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 27 June 2021
! summary:          add values in a vector of real numbers
!
!# Introduction
!
! This subroutine is designed to add values in a vector of real number
! - [[DOF_]] object `obj` contains the storage pattern of degrees of freedom
! inside `vec`. This storage pattern can be `FMT_Nodes` or `FMT_DOF`
! - `value` denotes the nodal values of all dof defined inside `obj`. Once
! storage pattern in `value` can be `FMT_DOF` or `FMT_Nodes`.
! - To tackle this `conversion`  can be add to `DOFToNodes`, `NodesToDOF`
! or `NONE`.
!
! This subroutine effectivily performes
! `vec( nptrs ) = vec(nptrs) + scale * value`

INTERFACE
  MODULE PURE SUBROUTINE dof_add2(vec, obj, nodenum, VALUE, scale)
    REAL(DFP), INTENT(INOUT) :: vec(:)
    CLASS(DOF_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: nodenum(:)
    REAL(DFP), INTENT(IN) :: VALUE
    REAL(DFP), INTENT(IN) :: scale
  END SUBROUTINE dof_add2
END INTERFACE

INTERFACE add
  MODULE PROCEDURE dof_add2
END INTERFACE add

!----------------------------------------------------------------------------
!                                                            add@addMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         27 June 2021
! summary:         add values in a vector of real numbers
!
!# Introduction
!
! This subroutine is designed to add values in a vector of real number
! - [[DOF_]] object `obj` contains the storage pattern of degrees of freedom
! inside `vec`. This storage pattern can be `FMT_Nodes` or `FMT_DOF`
! - `value` denotes the nodal values of dof `dofno`.
!
! This subroutine effectivily performes
! `vec( nptrs ) = vec(nptrs) + scale * value`

INTERFACE
  MODULE PURE SUBROUTINE dof_add3(vec, obj, nodenum, VALUE, scale, idof)
    REAL(DFP), INTENT(INOUT) :: vec(:)
    CLASS(DOF_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: nodenum(:)
    REAL(DFP), INTENT(IN) :: VALUE(:)
    REAL(DFP), INTENT(IN) :: scale
    INTEGER(I4B), INTENT(IN) :: idof
  END SUBROUTINE dof_add3
END INTERFACE

INTERFACE add
  MODULE PROCEDURE dof_add3
END INTERFACE add

!----------------------------------------------------------------------------
!                                                             add@addMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 27 June 2021
! summary: add values in a vector of real numbers
!

INTERFACE
  MODULE PURE SUBROUTINE dof_add4(vec, obj, nodenum, VALUE, scale, ivar, idof)
    REAL(DFP), INTENT(INOUT) :: vec(:)
    CLASS(DOF_), INTENT(IN) :: obj
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
  END SUBROUTINE dof_add4
END INTERFACE

INTERFACE add
  MODULE PROCEDURE dof_add4
END INTERFACE add

!----------------------------------------------------------------------------
!                                                             add@addMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 27 June 2021
! summary: add values in a vector of real numbers

INTERFACE
  MODULE PURE SUBROUTINE dof_add5(vec, obj, nodenum, VALUE, scale, ivar, &
      & spacecompo, timecompo)
    REAL(DFP), INTENT(INOUT) :: vec(:)
    CLASS(DOF_), INTENT(IN) :: obj
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
    INTEGER(I4B), INTENT(IN) :: timecompo
  !! time components
  END SUBROUTINE dof_add5
END INTERFACE

INTERFACE add
  MODULE PROCEDURE dof_add5
END INTERFACE add

!----------------------------------------------------------------------------
!                                                             add@addMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 27 June 2021
! summary: add values in a vector of real numbers

INTERFACE
  MODULE PURE SUBROUTINE dof_add6(vec, obj, nodenum, VALUE, scale, ivar, &
      & spacecompo, timecompo)
    REAL(DFP), INTENT(INOUT) :: vec(:)
    CLASS(DOF_), INTENT(IN) :: obj
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
  END SUBROUTINE dof_add6
END INTERFACE

INTERFACE add
  MODULE PROCEDURE dof_add6
END INTERFACE add

!----------------------------------------------------------------------------
!                                                             add@addMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 27 June 2021
! summary: add values in a vector of real numbers

INTERFACE
  MODULE PURE SUBROUTINE dof_add7(vec, obj, nodenum, VALUE, scale, ivar, &
      & spacecompo, timecompo)
    REAL(DFP), INTENT(INOUT) :: vec(:)
    CLASS(DOF_), INTENT(IN) :: obj
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
  END SUBROUTINE dof_add7
END INTERFACE

INTERFACE add
  MODULE PROCEDURE dof_add7
END INTERFACE add

!----------------------------------------------------------------------------
!                                                            add@addMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 27 June 2021
! summary:          add values in a vector of real numbers
!
!# Introduction
!
! This subroutine is designed to add values in a vector of real number
! - [[DOF_]] object `obj` contains the storage pattern of degrees of freedom
! inside `vec`. This storage pattern can be `FMT_Nodes` or `FMT_DOF`
! - `value` denotes the nodal values of all dof defined inside `obj`. Once
! storage pattern in `value` can be `FMT_DOF` or `FMT_Nodes`.
! - To tackle this `conversion`  can be add to `DOFToNodes`, `NodesToDOF`
! or `NONE`.
!
! This subroutine effectivily performes
! `vec( nptrs ) = vec(nptrs) + scale * value`

INTERFACE
  MODULE PURE SUBROUTINE dof_add8(vec, obj, nodenum, VALUE, scale)
    REAL(DFP), INTENT(INOUT) :: vec(:)
    CLASS(DOF_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: nodenum
    REAL(DFP), INTENT(IN) :: VALUE
    REAL(DFP), INTENT(IN) :: scale
  END SUBROUTINE dof_add8
END INTERFACE

INTERFACE add
  MODULE PROCEDURE dof_add8
END INTERFACE add

!----------------------------------------------------------------------------
!                                                            add@addMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         27 June 2021
! summary:         add values in a vector of real numbers
!
!# Introduction
!
! This subroutine is designed to add values in a vector of real number
! - [[DOF_]] object `obj` contains the storage pattern of degrees of freedom
! inside `vec`. This storage pattern can be `FMT_Nodes` or `FMT_DOF`
! - `value` denotes the nodal values of dof `dofno`.
!
! This subroutine effectivily performes
! `vec( nptrs ) = vec(nptrs) + scale * value`

INTERFACE
  MODULE PURE SUBROUTINE dof_add9(vec, obj, nodenum, VALUE, scale, idof)
    REAL(DFP), INTENT(INOUT) :: vec(:)
    CLASS(DOF_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: nodenum
    REAL(DFP), INTENT(IN) :: VALUE
    REAL(DFP), INTENT(IN) :: scale
    INTEGER(I4B), INTENT(IN) :: idof
  END SUBROUTINE dof_add9
END INTERFACE

INTERFACE add
  MODULE PROCEDURE dof_add9
END INTERFACE add

!----------------------------------------------------------------------------
!                                                            add@addMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         27 June 2021
! summary:         add values in a vector of real numbers
!
!# Introduction
!
! This subroutine is designed to add values in a vector of real number
! - [[DOF_]] object `obj` contains the storage pattern of degrees of freedom
! inside `vec`. This storage pattern can be `FMT_Nodes` or `FMT_DOF`
! - `value` denotes the nodal values of dof `dofno`.
!
! This subroutine effectivily performes
! `vec( nptrs ) = vec(nptrs) + scale * value`

INTERFACE
  MODULE PURE SUBROUTINE dof_add10(vec, obj, nodenum, VALUE, scale, &
    & ivar, idof)
    REAL(DFP), INTENT(INOUT) :: vec(:)
    CLASS(DOF_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: nodenum
    REAL(DFP), INTENT(IN) :: VALUE
    REAL(DFP), INTENT(IN) :: scale
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: idof
  END SUBROUTINE dof_add10
END INTERFACE

INTERFACE add
  MODULE PROCEDURE dof_add10
END INTERFACE add

!----------------------------------------------------------------------------
!                                                            add@addMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         27 June 2021
! summary:         add values in a vector of real numbers
!
!# Introduction
!
! This subroutine is designed to add values in a vector of real number
! - [[DOF_]] object `obj` contains the storage pattern of degrees of freedom
! inside `vec`. This storage pattern can be `FMT_Nodes` or `FMT_DOF`
! - `value` denotes the nodal values of dof `dofno`.
!
! This subroutine effectivily performes
! `vec( nptrs ) = vec(nptrs) + scale * value`

INTERFACE
  MODULE PURE SUBROUTINE dof_add11(vec, obj, nodenum, VALUE, scale, &
    & ivar, spacecompo, timecompo)
    REAL(DFP), INTENT(INOUT) :: vec(:)
    CLASS(DOF_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: nodenum
    REAL(DFP), INTENT(IN) :: VALUE
    REAL(DFP), INTENT(IN) :: scale
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: spacecompo
    INTEGER(I4B), INTENT(IN) :: timecompo
  END SUBROUTINE dof_add11
END INTERFACE

INTERFACE add
  MODULE PROCEDURE dof_add11
END INTERFACE add

!----------------------------------------------------------------------------
!                                                            add@addMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         27 June 2021
! summary:         add values in a vector of real numbers
!
!# Introduction
!
! This subroutine is designed to add values in a vector of real number
! - [[DOF_]] object `obj` contains the storage pattern of degrees of freedom
! inside `vec`. This storage pattern can be `FMT_Nodes` or `FMT_DOF`
! - `value` denotes the nodal values of dof `dofno`.
!
! This subroutine effectivily performes
! `vec( nptrs ) = vec(nptrs) + scale * value`

INTERFACE
  MODULE PURE SUBROUTINE dof_add12(vec, obj, nodenum, VALUE, scale, &
    & ivar, spacecompo, timecompo)
    REAL(DFP), INTENT(INOUT) :: vec(:)
    CLASS(DOF_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: nodenum
    REAL(DFP), INTENT(IN) :: VALUE
    REAL(DFP), INTENT(IN) :: scale
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: spacecompo
    INTEGER(I4B), INTENT(IN) :: timecompo(:)
  END SUBROUTINE dof_add12
END INTERFACE

INTERFACE add
  MODULE PROCEDURE dof_add12
END INTERFACE add

!----------------------------------------------------------------------------
!                                                            add@addMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         27 June 2021
! summary:         add values in a vector of real numbers
!
!# Introduction
!
! This subroutine is designed to add values in a vector of real number
! - [[DOF_]] object `obj` contains the storage pattern of degrees of freedom
! inside `vec`. This storage pattern can be `FMT_Nodes` or `FMT_DOF`
! - `value` denotes the nodal values of dof `dofno`.
!
! This subroutine effectivily performes
! `vec( nptrs ) = vec(nptrs) + scale * value`

INTERFACE
  MODULE PURE SUBROUTINE dof_add13(vec, obj, nodenum, VALUE, scale, &
    & ivar, spacecompo, timecompo)
    REAL(DFP), INTENT(INOUT) :: vec(:)
    CLASS(DOF_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: nodenum
    REAL(DFP), INTENT(IN) :: VALUE
    REAL(DFP), INTENT(IN) :: scale
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: spacecompo(:)
    INTEGER(I4B), INTENT(IN) :: timecompo
  END SUBROUTINE dof_add13
END INTERFACE

INTERFACE add
  MODULE PROCEDURE dof_add13
END INTERFACE add

END MODULE DOF_AddMethods
