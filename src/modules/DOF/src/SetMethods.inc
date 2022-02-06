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

PUBLIC :: set

!----------------------------------------------------------------------------
!                                                             set@setMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 26 June 2021
! summary: Set values in a vector of real numbers
!
!# Introduction
!
! - This subroutine is designed to set the values in a vector of real number
! - This subroutine effectivily performes `vec( nptrs ) = value`
! - If `SIZE(value)==1` then all values are set to `value(1)`
! - If `SIZE(value) .EQ. SIZE(nptrs)` then, each dof is set to value
! - If `SIZE(value)=tDOF*Size(nptrs)` then each dof is set to appropriate
! value from value

INTERFACE
MODULE PURE SUBROUTINE dof_set1( vec, obj, nodenum, value, conversion )
  REAL( DFP ), INTENT( INOUT ) :: vec( : )
  CLASS( DOF_ ), INTENT( IN ) :: obj
  !! `obj` contains the storage pattern of degrees of freedom
  !! inside `vec`.
  !! This storage pattern can be `FMT_Nodes` or `FMT_DOF`
  INTEGER( I4B ), INTENT( IN ) :: nodenum( : )
  INTEGER( I4B ), INTENT( IN ) :: conversion( 1 )
  !! DOFToNodes
  !! NodesTODOF
  !! None
  REAL( DFP ), INTENT( IN ) :: value( : )
  !! `value` denotes the nodal values of all dof defined inside `obj`.
  !! The storage pattern in `value` can be `FMT_DOF` or `FMT_Nodes`.
END SUBROUTINE dof_set1
END INTERFACE

INTERFACE set
  MODULE PROCEDURE dof_set1
END INTERFACE set

!----------------------------------------------------------------------------
!                                                             set@setMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 26 June 2021
! summary: Set values in a vector of real numbers

INTERFACE
MODULE PURE SUBROUTINE dof_set2( vec, obj, nodenum, value )
  REAL( DFP ), INTENT( INOUT ) :: vec( : )
  CLASS( DOF_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: nodenum( : )
  REAL( DFP ), INTENT( IN ) :: value
END SUBROUTINE dof_set2
END INTERFACE

INTERFACE set
  MODULE PROCEDURE dof_set2
END INTERFACE set

!----------------------------------------------------------------------------
!                                                             set@setMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 27 June 2021
! summary: Set values in a vector of real numbers
!
!# Introduction
!
! - This subroutine is designed to set the values in a array of real number.
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

INTERFACE
MODULE PURE SUBROUTINE dof_set3( vec, obj, nodenum, value, idof )
  REAL( DFP ), INTENT( INOUT ) :: vec( : )
  CLASS( DOF_ ), INTENT( IN ) :: obj
  !! Object `obj` contains the storage pattern of degrees of freedom
  !! inside `vec`.
  !! This storage pattern can be `FMT_Nodes` or `FMT_DOF`
  INTEGER( I4B ), INTENT( IN ) :: nodenum( : )
  !! node number
  REAL( DFP ), INTENT( IN ) :: value( : )
  !! `value` denotes the nodal values of dof `idof`.
  INTEGER( I4B ), INTENT( IN ) :: idof
END SUBROUTINE dof_set3
END INTERFACE

INTERFACE set
  MODULE PROCEDURE dof_set3
END INTERFACE set

!----------------------------------------------------------------------------
!                                                             set@setMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 27 June 2021
! summary: Set values in a vector of real numbers

INTERFACE
MODULE PURE SUBROUTINE dof_set4( vec, obj, nodenum, value, ivar, idof )
  REAL( DFP ), INTENT( INOUT ) :: vec( : )
  CLASS( DOF_ ), INTENT( IN ) :: obj
  !! Object `obj` contains the storage pattern of degrees of freedom
  !! inside `vec`.
  !! This storage pattern can be `FMT_Nodes` or `FMT_DOF`
  INTEGER( I4B ), INTENT( IN ) :: nodenum( : )
  !! node number
  REAL( DFP ), INTENT( IN ) :: value( : )
  !! `value` denotes the nodal values of dof `idof`.
  INTEGER( I4B ), INTENT( IN ) :: ivar
  !! physical variable
  INTEGER( I4B ), INTENT( IN ) :: idof
END SUBROUTINE dof_set4
END INTERFACE

INTERFACE set
  MODULE PROCEDURE dof_set4
END INTERFACE set

!----------------------------------------------------------------------------
!                                                             set@setMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 27 June 2021
! summary: Set values in a vector of real numbers

INTERFACE
MODULE PURE SUBROUTINE dof_set5( vec, obj, nodenum, value, ivar, &
    & spacecompo, timecompo )
  REAL( DFP ), INTENT( INOUT ) :: vec( : )
  CLASS( DOF_ ), INTENT( IN ) :: obj
  !! Object `obj` contains the storage pattern of degrees of freedom
  !! inside `vec`.
  !! This storage pattern can be `FMT_Nodes` or `FMT_DOF`
  INTEGER( I4B ), INTENT( IN ) :: nodenum( : )
  !! node number
  REAL( DFP ), INTENT( IN ) :: value( : )
  !! `value` denotes the nodal values of dof `idof`.
  INTEGER( I4B ), INTENT( IN ) :: ivar
  !! physical variable
  INTEGER( I4B ), INTENT( IN ) :: spacecompo
  INTEGER( I4B ), INTENT( IN ) :: timecompo
END SUBROUTINE dof_set5
END INTERFACE

INTERFACE set
  MODULE PROCEDURE dof_set5
END INTERFACE set

!----------------------------------------------------------------------------
!                                                             set@setMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 27 June 2021
! summary: Set values in a vector of real numbers

INTERFACE
MODULE PURE SUBROUTINE dof_set6( vec, obj, nodenum, value, ivar, &
    & spacecompo, timecompo )
  REAL( DFP ), INTENT( INOUT ) :: vec( : )
  CLASS( DOF_ ), INTENT( IN ) :: obj
  !! Object `obj` contains the storage pattern of degrees of freedom
  !! inside `vec`.
  !! This storage pattern can be `FMT_Nodes` or `FMT_DOF`
  INTEGER( I4B ), INTENT( IN ) :: nodenum( : )
  !! node number
  REAL( DFP ), INTENT( IN ) :: value( : )
  !! `value` denotes the nodal values of dof `idof`.
  INTEGER( I4B ), INTENT( IN ) :: ivar
  !! physical variable
  INTEGER( I4B ), INTENT( IN ) :: spacecompo
  INTEGER( I4B ), INTENT( IN ) :: timecompo(:)
END SUBROUTINE dof_set6
END INTERFACE

INTERFACE set
  MODULE PROCEDURE dof_set6
END INTERFACE set

!----------------------------------------------------------------------------
!                                                             set@setMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 27 June 2021
! summary: Set values in a vector of real numbers

INTERFACE
MODULE PURE SUBROUTINE dof_set7( vec, obj, nodenum, value, ivar, &
    & spacecompo, timecompo )
  REAL( DFP ), INTENT( INOUT ) :: vec( : )
  CLASS( DOF_ ), INTENT( IN ) :: obj
  !! Object `obj` contains the storage pattern of degrees of freedom
  !! inside `vec`.
  !! This storage pattern can be `FMT_Nodes` or `FMT_DOF`
  INTEGER( I4B ), INTENT( IN ) :: nodenum( : )
  !! node number
  REAL( DFP ), INTENT( IN ) :: value( : )
  !! `value` denotes the nodal values of dof `idof`.
  INTEGER( I4B ), INTENT( IN ) :: ivar
  !! physical variable
  INTEGER( I4B ), INTENT( IN ) :: spacecompo(:)
  INTEGER( I4B ), INTENT( IN ) :: timecompo
END SUBROUTINE dof_set7
END INTERFACE

INTERFACE set
  MODULE PROCEDURE dof_set7
END INTERFACE set

!----------------------------------------------------------------------------
!                                                             set@setMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 26 June 2021
! summary: Set values in a vector of real numbers

INTERFACE
MODULE PURE SUBROUTINE dof_set8( vec, obj, nodenum, value )
  REAL( DFP ), INTENT( INOUT ) :: vec( : )
  CLASS( DOF_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: nodenum
  REAL( DFP ), INTENT( IN ) :: value
END SUBROUTINE dof_set8
END INTERFACE

INTERFACE set
  MODULE PROCEDURE dof_set8
END INTERFACE set

!----------------------------------------------------------------------------
!                                                             set@setMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 27 June 2021
! summary: Set values in a vector of real numbers
!
!# Introduction
!
! - This subroutine is designed to set the values in a array of real number.
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

INTERFACE
MODULE PURE SUBROUTINE dof_set9( vec, obj, nodenum, value, idof )
  REAL( DFP ), INTENT( INOUT ) :: vec( : )
  CLASS( DOF_ ), INTENT( IN ) :: obj
  !! Object `obj` contains the storage pattern of degrees of freedom
  !! inside `vec`.
  !! This storage pattern can be `FMT_Nodes` or `FMT_DOF`
  INTEGER( I4B ), INTENT( IN ) :: nodenum
  !! node number
  REAL( DFP ), INTENT( IN ) :: value
  !! `value` denotes the nodal values of dof `idof`.
  INTEGER( I4B ), INTENT( IN ) :: idof
END SUBROUTINE dof_set9
END INTERFACE

INTERFACE set
  MODULE PROCEDURE dof_set9
END INTERFACE set

!----------------------------------------------------------------------------
!                                                             set@setMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 27 June 2021
! summary: Set values in a vector of real numbers
!
!# Introduction
!
! - This subroutine is designed to set the values in a array of real number.
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

INTERFACE
MODULE PURE SUBROUTINE dof_set10( vec, obj, nodenum, value, ivar, idof )
  REAL( DFP ), INTENT( INOUT ) :: vec( : )
  CLASS( DOF_ ), INTENT( IN ) :: obj
  !! object `obj` contains the storage pattern of degrees of freedom
  !! inside `vec`.
  !! This storage pattern can be `FMT_Nodes` or `FMT_DOF`
  INTEGER( I4B ), INTENT( IN ) :: nodenum
  !! node number
  REAL( DFP ), INTENT( IN ) :: value
  !! `value` denotes the nodal values of dof `idof`.
  INTEGER( I4B ), INTENT( IN ) :: ivar
  INTEGER( I4B ), INTENT( IN ) :: idof
END SUBROUTINE dof_set10
END INTERFACE

INTERFACE set
  MODULE PROCEDURE dof_set10
END INTERFACE set

!----------------------------------------------------------------------------
!                                                             set@setMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 27 June 2021
! summary: Set values in a vector of real numbers
!
!# Introduction
!
! - This subroutine is designed to set the values in a array of real number.
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

INTERFACE
MODULE PURE SUBROUTINE dof_set11( vec, obj, nodenum, value, ivar, &
  & spacecompo, timecompo )
  REAL( DFP ), INTENT( INOUT ) :: vec( : )
  CLASS( DOF_ ), INTENT( IN ) :: obj
  !! object `obj` contains the storage pattern of degrees of freedom
  !! inside `vec`.
  !! This storage pattern can be `FMT_Nodes` or `FMT_DOF`
  INTEGER( I4B ), INTENT( IN ) :: nodenum
  !! node number
  REAL( DFP ), INTENT( IN ) :: value
  !! `value` denotes the nodal values of dof `idof`.

  INTEGER( I4B ), INTENT( IN ) :: ivar
  INTEGER( I4B ), INTENT( IN ) :: spacecompo
  INTEGER( I4B ), INTENT( IN ) :: timecompo
END SUBROUTINE dof_set11
END INTERFACE

INTERFACE set
  MODULE PROCEDURE dof_set11
END INTERFACE set

!----------------------------------------------------------------------------
!                                                             set@setMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 27 June 2021
! summary: Set values in a vector of real numbers
!
!# Introduction
!
! - This subroutine is designed to set the values in a array of real number.
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

INTERFACE
MODULE PURE SUBROUTINE dof_set12( vec, obj, nodenum, value, ivar, &
  & spacecompo, timecompo )
  REAL( DFP ), INTENT( INOUT ) :: vec( : )
  CLASS( DOF_ ), INTENT( IN ) :: obj
  !! object `obj` contains the storage pattern of degrees of freedom
  !! inside `vec`.
  !! This storage pattern can be `FMT_Nodes` or `FMT_DOF`
  INTEGER( I4B ), INTENT( IN ) :: nodenum
  !! node number
  REAL( DFP ), INTENT( IN ) :: value
  !! `value` denotes the nodal values of dof `idof`.

  INTEGER( I4B ), INTENT( IN ) :: ivar
  INTEGER( I4B ), INTENT( IN ) :: spacecompo
  INTEGER( I4B ), INTENT( IN ) :: timecompo(:)
END SUBROUTINE dof_set12
END INTERFACE

INTERFACE set
  MODULE PROCEDURE dof_set12
END INTERFACE set

!----------------------------------------------------------------------------
!                                                             set@setMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 27 June 2021
! summary: Set values in a vector of real numbers
!
!# Introduction
!
! - This subroutine is designed to set the values in a array of real number.
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

INTERFACE
MODULE PURE SUBROUTINE dof_set13( vec, obj, nodenum, value, ivar, &
  & spacecompo, timecompo )
  REAL( DFP ), INTENT( INOUT ) :: vec( : )
  CLASS( DOF_ ), INTENT( IN ) :: obj
  !! object `obj` contains the storage pattern of degrees of freedom
  !! inside `vec`.
  !! This storage pattern can be `FMT_Nodes` or `FMT_DOF`
  INTEGER( I4B ), INTENT( IN ) :: nodenum
  !! node number
  REAL( DFP ), INTENT( IN ) :: value
  !! `value` denotes the nodal values of dof `idof`.

  INTEGER( I4B ), INTENT( IN ) :: ivar
  INTEGER( I4B ), INTENT( IN ) :: spacecompo(:)
  INTEGER( I4B ), INTENT( IN ) :: timecompo
END SUBROUTINE dof_set13
END INTERFACE

INTERFACE set
  MODULE PROCEDURE dof_set13
END INTERFACE set
