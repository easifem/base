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

MODULE RealVector_SetMethods
USE GlobalData, ONLY: DFP, I4B
USE BaseType, ONLY: RealVector_, DOF_

IMPLICIT NONE
PRIVATE

PUBLIC :: Set

!----------------------------------------------------------------------------
!                                                                        Set
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 26 June 2021
! summary: Set all values to given scalar
!
!# Introduction
!
!@note
!   F77_Copy method from F77_Blas is called.
!@endnote

INTERFACE Set
  MODULE SUBROUTINE obj_Set1(obj, VALUE)
    TYPE(RealVector_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: VALUE
  END SUBROUTINE obj_Set1
END INTERFACE Set

!----------------------------------------------------------------------------
!                                                                         Set
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 26 June 2021
! summary: Set all values by given vector (obj=value)
!
!# Introduction
!
!@note
!   F95_Copy method from F95_Blas is called.
!@endnote

INTERFACE Set
  MODULE SUBROUTINE obj_Set2(obj, VALUE)
    TYPE(RealVector_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: VALUE(:)
    !! the length of the vector must be equal to the length of the object
  END SUBROUTINE obj_Set2
END INTERFACE Set

!----------------------------------------------------------------------------
!                                                                        Set
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 5 Jan 2022
! summary: set selected values (obj(nodenum)=VALUE)

INTERFACE Set
  MODULE SUBROUTINE obj_Set3(obj, nodenum, VALUE)
    TYPE(RealVector_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: nodenum
    !! node number to set the value
    REAL(DFP), INTENT(IN) :: VALUE
    !! scalar value
  END SUBROUTINE obj_Set3
END INTERFACE Set

!----------------------------------------------------------------------------
!                                                                         Set
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 27 June 2021
! summary: See [[DOF_Method::dof_Set2]]

INTERFACE Set
  MODULE PURE SUBROUTINE obj_Set4(obj, nodenum, VALUE)
    TYPE(Realvector_), INTENT(INOUT) :: obj
    !! obj(nodenum)=VALUE
    INTEGER(I4B), INTENT(IN) :: nodenum(:)
    !! node number to set the value
    REAL(DFP), INTENT(IN) :: VALUE
    !! scalar value
  END SUBROUTINE obj_Set4
END INTERFACE Set

!----------------------------------------------------------------------------
!                                                                        Set
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 5 Jan 2022
! summary: set selected values

INTERFACE Set
  MODULE SUBROUTINE obj_Set5(obj, nodenum, VALUE)
    TYPE(RealVector_), INTENT(INOUT) :: obj
    !! obj(nodenum)=VALUE
    INTEGER(I4B), INTENT(IN) :: nodenum(:)
    !! node number to set the value
    REAL(DFP), INTENT(IN) :: VALUE(:)
    !! vector value, the size of value should be equal to tdof * size(nodenum)
  END SUBROUTINE obj_Set5
END INTERFACE Set

!----------------------------------------------------------------------------
!                                                                          Set
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 26 June 2021
! summary: Set range of values to a scalar

INTERFACE Set
  MODULE SUBROUTINE obj_Set6(obj, istart, iend, stride, VALUE)
    TYPE(RealVector_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: istart, iend, stride
    !! range of values to set
    REAL(DFP), INTENT(IN) :: VALUE
    !! Scalar value
  END SUBROUTINE obj_Set6
END INTERFACE Set

!----------------------------------------------------------------------------
!                                                                         Set
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 26 June 2021
! summary: Set range of values to a vector

INTERFACE Set
  MODULE SUBROUTINE obj_Set7(obj, istart, iend, stride, VALUE)
    TYPE(RealVector_), INTENT(INOUT) :: obj
    !! ob(istart:iend:stride)=VALUE
    INTEGER(I4B), INTENT(IN) :: istart, iend, stride
    !! range of values to set
    REAL(DFP), INTENT(IN) :: VALUE(:)
    !! vector value
  END SUBROUTINE obj_Set7
END INTERFACE Set

!----------------------------------------------------------------------------
!                                                                         Set
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 26 June 2021
! summary: See [[DOF_Method::dof_Set1]]

INTERFACE Set
  MODULE PURE SUBROUTINE obj_Set8(obj, dofobj, nodenum, VALUE, conversion)
    TYPE(Realvector_), INTENT(INOUT) :: obj
    !! obj(nodenum)=VALUE
    TYPE(DOF_), INTENT(IN) :: dofobj
    !! degree of freedom object
    INTEGER(I4B), INTENT(IN) :: nodenum(:)
    !! node number to set the value
    REAL(DFP), INTENT(IN) :: VALUE(:)
    !! vector value
    INTEGER(I4B), INTENT(IN) :: conversion(1)
    !! conversion factor, NodesToDOF, DOFToNodes
  END SUBROUTINE obj_Set8
END INTERFACE Set

!----------------------------------------------------------------------------
!                                                                         Set
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 26 June 2021
! summary: See [[DOF_Method::dof_Set1]]

INTERFACE Set
  MODULE PURE SUBROUTINE obj_Set9(obj, dofobj, nodenum, VALUE)
    TYPE(Realvector_), INTENT(INOUT) :: obj
    !! obj(nodenum)=VALUE
    TYPE(DOF_), INTENT(IN) :: dofobj
    !! degree of freedom object
    INTEGER(I4B), INTENT(IN) :: nodenum(:)
    !! node number to set the value
    REAL(DFP), INTENT(IN) :: VALUE
    !! scalar value
  END SUBROUTINE obj_Set9
END INTERFACE Set

!----------------------------------------------------------------------------
!                                                                         Set
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 27 June 2021
! summary: See [[DOF_Method::dof_Set2]]

INTERFACE Set
  MODULE PURE SUBROUTINE obj_Set10(obj, dofobj, nodenum, VALUE, idof)
    TYPE(Realvector_), INTENT(INOUT) :: obj
    !! obj(nodenum)=VALUE
    TYPE(DOF_), INTENT(IN) :: dofobj
    !! degree of freedom object
    INTEGER(I4B), INTENT(IN) :: nodenum(:)
    !! node number to set the value
    REAL(DFP), INTENT(IN) :: VALUE(:)
    !! vector value,
    !! the size of value should be equal to size(nodenum)
    INTEGER(I4B), INTENT(IN) :: idof
    !! global degree of freedom number
  END SUBROUTINE obj_Set10
END INTERFACE Set

!----------------------------------------------------------------------------
!                                                                         Set
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 27 June 2021
! summary: See [[DOF_Method::dof_Set2]]

INTERFACE Set
  MODULE PURE SUBROUTINE obj_Set11(obj, dofobj, nodenum, VALUE, idof)
    TYPE(Realvector_), INTENT(INOUT) :: obj
    !! obj(nodenum)=VALUE
    TYPE(DOF_), INTENT(IN) :: dofobj
    !! degree of freedom object
    INTEGER(I4B), INTENT(IN) :: nodenum(:)
    !! node number to set the value
    REAL(DFP), INTENT(IN) :: VALUE
    !! scalar value
    INTEGER(I4B), INTENT(IN) :: idof
    !! global degree of freedom number
  END SUBROUTINE obj_Set11
END INTERFACE Set

!----------------------------------------------------------------------------
!                                                                         Set
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 27 June 2021
! summary: See [[DOF_Method::dof_Set2]]

INTERFACE Set
  MODULE PURE SUBROUTINE obj_Set12(obj, dofobj, nodenum, VALUE, ivar, idof)
    TYPE(Realvector_), INTENT(INOUT) :: obj
    !! obj(nodenum)=VALUE
    TYPE(DOF_), INTENT(IN) :: dofobj
    !! degree of freedom object
    INTEGER(I4B), INTENT(IN) :: nodenum(:)
    !! node number to set the value
    REAL(DFP), INTENT(IN) :: VALUE(:)
    !! vector value
    !! the size of value should be equal to size(nodenum)
    INTEGER(I4B), INTENT(IN) :: ivar
    !! physical variable number
    INTEGER(I4B), INTENT(IN) :: idof
    !! local degree of freedom number in physical variable
  END SUBROUTINE obj_Set12
END INTERFACE Set

!----------------------------------------------------------------------------
!                                                                         Set
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 27 June 2021
! summary: See [[DOF_Method::dof_Set2]]

INTERFACE Set
  MODULE PURE SUBROUTINE obj_Set13(obj, dofobj, nodenum, VALUE, ivar, idof)
    TYPE(Realvector_), INTENT(INOUT) :: obj
    !! obj(nodenum)=VALUE
    TYPE(DOF_), INTENT(IN) :: dofobj
    !! degree of freedom object
    INTEGER(I4B), INTENT(IN) :: nodenum(:)
    !! node number to set the value
    REAL(DFP), INTENT(IN) :: VALUE
    !! scalar value
    INTEGER(I4B), INTENT(IN) :: ivar
    !! physical variable number
    INTEGER(I4B), INTENT(IN) :: idof
    !! local degree of freedom number in physical variable
  END SUBROUTINE obj_Set13
END INTERFACE Set

!----------------------------------------------------------------------------
!                                                                         Set
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 27 June 2021
! summary: See [[DOF_Method::dof_Set2]]

INTERFACE Set
  MODULE PURE SUBROUTINE obj_Set14(obj, dofobj, nodenum, VALUE, ivar, &
                                   spacecompo, timecompo)
    TYPE(Realvector_), INTENT(INOUT) :: obj
    TYPE(DOF_), INTENT(IN) :: dofobj
    !! degree of freedom object
    INTEGER(I4B), INTENT(IN) :: nodenum(:)
    !! node number to set the value
    REAL(DFP), INTENT(IN) :: VALUE(:)
    !! vector value
    !! the size of value should be equal to size(nodenum)
    INTEGER(I4B), INTENT(IN) :: ivar
    !! physical variable number
    INTEGER(I4B), INTENT(IN) :: spacecompo
    !! space component number
    INTEGER(I4B), INTENT(IN) :: timecompo
    !! time component number
  END SUBROUTINE obj_Set14
END INTERFACE Set

!----------------------------------------------------------------------------
!                                                                         Set
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 27 June 2021
! summary: See [[DOF_Method::dof_Set2]]

INTERFACE Set
  MODULE PURE SUBROUTINE obj_Set15(obj, dofobj, nodenum, VALUE, ivar, &
                                   spacecompo, timecompo)
    TYPE(Realvector_), INTENT(INOUT) :: obj
    TYPE(DOF_), INTENT(IN) :: dofobj
    !! degree of freedom object
    INTEGER(I4B), INTENT(IN) :: nodenum(:)
    !! node number to set the value
    REAL(DFP), INTENT(IN) :: VALUE
    !! scalar value
    INTEGER(I4B), INTENT(IN) :: ivar
    !! physical variable number
    INTEGER(I4B), INTENT(IN) :: spacecompo
    !! space component number
    INTEGER(I4B), INTENT(IN) :: timecompo
    !! time component number
  END SUBROUTINE obj_Set15
END INTERFACE Set

!----------------------------------------------------------------------------
!                                                                         Set
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 27 June 2021
! summary: See [[DOF_Method::dof_Set2]]

INTERFACE Set
  MODULE PURE SUBROUTINE obj_Set16(obj, dofobj, nodenum, VALUE, ivar, &
                                   spacecompo, timecompo)
    TYPE(Realvector_), INTENT(INOUT) :: obj
    TYPE(DOF_), INTENT(IN) :: dofobj
    !! degree of freedom object
    INTEGER(I4B), INTENT(IN) :: nodenum(:)
    !! node number to set the value
    REAL(DFP), INTENT(IN) :: VALUE(:)
    !! vector value
    !! the size of value should be equal to size(nodenum)*size(timecompo)
    INTEGER(I4B), INTENT(IN) :: ivar
    !! physical variable number
    INTEGER(I4B), INTENT(IN) :: spacecompo
    !! space component number
    INTEGER(I4B), INTENT(IN) :: timecompo(:)
    !! time component number
  END SUBROUTINE obj_Set16
END INTERFACE Set

!----------------------------------------------------------------------------
!                                                                         Set
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 27 June 2021
! summary: See [[DOF_Method::dof_Set2]]

INTERFACE Set
  MODULE PURE SUBROUTINE obj_Set17(obj, dofobj, nodenum, VALUE, ivar, &
                                   spacecompo, timecompo)
    TYPE(Realvector_), INTENT(INOUT) :: obj
    TYPE(DOF_), INTENT(IN) :: dofobj
    !! degree of freedom object
    INTEGER(I4B), INTENT(IN) :: nodenum(:)
    !! node number to set the value
    REAL(DFP), INTENT(IN) :: VALUE
    !! scalar value
    INTEGER(I4B), INTENT(IN) :: ivar
    !! physical variable number
    INTEGER(I4B), INTENT(IN) :: spacecompo
    !! space component number
    INTEGER(I4B), INTENT(IN) :: timecompo(:)
    !! time component number
  END SUBROUTINE obj_Set17
END INTERFACE Set

!----------------------------------------------------------------------------
!                                                                         Set
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 27 June 2021
! summary: See [[DOF_Method::dof_Set2]]

INTERFACE Set
  MODULE PURE SUBROUTINE obj_Set18(obj, dofobj, nodenum, VALUE, ivar, &
                                   spacecompo, timecompo)
    TYPE(Realvector_), INTENT(INOUT) :: obj
    TYPE(DOF_), INTENT(IN) :: dofobj
    !! degree of freedom object
    INTEGER(I4B), INTENT(IN) :: nodenum(:)
    !! node number to set the value
    REAL(DFP), INTENT(IN) :: VALUE(:)
    !! vector value
    !! the size of value should be equal to size(nodenum)*size(spacecompo)
    INTEGER(I4B), INTENT(IN) :: ivar
    !! physical variable number
    INTEGER(I4B), INTENT(IN) :: spacecompo(:)
    !! space component number of physical variable
    INTEGER(I4B), INTENT(IN) :: timecompo
    !! time component number of physical variable
  END SUBROUTINE obj_Set18
END INTERFACE Set

!----------------------------------------------------------------------------
!                                                                         Set
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 27 June 2021
! summary: See [[DOF_Method::dof_Set2]]

INTERFACE Set
  MODULE PURE SUBROUTINE obj_Set19(obj, dofobj, nodenum, VALUE, ivar, &
                                   spacecompo, timecompo)
    TYPE(Realvector_), INTENT(INOUT) :: obj
    TYPE(DOF_), INTENT(IN) :: dofobj
    !! degree of freedom object
    INTEGER(I4B), INTENT(IN) :: nodenum(:)
    !! node number to set the value
    REAL(DFP), INTENT(IN) :: VALUE
    !! scalar value
    INTEGER(I4B), INTENT(IN) :: ivar
    !! physical variable number
    INTEGER(I4B), INTENT(IN) :: spacecompo(:)
    !! space component number of physical variable
    INTEGER(I4B), INTENT(IN) :: timecompo
    !! time component number of physical variable
  END SUBROUTINE obj_Set19
END INTERFACE Set

!----------------------------------------------------------------------------
!                                                                         Set
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 26 June 2021
! summary: See [[DOF_Method::dof_Set1]]

INTERFACE Set
  MODULE PURE SUBROUTINE obj_Set20(obj, dofobj, nodenum, VALUE)
    TYPE(Realvector_), INTENT(INOUT) :: obj
    TYPE(DOF_), INTENT(IN) :: dofobj
    !! degree of freedom object
    INTEGER(I4B), INTENT(IN) :: nodenum
    !! node number to set the value
    REAL(DFP), INTENT(IN) :: VALUE
    !! scalar value
  END SUBROUTINE obj_Set20
END INTERFACE Set

!----------------------------------------------------------------------------
!                                                                         Set
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 26 June 2021
! summary: See [[DOF_Method::dof_Set1]]

INTERFACE Set
  MODULE PURE SUBROUTINE obj_Set21(obj, dofobj, nodenum, VALUE, idof)
    TYPE(Realvector_), INTENT(INOUT) :: obj
    TYPE(DOF_), INTENT(IN) :: dofobj
    !! degree of freedom object
    INTEGER(I4B), INTENT(IN) :: nodenum
    !! node number to set the value
    REAL(DFP), INTENT(IN) :: VALUE
    !! scalar value
    INTEGER(I4B), INTENT(IN) :: idof
    !! global degree of freedom number
  END SUBROUTINE obj_Set21
END INTERFACE Set

!----------------------------------------------------------------------------
!                                                                         Set
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 26 June 2021
! summary: See [[DOF_Method::dof_Set1]]

INTERFACE Set
  MODULE PURE SUBROUTINE obj_Set22(obj, dofobj, nodenum, VALUE, ivar, idof)
    TYPE(Realvector_), INTENT(INOUT) :: obj
    TYPE(DOF_), INTENT(IN) :: dofobj
    !! degree of freedom object
    INTEGER(I4B), INTENT(IN) :: nodenum
    !! node number to set the value
    REAL(DFP), INTENT(IN) :: VALUE
    !! scalar value
    INTEGER(I4B), INTENT(IN) :: ivar
    !! physical variable number
    INTEGER(I4B), INTENT(IN) :: idof
    !! local degree of freedom number in physical variable
  END SUBROUTINE obj_Set22
END INTERFACE Set

!----------------------------------------------------------------------------
!                                                                         Set
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 26 June 2021
! summary: See [[DOF_Method::dof_Set1]]

INTERFACE Set
  MODULE PURE SUBROUTINE obj_Set23(obj, dofobj, nodenum, VALUE, ivar, &
                                   spacecompo, timecompo)
    TYPE(Realvector_), INTENT(INOUT) :: obj
    TYPE(DOF_), INTENT(IN) :: dofobj
    !! degree of freedom object
    INTEGER(I4B), INTENT(IN) :: nodenum
    !! node number to set the value
    REAL(DFP), INTENT(IN) :: VALUE
    !! scalar value
    INTEGER(I4B), INTENT(IN) :: ivar
    !! physical variable number
    INTEGER(I4B), INTENT(IN) :: spacecompo
    !! space component number
    INTEGER(I4B), INTENT(IN) :: timecompo
    !! time component number
  END SUBROUTINE obj_Set23
END INTERFACE Set

!----------------------------------------------------------------------------
!                                                                         Set
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 26 June 2021
! summary: See [[DOF_Method::dof_Set1]]

INTERFACE Set
  MODULE PURE SUBROUTINE obj_Set24(obj, dofobj, nodenum, VALUE, ivar, &
                                   spacecompo, timecompo)
    TYPE(Realvector_), INTENT(INOUT) :: obj
    TYPE(DOF_), INTENT(IN) :: dofobj
    !! degree of freedom object
    INTEGER(I4B), INTENT(IN) :: nodenum
    !! node number to set the value
    REAL(DFP), INTENT(IN) :: VALUE
    !! scalar value
    INTEGER(I4B), INTENT(IN) :: ivar
    !! physical variable number
    INTEGER(I4B), INTENT(IN) :: spacecompo
    !! space component number
    INTEGER(I4B), INTENT(IN) :: timecompo(:)
    !! time component number
  END SUBROUTINE obj_Set24
END INTERFACE Set

!----------------------------------------------------------------------------
!                                                                         Set
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 26 June 2021
! summary: See [[DOF_Method::dof_Set1]]

INTERFACE Set
  MODULE PURE SUBROUTINE obj_Set25(obj, dofobj, nodenum, VALUE, ivar, &
                                   spacecompo, timecompo)
    TYPE(Realvector_), INTENT(INOUT) :: obj
    TYPE(DOF_), INTENT(IN) :: dofobj
    !! degree of freedom object
    INTEGER(I4B), INTENT(IN) :: nodenum
    !! node number to set the value
    REAL(DFP), INTENT(IN) :: VALUE
    !! scalar value
    INTEGER(I4B), INTENT(IN) :: ivar
    !! physical variable number
    INTEGER(I4B), INTENT(IN) :: spacecompo(:)
    !! space component number
    INTEGER(I4B), INTENT(IN) :: timecompo
    !! time component number
  END SUBROUTINE obj_Set25
END INTERFACE Set

!----------------------------------------------------------------------------
!                                                                        Set
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 9 June 2022
! summary: obj1=obj2

INTERFACE Set
  MODULE PURE SUBROUTINE obj_Set26(obj, VALUE)
    TYPE(RealVector_), INTENT(INOUT) :: obj
    TYPE(RealVector_), INTENT(IN) :: VALUE
  END SUBROUTINE obj_Set26
END INTERFACE Set

!----------------------------------------------------------------------------
!                                                                        Set
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-05-29
! summary: obj = Value
!
!# Introduction
!
! Value contains the nodal values of all dofs
! Number of cols in values should be at least equal to the total dof in obj
! Number of rows in values should be at least equal to the total nodes in obj

INTERFACE Set
  MODULE SUBROUTINE obj_Set27(obj, dofobj, VALUE)
    TYPE(RealVector_), INTENT(INOUT) :: obj
    !! real vector
    TYPE(DOF_), INTENT(IN) :: dofobj
    !! degree of freedom object
    REAL(DFP), INTENT(IN) :: VALUE(:, :)
    !! number of cols should be equal to the total dof in obj
    !! number of rows should be equal to the total nodes in obj
  END SUBROUTINE obj_Set27
END INTERFACE Set

!----------------------------------------------------------------------------
!                                                                       Set
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-05-29
! summary: obj = obj + scale*VALUE

INTERFACE Set
  MODULE SUBROUTINE obj_Set28(obj, dofobj, VALUE, idof)
    TYPE(RealVector_), INTENT(INOUT) :: obj
    !! real vector
    TYPE(DOF_), INTENT(IN) :: dofobj
    !! degree of freedom object
    REAL(DFP), INTENT(IN) :: VALUE(:)
    !! number of cols should be equal to the total dof in obj
    !! number of rows should be equal to the total nodes in obj
    INTEGER(I4B), INTENT(IN) :: idof
    !! global degree of freedom in dofobj
  END SUBROUTINE obj_Set28
END INTERFACE Set

!----------------------------------------------------------------------------
!                                                                       Set
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-05-29
! summary: obj = obj + scale*VALUE

INTERFACE Set
  MODULE SUBROUTINE obj_Set29(obj1, dofobj1, idof1, obj2, dofobj2, idof2)
    TYPE(RealVector_), INTENT(INOUT) :: obj1
    !! real vector
    TYPE(DOF_), INTENT(IN) :: dofobj1
    !! degree of freedom object
    INTEGER(I4B), INTENT(IN) :: idof1
    !! global degree of freedom in dof1
    TYPE(RealVector_), INTENT(IN) :: obj2
    !! real vector
    TYPE(DOF_), INTENT(IN) :: dofobj2
    !! degree of freedom object
    INTEGER(I4B), INTENT(IN) :: idof2
    !! global degree of freedom in dof2
  END SUBROUTINE obj_Set29
END INTERFACE Set

END MODULE RealVector_SetMethods
