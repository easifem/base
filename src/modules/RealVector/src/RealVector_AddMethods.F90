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

MODULE RealVector_AddMethods
USE GlobalData, ONLY: DFP, I4B
USE BaseType, ONLY: RealVector_, DOF_

IMPLICIT NONE
PRIVATE

PUBLIC :: Add

!----------------------------------------------------------------------------
!                                                            set@SetMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 26 June 2021
! summary: Add all values to given scalar
!
!# Introduction
!
!@note
!   We call F77_AXPY in this method
!@endnote

INTERFACE Add
  MODULE SUBROUTINE obj_add1(obj, VALUE, scale)
    CLASS(RealVector_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: VALUE
    REAL(DFP), INTENT(IN) :: scale
  END SUBROUTINE obj_add1
END INTERFACE Add

!----------------------------------------------------------------------------
!                                                             set@SetMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 26 June 2021
! summary: Add all values by given vector
!
!@note
!   We call F95_AXPY in this method
!@endnote

INTERFACE Add
  MODULE SUBROUTINE obj_add2(obj, VALUE, scale)
    CLASS(RealVector_), INTENT(INOUT) :: obj
    !! obj = obj + scale*VALUE
    REAL(DFP), INTENT(IN) :: VALUE(:)
    !! Size of value should be equal to the size of obj
    REAL(DFP), INTENT(IN) :: scale
    !! scale
  END SUBROUTINE obj_add2
END INTERFACE Add

!----------------------------------------------------------------------------
!                                                            add@AddMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 5 Jan 2022
! summary: set selected values

INTERFACE Add
  MODULE SUBROUTINE obj_add3(obj, nodenum, VALUE, scale)
    CLASS(RealVector_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: nodenum
    REAL(DFP), INTENT(IN) :: VALUE
    REAL(DFP), INTENT(IN) :: scale
  END SUBROUTINE obj_add3
END INTERFACE Add

!----------------------------------------------------------------------------
!                                                             set@setMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 27 June 2021
! summary: See [[DOF_Method::dof_set2]]

INTERFACE Add
  MODULE PURE SUBROUTINE obj_add4(obj, nodenum, VALUE, scale)
    TYPE(Realvector_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: nodenum(:)
    REAL(DFP), INTENT(IN) :: VALUE
    REAL(DFP), INTENT(IN) :: scale
  END SUBROUTINE obj_add4
END INTERFACE Add

!----------------------------------------------------------------------------
!                                                            set@SetMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 5 Jan 2022
! summary: set selected values

INTERFACE Add
  MODULE SUBROUTINE obj_add5(obj, nodenum, VALUE, scale)
    CLASS(RealVector_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: nodenum(:)
    REAL(DFP), INTENT(IN) :: VALUE(:)
    REAL(DFP), INTENT(IN) :: scale
  END SUBROUTINE obj_add5
END INTERFACE Add

!----------------------------------------------------------------------------
!                                                              set@SetMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 26 June 2021
! summary: Add range of values to a scalar
!
!@note
!   We call F77_AXPY in this method
!@endnote

INTERFACE Add
  MODULE SUBROUTINE obj_add6(obj, istart, iend, stride, VALUE, scale)
    CLASS(RealVector_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: istart, iend, stride
    !! range of values to be added
    REAL(DFP), INTENT(IN) :: VALUE
    !! scalar value
    REAL(DFP), INTENT(IN) :: scale
    !! scale
  END SUBROUTINE obj_add6
END INTERFACE Add

!----------------------------------------------------------------------------
!                                                             set@SetMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 26 June 2021
! summary: Add range of values to a vector
!
!@note!
! We call F77_AXPY
!@endnote

INTERFACE Add
  MODULE SUBROUTINE obj_add7(obj, istart, iend, stride, VALUE, scale)
    CLASS(RealVector_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: istart, iend, stride
    REAL(DFP), INTENT(IN) :: VALUE(:)
    REAL(DFP), INTENT(IN) :: scale
  END SUBROUTINE obj_add7
END INTERFACE Add

!----------------------------------------------------------------------------
!                                                             set@setMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 26 June 2021
! summary: See [[DOF_Method::dof_set1]]

INTERFACE Add
  MODULE PURE SUBROUTINE obj_add8(obj, dofobj, nodenum, VALUE, &
                                  scale, conversion)
    TYPE(Realvector_), INTENT(INOUT) :: obj
    CLASS(DOF_), INTENT(IN) :: dofobj
    INTEGER(I4B), INTENT(IN) :: nodenum(:)
    REAL(DFP), INTENT(IN) :: VALUE(:)
    REAL(DFP), INTENT(IN) :: scale
    INTEGER(I4B), INTENT(IN) :: conversion(1)
  END SUBROUTINE obj_add8
END INTERFACE Add

!----------------------------------------------------------------------------
!                                                             set@setMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 26 June 2021
! summary: See [[DOF_Method::dof_set1]]

INTERFACE Add
  MODULE PURE SUBROUTINE obj_add9(obj, dofobj, nodenum, VALUE, &
                                  scale)
    TYPE(Realvector_), INTENT(INOUT) :: obj
    CLASS(DOF_), INTENT(IN) :: dofobj
    INTEGER(I4B), INTENT(IN) :: nodenum(:)
    REAL(DFP), INTENT(IN) :: VALUE
    REAL(DFP), INTENT(IN) :: scale
  END SUBROUTINE obj_add9
END INTERFACE Add

!----------------------------------------------------------------------------
!                                                             set@setMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 27 June 2021
! summary: See [[DOF_Method::dof_set2]]

INTERFACE Add
  MODULE PURE SUBROUTINE obj_add10(obj, dofobj, nodenum, VALUE, &
                                   scale, idof)
    TYPE(Realvector_), INTENT(INOUT) :: obj
    CLASS(DOF_), INTENT(IN) :: dofobj
    INTEGER(I4B), INTENT(IN) :: nodenum(:)
    REAL(DFP), INTENT(IN) :: VALUE(:)
    REAL(DFP), INTENT(IN) :: scale
    INTEGER(I4B), INTENT(IN) :: idof
  END SUBROUTINE obj_add10
END INTERFACE Add

!----------------------------------------------------------------------------
!                                                             set@setMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 27 June 2021
! summary: See [[DOF_Method::dof_set2]]

INTERFACE Add
  MODULE PURE SUBROUTINE obj_add11(obj, dofobj, nodenum, VALUE, &
                                   scale, idof)
    TYPE(Realvector_), INTENT(INOUT) :: obj
    CLASS(DOF_), INTENT(IN) :: dofobj
    INTEGER(I4B), INTENT(IN) :: nodenum(:)
    REAL(DFP), INTENT(IN) :: VALUE
    REAL(DFP), INTENT(IN) :: scale
    INTEGER(I4B), INTENT(IN) :: idof
  END SUBROUTINE obj_add11
END INTERFACE Add

!----------------------------------------------------------------------------
!                                                             set@setMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 27 June 2021
! summary: See [[DOF_Method::dof_set2]]

INTERFACE Add
  MODULE PURE SUBROUTINE obj_add12(obj, dofobj, nodenum, VALUE, &
                                   scale, ivar, idof)
    TYPE(Realvector_), INTENT(INOUT) :: obj
    CLASS(DOF_), INTENT(IN) :: dofobj
    INTEGER(I4B), INTENT(IN) :: nodenum(:)
    REAL(DFP), INTENT(IN) :: VALUE(:)
    REAL(DFP), INTENT(IN) :: scale
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: idof
  END SUBROUTINE obj_add12
END INTERFACE Add

!----------------------------------------------------------------------------
!                                                             set@setMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 27 June 2021
! summary: See [[DOF_Method::dof_set2]]

INTERFACE Add
  MODULE PURE SUBROUTINE obj_add13(obj, dofobj, nodenum, VALUE, &
                                   scale, ivar, idof)
    TYPE(Realvector_), INTENT(INOUT) :: obj
    CLASS(DOF_), INTENT(IN) :: dofobj
    INTEGER(I4B), INTENT(IN) :: nodenum(:)
    REAL(DFP), INTENT(IN) :: VALUE
    REAL(DFP), INTENT(IN) :: scale
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: idof
  END SUBROUTINE obj_add13
END INTERFACE Add

!----------------------------------------------------------------------------
!                                                             set@setMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 27 June 2021
! summary: See [[DOF_Method::dof_set2]]

INTERFACE Add
  MODULE PURE SUBROUTINE obj_add14(obj, dofobj, nodenum, VALUE, &
                                   scale, ivar, spacecompo, timecompo)
    TYPE(Realvector_), INTENT(INOUT) :: obj
    CLASS(DOF_), INTENT(IN) :: dofobj
    INTEGER(I4B), INTENT(IN) :: nodenum(:)
    REAL(DFP), INTENT(IN) :: VALUE(:)
    REAL(DFP), INTENT(IN) :: scale
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: spacecompo
    INTEGER(I4B), INTENT(IN) :: timecompo
  END SUBROUTINE obj_add14
END INTERFACE Add

!----------------------------------------------------------------------------
!                                                             set@setMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 27 June 2021
! summary: See [[DOF_Method::dof_set2]]

INTERFACE Add
  MODULE PURE SUBROUTINE obj_add15(obj, dofobj, nodenum, VALUE, &
                                   scale, ivar, spacecompo, timecompo)
    TYPE(Realvector_), INTENT(INOUT) :: obj
    CLASS(DOF_), INTENT(IN) :: dofobj
    INTEGER(I4B), INTENT(IN) :: nodenum(:)
    REAL(DFP), INTENT(IN) :: VALUE
    REAL(DFP), INTENT(IN) :: scale
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: spacecompo
    INTEGER(I4B), INTENT(IN) :: timecompo
  END SUBROUTINE obj_add15
END INTERFACE Add

!----------------------------------------------------------------------------
!                                                             set@setMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 27 June 2021
! summary: See [[DOF_Method::dof_set2]]

INTERFACE Add
  MODULE PURE SUBROUTINE obj_add16(obj, dofobj, nodenum, VALUE, &
                                   scale, ivar, spacecompo, timecompo)
    TYPE(Realvector_), INTENT(INOUT) :: obj
    CLASS(DOF_), INTENT(IN) :: dofobj
    INTEGER(I4B), INTENT(IN) :: nodenum(:)
    REAL(DFP), INTENT(IN) :: VALUE(:)
    REAL(DFP), INTENT(IN) :: scale
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: spacecompo
    INTEGER(I4B), INTENT(IN) :: timecompo(:)
  END SUBROUTINE obj_add16
END INTERFACE Add

!----------------------------------------------------------------------------
!                                                             set@setMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 27 June 2021
! summary: See [[DOF_Method::dof_set2]]

INTERFACE Add
  MODULE PURE SUBROUTINE obj_add17(obj, dofobj, nodenum, VALUE, &
                                   scale, ivar, spacecompo, timecompo)
    TYPE(Realvector_), INTENT(INOUT) :: obj
    CLASS(DOF_), INTENT(IN) :: dofobj
    INTEGER(I4B), INTENT(IN) :: nodenum(:)
    REAL(DFP), INTENT(IN) :: VALUE
    REAL(DFP), INTENT(IN) :: scale
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: spacecompo
    INTEGER(I4B), INTENT(IN) :: timecompo(:)
  END SUBROUTINE obj_add17
END INTERFACE Add

!----------------------------------------------------------------------------
!                                                             set@setMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 27 June 2021
! summary: See [[DOF_Method::dof_set2]]

INTERFACE Add
  MODULE PURE SUBROUTINE obj_add18(obj, dofobj, nodenum, VALUE, &
                                   scale, ivar, spacecompo, timecompo)
    TYPE(Realvector_), INTENT(INOUT) :: obj
    CLASS(DOF_), INTENT(IN) :: dofobj
    INTEGER(I4B), INTENT(IN) :: nodenum(:)
    REAL(DFP), INTENT(IN) :: VALUE(:)
    REAL(DFP), INTENT(IN) :: scale
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: spacecompo(:)
    INTEGER(I4B), INTENT(IN) :: timecompo
  END SUBROUTINE obj_add18
END INTERFACE Add

!----------------------------------------------------------------------------
!                                                             set@setMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 27 June 2021
! summary: See [[DOF_Method::dof_set2]]

INTERFACE Add
  MODULE PURE SUBROUTINE obj_add19(obj, dofobj, nodenum, VALUE, &
                                   scale, ivar, spacecompo, timecompo)
    TYPE(Realvector_), INTENT(INOUT) :: obj
    CLASS(DOF_), INTENT(IN) :: dofobj
    INTEGER(I4B), INTENT(IN) :: nodenum(:)
    REAL(DFP), INTENT(IN) :: VALUE
    REAL(DFP), INTENT(IN) :: scale
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: spacecompo(:)
    INTEGER(I4B), INTENT(IN) :: timecompo
  END SUBROUTINE obj_add19
END INTERFACE Add

!----------------------------------------------------------------------------
!                                                             set@setMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 26 June 2021
! summary: See [[DOF_Method::dof_set1]]

INTERFACE Add
  MODULE PURE SUBROUTINE obj_add20(obj, dofobj, nodenum, VALUE, &
                                   scale)
    TYPE(Realvector_), INTENT(INOUT) :: obj
    CLASS(DOF_), INTENT(IN) :: dofobj
    INTEGER(I4B), INTENT(IN) :: nodenum
    REAL(DFP), INTENT(IN) :: VALUE
    REAL(DFP), INTENT(IN) :: scale
  END SUBROUTINE obj_add20
END INTERFACE Add

!----------------------------------------------------------------------------
!                                                             set@setMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 26 June 2021
! summary: See [[DOF_Method::dof_set1]]

INTERFACE Add
  MODULE PURE SUBROUTINE obj_add21(obj, dofobj, nodenum, VALUE, &
                                   scale, idof)
    TYPE(Realvector_), INTENT(INOUT) :: obj
    CLASS(DOF_), INTENT(IN) :: dofobj
    INTEGER(I4B), INTENT(IN) :: nodenum
    REAL(DFP), INTENT(IN) :: VALUE
    REAL(DFP), INTENT(IN) :: scale
    INTEGER(I4B), INTENT(IN) :: idof
  END SUBROUTINE obj_add21
END INTERFACE Add

!----------------------------------------------------------------------------
!                                                             set@setMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 26 June 2021
! summary: See [[DOF_Method::dof_set1]]

INTERFACE Add
  MODULE PURE SUBROUTINE obj_add22(obj, dofobj, nodenum, VALUE, &
                                   scale, ivar, idof)
    TYPE(Realvector_), INTENT(INOUT) :: obj
    CLASS(DOF_), INTENT(IN) :: dofobj
    INTEGER(I4B), INTENT(IN) :: nodenum
    REAL(DFP), INTENT(IN) :: VALUE
    REAL(DFP), INTENT(IN) :: scale
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: idof
  END SUBROUTINE obj_add22
END INTERFACE Add

!----------------------------------------------------------------------------
!                                                             set@setMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 26 June 2021
! summary: See [[DOF_Method::dof_set1]]

INTERFACE Add
  MODULE PURE SUBROUTINE obj_add23(obj, dofobj, nodenum, VALUE, &
                                   scale, ivar, spacecompo, timecompo)
    TYPE(Realvector_), INTENT(INOUT) :: obj
    CLASS(DOF_), INTENT(IN) :: dofobj
    INTEGER(I4B), INTENT(IN) :: nodenum
    REAL(DFP), INTENT(IN) :: VALUE
    REAL(DFP), INTENT(IN) :: scale
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: spacecompo
    INTEGER(I4B), INTENT(IN) :: timecompo
  END SUBROUTINE obj_add23
END INTERFACE Add

!----------------------------------------------------------------------------
!                                                             set@setMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 26 June 2021
! summary: See [[DOF_Method::dof_set1]]

INTERFACE Add
  MODULE PURE SUBROUTINE obj_add24(obj, dofobj, nodenum, VALUE, &
                                   scale, ivar, spacecompo, timecompo)
    TYPE(Realvector_), INTENT(INOUT) :: obj
    CLASS(DOF_), INTENT(IN) :: dofobj
    INTEGER(I4B), INTENT(IN) :: nodenum
    REAL(DFP), INTENT(IN) :: VALUE
    REAL(DFP), INTENT(IN) :: scale
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: spacecompo
    INTEGER(I4B), INTENT(IN) :: timecompo(:)
  END SUBROUTINE obj_add24
END INTERFACE Add

!----------------------------------------------------------------------------
!                                                             set@setMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 26 June 2021
! summary: See [[DOF_Method::dof_set1]]

INTERFACE Add
  MODULE PURE SUBROUTINE obj_add25(obj, dofobj, nodenum, VALUE, &
                                   scale, ivar, spacecompo, timecompo)
    TYPE(Realvector_), INTENT(INOUT) :: obj
    CLASS(DOF_), INTENT(IN) :: dofobj
    INTEGER(I4B), INTENT(IN) :: nodenum
    REAL(DFP), INTENT(IN) :: VALUE
    REAL(DFP), INTENT(IN) :: scale
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: spacecompo(:)
    INTEGER(I4B), INTENT(IN) :: timecompo
  END SUBROUTINE obj_add25
END INTERFACE Add

!----------------------------------------------------------------------------
!                                                            add@addMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 9 June 2022
! summary: obj1=obj2

INTERFACE Add
  MODULE PURE SUBROUTINE obj_add26(obj, VALUE, scale)
    CLASS(RealVector_), INTENT(INOUT) :: obj
    CLASS(RealVector_), INTENT(IN) :: VALUE
    REAL(DFP), INTENT(IN) :: scale
  END SUBROUTINE obj_add26
END INTERFACE Add

!----------------------------------------------------------------------------
!                                                            set@SetMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-05-29
! summary: obj = obj + scale*VALUE
!
!# Introduction
!
! Value contains the nodal values of all dofs
! Number of rows in values should be at least equal to the total dof in obj
! Number of columns in values should be at least equal to the total nodes in obj

INTERFACE Add
  MODULE SUBROUTINE obj_add27(obj, dofobj, VALUE, scale)
    CLASS(RealVector_), INTENT(INOUT) :: obj
    !! real vector
    TYPE(DOF_), INTENT(IN) :: dofobj
    !! degree of freedom object
    REAL(DFP), INTENT(IN) :: VALUE(:, :)
    !! number of rows should be equal to the total dof in obj
    !! number of columns should be equal to the total nodes in obj
    REAL(DFP), INTENT(IN) :: scale
  END SUBROUTINE obj_add27
END INTERFACE Add

END MODULE RealVector_AddMethods
