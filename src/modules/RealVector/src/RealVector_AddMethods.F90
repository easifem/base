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
!                                                                        Add
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
  MODULE SUBROUTINE obj_Add1(obj, VALUE, scale)
    CLASS(RealVector_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: VALUE
    REAL(DFP), INTENT(IN) :: scale
  END SUBROUTINE obj_Add1
END INTERFACE Add

!----------------------------------------------------------------------------
!                                                                         Add
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 26 June 2021
! summary: Add all values by given vector
!
!@note
!   We call F95_AXPY in this method
!@endnote

INTERFACE Add
  MODULE SUBROUTINE obj_Add2(obj, VALUE, scale)
    CLASS(RealVector_), INTENT(INOUT) :: obj
    !! obj = obj + scale*VALUE
    REAL(DFP), INTENT(IN) :: VALUE(:)
    !! Size of value should be equal to the size of obj
    REAL(DFP), INTENT(IN) :: scale
    !! scale
  END SUBROUTINE obj_Add2
END INTERFACE Add

!----------------------------------------------------------------------------
!                                                                        add
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 5 Jan 2022
! summary: Add selected values

INTERFACE Add
  MODULE SUBROUTINE obj_Add3(obj, nodenum, VALUE, scale)
    CLASS(RealVector_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: nodenum
    REAL(DFP), INTENT(IN) :: VALUE
    REAL(DFP), INTENT(IN) :: scale
  END SUBROUTINE obj_Add3
END INTERFACE Add

!----------------------------------------------------------------------------
!                                                                         Add
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 27 June 2021
! summary: See [[DOF_Method::dof_Add2]]

INTERFACE Add
  MODULE SUBROUTINE obj_Add4(obj, nodenum, VALUE, scale)
    TYPE(Realvector_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: nodenum(:)
    REAL(DFP), INTENT(IN) :: VALUE
    REAL(DFP), INTENT(IN) :: scale
  END SUBROUTINE obj_Add4
END INTERFACE Add

!----------------------------------------------------------------------------
!                                                                        Add
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 5 Jan 2022
! summary: Add selected values

INTERFACE Add
  MODULE SUBROUTINE obj_Add5(obj, nodenum, VALUE, scale)
    CLASS(RealVector_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: nodenum(:)
    REAL(DFP), INTENT(IN) :: VALUE(:)
    REAL(DFP), INTENT(IN) :: scale
  END SUBROUTINE obj_Add5
END INTERFACE Add

!----------------------------------------------------------------------------
!                                                                          Add
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 26 June 2021
! summary: Add range of values to a scalar
!
!@note
!   We call F77_AXPY in this method
!@endnote

INTERFACE Add
  MODULE SUBROUTINE obj_Add6(obj, istart, iend, stride, VALUE, scale)
    CLASS(RealVector_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: istart, iend, stride
    !! range of values to be added
    REAL(DFP), INTENT(IN) :: VALUE
    !! scalar value
    REAL(DFP), INTENT(IN) :: scale
    !! scale
  END SUBROUTINE obj_Add6
END INTERFACE Add

!----------------------------------------------------------------------------
!                                                                         Add
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 26 June 2021
! summary: Add range of values to a vector
!
!@note!
! We call F77_AXPY
!@endnote

INTERFACE Add
  MODULE SUBROUTINE obj_Add7(obj, istart, iend, stride, VALUE, scale)
    CLASS(RealVector_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: istart, iend, stride
    REAL(DFP), INTENT(IN) :: VALUE(:)
    REAL(DFP), INTENT(IN) :: scale
  END SUBROUTINE obj_Add7
END INTERFACE Add

!----------------------------------------------------------------------------
!                                                                         Add
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 26 June 2021
! summary: See [[DOF_Method::dof_Add1]]

INTERFACE Add
  MODULE SUBROUTINE obj_Add8(obj, dofobj, nodenum, VALUE, &
                             scale, conversion)
    TYPE(Realvector_), INTENT(INOUT) :: obj
    CLASS(DOF_), INTENT(IN) :: dofobj
    INTEGER(I4B), INTENT(IN) :: nodenum(:)
    REAL(DFP), INTENT(IN) :: VALUE(:)
    REAL(DFP), INTENT(IN) :: scale
    INTEGER(I4B), INTENT(IN) :: conversion(1)
  END SUBROUTINE obj_Add8
END INTERFACE Add

!----------------------------------------------------------------------------
!                                                                         Add
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 26 June 2021
! summary: See [[DOF_Method::dof_Add1]]

INTERFACE Add
  MODULE SUBROUTINE obj_Add9(obj, dofobj, nodenum, VALUE, &
                             scale)
    TYPE(Realvector_), INTENT(INOUT) :: obj
    CLASS(DOF_), INTENT(IN) :: dofobj
    INTEGER(I4B), INTENT(IN) :: nodenum(:)
    REAL(DFP), INTENT(IN) :: VALUE
    REAL(DFP), INTENT(IN) :: scale
  END SUBROUTINE obj_Add9
END INTERFACE Add

!----------------------------------------------------------------------------
!                                                                         Add
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 27 June 2021
! summary: See [[DOF_Method::dof_Add2]]

INTERFACE Add
  MODULE SUBROUTINE obj_Add10(obj, dofobj, nodenum, VALUE, &
                              scale, idof)
    TYPE(Realvector_), INTENT(INOUT) :: obj
    CLASS(DOF_), INTENT(IN) :: dofobj
    INTEGER(I4B), INTENT(IN) :: nodenum(:)
    REAL(DFP), INTENT(IN) :: VALUE(:)
    REAL(DFP), INTENT(IN) :: scale
    INTEGER(I4B), INTENT(IN) :: idof
  END SUBROUTINE obj_Add10
END INTERFACE Add

!----------------------------------------------------------------------------
!                                                                         Add
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 27 June 2021
! summary: See [[DOF_Method::dof_Add2]]

INTERFACE Add
  MODULE SUBROUTINE obj_Add11(obj, dofobj, nodenum, VALUE, &
                              scale, idof)
    TYPE(Realvector_), INTENT(INOUT) :: obj
    CLASS(DOF_), INTENT(IN) :: dofobj
    INTEGER(I4B), INTENT(IN) :: nodenum(:)
    REAL(DFP), INTENT(IN) :: VALUE
    REAL(DFP), INTENT(IN) :: scale
    INTEGER(I4B), INTENT(IN) :: idof
  END SUBROUTINE obj_Add11
END INTERFACE Add

!----------------------------------------------------------------------------
!                                                                         Add
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 27 June 2021
! summary: See [[DOF_Method::dof_Add2]]

INTERFACE Add
  MODULE SUBROUTINE obj_Add12(obj, dofobj, nodenum, VALUE, &
                              scale, ivar, idof)
    TYPE(Realvector_), INTENT(INOUT) :: obj
    CLASS(DOF_), INTENT(IN) :: dofobj
    INTEGER(I4B), INTENT(IN) :: nodenum(:)
    REAL(DFP), INTENT(IN) :: VALUE(:)
    REAL(DFP), INTENT(IN) :: scale
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: idof
  END SUBROUTINE obj_Add12
END INTERFACE Add

!----------------------------------------------------------------------------
!                                                                         Add
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 27 June 2021
! summary: See [[DOF_Method::dof_Add2]]

INTERFACE Add
  MODULE SUBROUTINE obj_Add13(obj, dofobj, nodenum, VALUE, &
                              scale, ivar, idof)
    TYPE(Realvector_), INTENT(INOUT) :: obj
    CLASS(DOF_), INTENT(IN) :: dofobj
    INTEGER(I4B), INTENT(IN) :: nodenum(:)
    REAL(DFP), INTENT(IN) :: VALUE
    REAL(DFP), INTENT(IN) :: scale
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: idof
  END SUBROUTINE obj_Add13
END INTERFACE Add

!----------------------------------------------------------------------------
!                                                                         Add
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 27 June 2021
! summary: See [[DOF_Method::dof_Add2]]

INTERFACE Add
  MODULE SUBROUTINE obj_Add14(obj, dofobj, nodenum, VALUE, &
                              scale, ivar, spacecompo, timecompo)
    TYPE(Realvector_), INTENT(INOUT) :: obj
    CLASS(DOF_), INTENT(IN) :: dofobj
    INTEGER(I4B), INTENT(IN) :: nodenum(:)
    REAL(DFP), INTENT(IN) :: VALUE(:)
    REAL(DFP), INTENT(IN) :: scale
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: spacecompo
    INTEGER(I4B), INTENT(IN) :: timecompo
  END SUBROUTINE obj_Add14
END INTERFACE Add

!----------------------------------------------------------------------------
!                                                                         Add
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 27 June 2021
! summary: See [[DOF_Method::dof_Add2]]

INTERFACE Add
  MODULE SUBROUTINE obj_Add15(obj, dofobj, nodenum, VALUE, &
                              scale, ivar, spacecompo, timecompo)
    TYPE(Realvector_), INTENT(INOUT) :: obj
    CLASS(DOF_), INTENT(IN) :: dofobj
    INTEGER(I4B), INTENT(IN) :: nodenum(:)
    REAL(DFP), INTENT(IN) :: VALUE
    REAL(DFP), INTENT(IN) :: scale
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: spacecompo
    INTEGER(I4B), INTENT(IN) :: timecompo
  END SUBROUTINE obj_Add15
END INTERFACE Add

!----------------------------------------------------------------------------
!                                                                         Add
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 27 June 2021
! summary: See [[DOF_Method::dof_Add2]]

INTERFACE Add
  MODULE SUBROUTINE obj_Add16(obj, dofobj, nodenum, VALUE, &
                              scale, ivar, spacecompo, timecompo)
    TYPE(Realvector_), INTENT(INOUT) :: obj
    CLASS(DOF_), INTENT(IN) :: dofobj
    INTEGER(I4B), INTENT(IN) :: nodenum(:)
    REAL(DFP), INTENT(IN) :: VALUE(:)
    REAL(DFP), INTENT(IN) :: scale
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: spacecompo
    INTEGER(I4B), INTENT(IN) :: timecompo(:)
  END SUBROUTINE obj_Add16
END INTERFACE Add

!----------------------------------------------------------------------------
!                                                                         Add
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 27 June 2021
! summary: See [[DOF_Method::dof_Add2]]

INTERFACE Add
  MODULE SUBROUTINE obj_Add17(obj, dofobj, nodenum, VALUE, &
                              scale, ivar, spacecompo, timecompo)
    TYPE(Realvector_), INTENT(INOUT) :: obj
    CLASS(DOF_), INTENT(IN) :: dofobj
    INTEGER(I4B), INTENT(IN) :: nodenum(:)
    REAL(DFP), INTENT(IN) :: VALUE
    REAL(DFP), INTENT(IN) :: scale
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: spacecompo
    INTEGER(I4B), INTENT(IN) :: timecompo(:)
  END SUBROUTINE obj_Add17
END INTERFACE Add

!----------------------------------------------------------------------------
!                                                                         Add
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 27 June 2021
! summary: See [[DOF_Method::dof_Add2]]

INTERFACE Add
  MODULE SUBROUTINE obj_Add18(obj, dofobj, nodenum, VALUE, &
                              scale, ivar, spacecompo, timecompo)
    TYPE(Realvector_), INTENT(INOUT) :: obj
    CLASS(DOF_), INTENT(IN) :: dofobj
    INTEGER(I4B), INTENT(IN) :: nodenum(:)
    REAL(DFP), INTENT(IN) :: VALUE(:)
    REAL(DFP), INTENT(IN) :: scale
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: spacecompo(:)
    INTEGER(I4B), INTENT(IN) :: timecompo
  END SUBROUTINE obj_Add18
END INTERFACE Add

!----------------------------------------------------------------------------
!                                                                         Add
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 27 June 2021
! summary: See [[DOF_Method::dof_Add2]]

INTERFACE Add
  MODULE SUBROUTINE obj_Add19(obj, dofobj, nodenum, VALUE, &
                              scale, ivar, spacecompo, timecompo)
    TYPE(Realvector_), INTENT(INOUT) :: obj
    CLASS(DOF_), INTENT(IN) :: dofobj
    INTEGER(I4B), INTENT(IN) :: nodenum(:)
    REAL(DFP), INTENT(IN) :: VALUE
    REAL(DFP), INTENT(IN) :: scale
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: spacecompo(:)
    INTEGER(I4B), INTENT(IN) :: timecompo
  END SUBROUTINE obj_Add19
END INTERFACE Add

!----------------------------------------------------------------------------
!                                                                         Add
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 26 June 2021
! summary: See [[DOF_Method::dof_Add1]]

INTERFACE Add
  MODULE SUBROUTINE obj_Add20(obj, dofobj, nodenum, VALUE, &
                              scale)
    TYPE(Realvector_), INTENT(INOUT) :: obj
    CLASS(DOF_), INTENT(IN) :: dofobj
    INTEGER(I4B), INTENT(IN) :: nodenum
    REAL(DFP), INTENT(IN) :: VALUE
    REAL(DFP), INTENT(IN) :: scale
  END SUBROUTINE obj_Add20
END INTERFACE Add

!----------------------------------------------------------------------------
!                                                                         Add
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 26 June 2021
! summary: See [[DOF_Method::dof_Add1]]

INTERFACE Add
  MODULE SUBROUTINE obj_Add21(obj, dofobj, nodenum, VALUE, &
                              scale, idof)
    TYPE(Realvector_), INTENT(INOUT) :: obj
    CLASS(DOF_), INTENT(IN) :: dofobj
    INTEGER(I4B), INTENT(IN) :: nodenum
    REAL(DFP), INTENT(IN) :: VALUE
    REAL(DFP), INTENT(IN) :: scale
    INTEGER(I4B), INTENT(IN) :: idof
  END SUBROUTINE obj_Add21
END INTERFACE Add

!----------------------------------------------------------------------------
!                                                                         Add
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 26 June 2021
! summary: See [[DOF_Method::dof_Add1]]

INTERFACE Add
  MODULE SUBROUTINE obj_Add22(obj, dofobj, nodenum, VALUE, &
                              scale, ivar, idof)
    TYPE(Realvector_), INTENT(INOUT) :: obj
    CLASS(DOF_), INTENT(IN) :: dofobj
    INTEGER(I4B), INTENT(IN) :: nodenum
    REAL(DFP), INTENT(IN) :: VALUE
    REAL(DFP), INTENT(IN) :: scale
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: idof
  END SUBROUTINE obj_Add22
END INTERFACE Add

!----------------------------------------------------------------------------
!                                                                         Add
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 26 June 2021
! summary: See [[DOF_Method::dof_Add1]]

INTERFACE Add
  MODULE SUBROUTINE obj_Add23(obj, dofobj, nodenum, VALUE, &
                              scale, ivar, spacecompo, timecompo)
    TYPE(Realvector_), INTENT(INOUT) :: obj
    CLASS(DOF_), INTENT(IN) :: dofobj
    INTEGER(I4B), INTENT(IN) :: nodenum
    REAL(DFP), INTENT(IN) :: VALUE
    REAL(DFP), INTENT(IN) :: scale
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: spacecompo
    INTEGER(I4B), INTENT(IN) :: timecompo
  END SUBROUTINE obj_Add23
END INTERFACE Add

!----------------------------------------------------------------------------
!                                                                         Add
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 26 June 2021
! summary: See [[DOF_Method::dof_Add1]]

INTERFACE Add
  MODULE SUBROUTINE obj_Add24(obj, dofobj, nodenum, VALUE, &
                              scale, ivar, spacecompo, timecompo)
    TYPE(Realvector_), INTENT(INOUT) :: obj
    CLASS(DOF_), INTENT(IN) :: dofobj
    INTEGER(I4B), INTENT(IN) :: nodenum
    REAL(DFP), INTENT(IN) :: VALUE
    REAL(DFP), INTENT(IN) :: scale
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: spacecompo
    INTEGER(I4B), INTENT(IN) :: timecompo(:)
  END SUBROUTINE obj_Add24
END INTERFACE Add

!----------------------------------------------------------------------------
!                                                                         Add
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 26 June 2021
! summary: See [[DOF_Method::dof_Add1]]

INTERFACE Add
  MODULE SUBROUTINE obj_Add25(obj, dofobj, nodenum, VALUE, &
                              scale, ivar, spacecompo, timecompo)
    TYPE(Realvector_), INTENT(INOUT) :: obj
    CLASS(DOF_), INTENT(IN) :: dofobj
    INTEGER(I4B), INTENT(IN) :: nodenum
    REAL(DFP), INTENT(IN) :: VALUE
    REAL(DFP), INTENT(IN) :: scale
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: spacecompo(:)
    INTEGER(I4B), INTENT(IN) :: timecompo
  END SUBROUTINE obj_Add25
END INTERFACE Add

!----------------------------------------------------------------------------
!                                                                        add
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 9 June 2022
! summary: obj1=obj2

INTERFACE Add
  MODULE SUBROUTINE obj_Add26(obj, VALUE, scale)
    CLASS(RealVector_), INTENT(INOUT) :: obj
    CLASS(RealVector_), INTENT(IN) :: VALUE
    REAL(DFP), INTENT(IN) :: scale
  END SUBROUTINE obj_Add26
END INTERFACE Add

!----------------------------------------------------------------------------
!                                                                        Add
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-05-29
! summary: obj = obj + scale*VALUE
!
!# Introduction
!
! Value contains the nodal values of all dofs
! Number of cols in values should be at least equal to the total dof in obj
! Number of rows in values should be at least equal to the total nodes in obj

INTERFACE Add
  MODULE SUBROUTINE obj_Add27(obj, dofobj, VALUE, scale)
    CLASS(RealVector_), INTENT(INOUT) :: obj
    !! real vector
    TYPE(DOF_), INTENT(IN) :: dofobj
    !! degree of freedom object
    REAL(DFP), INTENT(IN) :: VALUE(:, :)
    !! number of cols should be equal to the total dof in obj
    !! number of rows should be equal to the total nodes in obj
    REAL(DFP), INTENT(IN) :: scale
  END SUBROUTINE obj_Add27
END INTERFACE Add

!----------------------------------------------------------------------------
!                                                                        Add
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-05-29
! summary: obj = obj + scale*VALUE

INTERFACE Add
  MODULE SUBROUTINE obj_Add28(obj, dofobj, VALUE, scale, idof)
    CLASS(RealVector_), INTENT(INOUT) :: obj
    !! real vector
    TYPE(DOF_), INTENT(IN) :: dofobj
    !! degree of freedom object
    REAL(DFP), INTENT(IN) :: VALUE(:)
    !! number of cols should be equal to the total dof in obj
    !! number of rows should be equal to the total nodes in obj
    REAL(DFP), INTENT(IN) :: scale
    !! scale
    INTEGER(I4B), INTENT(IN) :: idof
    !! global degree of freedom in dofobj
  END SUBROUTINE obj_Add28
END INTERFACE Add

END MODULE RealVector_AddMethods
