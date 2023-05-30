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
USE GlobalData
USE BaseType
IMPLICIT NONE
PRIVATE
PUBLIC :: Add

!----------------------------------------------------------------------------
!                                                            set@SetMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 26 June 2021
! summary: Add all values to given scalar

INTERFACE
  MODULE SUBROUTINE realVec_add1(obj, VALUE, scale)
    CLASS(RealVector_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: VALUE
    REAL(DFP), INTENT(IN) :: scale
  END SUBROUTINE realVec_add1
END INTERFACE

INTERFACE Add
  MODULE PROCEDURE realVec_add1
END INTERFACE Add

!----------------------------------------------------------------------------
!                                                             set@SetMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 26 June 2021
! summary: Add all values by given vector

INTERFACE
  MODULE SUBROUTINE realVec_add2(obj, VALUE, scale)
    CLASS(RealVector_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: VALUE(:)
    REAL(DFP), INTENT(IN) :: scale
  END SUBROUTINE realVec_add2
END INTERFACE

INTERFACE Add
  MODULE PROCEDURE realVec_add2
END INTERFACE Add

!----------------------------------------------------------------------------
!                                                            add@AddMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 5 Jan 2022
! summary: set selected values

INTERFACE
  MODULE SUBROUTINE realVec_add3(obj, nodenum, VALUE, scale)
    CLASS(RealVector_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: nodenum
    REAL(DFP), INTENT(IN) :: VALUE
    REAL(DFP), INTENT(IN) :: scale
  END SUBROUTINE realVec_add3
END INTERFACE

INTERFACE Add
  MODULE PROCEDURE realVec_add3
END INTERFACE Add

!----------------------------------------------------------------------------
!                                                             set@setMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 27 June 2021
! summary: See [[DOF_Method::dof_set2]]

INTERFACE
  MODULE PURE SUBROUTINE realvec_add4(obj, nodenum, VALUE, scale)
    TYPE(Realvector_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: nodenum(:)
    REAL(DFP), INTENT(IN) :: VALUE
    REAL(DFP), INTENT(IN) :: scale
  END SUBROUTINE realvec_add4
END INTERFACE

INTERFACE Add
  MODULE PROCEDURE realvec_add4
END INTERFACE Add

!----------------------------------------------------------------------------
!                                                            set@SetMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 5 Jan 2022
! summary: set selected values

INTERFACE
  MODULE SUBROUTINE realVec_add5(obj, nodenum, VALUE, scale)
    CLASS(RealVector_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: nodenum(:)
    REAL(DFP), INTENT(IN) :: VALUE(:)
    REAL(DFP), INTENT(IN) :: scale
  END SUBROUTINE realVec_add5
END INTERFACE

INTERFACE Add
  MODULE PROCEDURE realVec_add5
END INTERFACE Add

!----------------------------------------------------------------------------
!                                                              set@SetMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 26 June 2021
! summary: Add range of values to a scalar

INTERFACE
  MODULE SUBROUTINE realVec_add6(obj, istart, iend, stride, VALUE, scale)
    CLASS(RealVector_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: istart, iend, stride
    REAL(DFP), INTENT(IN) :: VALUE
    REAL(DFP), INTENT(IN) :: scale
  END SUBROUTINE realVec_add6
END INTERFACE

INTERFACE Add
  MODULE PROCEDURE realVec_add6
END INTERFACE Add

!----------------------------------------------------------------------------
!                                                             set@SetMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 26 June 2021
! summary: Add range of values to a vector

INTERFACE
  MODULE SUBROUTINE realVec_add7(obj, istart, iend, stride, VALUE, scale)
    CLASS(RealVector_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: istart, iend, stride
    REAL(DFP), INTENT(IN) :: VALUE(:)
    REAL(DFP), INTENT(IN) :: scale
  END SUBROUTINE realVec_add7
END INTERFACE

INTERFACE Add
  MODULE PROCEDURE realVec_add7
END INTERFACE Add

!----------------------------------------------------------------------------
!                                                             set@setMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 26 June 2021
! summary: See [[DOF_Method::dof_set1]]

INTERFACE
  MODULE PURE SUBROUTINE realvec_add8(obj, dofobj, nodenum, VALUE, &
    & scale, conversion)
    TYPE(Realvector_), INTENT(INOUT) :: obj
    CLASS(DOF_), INTENT(IN) :: dofobj
    INTEGER(I4B), INTENT(IN) :: nodenum(:)
    REAL(DFP), INTENT(IN) :: VALUE(:)
    REAL(DFP), INTENT(IN) :: scale
    INTEGER(I4B), INTENT(IN) :: conversion(1)
  END SUBROUTINE realvec_add8
END INTERFACE

INTERFACE Add
  MODULE PROCEDURE realvec_add8
END INTERFACE Add

!----------------------------------------------------------------------------
!                                                             set@setMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 26 June 2021
! summary: See [[DOF_Method::dof_set1]]

INTERFACE
  MODULE PURE SUBROUTINE realvec_add9(obj, dofobj, nodenum, VALUE, &
    & scale)
    TYPE(Realvector_), INTENT(INOUT) :: obj
    CLASS(DOF_), INTENT(IN) :: dofobj
    INTEGER(I4B), INTENT(IN) :: nodenum(:)
    REAL(DFP), INTENT(IN) :: VALUE
    REAL(DFP), INTENT(IN) :: scale
  END SUBROUTINE realvec_add9
END INTERFACE

INTERFACE Add
  MODULE PROCEDURE realvec_add9
END INTERFACE Add

!----------------------------------------------------------------------------
!                                                             set@setMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 27 June 2021
! summary: See [[DOF_Method::dof_set2]]

INTERFACE
  MODULE PURE SUBROUTINE realvec_add10(obj, dofobj, nodenum, VALUE, &
    & scale, idof)
    TYPE(Realvector_), INTENT(INOUT) :: obj
    CLASS(DOF_), INTENT(IN) :: dofobj
    INTEGER(I4B), INTENT(IN) :: nodenum(:)
    REAL(DFP), INTENT(IN) :: VALUE(:)
    REAL(DFP), INTENT(IN) :: scale
    INTEGER(I4B), INTENT(IN) :: idof
  END SUBROUTINE realvec_add10
END INTERFACE

INTERFACE Add
  MODULE PROCEDURE realvec_add10
END INTERFACE Add

!----------------------------------------------------------------------------
!                                                             set@setMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 27 June 2021
! summary: See [[DOF_Method::dof_set2]]

INTERFACE
  MODULE PURE SUBROUTINE realvec_add11(obj, dofobj, nodenum, VALUE, &
    & scale, idof)
    TYPE(Realvector_), INTENT(INOUT) :: obj
    CLASS(DOF_), INTENT(IN) :: dofobj
    INTEGER(I4B), INTENT(IN) :: nodenum(:)
    REAL(DFP), INTENT(IN) :: VALUE
    REAL(DFP), INTENT(IN) :: scale
    INTEGER(I4B), INTENT(IN) :: idof
  END SUBROUTINE realvec_add11
END INTERFACE

INTERFACE Add
  MODULE PROCEDURE realvec_add11
END INTERFACE Add

!----------------------------------------------------------------------------
!                                                             set@setMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 27 June 2021
! summary: See [[DOF_Method::dof_set2]]

INTERFACE
  MODULE PURE SUBROUTINE realvec_add12(obj, dofobj, nodenum, VALUE, &
    & scale, ivar, idof)
    TYPE(Realvector_), INTENT(INOUT) :: obj
    CLASS(DOF_), INTENT(IN) :: dofobj
    INTEGER(I4B), INTENT(IN) :: nodenum(:)
    REAL(DFP), INTENT(IN) :: VALUE(:)
    REAL(DFP), INTENT(IN) :: scale
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: idof
  END SUBROUTINE realvec_add12
END INTERFACE

INTERFACE Add
  MODULE PROCEDURE realvec_add12
END INTERFACE Add

!----------------------------------------------------------------------------
!                                                             set@setMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 27 June 2021
! summary: See [[DOF_Method::dof_set2]]

INTERFACE
  MODULE PURE SUBROUTINE realvec_add13(obj, dofobj, nodenum, VALUE, &
    & scale, ivar, idof)
    TYPE(Realvector_), INTENT(INOUT) :: obj
    CLASS(DOF_), INTENT(IN) :: dofobj
    INTEGER(I4B), INTENT(IN) :: nodenum(:)
    REAL(DFP), INTENT(IN) :: VALUE
    REAL(DFP), INTENT(IN) :: scale
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: idof
  END SUBROUTINE realvec_add13
END INTERFACE

INTERFACE Add
  MODULE PROCEDURE realvec_add13
END INTERFACE Add

!----------------------------------------------------------------------------
!                                                             set@setMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 27 June 2021
! summary: See [[DOF_Method::dof_set2]]

INTERFACE
  MODULE PURE SUBROUTINE realvec_add14(obj, dofobj, nodenum, VALUE, &
    & scale, ivar, spacecompo, timecompo)
    TYPE(Realvector_), INTENT(INOUT) :: obj
    CLASS(DOF_), INTENT(IN) :: dofobj
    INTEGER(I4B), INTENT(IN) :: nodenum(:)
    REAL(DFP), INTENT(IN) :: VALUE(:)
    REAL(DFP), INTENT(IN) :: scale
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: spacecompo
    INTEGER(I4B), INTENT(IN) :: timecompo
  END SUBROUTINE realvec_add14
END INTERFACE

INTERFACE add
  MODULE PROCEDURE realvec_add14
END INTERFACE add

!----------------------------------------------------------------------------
!                                                             set@setMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 27 June 2021
! summary: See [[DOF_Method::dof_set2]]

INTERFACE
  MODULE PURE SUBROUTINE realvec_add15(obj, dofobj, nodenum, VALUE, &
    & scale, ivar, spacecompo, timecompo)
    TYPE(Realvector_), INTENT(INOUT) :: obj
    CLASS(DOF_), INTENT(IN) :: dofobj
    INTEGER(I4B), INTENT(IN) :: nodenum(:)
    REAL(DFP), INTENT(IN) :: VALUE
    REAL(DFP), INTENT(IN) :: scale
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: spacecompo
    INTEGER(I4B), INTENT(IN) :: timecompo
  END SUBROUTINE realvec_add15
END INTERFACE

INTERFACE add
  MODULE PROCEDURE realvec_add15
END INTERFACE add

!----------------------------------------------------------------------------
!                                                             set@setMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 27 June 2021
! summary: See [[DOF_Method::dof_set2]]

INTERFACE
  MODULE PURE SUBROUTINE realvec_add16(obj, dofobj, nodenum, VALUE, &
    & scale, ivar, spacecompo, timecompo)
    TYPE(Realvector_), INTENT(INOUT) :: obj
    CLASS(DOF_), INTENT(IN) :: dofobj
    INTEGER(I4B), INTENT(IN) :: nodenum(:)
    REAL(DFP), INTENT(IN) :: VALUE(:)
    REAL(DFP), INTENT(IN) :: scale
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: spacecompo
    INTEGER(I4B), INTENT(IN) :: timecompo(:)
  END SUBROUTINE realvec_add16
END INTERFACE

INTERFACE add
  MODULE PROCEDURE realvec_add16
END INTERFACE add

!----------------------------------------------------------------------------
!                                                             set@setMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 27 June 2021
! summary: See [[DOF_Method::dof_set2]]

INTERFACE
  MODULE PURE SUBROUTINE realvec_add17(obj, dofobj, nodenum, VALUE, &
    & scale, ivar, spacecompo, timecompo)
    TYPE(Realvector_), INTENT(INOUT) :: obj
    CLASS(DOF_), INTENT(IN) :: dofobj
    INTEGER(I4B), INTENT(IN) :: nodenum(:)
    REAL(DFP), INTENT(IN) :: VALUE
    REAL(DFP), INTENT(IN) :: scale
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: spacecompo
    INTEGER(I4B), INTENT(IN) :: timecompo(:)
  END SUBROUTINE realvec_add17
END INTERFACE

INTERFACE add
  MODULE PROCEDURE realvec_add17
END INTERFACE add

!----------------------------------------------------------------------------
!                                                             set@setMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 27 June 2021
! summary: See [[DOF_Method::dof_set2]]

INTERFACE
  MODULE PURE SUBROUTINE realvec_add18(obj, dofobj, nodenum, VALUE, &
    & scale, ivar, spacecompo, timecompo)
    TYPE(Realvector_), INTENT(INOUT) :: obj
    CLASS(DOF_), INTENT(IN) :: dofobj
    INTEGER(I4B), INTENT(IN) :: nodenum(:)
    REAL(DFP), INTENT(IN) :: VALUE(:)
    REAL(DFP), INTENT(IN) :: scale
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: spacecompo(:)
    INTEGER(I4B), INTENT(IN) :: timecompo
  END SUBROUTINE realvec_add18
END INTERFACE

INTERFACE add
  MODULE PROCEDURE realvec_add18
END INTERFACE add

!----------------------------------------------------------------------------
!                                                             set@setMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 27 June 2021
! summary: See [[DOF_Method::dof_set2]]

INTERFACE
  MODULE PURE SUBROUTINE realvec_add19(obj, dofobj, nodenum, VALUE, &
    & scale, ivar, spacecompo, timecompo)
    TYPE(Realvector_), INTENT(INOUT) :: obj
    CLASS(DOF_), INTENT(IN) :: dofobj
    INTEGER(I4B), INTENT(IN) :: nodenum(:)
    REAL(DFP), INTENT(IN) :: VALUE
    REAL(DFP), INTENT(IN) :: scale
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: spacecompo(:)
    INTEGER(I4B), INTENT(IN) :: timecompo
  END SUBROUTINE realvec_add19
END INTERFACE

INTERFACE add
  MODULE PROCEDURE realvec_add19
END INTERFACE add

!----------------------------------------------------------------------------
!                                                             set@setMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 26 June 2021
! summary: See [[DOF_Method::dof_set1]]

INTERFACE
  MODULE PURE SUBROUTINE realvec_add20(obj, dofobj, nodenum, VALUE, &
    & scale)
    TYPE(Realvector_), INTENT(INOUT) :: obj
    CLASS(DOF_), INTENT(IN) :: dofobj
    INTEGER(I4B), INTENT(IN) :: nodenum
    REAL(DFP), INTENT(IN) :: VALUE
    REAL(DFP), INTENT(IN) :: scale
  END SUBROUTINE realvec_add20
END INTERFACE

INTERFACE Add
  MODULE PROCEDURE realvec_add20
END INTERFACE Add

!----------------------------------------------------------------------------
!                                                             set@setMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 26 June 2021
! summary: See [[DOF_Method::dof_set1]]

INTERFACE
  MODULE PURE SUBROUTINE realvec_add21(obj, dofobj, nodenum, VALUE, &
    & scale, idof)
    TYPE(Realvector_), INTENT(INOUT) :: obj
    CLASS(DOF_), INTENT(IN) :: dofobj
    INTEGER(I4B), INTENT(IN) :: nodenum
    REAL(DFP), INTENT(IN) :: VALUE
    REAL(DFP), INTENT(IN) :: scale
    INTEGER(I4B), INTENT(IN) :: idof
  END SUBROUTINE realvec_add21
END INTERFACE

INTERFACE Add
  MODULE PROCEDURE realvec_add21
END INTERFACE Add

!----------------------------------------------------------------------------
!                                                             set@setMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 26 June 2021
! summary: See [[DOF_Method::dof_set1]]

INTERFACE
  MODULE PURE SUBROUTINE realvec_add22(obj, dofobj, nodenum, VALUE, &
    & scale, ivar, idof)
    TYPE(Realvector_), INTENT(INOUT) :: obj
    CLASS(DOF_), INTENT(IN) :: dofobj
    INTEGER(I4B), INTENT(IN) :: nodenum
    REAL(DFP), INTENT(IN) :: VALUE
    REAL(DFP), INTENT(IN) :: scale
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: idof
  END SUBROUTINE realvec_add22
END INTERFACE

INTERFACE Add
  MODULE PROCEDURE realvec_add22
END INTERFACE Add

!----------------------------------------------------------------------------
!                                                             set@setMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 26 June 2021
! summary: See [[DOF_Method::dof_set1]]

INTERFACE
  MODULE PURE SUBROUTINE realvec_add23(obj, dofobj, nodenum, VALUE, &
    & scale, ivar, spacecompo, timecompo)
    TYPE(Realvector_), INTENT(INOUT) :: obj
    CLASS(DOF_), INTENT(IN) :: dofobj
    INTEGER(I4B), INTENT(IN) :: nodenum
    REAL(DFP), INTENT(IN) :: VALUE
    REAL(DFP), INTENT(IN) :: scale
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: spacecompo
    INTEGER(I4B), INTENT(IN) :: timecompo
  END SUBROUTINE realvec_add23
END INTERFACE

INTERFACE Add
  MODULE PROCEDURE realvec_add23
END INTERFACE Add

!----------------------------------------------------------------------------
!                                                             set@setMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 26 June 2021
! summary: See [[DOF_Method::dof_set1]]

INTERFACE
  MODULE PURE SUBROUTINE realvec_add24(obj, dofobj, nodenum, VALUE, &
    & scale, ivar, spacecompo, timecompo)
    TYPE(Realvector_), INTENT(INOUT) :: obj
    CLASS(DOF_), INTENT(IN) :: dofobj
    INTEGER(I4B), INTENT(IN) :: nodenum
    REAL(DFP), INTENT(IN) :: VALUE
    REAL(DFP), INTENT(IN) :: scale
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: spacecompo
    INTEGER(I4B), INTENT(IN) :: timecompo(:)
  END SUBROUTINE realvec_add24
END INTERFACE

INTERFACE Add
  MODULE PROCEDURE realvec_add24
END INTERFACE Add

!----------------------------------------------------------------------------
!                                                             set@setMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 26 June 2021
! summary: See [[DOF_Method::dof_set1]]

INTERFACE
  MODULE PURE SUBROUTINE realvec_add25(obj, dofobj, nodenum, VALUE, &
    & scale, ivar, spacecompo, timecompo)
    TYPE(Realvector_), INTENT(INOUT) :: obj
    CLASS(DOF_), INTENT(IN) :: dofobj
    INTEGER(I4B), INTENT(IN) :: nodenum
    REAL(DFP), INTENT(IN) :: VALUE
    REAL(DFP), INTENT(IN) :: scale
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: spacecompo(:)
    INTEGER(I4B), INTENT(IN) :: timecompo
  END SUBROUTINE realvec_add25
END INTERFACE

INTERFACE Add
  MODULE PROCEDURE realvec_add25
END INTERFACE Add

!----------------------------------------------------------------------------
!                                                            add@addMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 9 June 2022
! summary: obj1=obj2

INTERFACE
  MODULE PURE SUBROUTINE realVec_add26(obj, VALUE, scale)
    CLASS(RealVector_), INTENT(INOUT) :: obj
    CLASS(RealVector_), INTENT(IN) :: VALUE
    REAL(DFP), INTENT(IN) :: scale
  END SUBROUTINE realVec_add26
END INTERFACE

INTERFACE Add
  MODULE PROCEDURE realVec_add26
END INTERFACE Add
END MODULE RealVector_AddMethods
