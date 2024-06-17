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

MODULE CSRMatrix_AddMethods
USE GlobalData, ONLY: I4B, DFP, LGT
USE BaseType, ONLY: CSRMatrix_
IMPLICIT NONE
PRIVATE

PUBLIC :: Add

!----------------------------------------------------------------------------
!                                                             Add@addMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         22 Marach 2021
! summary: This subroutine Add contribution

INTERFACE Add
  MODULE PURE SUBROUTINE obj_Add0(obj, nodenum, VALUE, scale)
    TYPE(CSRMatrix_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: nodenum(:)
    !! Node numbers
    REAL(DFP), INTENT(IN) :: VALUE(:, :)
    !! Element finite element matrix
    REAL(DFP), INTENT(IN) :: scale
    !! Scale is used to scale the Val before Adding it to the obj
  END SUBROUTINE obj_Add0
END INTERFACE Add

!----------------------------------------------------------------------------
!                                                             Add@addMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         22 Marach 2021
! summary: This subroutine Add contribution

INTERFACE Add
  MODULE PURE SUBROUTINE obj_Add1(obj, nodenum, VALUE, scale, storageFMT)
    TYPE(CSRMatrix_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: nodenum(:)
    !! Node numbers
    REAL(DFP), INTENT(IN) :: VALUE(:, :)
    !! Element finite element matrix
    REAL(DFP), INTENT(IN) :: scale
    !! Scale is used to scale the Val before Adding it to the obj
    INTEGER(I4B), INTENT(IN) :: storageFMT
    !! Storage format of element finite matrix
  END SUBROUTINE obj_Add1
END INTERFACE Add

!----------------------------------------------------------------------------
!                                                             Add@addMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 22 March 2021
! summary: Adds all values of sparse matrix to given scalar value
!
!# Introduction
! This routine Adds all values of sparse matrix to given value.
! This routine signifies `obj=obj+scale*value`.

INTERFACE Add
  MODULE PURE SUBROUTINE obj_Add2(obj, VALUE, scale)
    TYPE(CSRMatrix_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: VALUE
    REAL(DFP), INTENT(IN) :: scale
  END SUBROUTINE obj_Add2
END INTERFACE Add

!----------------------------------------------------------------------------
!                                                             Add@addMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         22 March 2021
! summary:         This subroutine Adds a single entry of sparse matrix
!
!# Introduction
!
! This subroutine Adds a single entry of sparse matrix.
! Before using this subroutien the user should be aware of the storage
! pattern of degree of freedom. However, if total number of degrees of
! freedom is one then there is not need to worry. In my opinion, this routine
! should be avoided by general user.

INTERFACE Add
  MODULE PURE SUBROUTINE obj_Add3(obj, irow, icolumn, VALUE, scale)
    TYPE(CSRMatrix_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: irow
    INTEGER(I4B), INTENT(IN) :: icolumn
    REAL(DFP), INTENT(IN) :: VALUE
    REAL(DFP), INTENT(IN) :: scale
  END SUBROUTINE obj_Add3
END INTERFACE Add

!----------------------------------------------------------------------------
!                                                             Add@addMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         22 March 2021
! summary: Adds the specific row and column entry to a given value
!
!# Introduction
!
! This routine Adds the specific row and column entry to a given value.
! The row and column index is calculated by using (iNodeNum, idof) and
! (jNodeNum, jdof), respectively.
! After computing the irow and icolumn (internally) this routine calls,
! `obj_Add3`.

INTERFACE Add
  MODULE PURE SUBROUTINE obj_Add4(obj, iNodeNum, jNodeNum, idof, &
    & jdof, VALUE, scale)
    TYPE(CSRMatrix_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: iNodeNum
    INTEGER(I4B), INTENT(IN) :: jNodeNum
    INTEGER(I4B), INTENT(IN) :: idof
    INTEGER(I4B), INTENT(IN) :: jdof
    REAL(DFP), INTENT(IN) :: VALUE
    REAL(DFP), INTENT(IN) :: scale
  END SUBROUTINE obj_Add4
END INTERFACE Add

!----------------------------------------------------------------------------
!                                                            Add@addMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         22 March 2021
! summary: This subroutine Add the selected value in sparse matrix

INTERFACE Add
  MODULE PURE SUBROUTINE obj_Add5(obj, nodenum, VALUE, scale)
    TYPE(CSRMatrix_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: nodenum(:)
    REAL(DFP), INTENT(IN) :: VALUE
    REAL(DFP), INTENT(IN) :: scale
  END SUBROUTINE obj_Add5
END INTERFACE Add

!----------------------------------------------------------------------------
!                                                            Add@addMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         22 March 2021
! summary: This subroutine Add the value in sparse matrix
!
!# Introduction
!
! - This subroutine Adds the values in block sparse matrix.
! - The storage pattern of both sparse matrix and value
! (the element matrix) should be in `FMT_DOF`.
!
!$$
! obj(Nptrs,Nptrs)=value(:,:)
!$$
!

INTERFACE Add
  MODULE PURE SUBROUTINE obj_Add6(obj, iNodeNum, jNodeNum, &
    & ivar, jvar, VALUE, scale)
    TYPE(CSRMatrix_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: iNodeNum(:)
    INTEGER(I4B), INTENT(IN) :: jNodeNum(:)
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: jvar
    REAL(DFP), INTENT(IN) :: VALUE(:, :)
    REAL(DFP), INTENT(IN) :: scale
  END SUBROUTINE obj_Add6
END INTERFACE Add

!----------------------------------------------------------------------------
!                                                             Add@addMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         22 March 2021
! summary:         Adds the specific row and column entry to a given value
!
!# Introduction
!
! - This routine Adds the specific row and column entry to a given value.
! - The irow and icolumn index in [[CSRMatrix_]] are calculated by using
! (iNodeNum, iDOF) and (jNodeNum, jDOF), respectively.
! - To do the above task, the routine employs [[DOF_Method:getNodeLoc]] method
! - After computing the irow and icolumn (internally) this routine calls,
! `obj_Add3`.
!
!@note
! General user should prefer this routine over
! [[CSRMatrix_Method:obj_Add3]]
!@endnote
!
!@note
! idof, jdof are continuously numbered, so if there are two
! or more physical variables, then idof and jdof of the second
! or later physical variables will not start from 1.
!@endnote

INTERFACE Add
  MODULE PURE SUBROUTINE obj_Add7(obj, iNodeNum, jNodeNum, ivar,  &
    & jvar, iDOF, jDOF, VALUE, scale)
    TYPE(CSRMatrix_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: iNodeNum
    !! row node number
    INTEGER(I4B), INTENT(IN) :: jNodeNum
    !! column node number
    INTEGER(I4B), INTENT(IN) :: ivar
    !!
    INTEGER(I4B), INTENT(IN) :: jvar
    !!
    INTEGER(I4B), INTENT(IN) :: iDOF
    !! row degree of freedom
    INTEGER(I4B), INTENT(IN) :: jDOF
    !! col degree of freedom
    REAL(DFP), INTENT(IN) :: VALUE
    !! scalar value to be Add
    REAL(DFP), INTENT(IN) :: scale
  END SUBROUTINE obj_Add7
END INTERFACE Add

!----------------------------------------------------------------------------
!                                                             Add@addMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         22 March 2021
! summary:         Adds the specific row and column entry to a given value

INTERFACE Add
  MODULE PURE SUBROUTINE obj_Add8(obj, iNodeNum, jNodeNum, ivar,  &
    & jvar, iDOF, jDOF, VALUE, scale)
    TYPE(CSRMatrix_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: iNodeNum(:)
    !! row node number
    INTEGER(I4B), INTENT(IN) :: jNodeNum(:)
    !! column node number
    INTEGER(I4B), INTENT(IN) :: ivar
    !!
    INTEGER(I4B), INTENT(IN) :: jvar
    !!
    INTEGER(I4B), INTENT(IN) :: iDOF
    !! row degree of freedom
    INTEGER(I4B), INTENT(IN) :: jDOF
    !! col degree of freedom
    REAL(DFP), INTENT(IN) :: VALUE(:, :)
    !! scalar value to be Add
    REAL(DFP), INTENT(IN) :: scale
  END SUBROUTINE obj_Add8
END INTERFACE Add

!----------------------------------------------------------------------------
!                                                             Add@addMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 17/01/2022
! summary:         Adds the specific row and column entry to a given value
!
!# Introduction
!
! - This routine Adds the specific row and column entry to a given value.
! - The irow and icolumn index in [[CSRMatrix_]] are calculated by using
! (iNodeNum, iDOF) and (jNodeNum, jDOF), respectively.
! - To do the above task, the routine employs [[DOF_Method:getNodeLoc]] method
! - After computing the irow and icolumn (internally) this routine calls,
! `obj_Add3`.
!
!@note
! General user should prefer this routine over
! [[CSRMatrix_Method:obj_Add3]]
!@endnote
!
!@note
! idof, jdof are continuously numbered, so if there are two
! or more physical variables, then idof and jdof of the second
! or later physical variables will not start from 1.
!@endnote

INTERFACE Add
  MODULE PURE SUBROUTINE obj_Add9(obj, iNodeNum, jNodeNum, ivar,  &
    & jvar, ispacecompo, itimecompo, jspacecompo, jtimecompo, VALUE, scale)
    TYPE(CSRMatrix_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: iNodeNum
    !! row node number
    INTEGER(I4B), INTENT(IN) :: jNodeNum
    !! column node number
    INTEGER(I4B), INTENT(IN) :: ivar
    !!
    INTEGER(I4B), INTENT(IN) :: jvar
    !!
    INTEGER(I4B), INTENT(IN) :: ispacecompo
    INTEGER(I4B), INTENT(IN) :: itimecompo
    INTEGER(I4B), INTENT(IN) :: jspacecompo
    INTEGER(I4B), INTENT(IN) :: jtimecompo
    REAL(DFP), INTENT(IN) :: VALUE
    !! scalar value to be Add
    REAL(DFP), INTENT(IN) :: scale
  END SUBROUTINE obj_Add9
END INTERFACE Add

!----------------------------------------------------------------------------
!                                                            Add@addMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 17/01/2022
! summary: This subroutine Add the value in sparse matrix
!
!# Introduction
!
! - This subroutine Adds the values in block sparse matrix.
! - The storage pattern of both sparse matrix and value
! (the element matrix) should be in `FMT_DOF`.
!
!$$
! obj(Nptrs,Nptrs)=value(:,:)
!$$
!

INTERFACE Add
  MODULE PURE SUBROUTINE obj_Add10(obj, iNodeNum, jNodeNum, &
    & ivar, jvar, VALUE, scale)
    TYPE(CSRMatrix_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: iNodeNum(:)
    INTEGER(I4B), INTENT(IN) :: jNodeNum(:)
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: jvar
    REAL(DFP), INTENT(IN) :: VALUE
    REAL(DFP), INTENT(IN) :: scale
  END SUBROUTINE obj_Add10
END INTERFACE Add

!----------------------------------------------------------------------------
!                                                             Add@addMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 17/01/2022
! summary:         Adds the specific row and column entry to a given value

INTERFACE Add
  MODULE PURE SUBROUTINE obj_Add11(obj, iNodeNum, jNodeNum, ivar,  &
    & jvar, iDOF, jDOF, VALUE, scale)
    TYPE(CSRMatrix_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: iNodeNum(:)
    !! row node number
    INTEGER(I4B), INTENT(IN) :: jNodeNum(:)
    !! column node number
    INTEGER(I4B), INTENT(IN) :: ivar
    !!
    INTEGER(I4B), INTENT(IN) :: jvar
    !!
    INTEGER(I4B), INTENT(IN) :: iDOF
    !! row degree of freedom
    INTEGER(I4B), INTENT(IN) :: jDOF
    !! col degree of freedom
    REAL(DFP), INTENT(IN) :: VALUE
    !! scalar value to be Add
    REAL(DFP), INTENT(IN) :: scale
  END SUBROUTINE obj_Add11
END INTERFACE Add

!----------------------------------------------------------------------------
!                                                             Add@addMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 17/01/2022
! summary:         Adds the specific row and column entry to a given value

INTERFACE Add
  MODULE PURE SUBROUTINE obj_Add12(obj, iNodeNum, jNodeNum, ivar,  &
    & jvar, ispacecompo, itimecompo, jspacecompo, jtimecompo, VALUE, scale)
    TYPE(CSRMatrix_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: iNodeNum(:)
    !! row node number
    INTEGER(I4B), INTENT(IN) :: jNodeNum(:)
    !! column node number
    INTEGER(I4B), INTENT(IN) :: ivar
    !!
    INTEGER(I4B), INTENT(IN) :: jvar
    !!
    INTEGER(I4B), INTENT(IN) :: ispacecompo
    INTEGER(I4B), INTENT(IN) :: itimecompo
    INTEGER(I4B), INTENT(IN) :: jspacecompo
    INTEGER(I4B), INTENT(IN) :: jtimecompo
    REAL(DFP), INTENT(IN) :: VALUE
    !! scalar value to be Add
    REAL(DFP), INTENT(IN) :: scale
  END SUBROUTINE obj_Add12
END INTERFACE Add

!----------------------------------------------------------------------------
!                                                             Add@addMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 17/01/2022
! summary:         Adds the specific row and column entry to a given value

INTERFACE Add
  MODULE PURE SUBROUTINE obj_Add13(obj, iNodeNum, jNodeNum, ivar,  &
    & jvar, ispacecompo, itimecompo, jspacecompo, jtimecompo, VALUE, scale)
    TYPE(CSRMatrix_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: iNodeNum(:)
    !! row node number
    INTEGER(I4B), INTENT(IN) :: jNodeNum(:)
    !! column node number
    INTEGER(I4B), INTENT(IN) :: ivar
    !!
    INTEGER(I4B), INTENT(IN) :: jvar
    !!
    INTEGER(I4B), INTENT(IN) :: ispacecompo
    INTEGER(I4B), INTENT(IN) :: itimecompo(:)
    INTEGER(I4B), INTENT(IN) :: jspacecompo
    INTEGER(I4B), INTENT(IN) :: jtimecompo(:)
    REAL(DFP), INTENT(IN) :: VALUE
    !! scalar value to be Add
    REAL(DFP), INTENT(IN) :: scale
  END SUBROUTINE obj_Add13
END INTERFACE Add

!----------------------------------------------------------------------------
!                                                             Add@addMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 17/01/2022
! summary:  Adds the specific row and column entry to a given value

INTERFACE Add
  MODULE PURE SUBROUTINE obj_Add14(obj, iNodeNum, jNodeNum, ivar,  &
    & jvar, ispacecompo, itimecompo, jspacecompo, jtimecompo, VALUE, scale)
    TYPE(CSRMatrix_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: iNodeNum(:)
    !! row node number
    INTEGER(I4B), INTENT(IN) :: jNodeNum(:)
    !! column node number
    INTEGER(I4B), INTENT(IN) :: ivar
    !!
    INTEGER(I4B), INTENT(IN) :: jvar
    !!
    INTEGER(I4B), INTENT(IN) :: ispacecompo(:)
    INTEGER(I4B), INTENT(IN) :: itimecompo
    INTEGER(I4B), INTENT(IN) :: jspacecompo(:)
    INTEGER(I4B), INTENT(IN) :: jtimecompo
    REAL(DFP), INTENT(IN) :: VALUE
    !! scalar value to be Add
    REAL(DFP), INTENT(IN) :: scale
  END SUBROUTINE obj_Add14
END INTERFACE Add

!----------------------------------------------------------------------------
!                                                             Add@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-12-16
! summary:  obj = obj + scale * value
!
!# Introduction
!
! Add a csrmatrix to another csrmatrix

INTERFACE Add
  MODULE SUBROUTINE obj_Add15(obj, VALUE, scale, isSameStructure,  &
    & isSorted)
    TYPE(CSRMatrix_), INTENT(INOUT) :: obj
    !! CSRMatrix_
    TYPE(CSRMatrix_), INTENT(IN) :: VALUE
    !! CSRMatrix to add to obj
    REAL(DFP), INTENT(IN) :: scale
    !! scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isSameStructure
    !! If obj and value has same structure
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isSorted
    !! True if the matrix is sorted.
  END SUBROUTINE obj_Add15
END INTERFACE Add

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE CSRMatrix_AddMethods
