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

MODULE CSRMatrix_SetMethods
USE GlobalData, ONLY: I4B, DFP, LGT
USE BaSetype, ONLY: CSRMatrix_
IMPLICIT NONE

PRIVATE
PUBLIC :: Set
PUBLIC :: SetSingleValue
PUBLIC :: ASSIGNMENT(=)
PUBLIC :: SetIA, SetJA

!----------------------------------------------------------------------------
!                                                            Set@setMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-12-14
! summary: This subroutine sets the single value

INTERFACE SetSingleValue
  MODULE PURE SUBROUTINE obj_SetSingleValue(obj, indx, VALUE)
    TYPE(CSRMatrix_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: indx
    REAL(DFP), INTENT(IN) :: VALUE
  END SUBROUTINE obj_SetSingleValue
END INTERFACE SetSingleValue

!----------------------------------------------------------------------------
!                                                            Set@setMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         22 March 2021
! summary: This subroutine Set the value in sparse matrix
!
!# Introduction
!
! - This subroutine Sets the value in [[CSRMatrix_]]
! - Shape( value ) = [SIZE(nodenum)*tdof, SIZE(nodenum)*tdof]
! - Usually `value` denotes the element matrix
! - Symbolic we are performing following task `obj(nodenum, nodenum)=value`

INTERFACE Set
  MODULE PURE SUBROUTINE obj_Set0(obj, nodenum, VALUE)
    TYPE(CSRMatrix_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: nodenum(:)
    REAL(DFP), INTENT(IN) :: VALUE(:, :)
  END SUBROUTINE obj_Set0
END INTERFACE Set

!----------------------------------------------------------------------------
!                                                            Set@setMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         22 March 2021
! summary: This subroutine Set the value in sparse matrix
!
!# Introduction
!
! This subroutine Sets the values in sparse matrix.
!
!$$
! obj(Nptrs,Nptrs)=value(:,:)
!$$
!
! - Usually `value(:,:)` represents the element finite element matrix
! - The shape of `value` should be the tdof*size(nodenum), tdof*size(nodenum)
! - `tdof` is the total degree of freedom in obj%csr%dof
!
! - `StorageFMT` denotes the storage format of `value`
! It can be `Nodes_FMT` or `DOF_FMT`
!
! - Usually, element matrix is stored with `DOF_FMT`

INTERFACE Set
  MODULE PURE SUBROUTINE obj_Set1(obj, nodenum, VALUE, storageFMT)
    TYPE(CSRMatrix_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: nodenum(:)
    REAL(DFP), INTENT(IN) :: VALUE(:, :)
    INTEGER(I4B), INTENT(IN) :: storageFMT
  END SUBROUTINE obj_Set1
END INTERFACE Set

!----------------------------------------------------------------------------
!                                                             Set@setMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         22 March 2021
! summary:         Sets all values of sparse matrix to given scalar value
!
!# Introduction
! This routine Sets all values of sparse matrix to given value.
! This routine is used to define an assignment operator. Therefore, we can
! call this routine by `obj=value`.

INTERFACE Set
  MODULE PURE SUBROUTINE obj_Set2(obj, VALUE)
    TYPE(CSRMatrix_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: VALUE
  END SUBROUTINE obj_Set2
END INTERFACE Set

INTERFACE ASSIGNMENT(=)
  MODULE PROCEDURE obj_Set2
END INTERFACE ASSIGNMENT(=)

!----------------------------------------------------------------------------
!                                                             Set@setMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         22 March 2021
! summary: Sets a single entry of sparse matrix
!
!# Introduction
!
! - This subroutine Sets a single entry of sparse matrix.
! - Before using this routine the user should be aware of the storage
! pattern of degree of freedom.
! - However, if total number of degrees of freedom is one then there is not
! need to worry.
!
!@warning
! This routine should be avoided by general user.
!@endwarning

INTERFACE Set
  MODULE PURE SUBROUTINE obj_Set3(obj, irow, icolumn, VALUE)
    TYPE(CSRMatrix_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: irow
    !! row index
    INTEGER(I4B), INTENT(IN) :: icolumn
    !! column index
    REAL(DFP), INTENT(IN) :: VALUE
    !! value
  END SUBROUTINE obj_Set3
END INTERFACE Set

!----------------------------------------------------------------------------
!                                                             Set@setMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         22 March 2021
! summary:         Sets the specific row and column entry to a given value
!
!# Introduction
!
! - This routine Sets the specific row and column entry to a given value.
! - The irow and icolumn index in [[CSRMatrix_]] are calculated by using
! (iNodeNum, iDOF) and (jNodeNum, jDOF), respectively.
! - To do the above task, the routine employs [[DOF_Method:getNodeLoc]] method
! - After computing the irow and icolumn (internally) this routine calls,
! `obj_Set3`.
!
!@note
! General user should prefer this routine over
! [[CSRMatrix_Method:obj_Set3]]
!@endnote
!
!@note
! rowdof, coldof are continuously numbered, so if there are two
! or more physical variables, then rowdof and coldof of the second
! or later physical variables will not start from 1.
!@endnote

INTERFACE Set
  MODULE PURE SUBROUTINE obj_Set4(obj, iNodeNum, jNodeNum, iDOF, &
    & jDOF, VALUE)
    TYPE(CSRMatrix_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: iNodeNum
    !! row node number
    INTEGER(I4B), INTENT(IN) :: jNodeNum
    !! column node number
    INTEGER(I4B), INTENT(IN) :: iDOF
    !! row degree of freedom
    INTEGER(I4B), INTENT(IN) :: jDOF
    !! col degree of freedom
    REAL(DFP), INTENT(IN) :: VALUE
    !! scalar value to be Set
  END SUBROUTINE obj_Set4
END INTERFACE Set

!----------------------------------------------------------------------------
!                                                            Set@setMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         22 March 2021
! summary: This subroutine Sets selected values in sparse matrix
!
!# Introduction
!
! This subroutine Sets selected values of the sparse matrix to the scalar
! value `value`
!
! This routine corresponds to `obj(nodenum) = value`

INTERFACE Set
  MODULE PURE SUBROUTINE obj_Set5(obj, nodenum, VALUE)
    TYPE(CSRMatrix_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: nodenum(:)
    REAL(DFP), INTENT(IN) :: VALUE
  END SUBROUTINE obj_Set5
END INTERFACE Set

!----------------------------------------------------------------------------
!                                                            Set@setMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         22 March 2021
! summary: This subroutine Set the value in sparse matrix
!
!# Introduction
!
! - This subroutine Sets the values in block sparse matrix.
! - The storage pattern of both sparse matrix and value
! (the element matrix) should be in `FMT_DOF`.
!
!$$
! obj(Nptrs,Nptrs)=value(:,:)
!$$
!

INTERFACE Set
  MODULE PURE SUBROUTINE obj_Set6(obj, iNodeNum, jNodeNum, &
    & ivar, jvar, VALUE)
    TYPE(CSRMatrix_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: iNodeNum(:)
    INTEGER(I4B), INTENT(IN) :: jNodeNum(:)
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: jvar
    REAL(DFP), INTENT(IN) :: VALUE(:, :)
  END SUBROUTINE obj_Set6
END INTERFACE Set

!----------------------------------------------------------------------------
!                                                             Set@setMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         22 March 2021
! summary:         Sets the specific row and column entry to a given value
!
!# Introduction
!
! - This routine Sets the specific row and column entry to a given value.
! - The irow and icolumn index in [[CSRMatrix_]] are calculated by using
! (iNodeNum, iDOF) and (jNodeNum, jDOF), respectively.
! - To do the above task, the routine employs [[DOF_Method:getNodeLoc]] method
! - After computing the irow and icolumn (internally) this routine calls,
! `obj_Set3`.
!
!@note
! General user should prefer this routine over
! [[CSRMatrix_Method:obj_Set3]]
!@endnote
!
!@note
! rowdof, coldof are continuously numbered, so if there are two
! or more physical variables, then rowdof and coldof of the second
! or later physical variables will not start from 1.
!@endnote

INTERFACE Set
  MODULE PURE SUBROUTINE obj_Set7(obj, iNodeNum, jNodeNum, ivar,  &
    & jvar, iDOF, jDOF, VALUE)
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
    !! scalar value to be Set
  END SUBROUTINE obj_Set7
END INTERFACE Set

!----------------------------------------------------------------------------
!                                                             Set@setMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         22 March 2021
! summary:         Sets the specific row and column entry to a given value

INTERFACE Set
  MODULE PURE SUBROUTINE obj_Set8(obj, iNodeNum, jNodeNum, ivar,  &
    & jvar, iDOF, jDOF, VALUE)
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
    !! scalar value to be Set
  END SUBROUTINE obj_Set8
END INTERFACE Set

!----------------------------------------------------------------------------
!                                                             Set@setMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         22 March 2021
! summary:         Sets the specific row and column entry to a given value
!
!# Introduction
!
! - This routine Sets the specific row and column entry to a given value.
! - The irow and icolumn index in [[CSRMatrix_]] are calculated by using
! (iNodeNum, iDOF) and (jNodeNum, jDOF), respectively.
! - To do the above task, the routine employs [[DOF_Method:getNodeLoc]] method
! - After computing the irow and icolumn (internally) this routine calls,
! `obj_Set3`.
!
!@note
! General user should prefer this routine over
! [[CSRMatrix_Method:obj_Set3]]
!@endnote
!
!@note
! rowdof, coldof are continuously numbered, so if there are two
! or more physical variables, then rowdof and coldof of the second
! or later physical variables will not start from 1.
!@endnote

INTERFACE Set
  MODULE PURE SUBROUTINE obj_Set9(obj, iNodeNum, jNodeNum, ivar,  &
    & jvar, ispacecompo, itimecompo, jspacecompo, jtimecompo, VALUE)
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
    !! col degree of freedom
    REAL(DFP), INTENT(IN) :: VALUE
    !! scalar value to be Set
  END SUBROUTINE obj_Set9
END INTERFACE Set

!----------------------------------------------------------------------------
!                                                            Set@setMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         22 March 2021
! summary: This subroutine Set the value in sparse matrix
!
!# Introduction
!
! - This subroutine Sets the values in block sparse matrix.
! - The storage pattern of both sparse matrix and value
! (the element matrix) should be in `FMT_DOF`.
!
!$$
! obj(Nptrs,Nptrs)=value(:,:)
!$$
!

INTERFACE Set
  MODULE PURE SUBROUTINE obj_Set10(obj, iNodeNum, jNodeNum, &
    & ivar, jvar, VALUE)
    TYPE(CSRMatrix_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: iNodeNum(:)
    INTEGER(I4B), INTENT(IN) :: jNodeNum(:)
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: jvar
    REAL(DFP), INTENT(IN) :: VALUE
  END SUBROUTINE obj_Set10
END INTERFACE Set

!----------------------------------------------------------------------------
!                                                             Set@setMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         22 March 2021
! summary:         Sets the specific row and column entry to a given value

INTERFACE Set
  MODULE PURE SUBROUTINE obj_Set11(obj, iNodeNum, jNodeNum, ivar,  &
    & jvar, iDOF, jDOF, VALUE)
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
    !! scalar value to be Set
  END SUBROUTINE obj_Set11
END INTERFACE Set

!----------------------------------------------------------------------------
!                                                             Set@setMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         22 March 2021
! summary:         Sets the specific row and column entry to a given value

INTERFACE Set
  MODULE PURE SUBROUTINE obj_Set12(obj, iNodeNum, jNodeNum, ivar,  &
    & jvar, ispacecompo, itimecompo, jspacecompo, jtimecompo, VALUE)
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
    !! scalar value to be Set
  END SUBROUTINE obj_Set12
END INTERFACE Set

!----------------------------------------------------------------------------
!                                                             Set@setMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         22 March 2021
! summary:         Sets the specific row and column entry to a given value

INTERFACE Set
  MODULE PURE SUBROUTINE obj_Set13(obj, iNodeNum, jNodeNum, ivar,  &
    & jvar, ispacecompo, itimecompo, jspacecompo, jtimecompo, VALUE)
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
    !! scalar value to be Set
  END SUBROUTINE obj_Set13
END INTERFACE Set

!----------------------------------------------------------------------------
!                                                             Set@setMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         22 March 2021
! summary:         Sets the specific row and column entry to a given value

INTERFACE Set
  MODULE PURE SUBROUTINE obj_Set14(obj, iNodeNum, jNodeNum, ivar,  &
    & jvar, ispacecompo, itimecompo, jspacecompo, jtimecompo, VALUE)
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
    !! scalar value to be Set
  END SUBROUTINE obj_Set14
END INTERFACE Set

!----------------------------------------------------------------------------
!                                                             Set@setMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-12-17
! summary: Scale the sparse matrix , obj = scale*Value

INTERFACE Set
  MODULE PURE SUBROUTINE obj_Set15(obj, VALUE, scale)
    TYPE(CSRMatrix_), INTENT(INOUT) :: obj
    TYPE(CSRMatrix_), INTENT(IN) :: VALUE
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    !! scale
  END SUBROUTINE obj_Set15
END INTERFACE Set

!----------------------------------------------------------------------------
!                                                        SetIA@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-12-14
! summary:  Set entry in IA

INTERFACE SetIA
  MODULE PURE SUBROUTINE obj_SetIA(obj, irow, VALUE)
    TYPE(CSRMatrix_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: irow
    INTEGER(I4B), INTENT(IN) :: VALUE
  END SUBROUTINE obj_SetIA
END INTERFACE SetIA

!----------------------------------------------------------------------------
!                                                        SetJA@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-12-14
! summary:  Set entry in JA

INTERFACE SetJA
  MODULE PURE SUBROUTINE obj_SetJA(obj, indx, VALUE)
    TYPE(CSRMatrix_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: indx
    INTEGER(I4B), INTENT(IN) :: VALUE
  END SUBROUTINE obj_SetJA
END INTERFACE SetJA

END MODULE CSRMatrix_SetMethods
