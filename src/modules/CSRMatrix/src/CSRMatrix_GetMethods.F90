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

MODULE CSRMatrix_GetMethods
USE GlobalData, ONLY: I4B, DFP, LGT
USE BaseType, ONLY: CSRMatrix_, DOF_
IMPLICIT NONE
PRIVATE

PUBLIC :: GetStorageFMT
PUBLIC :: OPERATOR(.storageFMT.)
PUBLIC :: OPERATOR(.MatrixProp.)
PUBLIC :: GetMatrixProp
PUBLIC :: GetDOFPointer
PUBLIC :: isSquare
PUBLIC :: isRectangle
PUBLIC :: GetColIndex
PUBLIC :: GetColNumber
PUBLIC :: OPERATOR(.startColumn.)
PUBLIC :: OPERATOR(.endColumn.)
PUBLIC :: GetSingleValue
PUBLIC :: Get
PUBLIC :: GetIA
PUBLIC :: GetJA
PUBLIC :: GetValue

!----------------------------------------------------------------------------
!                                                        GetIA@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-12-14
! summary:  Get entry in IA

INTERFACE GetIA
  MODULE PURE FUNCTION obj_GetIA(obj, irow) RESULT(ans)
    TYPE(CSRMatrix_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: irow
    INTEGER(I4B) :: ans
  END FUNCTION obj_GetIA
END INTERFACE GetIA

!----------------------------------------------------------------------------
!                                                        GetJA@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-12-14
! summary:  Get entry in JA

INTERFACE GetJA
  MODULE PURE FUNCTION obj_GetJA(obj, indx) RESULT(ans)
    TYPE(CSRMatrix_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: indx
    INTEGER(I4B) :: ans
  END FUNCTION obj_GetJA
END INTERFACE GetJA

!----------------------------------------------------------------------------
!                                                   GetSingleValue
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-12-14
! summary:  Get single value

INTERFACE GetSingleValue
  MODULE PURE FUNCTION obj_GetSingleValue(obj, indx) RESULT(ans)
    TYPE(CSRMatrix_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: indx
    REAL(DFP) :: ans
  END FUNCTION obj_GetSingleValue
END INTERFACE GetSingleValue

INTERFACE Get
  MODULE PROCEDURE obj_GetSingleValue
END INTERFACE Get

!----------------------------------------------------------------------------
!                                                   GetSingleValue
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-12-14
! summary:  Get single value

INTERFACE GetSeveralValue
  MODULE PURE FUNCTION obj_GetSeveralValue(obj, indx) RESULT(ans)
    TYPE(CSRMatrix_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: indx(:)
    REAL(DFP) :: ans(SIZE(indx))
  END FUNCTION obj_GetSeveralValue
END INTERFACE GetSeveralValue

INTERFACE Get
  MODULE PROCEDURE obj_GetSeveralValue
END INTERFACE Get

!----------------------------------------------------------------------------
!                                                  GetStorageFMT@getMethods
!----------------------------------------------------------------------------

INTERFACE GetStorageFMT
  MODULE PURE FUNCTION obj_GetStorageFMT(obj, i) RESULT(ans)
    TYPE(CSRMatrix_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: i
    INTEGER(I4B) :: ans
  END FUNCTION obj_GetStorageFMT
END INTERFACE GetStorageFMT

INTERFACE OPERATOR(.storageFMT.)
  MODULE PROCEDURE obj_GetStorageFMT
END INTERFACE OPERATOR(.storageFMT.)

!----------------------------------------------------------------------------
!                                                   GetMatrixProp@getMethod
!----------------------------------------------------------------------------

INTERFACE GetMatrixProp
  MODULE PURE FUNCTION obj_GetMatrixProp(obj) RESULT(ans)
    TYPE(CSRMatrix_), TARGET, INTENT(IN) :: obj
    CHARACTER(20) :: ans
  END FUNCTION obj_GetMatrixProp
END INTERFACE GetMatrixProp

INTERFACE OPERATOR(.MatrixProp.)
  MODULE PROCEDURE obj_GetMatrixProp
END INTERFACE OPERATOR(.MatrixProp.)

!----------------------------------------------------------------------------
!                                                  GetDOFPointer@getMethod
!----------------------------------------------------------------------------

INTERFACE GetDOFPointer
  MODULE FUNCTION obj_GetDOFPointer(obj, i) RESULT(ans)
    TYPE(CSRMatrix_), TARGET, INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: i
    CLASS(DOF_), POINTER :: ans
  END FUNCTION obj_GetDOFPointer
END INTERFACE GetDOFPointer

!----------------------------------------------------------------------------
!                                                          isSquare@GetMethod
!----------------------------------------------------------------------------

INTERFACE isSquare
  MODULE PURE FUNCTION obj_isSquare(obj) RESULT(ans)
    TYPE(CSRMatrix_), INTENT(IN) :: obj
    LOGICAL(LGT) :: ans
  END FUNCTION obj_isSquare
END INTERFACE isSquare

!----------------------------------------------------------------------------
!                                                    isRectangle@GetMethod
!----------------------------------------------------------------------------

INTERFACE isRectangle
  MODULE PURE FUNCTION obj_isRectangle(obj) RESULT(ans)
    TYPE(CSRMatrix_), INTENT(IN) :: obj
    LOGICAL(LGT) :: ans
  END FUNCTION obj_isRectangle
END INTERFACE isRectangle

!----------------------------------------------------------------------------
!                                                   GetColNumber@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-12-14
! summary:  Get the column number from JA.

INTERFACE GetColNumber
  MODULE PURE FUNCTION obj_GetColNumber(obj, indx) RESULT(ans)
    TYPE(CSRMatrix_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: indx
    INTEGER(I4B) :: ans
  END FUNCTION obj_GetColNumber
END INTERFACE GetColNumber

!----------------------------------------------------------------------------
!                                                     GetColIndex@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-12-14
! summary:  Get the starting  and ending column index of irow

INTERFACE GetColIndex
  MODULE PURE FUNCTION obj_GetColIndex(obj, irow) RESULT(ans)
    TYPE(CSRMatrix_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: irow
    INTEGER(I4B) :: ans(2)
  END FUNCTION obj_GetColIndex
END INTERFACE GetColIndex

!----------------------------------------------------------------------------
!                                                     startColumn@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-12-14
! summary:  Get the starting column index of irow

INTERFACE OPERATOR(.startColumn.)
  MODULE PURE FUNCTION obj_startColumn(obj, irow) RESULT(ans)
    TYPE(CSRMatrix_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: irow
    INTEGER(I4B) :: ans
  END FUNCTION obj_startColumn
END INTERFACE OPERATOR(.startColumn.)

!----------------------------------------------------------------------------
!                                                        endColumn@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-12-14
! summary:  Get the ending column index of irow

INTERFACE OPERATOR(.endColumn.)
  MODULE PURE FUNCTION obj_endColumn(obj, irow) RESULT(ans)
    TYPE(CSRMatrix_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: irow
    INTEGER(I4B) :: ans
  END FUNCTION obj_endColumn
END INTERFACE OPERATOR(.endColumn.)

!----------------------------------------------------------------------------
!                                                            Get@getMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-12-23
! summary: This subroutine Get the value in sparse matrix
!
!# Introduction
!
! - This subroutine Gets the value in [[CSRMatrix_]]
! - Shape( value ) = [SIZE(nodenum)*tdof, SIZE(nodenum)*tdof]
! - Usually `value` denotes the element matrix
! - Symbolic we are performing following task `obj(nodenum, nodenum)=value`

INTERFACE GetValue
  MODULE PURE SUBROUTINE obj_Get0(obj, nodenum, VALUE)
    TYPE(CSRMatrix_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: nodenum(:)
    REAL(DFP), INTENT(INOUT) :: VALUE(:, :)
  END SUBROUTINE obj_Get0
END INTERFACE GetValue

!----------------------------------------------------------------------------
!                                                            Get@getMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:         22 March 2021
! summary: This subroutine Get the value in sparse matrix
!
!# Introduction
!
! This subroutine Gets the values in sparse matrix.
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

INTERFACE GetValue
  MODULE PURE SUBROUTINE obj_Get1(obj, nodenum, storageFMT, VALUE)
    TYPE(CSRMatrix_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: nodenum(:)
    REAL(DFP), INTENT(INOUT) :: VALUE(:, :)
    INTEGER(I4B), INTENT(IN) :: storageFMT
    !! storage format of value (desired format of value)
  END SUBROUTINE obj_Get1
END INTERFACE GetValue

!----------------------------------------------------------------------------
!                                                             Get@getMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 22 March 2021
! summary: Gets a single entry of sparse matrix
!
!# Introduction
!
! - This subroutine Gets a single entry of sparse matrix.
! - Before using this routine the user should be aware of the storage
! pattern of degree of freedom.
! - However, if total number of degrees of freedom is one then there is not
! need to worry.
!
!@warning
! This routine should be avoided by general user.
!@endwarning

INTERFACE GetValue
  MODULE PURE SUBROUTINE obj_Get2(obj, irow, icolumn, VALUE)
    TYPE(CSRMatrix_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: irow
    !! row index
    INTEGER(I4B), INTENT(IN) :: icolumn
    !! column index
    REAL(DFP), INTENT(INOUT) :: VALUE
    !! value
  END SUBROUTINE obj_Get2
END INTERFACE GetValue

INTERFACE GetValue
  MODULE PURE SUBROUTINE obj_Get10(obj, irow, icolumn, VALUE)
    TYPE(CSRMatrix_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: irow(:)
    !! row index
    INTEGER(I4B), INTENT(IN) :: icolumn(:)
    !! column index
    REAL(DFP), INTENT(INOUT) :: VALUE(:, :)
    !! value
  END SUBROUTINE obj_Get10
END INTERFACE GetValue

!----------------------------------------------------------------------------
!                                                             Get@getMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-12-23
! summary:  Gets the specific row and column entry to a given value
!
!# Introduction
!
! - This routine Gets the specific row and column entry to a given value.
! - The irow and icolumn index in [[CSRMatrix_]] are calculated by using
! (iNodeNum, iDOF) and (jNodeNum, jDOF), respectively.
! - To do the above task, the routine employs [[DOF_Method:getNodeLoc]]
! method
! - After computing the irow and icolumn (internally) this routine calls,
! `obj_Get3`.
!
!@note
! General user should prefer this routine over
! [[CSRMatrix_Method:obj_Get2]]
!@endnote
!
!@note
! idof, jdof are continuously numbered, so if there are two
! or more physical variables, then idof and jdof of the second
! or later physical variables will not start from 1.
!@endnote

INTERFACE GetValue
  MODULE PURE SUBROUTINE obj_Get3(obj, iNodeNum, jNodeNum, iDOF, &
    & jDOF, VALUE)
    TYPE(CSRMatrix_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: iNodeNum
    !! row node number
    INTEGER(I4B), INTENT(IN) :: jNodeNum
    !! column node number
    INTEGER(I4B), INTENT(IN) :: iDOF
    !! row degree of freedom
    INTEGER(I4B), INTENT(IN) :: jDOF
    !! col degree of freedom
    REAL(DFP), INTENT(INOUT) :: VALUE
    !! scalar value to be Get
  END SUBROUTINE obj_Get3
END INTERFACE GetValue

!----------------------------------------------------------------------------
!                                                            Get@getMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-12-23
! summary: This subroutine get the value from the sparse matrix
!
!# Introduction
!
! - This subroutine Gets the values from block sparse matrix.
! - The storage pattern of both sparse matrix and value
! (the element matrix) should be in `FMT_DOF`.
!
!$$
! obj(Nptrs,Nptrs)=value(:,:)
!$$

INTERFACE GetValue
  MODULE PURE SUBROUTINE obj_Get4(obj, iNodeNum, jNodeNum, &
    & ivar, jvar, VALUE)
    TYPE(CSRMatrix_), INTENT(IN) :: obj
    !! Block csr matrix
    INTEGER(I4B), INTENT(IN) :: iNodeNum(:)
    !! row node numbers
    INTEGER(I4B), INTENT(IN) :: jNodeNum(:)
    !! column node numbers
    INTEGER(I4B), INTENT(IN) :: ivar
    !! row physical variables
    INTEGER(I4B), INTENT(IN) :: jvar
    !! column physical variables
    REAL(DFP), INTENT(INOUT) :: VALUE(:, :)
    !! value
  END SUBROUTINE obj_Get4
END INTERFACE GetValue

!----------------------------------------------------------------------------
!                                                             Get@getMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-12-23
! summary:  Gets the specific row and column entry to a given value
!
!# Introduction
!
! - This routine Gets the specific row and column entry to a given value.
! - The irow and icolumn index in [[CSRMatrix_]] are calculated by using
! (iNodeNum, iDOF) and (jNodeNum, jDOF), respectively.
! - To do the above task, routine employs [[DOF_Method:getNodeLoc]] method
! - After computing the irow and icolumn (internally) this routine calls,
! `obj_Get3`.
!
!@note
! General user should prefer this routine over
! [[CSRMatrix_Method:obj_Get3]]
!@endnote
!
!@note
! rowdof, coldof are continuously numbered, so if there are two
! or more physical variables, then rowdof and coldof of the second
! or later physical variables will not start from 1.
!@endnote

INTERFACE GetValue
  MODULE PURE SUBROUTINE obj_Get5(obj, iNodeNum, jNodeNum, ivar,  &
    & jvar, iDOF, jDOF, VALUE)
    TYPE(CSRMatrix_), INTENT(IN) :: obj
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
    REAL(DFP), INTENT(INOUT) :: VALUE
    !! scalar value to be Get
  END SUBROUTINE obj_Get5
END INTERFACE GetValue

!----------------------------------------------------------------------------
!                                                             Get@getMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-12-23
! summary:  Gets the specific row and column entry from a given value

INTERFACE GetValue
  MODULE PURE SUBROUTINE obj_Get6(obj, iNodeNum, jNodeNum, ivar,  &
    & jvar, iDOF, jDOF, VALUE)
    TYPE(CSRMatrix_), INTENT(IN) :: obj
    !! block matrix field
    INTEGER(I4B), INTENT(IN) :: iNodeNum(:)
    !! row node number
    INTEGER(I4B), INTENT(IN) :: jNodeNum(:)
    !! column node number
    INTEGER(I4B), INTENT(IN) :: ivar
    !! row physical variables
    INTEGER(I4B), INTENT(IN) :: jvar
    !! column physical variable
    INTEGER(I4B), INTENT(IN) :: iDOF
    !! row degree of freedom
    INTEGER(I4B), INTENT(IN) :: jDOF
    !! col degree of freedom
    REAL(DFP), INTENT(INOUT) :: VALUE(:, :)
    !! Matrix value
  END SUBROUTINE obj_Get6
END INTERFACE GetValue

!----------------------------------------------------------------------------
!                                                             Get@getMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-12-23
! summary: Gets the specific row and column entry from the matrix
!
!# Introduction
!
! - This routine Gets the specific row and column entry from the matrix.
! - The irow and icolumn index in `CSRMatrix_` are calculated by using
! (iNodeNum, iDOF) and (jNodeNum, jDOF), respectively.
! - To do above task, the routine employs [[DOF_Method:getNodeLoc]] method
! - After computing the irow and icolumn (internally) this routine calls,
! `obj_Get3`.
!
!@note
! General user should prefer this routine over
! [[CSRMatrix_Method:obj_Get3]]
!@endnote
!
!@note
! rowdof, coldof are continuously numbered, so if there are two
! or more physical variables, then rowdof and coldof of the second
! or later physical variables will not start from 1.
!@endnote

INTERFACE GetValue
  MODULE PURE SUBROUTINE obj_Get7(obj, iNodeNum, jNodeNum, ivar,  &
    & jvar, ispacecompo, itimecompo, jspacecompo, jtimecompo, VALUE)
    TYPE(CSRMatrix_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: iNodeNum
    !! row node number
    INTEGER(I4B), INTENT(IN) :: jNodeNum
    !! column node number
    INTEGER(I4B), INTENT(IN) :: ivar
    !! row physical variable
    INTEGER(I4B), INTENT(IN) :: jvar
    !! col physical variable
    INTEGER(I4B), INTENT(IN) :: ispacecompo
    !! row space component
    INTEGER(I4B), INTENT(IN) :: itimecompo
    !! row time component
    INTEGER(I4B), INTENT(IN) :: jspacecompo
    !! col space component
    INTEGER(I4B), INTENT(IN) :: jtimecompo
    !! col time component
    REAL(DFP), INTENT(INOUT) :: VALUE
    !! scalar value to be Get
  END SUBROUTINE obj_Get7
END INTERFACE GetValue

!----------------------------------------------------------------------------
!                                                           GetValue
!----------------------------------------------------------------------------

INTERFACE GetValue
  MODULE PURE SUBROUTINE obj_Get9(obj, iNodeNum, jNodeNum, ivar,  &
    & jvar, ispacecompo, itimecompo, jspacecompo, jtimecompo, VALUE)
    TYPE(CSRMatrix_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: iNodeNum(:)
    !! row node number
    INTEGER(I4B), INTENT(IN) :: jNodeNum(:)
    !! column node number
    INTEGER(I4B), INTENT(IN) :: ivar
    !! row physical variable
    INTEGER(I4B), INTENT(IN) :: jvar
    !! col physical variable
    INTEGER(I4B), INTENT(IN) :: ispacecompo
    !! row space component
    INTEGER(I4B), INTENT(IN) :: itimecompo
    !! row time component
    INTEGER(I4B), INTENT(IN) :: jspacecompo
    !! col space component
    INTEGER(I4B), INTENT(IN) :: jtimecompo
    !! col time component
    REAL(DFP), INTENT(INOUT) :: VALUE(:, :)
    !! scalar value to be Get
  END SUBROUTINE obj_Get9
END INTERFACE GetValue

!----------------------------------------------------------------------------
!                                                             Get@GetMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-12-23
! summary: Gets the specific row and column entry from the matrix
!
!# Introduction
!
! - The number of nodes in obj1 and obj2 should be same

INTERFACE GetValue
  MODULE SUBROUTINE obj_Get8(obj1, obj2,  &
    & ivar1, jvar1,  &
    & ispacecompo1, jspacecompo1, &
    & itimecompo1, jtimecompo1, &
    & ivar2, jvar2,  &
    & ispacecompo2, jspacecompo2, &
    & itimecompo2, jtimecompo2, ierr)
    TYPE(CSRMatrix_), INTENT(IN) :: obj1
    !! master object
    TYPE(CSRMatrix_), INTENT(INOUT) :: obj2
    !! slave object
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: ivar1
    !! row physical variable obj1
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: jvar1
    !! col physical variable obj1
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: ispacecompo1
    !! row space component obj1
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: itimecompo1
    !! row time component obj1
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: jspacecompo1
    !! col space component obj1
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: jtimecompo1
    !! col time component obj1
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: ivar2
    !! row physical variable obj2
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: jvar2
    !! col physical variable obj2
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: ispacecompo2
    !! row space component obj2
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: itimecompo2
    !! row time component obj2
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: jspacecompo2
    !! col space component obj2
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: jtimecompo2
    !! col time component obj2
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: ierr
    !! Error code, if 0 no error, else error
  END SUBROUTINE obj_Get8
END INTERFACE GetValue

!----------------------------------------------------------------------------
!                                                             Get@GetMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-12-23
! summary: Gets the specific row and column entry from the matrix
!
!# Introduction
!
! - The number of nodes in obj1 and obj2 should be same

INTERFACE
  MODULE SUBROUTINE CSR2CSR_Get_Master(obj1, obj2, idof1, jdof1, idof2,  &
    & jdof2, tNodes1, tNodes2)
    TYPE(CSRMatrix_), INTENT(IN) :: obj1
    !! master object
    TYPE(CSRMatrix_), INTENT(INOUT) :: obj2
    !! slave object
    INTEGER(I4B), INTENT(IN) :: idof1
    !! row space component obj1
    INTEGER(I4B), INTENT(IN) :: jdof1
    !! row time component obj1
    INTEGER(I4B), INTENT(IN) :: idof2
    !! col space component obj1
    INTEGER(I4B), INTENT(IN) :: jdof2
    !! col time component obj1
    INTEGER(I4B), INTENT(IN) :: tNodes1
    INTEGER(I4B), INTENT(IN) :: tNodes2
  END SUBROUTINE CSR2CSR_Get_Master
END INTERFACE

END MODULE CSRMatrix_GetMethods
