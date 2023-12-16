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

INTERFACE Get
  MODULE PURE FUNCTION obj_GetSeveralValue(obj, indx) RESULT(ans)
    TYPE(CSRMatrix_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: indx(:)
    REAL(DFP) :: ans(SIZE(indx))
  END FUNCTION obj_GetSeveralValue
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

END MODULE CSRMatrix_GetMethods
