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

PUBLIC :: getStorageFMT
PUBLIC :: OPERATOR(.storageFMT.)
PUBLIC :: OPERATOR(.MatrixProp.)
PUBLIC :: getMatrixProp
PUBLIC :: getDOFPointer
PUBLIC :: isSquare
PUBLIC :: isRectangle

!----------------------------------------------------------------------------
!                                                  getStorageFMT@getMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION csrMat_getStorageFMT(obj, i) RESULT(Ans)
    TYPE(CSRMatrix_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: i
    INTEGER(I4B) :: ans
  END FUNCTION csrMat_getStorageFMT
END INTERFACE

INTERFACE getStorageFMT
  MODULE PROCEDURE csrMat_getStorageFMT
END INTERFACE getStorageFMT

INTERFACE OPERATOR(.storageFMT.)
  MODULE PROCEDURE csrMat_getStorageFMT
END INTERFACE OPERATOR(.storageFMT.)

!----------------------------------------------------------------------------
!                                                   getMatrixProp@getMethod
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION csrMat_getMatrixProp(obj) RESULT(Ans)
    TYPE(CSRMatrix_), TARGET, INTENT(IN) :: obj
    CHARACTER(20) :: ans
  END FUNCTION csrMat_getMatrixProp
END INTERFACE

INTERFACE OPERATOR(.MatrixProp.)
  MODULE PROCEDURE csrMat_getMatrixProp
END INTERFACE OPERATOR(.MatrixProp.)

INTERFACE getMatrixProp
  MODULE PROCEDURE csrMat_getMatrixProp
END INTERFACE getMatrixProp

!----------------------------------------------------------------------------
!                                                  getDOFPointer@getMethod
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION csrMat_getDOFPointer(obj, i) RESULT(Ans)
    TYPE(CSRMatrix_), TARGET, INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: i
    CLASS(DOF_), POINTER :: Ans
  END FUNCTION csrMat_getDOFPointer
END INTERFACE

INTERFACE getDOFPointer
  MODULE PROCEDURE csrMat_getDOFPointer
END INTERFACE getDOFPointer

!----------------------------------------------------------------------------
!                                                          isSquare@getMethod
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION csrMat_isSquare(obj) RESULT(Ans)
    TYPE(CSRMatrix_), INTENT(IN) :: obj
    LOGICAL(LGT) :: ans
  END FUNCTION csrMat_isSquare
END INTERFACE

INTERFACE isSquare
  MODULE PROCEDURE csrMat_isSquare
END INTERFACE isSquare

!----------------------------------------------------------------------------
!                                                    isRectangle@getMethod
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION csrMat_isRectangle(obj) RESULT(Ans)
    TYPE(CSRMatrix_), INTENT(IN) :: obj
    LOGICAL(LGT) :: ans
  END FUNCTION csrMat_isRectangle
END INTERFACE

INTERFACE isRectangle
  MODULE PROCEDURE csrMat_isRectangle
END INTERFACE isRectangle

END MODULE CSRMatrix_GetMethods
