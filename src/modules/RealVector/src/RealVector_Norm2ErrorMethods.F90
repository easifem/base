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

MODULE RealVector_Norm2ErrorMethods
USE GlobalData, ONLY: I4B, DFP
USE BaseType, ONLY: RealVector_, DOF_

IMPLICIT NONE
PRIVATE

PUBLIC :: Norm2Error

!----------------------------------------------------------------------------
!                                                                Norm2Error
!----------------------------------------------------------------------------

INTERFACE Norm2Error
  MODULE PURE FUNCTION obj_norm2error_1(obj, dofobj, ivarobj, &
                       idofobj, obj2, dofobj2, ivarobj2, idofobj2) RESULT(ans)
    CLASS(RealVector_), INTENT(IN) :: obj
    TYPE(DOF_), INTENT(IN) :: dofobj
    INTEGER(I4B), INTENT(IN) :: ivarobj
    INTEGER(I4B), INTENT(IN) :: idofobj
    CLASS(RealVector_), INTENT(IN) :: obj2
    TYPE(DOF_), INTENT(IN) :: dofobj2
    INTEGER(I4B), INTENT(IN) :: ivarobj2
    INTEGER(I4B), INTENT(IN) :: idofobj2
    REAL(DFP) :: ans
  END FUNCTION obj_norm2error_1
END INTERFACE Norm2Error

!----------------------------------------------------------------------------
!                                                                Norm2Error
!----------------------------------------------------------------------------

INTERFACE Norm2Error
  MODULE PURE FUNCTION obj_norm2error_2(obj, dofobj, ivarobj, &
                       idofobj, obj2, dofobj2, ivarobj2, idofobj2) RESULT(ans)
    CLASS(RealVector_), INTENT(IN) :: obj
    TYPE(DOF_), INTENT(IN) :: dofobj
    INTEGER(I4B), INTENT(IN) :: ivarobj
    INTEGER(I4B), INTENT(IN) :: idofobj(:)
    CLASS(RealVector_), INTENT(IN) :: obj2
    TYPE(DOF_), INTENT(IN) :: dofobj2
    INTEGER(I4B), INTENT(IN) :: ivarobj2
    INTEGER(I4B), INTENT(IN) :: idofobj2(:)
    REAL(DFP) :: ans
  END FUNCTION obj_norm2error_2
END INTERFACE Norm2Error

!----------------------------------------------------------------------------
!                                                                Norm2Error
!----------------------------------------------------------------------------

INTERFACE Norm2Error
  MODULE PURE FUNCTION obj_norm2error_3(obj, dofobj, idofobj, obj2, &
                                        dofobj2, idofobj2) RESULT(ans)
    CLASS(RealVector_), INTENT(IN) :: obj
    TYPE(DOF_), INTENT(IN) :: dofobj
    INTEGER(I4B), INTENT(IN) :: idofobj
    CLASS(RealVector_), INTENT(IN) :: obj2
    TYPE(DOF_), INTENT(IN) :: dofobj2
    INTEGER(I4B), INTENT(IN) :: idofobj2
    REAL(DFP) :: ans
  END FUNCTION obj_norm2error_3
END INTERFACE Norm2Error

!----------------------------------------------------------------------------
!                                                                Norm2Error
!----------------------------------------------------------------------------

INTERFACE Norm2Error
  MODULE PURE FUNCTION obj_norm2error_4(obj, dofobj, idofobj, obj2, &
                                        dofobj2, idofobj2) RESULT(ans)
    CLASS(RealVector_), INTENT(IN) :: obj
    TYPE(DOF_), INTENT(IN) :: dofobj
    INTEGER(I4B), INTENT(IN) :: idofobj(:)
    CLASS(RealVector_), INTENT(IN) :: obj2
    TYPE(DOF_), INTENT(IN) :: dofobj2
    INTEGER(I4B), INTENT(IN) :: idofobj2(:)
    REAL(DFP) :: ans
  END FUNCTION obj_norm2error_4
END INTERFACE Norm2Error

!----------------------------------------------------------------------------
!                                                                Norm2Error
!----------------------------------------------------------------------------

INTERFACE Norm2Error
  MODULE PURE FUNCTION obj_norm2error_5(obj, dofobj, ivarobj, &
                       spaceCompoObj, timeCompoObj, obj2, dofobj2, ivarobj2, &
                                    spaceCompoobj2, timeCompoobj2) RESULT(ans)
    CLASS(RealVector_), INTENT(IN) :: obj
    TYPE(DOF_), INTENT(IN) :: dofobj
    INTEGER(I4B), INTENT(IN) :: ivarobj
    INTEGER(I4B), INTENT(IN) :: spaceCompoObj
    INTEGER(I4B), INTENT(IN) :: timeCompoObj
    CLASS(RealVector_), INTENT(IN) :: obj2
    TYPE(DOF_), INTENT(IN) :: dofobj2
    INTEGER(I4B), INTENT(IN) :: ivarobj2
    INTEGER(I4B), INTENT(IN) :: spaceCompoobj2
    INTEGER(I4B), INTENT(IN) :: timeCompoobj2
    REAL(DFP) :: ans
  END FUNCTION obj_norm2error_5
END INTERFACE Norm2Error

!----------------------------------------------------------------------------
!                                                                Norm2Error
!----------------------------------------------------------------------------

INTERFACE Norm2Error
  MODULE PURE FUNCTION obj_norm2error_6(obj, dofobj, ivarobj, &
                       spaceCompoObj, timeCompoObj, obj2, dofobj2, ivarobj2, &
                                    spaceCompoobj2, timeCompoobj2) RESULT(ans)
    CLASS(RealVector_), INTENT(IN) :: obj
    TYPE(DOF_), INTENT(IN) :: dofobj
    INTEGER(I4B), INTENT(IN) :: ivarobj
    INTEGER(I4B), INTENT(IN) :: spaceCompoObj
    INTEGER(I4B), INTENT(IN) :: timeCompoObj(:)
    CLASS(RealVector_), INTENT(IN) :: obj2
    TYPE(DOF_), INTENT(IN) :: dofobj2
    INTEGER(I4B), INTENT(IN) :: ivarobj2
    INTEGER(I4B), INTENT(IN) :: spaceCompoobj2
    INTEGER(I4B), INTENT(IN) :: timeCompoobj2(:)
    REAL(DFP) :: ans
  END FUNCTION obj_norm2error_6
END INTERFACE Norm2Error

!----------------------------------------------------------------------------
!                                                                Norm2Error
!----------------------------------------------------------------------------

INTERFACE Norm2Error
  MODULE PURE FUNCTION obj_norm2error_7(obj, dofobj, ivarobj, &
                       spaceCompoObj, timeCompoObj, obj2, dofobj2, ivarobj2, &
                                    spaceCompoobj2, timeCompoobj2) RESULT(ans)
    CLASS(RealVector_), INTENT(IN) :: obj
    TYPE(DOF_), INTENT(IN) :: dofobj
    INTEGER(I4B), INTENT(IN) :: ivarobj
    INTEGER(I4B), INTENT(IN) :: spaceCompoObj(:)
    INTEGER(I4B), INTENT(IN) :: timeCompoObj
    CLASS(RealVector_), INTENT(IN) :: obj2
    TYPE(DOF_), INTENT(IN) :: dofobj2
    INTEGER(I4B), INTENT(IN) :: ivarobj2
    INTEGER(I4B), INTENT(IN) :: spaceCompoobj2(:)
    INTEGER(I4B), INTENT(IN) :: timeCompoobj2
    REAL(DFP) :: ans
  END FUNCTION obj_norm2error_7
END INTERFACE Norm2Error

END MODULE RealVector_Norm2ErrorMethods
