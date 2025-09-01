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
!
! This file contains the interpolation methods interfaces\

MODULE ElemshapeData_InterpolMethods
USE GlobalData, ONLY: DFP, I4B, LGT
USE BaseType, ONLY: ElemShapeData_, STElemShapeData_, FEVariable_
IMPLICIT NONE
PRIVATE

PUBLIC :: GetInterpolation
PUBLIC :: Interpolation

!----------------------------------------------------------------------------
!                                                   GetInterpolation@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 4 March 2021
! summary: returns the interpolation of a FEVariable
!
!# Introduction
!
! - Returns the interpolation of a [[fevariable_]]
! - The result is returned in interpol
! - interpol is a FEVariable
! - The rank of interpol is same as the rank of val
! - interpol is defined on Quadrature, that is, interpol is QuadratureVariable
!
! - The val can have following ranks; scalar, vector, matrix
! - the val can be defined on quadrature (do nothing) or nodal (interpol)
! - The `vartype` of val can be constant, space, time, spacetime
!
INTERFACE GetInterpolation
  MODULE PURE SUBROUTINE GetInterpolation1(obj, ans, val)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    TYPE(FEVariable_), INTENT(INOUT) :: ans
    TYPE(FEVariable_), INTENT(IN) :: val
  END SUBROUTINE GetInterpolation1
END INTERFACE GetInterpolation

!----------------------------------------------------------------------------
!                                                   GetInterpolation@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 4 March 2021
! summary: returns the interpolation of a FEVariable
!
!# Introduction
!
! - Returns the interpolation of a FEVariable_
! - The result is returned in ans
! - ans is a FEVariable
! - The rank of ans is same as the rank of val
! - ans is defined on Quadrature, that is, ans is QuadratureVariable
!
! - The val can have following ranks; scalar, vector, matrix
! - the val can be defined on quadrature (do nothing) or nodal (interpol)
! - The `vartype` of val can be constant, space, time, spacetime
!
INTERFACE GetInterpolation_
  MODULE PURE SUBROUTINE GetInterpolation_1(obj, ans, val)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    TYPE(FEVariable_), INTENT(INOUT) :: ans
    TYPE(FEVariable_), INTENT(IN) :: val
  END SUBROUTINE GetInterpolation_1
END INTERFACE GetInterpolation_

!----------------------------------------------------------------------------
!                                                   GetInterpolation@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 4 March 2021
! summary: returns the interpolation of a FEVariable
!
!# Introduction
!
! - Returns the interpolation of a FEVariable_
! - The result is returned in ans
! - ans is a FEVariable
! - The rank of ans is same as the rank of val
! - ans is defined on Quadrature, that is, ans is QuadratureVariable
!
! - The val can have following ranks; scalar, vector, matrix
! - the val can be defined on quadrature (do nothing) or nodal (interpol)
! - The `vartype` of val can be constant, space, time, spacetime
!
INTERFACE GetInterpolation_
  MODULE PURE SUBROUTINE GetInterpolation_1a(obj, ans, val, scale, &
                                             addContribution)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    TYPE(FEVariable_), INTENT(INOUT) :: ans
    TYPE(FEVariable_), INTENT(IN) :: val
    REAL(DFP), INTENT(IN) :: scale
    LOGICAL, INTENT(IN) :: addContribution
  END SUBROUTINE GetInterpolation_1a
END INTERFACE GetInterpolation_

!----------------------------------------------------------------------------
!                                                    GetInterpolation@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 4 March 2021
! summary: returns the interpolation of a FEVariable
!
!# Introduction
!
! - Returns the interpolation of a [[fevariable_]]
! - The result is returned in interpol
! - interpol is a FEVariable
! - The rank of interpol is same as the rank of val
! - interpol is defined on Quadrature, that is, interpol is QuadratureVariable
!
! - The val can have following ranks; scalar, vector, matrix
! - the val can be defined on quadrature (do nothing) or nodal (interpol)
! - The `vartype` of val can be constant, space, time, spacetime
!
INTERFACE GetInterpolation
  MODULE PURE SUBROUTINE GetInterpolation2(obj, ans, val)
    CLASS(STElemshapeData_), INTENT(IN) :: obj(:)
    TYPE(FEVariable_), INTENT(INOUT) :: ans
    TYPE(FEVariable_), INTENT(IN) :: val
  END SUBROUTINE GetInterpolation2
END INTERFACE GetInterpolation

!----------------------------------------------------------------------------
!                                                    GetInterpolation@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 4 March 2021
! summary: returns the interpolation of a FEVariable
!
!# Introduction
!
! - Returns the interpolation of a [[fevariable_]]
! - The result is returned in interpol
! - interpol is a FEVariable
! - The rank of interpol is same as the rank of val
! - interpol is defined on Quadrature, that is, interpol is QuadratureVariable
!
! - The val can have following ranks; scalar, vector, matrix
! - the val can be defined on quadrature (do nothing) or nodal (interpol)
! - The `vartype` of val can be constant, space, time, spacetime
!
INTERFACE GetInterpolation_
  MODULE PURE SUBROUTINE GetInterpolation_2(obj, ans, val)
    CLASS(STElemshapeData_), INTENT(IN) :: obj(:)
    TYPE(FEVariable_), INTENT(INOUT) :: ans
    TYPE(FEVariable_), INTENT(IN) :: val
  END SUBROUTINE GetInterpolation_2
END INTERFACE GetInterpolation_

!----------------------------------------------------------------------------
!                                                    GetInterpolation@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-09-01
! summary: returns the interpolation of a FEVariable
!
!# Introduction
!
! - Returns the interpolation of a FEVariable_
! - The result is returned in ans
! - ans is a FEVariable
! - The rank of ans is same as the rank of val
! - ans is defined on Quadrature, that is, ans is QuadratureVariable
!
! - The val can have following ranks; scalar, vector, matrix
! - the val can be defined on quadrature (do nothing) or nodal (interpol)
! - The `vartype` of val can be constant, space, time, spacetime

INTERFACE GetInterpolation_
  MODULE PURE SUBROUTINE GetInterpolation_2a(obj, ans, val, scale, &
                                             addContribution)
    CLASS(STElemshapeData_), INTENT(IN) :: obj(:)
    TYPE(FEVariable_), INTENT(INOUT) :: ans
    TYPE(FEVariable_), INTENT(IN) :: val
    REAL(DFP), INTENT(IN) :: scale
    LOGICAL, INTENT(IN) :: addContribution
  END SUBROUTINE GetInterpolation_2a
END INTERFACE GetInterpolation_

!----------------------------------------------------------------------------
!                                                       Interpolation@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-13
! update: 2021-12-13
! summary: Interpolation of FEVariable

INTERFACE
  MODULE PURE FUNCTION Interpolation1(obj, val) RESULT(ans)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    TYPE(FEVariable_), INTENT(IN) :: val
    TYPE(FEVariable_) :: ans
  END FUNCTION Interpolation1
END INTERFACE

INTERFACE Interpolation
  MODULE PROCEDURE Interpolation1
END INTERFACE Interpolation

END MODULE ElemshapeData_InterpolMethods
