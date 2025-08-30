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

MODULE ElemshapeData_ScalarInterpolMethods
USE GlobalData, ONLY: DFP, I4B, LGT
USE BaseType, ONLY: ElemShapeData_, STElemShapeData_, FEVariable_
IMPLICIT NONE
PRIVATE

PUBLIC :: GetInterpolation
PUBLIC :: GetInterpolation_
PUBLIC :: Interpolation
PUBLIC :: STInterpolation

!----------------------------------------------------------------------------
!                                           getInterpolation@InterpolMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 4 March 2021
! summary: This subroutine performs interpolations of scalar
!
!# Introduction
!
! This subroutine performs interpolation of a scalar from its spatial nodal
! values.
!
! $$u=u_{I}N^{I}$$
!
! - TODO Make it work when the size of val is not the same as NNS

INTERFACE GetInterpolation
  MODULE PURE SUBROUTINE GetInterpolation1(obj, interpol, val)
    CLASS(ElemShapeData_), INTENT(IN) :: obj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: interpol(:)
    !! Interpolation value of of scalar
    REAL(DFP), INTENT(IN) :: val(:)
    !! spatial nodal values of scalar
  END SUBROUTINE GetInterpolation1
END INTERFACE GetInterpolation

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:   2025-03-03
! summary:  get interpolation of scalar without allocation

INTERFACE GetInterpolation_
  MODULE PURE SUBROUTINE GetInterpolation_1(obj, interpol, val, tsize)
    CLASS(ElemShapeData_), INTENT(IN) :: obj
    REAL(DFP), INTENT(INOUT) :: interpol(:)
    REAL(DFP), INTENT(IN) :: val(:)
    INTEGER(I4B), INTENT(OUT) :: tsize
  END SUBROUTINE GetInterpolation_1
END INTERFACE GetInterpolation_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:   2025-03-03
! summary:  get interpolation of scalar without allocation

INTERFACE GetInterpolation_
  MODULE PURE SUBROUTINE GetInterpolation_1a(obj, interpol, val, &
                                             tsize, scale, &
                                             addContribution)
    CLASS(ElemShapeData_), INTENT(IN) :: obj
    REAL(DFP), INTENT(INOUT) :: interpol(:)
    REAL(DFP), INTENT(IN) :: val(:)
    INTEGER(I4B), INTENT(OUT) :: tsize
    REAL(DFP), INTENT(IN) :: scale
    LOGICAL(LGT), INTENT(IN) :: addContribution
  END SUBROUTINE GetInterpolation_1a
END INTERFACE GetInterpolation_

!----------------------------------------------------------------------------
!                                           getInterpolation@InterpolMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 1 Nov 2021
! summary: This subroutine performs interpolations of scalar nodal values
!
!# Introduction
!
! This subroutine performs interpolation of a scalar from its space-time nodal
! values.
!
! $$u=u^{a}_{I}N^{I}T_{a}$$
!
! The resultant represents the interpolation value of `val` at
! spatial-quadrature points

INTERFACE GetInterpolation
  MODULE PURE SUBROUTINE GetInterpolation2(obj, interpol, val)
    CLASS(STElemShapeData_), INTENT(IN) :: obj
    REAL(DFP), INTENT(INOUT), ALLOCATABLE :: interpol(:)
    !! Interpolation of scalar
    REAL(DFP), INTENT(IN) :: val(:, :)
    !! space-time nodal values of scalar
    !! val(I,a) where I is the node number and a is the time level
  END SUBROUTINE GetInterpolation2
END INTERFACE GetInterpolation

!----------------------------------------------------------------------------
!                                                          GetInterpolation_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-08-29
! summary: Get interpolation of space-time nodal values at a single time
!
!# Introduction
!
! This method is like GetInterpolation_2 but without allocation

INTERFACE GetInterpolation_
  MODULE PURE SUBROUTINE GetInterpolation_2(obj, interpol, val, tsize)
    CLASS(STElemShapeData_), INTENT(IN) :: obj
    REAL(DFP), INTENT(INOUT) :: interpol(:)
    REAL(DFP), INTENT(IN) :: val(:, :)
    INTEGER(I4B), INTENT(OUT) :: tsize
  END SUBROUTINE GetInterpolation_2
END INTERFACE GetInterpolation_

!----------------------------------------------------------------------------
!                                                          GetInterpolation_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-08-29
! summary: Get interpolation of space-time nodal values at a single time
!
!# Introduction
!
! This method is like GetInterpolation_2 but without allocation

INTERFACE GetInterpolation_
  MODULE PURE SUBROUTINE GetInterpolation_2a(obj, interpol, val, &
                                             tsize, scale, &
                                             addContribution)
    CLASS(STElemShapeData_), INTENT(IN) :: obj
    REAL(DFP), INTENT(INOUT) :: interpol(:)
    REAL(DFP), INTENT(IN) :: val(:, :)
    INTEGER(I4B), INTENT(OUT) :: tsize
    REAL(DFP), INTENT(IN) :: scale
    LOGICAL(LGT), INTENT(IN) :: addContribution
  END SUBROUTINE GetInterpolation_2a
END INTERFACE GetInterpolation_

!----------------------------------------------------------------------------
!                                                            GetInterpolation
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 1 Nov 2021
! summary: This subroutine performs interpolations of scalar nodal values
!
!# Introduction
!
! This subroutine performs interpolation of a scalar from its space-time nodal
! values.
!
! $$u=u^{a}_{I}N^{I}T_{a}$$
!
! The resultant represents the interpolation value of `val` at
! spatial-temporal quadrature points

INTERFACE GetInterpolation
  MODULE PURE SUBROUTINE GetInterpolation3(obj, interpol, val)
    CLASS(STElemshapeData_), INTENT(IN) :: obj(:)
    REAL(DFP), INTENT(INOUT), ALLOCATABLE :: interpol(:, :)
    !! space-time Interpolation of scalar
    REAL(DFP), INTENT(IN) :: val(:, :)
    !! space-time nodal values of scalar
  END SUBROUTINE GetInterpolation3
END INTERFACE GetInterpolation

!----------------------------------------------------------------------------
!                                                           GetInterpolation_
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:   2025-03-03
! summary:  Get interpolation of scalar without allocation

INTERFACE GetInterpolation_
  MODULE PURE SUBROUTINE GetInterpolation_3(obj, interpol, val, &
                                            nrow, ncol)
    CLASS(STElemshapeData_), INTENT(IN) :: obj(:)
    REAL(DFP), INTENT(INOUT) :: interpol(:, :)
    REAL(DFP), INTENT(IN) :: val(:, :)
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  END SUBROUTINE GetInterpolation_3
END INTERFACE GetInterpolation_

!----------------------------------------------------------------------------
!                                                           GetInterpolation_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-08-29
! summary: Get interpolation of scalar without allocation

INTERFACE GetInterpolation_
  MODULE PURE SUBROUTINE GetInterpolation_3a(obj, interpol, val, &
                                             nrow, ncol, scale, &
                                             addContribution)
    CLASS(STElemshapeData_), INTENT(IN) :: obj(:)
    REAL(DFP), INTENT(INOUT) :: interpol(:, :)
    REAL(DFP), INTENT(IN) :: val(:, :)
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    REAL(DFP), INTENT(IN) :: scale
    LOGICAL(LGT), INTENT(IN) :: addContribution
  END SUBROUTINE GetInterpolation_3a
END INTERFACE GetInterpolation_

!----------------------------------------------------------------------------
!                                           getInterpolation@InterpolMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 4 March 2021
! summary: returns the interpolation of scalar FEVariable
!
!# Introduction
!
! Returns the interpolation of scalar variable
! The scalar variable can be+
!
! - constant
! - spatial nodal values
! - spatial quadrature values
! - space-time nodal values
!
!@note
!This routine calls [[Interpolation]] function from the same module.
!@endnote

INTERFACE GetInterpolation
  MODULE PURE SUBROUTINE GetInterpolation4(obj, interpol, val)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: interpol(:)
    !! interpolation of scalar
    TYPE(FEVariable_), INTENT(IN) :: val
    !! Scalar FE variable
  END SUBROUTINE GetInterpolation4
END INTERFACE GetInterpolation

!----------------------------------------------------------------------------
!                                                   GetInterpolation_@Methods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:   2025-03-03
! summary:  get interpolation of scalar without allocation

INTERFACE GetInterpolation_
  MODULE PURE SUBROUTINE GetInterpolation_4(obj, interpol, val, tsize)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    REAL(DFP), INTENT(INOUT) :: interpol(:)
    TYPE(FEVariable_), INTENT(IN) :: val
    INTEGER(I4B), INTENT(OUT) :: tsize
  END SUBROUTINE GetInterpolation_4
END INTERFACE GetInterpolation_

!----------------------------------------------------------------------------
!                                                   GetInterpolation_@Methods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:   2025-03-03
! summary:  get interpolation of scalar without allocation

INTERFACE GetInterpolation_
  MODULE PURE SUBROUTINE GetInterpolation_4a(obj, interpol, val, tsize, &
                                             scale, addContribution, timeIndx)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    REAL(DFP), INTENT(INOUT) :: interpol(:)
    TYPE(FEVariable_), INTENT(IN) :: val
    INTEGER(I4B), INTENT(OUT) :: tsize
    REAL(DFP), INTENT(IN) :: scale
    LOGICAL(LGT), INTENT(IN) :: addContribution
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: timeIndx
  END SUBROUTINE GetInterpolation_4a
END INTERFACE GetInterpolation_

!----------------------------------------------------------------------------
!                                           getInterpolation@InterpolMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 1 Nov 2021
! summary: This subroutine performs interpolations of scalar FEVariable
!
!# Introduction
!
! This subroutine performs interpolation of a scalar [[FEVariable_]]
! The FE Variable can be a
!
! - constant
! - spatial nodal values
! - spatial quadrature values
! - space-time nodal values
!
! $$u=u^{a}_{I}N^{I}T_{a}$$
!
! The resultant represents the interpolation value of `val` at
! spatial-quadrature points

INTERFACE GetInterpolation
  MODULE PURE SUBROUTINE GetInterpolation5(obj, interpol, val)
    CLASS(STElemshapeData_), INTENT(IN) :: obj(:)
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: interpol(:, :)
    !! space-time interpolation of scalar
    TYPE(FEVariable_), INTENT(IN) :: val
    !! scalar FE variable
  END SUBROUTINE GetInterpolation5
END INTERFACE GetInterpolation

!----------------------------------------------------------------------------
!                                                  GetInterpolation_@Methods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:   2025-03-03
! summary:  get interpolation of scalar without allocation

INTERFACE GetInterpolation_
  MODULE PURE SUBROUTINE GetInterpolation_5(obj, interpol, val, &
                                            nrow, ncol)
    CLASS(STElemshapeData_), INTENT(IN) :: obj(:)
    REAL(DFP), INTENT(INOUT) :: interpol(:, :)
    TYPE(FEVariable_), INTENT(IN) :: val
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
  END SUBROUTINE GetInterpolation_5
END INTERFACE GetInterpolation_

!----------------------------------------------------------------------------
!                                                   GetInterpolation_@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-08-29
! summary: Get interpolation of scalar without allocation

INTERFACE GetInterpolation_
  MODULE PURE SUBROUTINE GetInterpolation_5a(obj, interpol, val, &
                                             nrow, ncol, scale, &
                                             addContribution)
    CLASS(STElemshapeData_), INTENT(IN) :: obj(:)
    REAL(DFP), INTENT(INOUT) :: interpol(:, :)
    TYPE(FEVariable_), INTENT(IN) :: val
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    REAL(DFP), INTENT(IN) :: scale
    LOGICAL(LGT), INTENT(IN) :: addContribution
  END SUBROUTINE GetInterpolation_5a
END INTERFACE GetInterpolation_

!----------------------------------------------------------------------------
!                                              Interpolation@InterpolMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 4 March 2021
! summary: This function returns the interpolation of a scalar

INTERFACE Interpolation
  MODULE PURE FUNCTION Interpolation1(obj, val) RESULT(interpol)
    CLASS(ElemshapeData_), INTENT(IN) :: obj
    REAL(DFP), INTENT(IN) :: val(:)
    REAL(DFP), ALLOCATABLE :: interpol(:)
  END FUNCTION Interpolation1
END INTERFACE Interpolation

!----------------------------------------------------------------------------
!                                            STInterpolation@InterpolMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-23
! update: 2021-11-23
! summary: This function performs interpolations of scalar
!
!# Introduction
!
! This function performs interpolation of a scalar from its space-time nodal
! values.
!
! $$u=u^{a}_{I}N^{I}T_{a}$$

INTERFACE
  MODULE PURE FUNCTION STInterpolation1(obj, val) RESULT(interpol)
    CLASS(STElemshapeData_), INTENT(IN) :: obj
    REAL(DFP), INTENT(IN) :: val(:, :)
    !! space-time nodal values of scalar
    REAL(DFP), ALLOCATABLE :: interpol(:)
    !! Interpolation value of `val` at integration points
  END FUNCTION STInterpolation1
END INTERFACE

INTERFACE STInterpolation
  MODULE PROCEDURE STInterpolation1
END INTERFACE STInterpolation

END MODULE ElemshapeData_ScalarInterpolMethods
