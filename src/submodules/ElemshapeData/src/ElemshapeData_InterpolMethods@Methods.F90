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

SUBMODULE(ElemshapeData_InterpolMethods) Methods
USE ReallocateUtility, ONLY: Reallocate
USE FEVariable_Method, ONLY: FEVariableGetInterpolation_ => GetInterpolation_

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                           GetInterpolation
!----------------------------------------------------------------------------

MODULE PROCEDURE GetInterpolation1
! REAL(DFP), ALLOCATABLE :: r1(:), r2(:, :), r3(:, :, :)
! !! main
! !!
! !! if val is a quadrature variable then do nothing
! !!
! IF (val%defineOn .EQ. Quadrature) THEN
!   interpol = val
!   RETURN
! END IF
! !!
! !! if val is a nodal variable then interpolate
! !!
! SELECT CASE (val%rank)
! CASE (Scalar)
!   CALL getInterpolation(obj=obj, ans=r1, val=val)
!   interpol = QuadratureVariable(r1, typeFEVariableScalar, &
!     & typeFEVariableSpace)
!   DEALLOCATE (r1)
! CASE (Vector)
!   CALL getInterpolation(obj=obj, ans=r2, val=val)
!   interpol = QuadratureVariable(r2, typeFEVariableVector, &
!     & typeFEVariableSpace)
!   DEALLOCATE (r2)
! CASE (Matrix)
!   CALL getInterpolation(obj=obj, ans=r3, val=val)
!   interpol = QuadratureVariable(r3, typeFEVariableMatrix, &
!     & typeFEVariableSpace)
!   DEALLOCATE (r3)
! END SELECT
END PROCEDURE GetInterpolation1

!----------------------------------------------------------------------------
!                                                           GetInterpolation_
!----------------------------------------------------------------------------

MODULE PROCEDURE GetInterpolation_1
REAL(DFP), PARAMETER :: one = 1.0_DFP
LOGICAL, PARAMETER :: no = .FALSE.

CALL FEVariableGetInterpolation_(obj=val, ans=ans, N=obj%N, nns=obj%nns, &
                                 nips=obj%nips, scale=one, addContribution=no)
END PROCEDURE GetInterpolation_1

!----------------------------------------------------------------------------
!                                                           GetInterpolation_
!----------------------------------------------------------------------------

MODULE PROCEDURE GetInterpolation_1a
CALL FEVariableGetInterpolation_(obj=val, ans=ans, N=obj%N, nns=obj%nns, &
                                 nips=obj%nips, scale=scale, &
                                 addContribution=addContribution)
END PROCEDURE GetInterpolation_1a

!----------------------------------------------------------------------------
!                                                            GetInterpolation
!----------------------------------------------------------------------------

MODULE PROCEDURE GetInterpolation2
! REAL(DFP), ALLOCATABLE :: r2(:, :), r3(:, :, :), r4(:, :, :, :)
! !! main
! !!
! !! if val is a quadrature variable then do nothing
! !!
! IF (val%defineOn .EQ. Quadrature) THEN
!   interpol = val
!   RETURN
! END IF
! !!
! !! if val is a nodal variable then interpolate
! !!
! SELECT CASE (val%rank)
! CASE (Scalar)
!   CALL getInterpolation(obj=obj, ans=r2, val=val)
!   interpol = QuadratureVariable(r2, typeFEVariableScalar, &
!     & typeFEVariableSpaceTime)
!   DEALLOCATE (r2)
! CASE (Vector)
!   CALL getInterpolation(obj=obj, ans=r3, val=val)
!   interpol = QuadratureVariable(r3, typeFEVariableVector, &
!     & typeFEVariableSpaceTime)
!   DEALLOCATE (r3)
! CASE (Matrix)
!   CALL getInterpolation(obj=obj, ans=r4, val=val)
!   interpol = QuadratureVariable(r4, typeFEVariableMatrix, &
!     & typeFEVariableSpaceTime)
!   DEALLOCATE (r4)
! END SELECT
! !!
END PROCEDURE GetInterpolation2

!----------------------------------------------------------------------------
!                                                         GetInterpolation_
!----------------------------------------------------------------------------

MODULE PROCEDURE GetInterpolation_2
END PROCEDURE GetInterpolation_2

!----------------------------------------------------------------------------
!                                                          GetInterpolation_
!----------------------------------------------------------------------------

MODULE PROCEDURE GetInterpolation_2a
END PROCEDURE GetInterpolation_2a

!----------------------------------------------------------------------------
!                                                      interpolationOfVector
!----------------------------------------------------------------------------

MODULE PROCEDURE Interpolation1
! CALL getInterpolation(obj=obj, val=val, ans=ans)
END PROCEDURE Interpolation1

END SUBMODULE Methods
