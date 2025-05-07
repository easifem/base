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

SUBMODULE(ElemshapeData_GetMethods) Methods
USE ReallocateUtility, ONLY: Reallocate

USE FEVariable_Method, ONLY: QuadratureVariable, NodalVariable

USE BaseType, ONLY: TypeFEVariableSpace, &
                    TypeFEVariableVector, &
                    TypeFEVariableSpaceTime

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                 getNormal
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_getnormal_1
IF (PRESENT(nsd)) THEN
  CALL Reallocate(normal, nsd, obj%nips)
  normal(1:nsd, 1:obj%nips) = obj%normal(1:nsd, 1:obj%nips)
ELSE
  CALL Reallocate(normal, 3, obj%nips)
  normal(1:3, 1:obj%nips) = obj%normal(1:3, 1:obj%nips)
END IF
END PROCEDURE elemsd_GetNormal_1

!----------------------------------------------------------------------------
!                                                                 getNormal
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_getnormal_2
IF (PRESENT(nsd)) THEN
  normal = QuadratureVariable(obj%normal(1:nsd, 1:obj%nips), &
                              TypeFEVariableVector, &
                              TypeFEVariableSpace)
ELSE
  normal = QuadratureVariable(obj%normal(1:3, 1:obj%nips), &
                              TypeFEVariableVector, &
                              TypeFEVariableSpace)
END IF
END PROCEDURE elemsd_getnormal_2

!----------------------------------------------------------------------------
!                                                                 getNormal
!----------------------------------------------------------------------------

MODULE PROCEDURE elemsd_getnormal_3
REAL(DFP), ALLOCATABLE :: m3(:, :, :)
INTEGER(I4B) :: ii, nips, nipt, nsd0

nipt = SIZE(obj)
nips = 0
DO ii = 1, nipt
  IF (obj(ii)%nips > nips) nips = obj(ii)%nips
END DO

nsd0 = 3
IF (PRESENT(nsd)) nsd0 = nsd

ALLOCATE (m3(nsd0, nips, nipt))

DO ii = 1, nipt
  m3(1:nsd0, 1:obj(ii)%nips, ii) = obj(ii)%normal(1:nsd0, 1:obj(ii)%nips)
END DO

normal = QuadratureVariable(m3, TypeFEVariableVector, &
                            TypeFEVariableSpaceTime)

DEALLOCATE (m3)
END PROCEDURE elemsd_getnormal_3

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE Methods
