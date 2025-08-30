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

SUBMODULE(ElemshapeData_VectorInterpolMethods) Methods
USE ReallocateUtility, ONLY: Reallocate
IMPLICIT NONE

CONTAINS

!---------------------------------------------------------------------------
!                                                          getinterpolation
!----------------------------------------------------------------------------

MODULE PROCEDURE vector_getinterpolation_1
ans = MATMUL(val, obj%N)
END PROCEDURE vector_getinterpolation_1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE vector_getinterpolation1_
nrow = SIZE(val, 1)
ncol = SIZE(obj%N, 2)
ans(1:nrow, 1:ncol) = MATMUL(val, obj%N)
END PROCEDURE vector_getinterpolation1_

!----------------------------------------------------------------------------
!                                                         getSTinterpolation
!----------------------------------------------------------------------------

MODULE PROCEDURE vector_getinterpolation_2
! SELECT TYPE (obj)
! TYPE IS (STElemShapeData_)
!   interpol = MATMUL(MATMUL(val, obj%T), obj%N)
! END SELECT
END PROCEDURE vector_getinterpolation_2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE vector_getinterpolation2_
! SELECT TYPE (obj)
! TYPE IS (STElemShapeData_)
!   nrow = SIZE(val, 1)
!   ncol = SIZE(obj%N, 2)
!   interpol(1:nrow, 1:ncol) = MATMUL(MATMUL(val, obj%T), obj%N)
! END SELECT
END PROCEDURE vector_getinterpolation2_

!----------------------------------------------------------------------------
!                                                         getSTinterpolation
!----------------------------------------------------------------------------

MODULE PROCEDURE vector_getinterpolation_3
! INTEGER(I4B) :: ipt
! !!
! CALL reallocate(interpol, SIZE(val, 1), SIZE(obj(1)%N, 2), SIZE(obj))
! DO ipt = 1, SIZE(obj)
!   interpol(:, :, ipt) = MATMUL(MATMUL(val, obj(ipt)%T), obj(ipt)%N)
! END DO
END PROCEDURE vector_getinterpolation_3

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE vector_getinterpolation3_
! INTEGER(I4B) :: ipt
!
! dim1 = SIZE(val, 1)
! dim2 = SIZE(obj(1)%N, 2)
! dim3 = SIZE(obj)
! DO ipt = 1, dim3
!   interpol(1:dim1, 1:dim2, ipt) = MATMUL(MATMUL(val, obj(ipt)%T), &
!                                          obj(ipt)%N)
! END DO
END PROCEDURE vector_getinterpolation3_

!----------------------------------------------------------------------------
!                                                           getinterpolation
!----------------------------------------------------------------------------

MODULE PROCEDURE vector_getinterpolation_4
! REAL(DFP), ALLOCATABLE :: m1(:)
! INTEGER(I4B) :: ii
! !! main
! SELECT CASE (val%vartype)
! !!
! !! Constant
! !!
! CASE (Constant)
!   !!
!   m1 = Get(val, TypeFEVariableVector, TypeFEVariableConstant)
!   CALL Reallocate(interpol, SIZE(m1), SIZE(obj%N, 2))
!   DO ii = 1, SIZE(interpol, 2)
!     interpol(:, ii) = m1
!   END DO
!   DEALLOCATE (m1)
! !!
! !! Space
! !!
! CASE (Space)
!   !!
!   IF (val%DefineOn .EQ. Nodal) THEN
!     interpol = interpolation(obj, &
!       & Get(val, TypeFEVariableVector, TypeFEVariableSpace))
!   ELSE
!     interpol = Get(val, TypeFEVariableVector, TypeFEVariableSpace)
!   END IF
! !!
! !! SpaceTime
! !!
! CASE (SpaceTime)
!   !!
!   SELECT TYPE (obj)
!   TYPE IS (STElemShapeData_)
!     interpol = STinterpolation(obj, &
!       & Get(val, TypeFEVariableVector, TypeFEVariableSpaceTime))
!   END SELECT
! END SELECT
! !!
! !!
! !!
END PROCEDURE vector_getinterpolation_4

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE vector_getinterpolation4_
! INTEGER(I4B) :: ii
!
! SELECT CASE (val%vartype)
! CASE (Constant)
!   CALL Get_(obj=val, rank=TypeFEVariableVector, &
!             vartype=TypeFEVariableConstant, &
!             val=interpol(:, 1), tsize=nrow)
!   ncol = SIZE(obj%N, 2)
!   DO ii = 2, ncol
!     interpol(1:nrow, ii) = interpol(1:nrow, 1)
!   END DO
! CASE (Space)
!   IF (val%DefineOn .EQ. Nodal) THEN
!     CALL GetInterpolation_(obj=obj, &
!                            val=Get(val, TypeFEVariableVector, &
!                                    TypeFEVariableSpace), &
!                            ans=interpol, &
!                            nrow=nrow, ncol=ncol)
!   ELSE
!     CALL Get_(obj=val, rank=TypeFEVariableVector, &
!               vartype=TypeFEVariableSpace, &
!               val=interpol, nrow=nrow, ncol=ncol)
!   END IF
! CASE (SpaceTime)
!   SELECT TYPE (obj)
!   TYPE IS (STElemShapeData_)
!     CALL GetInterpolation_(obj=obj, &
!                            val=Get(val, TypeFEVariableVector, &
!                                    TypeFEVariableSpaceTime), &
!                            ans=interpol, &
!                            nrow=nrow, ncol=ncol)
!   END SELECT
! END SELECT
END PROCEDURE vector_getinterpolation4_

!----------------------------------------------------------------------------
!                                                         getSTinterpolation
!----------------------------------------------------------------------------

MODULE PROCEDURE vector_getinterpolation_5
! REAL(DFP), ALLOCATABLE :: m1(:)
! INTEGER(I4B) :: ii, jj
! INTEGER(I4B), ALLOCATABLE :: s(:)
! !!
! !! main
! !!
! s = SHAPE(val)
! CALL Reallocate(interpol, s(1), SIZE(obj(1)%N, 2), SIZE(obj))
! !!
! SELECT CASE (val%vartype)
! !!
! !! Constant
! !!
! CASE (Constant)
!   !!
!   m1 = Get(val, TypeFEVariableVector, TypeFEVariableConstant)
!   !!
!   DO jj = 1, SIZE(interpol, 3)
!     DO ii = 1, SIZE(interpol, 2)
!       interpol(:, ii, jj) = m1
!     END DO
!   END DO
!   DEALLOCATE (m1)
! !!
! !! Space
! !!
! CASE (Space)
!   !!
!   IF (val%DefineOn .EQ. Nodal) THEN
!     !!
!     DO ii = 1, SIZE(obj)
!       interpol(:, :, ii) = Interpolation(obj(ii), &
!         & Get(val, TypeFEVariableVector, TypeFEVariableSpace))
!     END DO
!     !!
!   ELSE
!     !!
!     interpol(:, :, 1) = Get(val, TypeFEVariableVector, TypeFEVariableSpace)
!     !!
!     DO ii = 2, SIZE(obj)
!       interpol(:, :, ii) = interpol(:, :, 1)
!     END DO
!     !!
!   END IF
! !!
! !! SpaceTime
! !!
! CASE (SpaceTime)
!   !!
!   IF (val%DefineOn .EQ. Nodal) THEN
!     !!
!     DO ii = 1, SIZE(obj)
!       interpol(:, :, ii) = STinterpolation(obj(ii), &
!         & Get(val, TypeFEVariableVector, TypeFEVariableSpaceTime))
!     END DO
!     !!
!   ELSE
!     interpol = Get(val, TypeFEVariableVector, typeFEVariableSpaceTime)
!   END IF
! !!
! !!
! !!
! !!
! END SELECT
! !!
END PROCEDURE vector_getinterpolation_5

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE vector_getinterpolation5_
! INTEGER(I4B) :: ii, jj
!
! dim1 = SIZE(val, 1)
! dim2 = SIZE(obj(1)%N, 2)
! dim3 = SIZE(obj)
! SELECT CASE (val%vartype)
! CASE (Constant)
!   CALL Get_(obj=val, rank=TypeFEVariableVector, &
!             vartype=TypeFEVariableConstant, &
!             val=interpol(:, 1, 1), tsize=dim1)
!   DO jj = 1, dim3
!     DO ii = 1, dim2
!       IF (jj .EQ. 1 .AND. ii .EQ. 1) CYCLE
!       interpol(1:dim1, ii, jj) = interpol(1:dim1, 1, 1)
!     END DO
!   END DO
! CASE (Space)
!   IF (val%DefineOn .EQ. Nodal) THEN
!     DO ii = 1, dim3
!       CALL GetInterpolation_(obj=obj(ii), &
!                              val=Get(val, TypeFEVariableVector, &
!                                      TypeFEVariableSpace), &
!                              ans=interpol(1:dim1, 1:dim2, ii), &
!                              nrow=dim1, ncol=dim2)
!     END DO
!   ELSE
!     CALL Get_(obj=val, rank=TypeFEVariableVector, &
!               vartype=TypeFEVariableSpace, &
!               val=interpol(:, :, 1), nrow=dim1, ncol=dim2)
!     DO ii = 2, SIZE(obj)
!       interpol(1:dim1, 1:dim2, ii) = interpol(1:dim1, 1:dim2, 1)
!     END DO
!   END IF
! CASE (SpaceTime)
!   IF (val%DefineOn .EQ. Nodal) THEN
!     DO ii = 1, SIZE(obj)
!       CALL GetInterpolation_(obj=obj(ii), &
!                              val=Get(val, TypeFEVariableVector, &
!                                      TypeFEVariableSpaceTime), &
!                              ans=interpol(1:dim1, 1:dim2, ii), &
!                              nrow=dim1, ncol=dim2)
!     END DO
!   ELSE
!     CALL Get_(obj=val, rank=TypeFEVariableVector, &
!               vartype=TypeFEVariableSpaceTime, &
!               val=interpol, dim1=dim1, dim2=dim2, dim3=dim3)
!   END IF
! END SELECT
END PROCEDURE vector_getinterpolation5_

!----------------------------------------------------------------------------
!                                                      interpolationOfVector
!----------------------------------------------------------------------------

MODULE PROCEDURE vector_interpolation_1
! interpol = MATMUL(val, obj%N)
END PROCEDURE vector_interpolation_1

END SUBMODULE Methods
