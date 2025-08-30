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

SUBMODULE(ElemshapeData_MatrixInterpolMethods) Methods
USE ReallocateUtility, ONLY: Reallocate
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                           getinterpolation
!----------------------------------------------------------------------------

MODULE PROCEDURE matrix_getinterpolation_1
! interpol = MATMUL(val, obj%N)
END PROCEDURE matrix_getinterpolation_1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE matrix_getinterpolation1_
! dim1 = SIZE(val, 1)
! dim2 = SIZE(val, 2)
! dim3 = SIZE(obj%N, 2)
! interpol(1:dim1, 1:dim2, 1:dim3) = MATMUL(val, obj%N)
END PROCEDURE matrix_getinterpolation1_

!----------------------------------------------------------------------------
!                                                         getSTinterpolation
!----------------------------------------------------------------------------

MODULE PROCEDURE matrix_getinterpolation_2
! SELECT TYPE (obj)
! TYPE IS (STElemShapeData_)
!   interpol = MATMUL(MATMUL(val, obj%T), obj%N)
! END SELECT
END PROCEDURE matrix_getinterpolation_2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE matrix_getinterpolation2_
! SELECT TYPE (obj)
! TYPE IS (STElemShapeData_)
!   dim1 = SIZE(val, 1)
!   dim2 = SIZE(val, 2)
!   dim3 = SIZE(obj%N, 2)
!   interpol(1:dim1, 1:dim2, 1:dim3) = MATMUL(MATMUL(val, obj%T), obj%N)
! END SELECT
END PROCEDURE matrix_getinterpolation2_

!----------------------------------------------------------------------------
!                                                         getSTinterpolation
!----------------------------------------------------------------------------

MODULE PROCEDURE matrix_getinterpolation_3
!! TODO
END PROCEDURE matrix_getinterpolation_3

!----------------------------------------------------------------------------
!                                                           getinterpolation
!----------------------------------------------------------------------------

MODULE PROCEDURE matrix_getinterpolation_4
! INTEGER(I4B) :: i
! INTEGER(I4B) :: s(2)
! !! main
! SELECT CASE (val%vartype)
! CASE (Constant)
!   s(1:2) = SHAPE(val)
!   CALL reallocate(interpol, s(1), s(2), SIZE(obj%N, 2))
!   interpol(:, :, 1) = Get(val, TypeFEVariableMatrix, &
!     & TypeFEVariableConstant)
!   DO i = 2, SIZE(interpol, 3)
!     interpol(:, :, i) = interpol(:, :, 1)
!   END DO
! CASE (Space)
!   IF (val%DefineOn .EQ. Nodal) THEN
!     interpol = interpolation(obj, &
!       & Get(val, TypeFEVariableMatrix, TypeFEVariableSpace))
!   ELSE
!     interpol = Get(val, TypeFEVariableMatrix, TypeFEVariableSpace)
!   END IF
! CASE (SpaceTime)
!   SELECT TYPE (obj)
!   TYPE IS (STElemShapeData_)
!     IF (val%DefineOn .EQ. Nodal) THEN
!       interpol = STinterpolation(obj, &
!         & Get(val, TypeFEVariableMatrix, TypeFEVariableSpaceTime))
!     END IF
!   END SELECT
! END SELECT
END PROCEDURE matrix_getinterpolation_4

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE matrix_getinterpolation4_
! INTEGER(I4B) :: ii
!
! SELECT CASE (val%vartype)
! CASE (Constant)
!   dim3 = SIZE(obj%N, 2)
!   CALL Get_(obj=val, rank=TypeFEVariableMatrix, &
!             vartype=TypeFEVariableConstant, &
!             val=interpol(:, :, 1), nrow=dim1, ncol=dim2)
!   DO ii = 2, dim3
!     interpol(1:dim1, 1:dim2, ii) = interpol(1:dim1, 1:dim2, 1)
!   END DO
! CASE (Space)
!   IF (val%DefineOn .EQ. Nodal) THEN
!     CALL GetInterpolation_(obj=obj, &
!                            val=Get(val, TypeFEVariableMatrix, &
!                                    TypeFEVariableSpace), &
!                            interpol=interpol, &
!                            dim1=dim1, dim2=dim2, dim3=dim3)
!   ELSE
!     CALL Get_(obj=val, rank=TypeFEVariableMatrix, &
!               vartype=TypeFEVariableSpace, val=interpol, &
!               dim1=dim1, dim2=dim2, dim3=dim3)
!   END IF
! CASE (SpaceTime)
!   SELECT TYPE (obj)
!   TYPE IS (STElemShapeData_)
!     IF (val%DefineOn .EQ. Nodal) THEN
!       CALL GetInterpolation_(obj=obj, &
!                              val=Get(val, TypeFEVariableMatrix, &
!                                      TypeFEVariableSpaceTime), &
!                              interpol=interpol, &
!                              dim1=dim1, dim2=dim2, dim3=dim3)
!     END IF
!   END SELECT
! END SELECT
END PROCEDURE matrix_getinterpolation4_

!----------------------------------------------------------------------------
!                                                           getinterpolation
!----------------------------------------------------------------------------

MODULE PROCEDURE matrix_getinterpolation_5
! INTEGER(I4B) :: ii, jj
! INTEGER(I4B), ALLOCATABLE :: s(:)
! REAL(DFP), ALLOCATABLE :: m2(:, :)
! !!
! !! main
! !!
! s = SHAPE(val)
! CALL Reallocate(interpol, s(1), s(2), SIZE(obj(1)%N, 2), SIZE(obj))
! !!
! SELECT CASE (val%vartype)
! !!
! !!
! !!
! !!
! CASE (Constant)
!   !!
!   m2 = Get(val, TypeFEVariableMatrix, TypeFEVariableConstant)
!   !!
!   DO jj = 1, SIZE(interpol, 4)
!     DO ii = 1, SIZE(interpol, 3)
!       interpol(:, :, ii, jj) = m2
!     END DO
!   END DO
!   !!
!   DEALLOCATE (m2)
! !!
! !!
! !!
! !!
! CASE (Space)
!   !!
!   IF (val%DefineOn .EQ. Nodal) THEN
!     !!
!     DO ii = 1, SIZE(obj)
!       interpol(:, :, :, ii) = Interpolation(obj(ii), &
!         & Get(val, TypeFEVariableMatrix, TypeFEVariableSpace))
!     END DO
!     !!
!   ELSE
!     !!
!     interpol(:, :, :, 1) = Get(val, TypeFEVariableMatrix, TypeFEVariableSpace)
!     !!
!     DO ii = 2, SIZE(obj)
!       interpol(:, :, :, ii) = interpol(:, :, :, 1)
!     END DO
!     !!
!   END IF
! !!
! !!
! !!
! !!
! CASE (SpaceTime)
!   !!
!   IF (val%DefineOn .EQ. Nodal) THEN
!     !!
!     DO ii = 1, SIZE(obj)
!       interpol(:, :, :, ii) = STinterpolation(obj(ii), &
!         & Get(val, TypeFEVariableMatrix, TypeFEVariableSpaceTime))
!     END DO
!     !!
!   ELSE
!     interpol = Get(val, TypeFEVariableMatrix, typeFEVariableSpaceTime)
!   END IF
! !!
! !!
! !!
! !!
! END SELECT
! !!
END PROCEDURE matrix_getinterpolation_5

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE matrix_getinterpolation5_
! INTEGER(I4B) :: ii, jj
! dim1 = SIZE(val, 1)
! dim2 = SIZE(val, 2)
! dim3 = SIZE(obj(1)%N, 2)
! dim4 = SIZE(obj)
!
! SELECT CASE (val%vartype)
! CASE (Constant)
!   CALL Get_(obj=val, rank=TypeFEVariableMatrix, &
!             vartype=TypeFEVariableConstant, val=interpol(:, :, 1, 1), &
!             nrow=dim1, ncol=dim2)
!   DO jj = 1, dim3
!     DO ii = 1, dim4
!       IF (jj .EQ. 1 .AND. ii .EQ. 1) CYCLE
!       interpol(1:dim1, 1:dim2, ii, jj) = interpol(1:dim1, 1:dim2, 1, 1)
!     END DO
!   END DO
! CASE (Space)
!   IF (val%DefineOn .EQ. Nodal) THEN
!     DO ii = 1, dim4
!       CALL GetInterpolation_(obj=obj(ii), &
!                              val=Get(val, TypeFEVariableMatrix, &
!                                      TypeFEVariableSpace), &
!                              interpol=interpol(:, :, :, ii), &
!                              dim1=dim1, dim2=dim2, dim3=dim3)
!     END DO
!   ELSE
!     CALL Get_(obj=val, rank=TypeFEVariableMatrix, &
!               vartype=TypeFEVariableSpace, val=interpol(:, :, :, 1), &
!               dim1=dim1, dim2=dim2, dim3=dim3)
!     DO ii = 2, dim4
!       interpol(1:dim1, 1:dim2, 1:dim3, ii) = &
!         interpol(1:dim1, 1:dim2, 1:dim3, 1)
!     END DO
!   END IF
! CASE (SpaceTime)
!   IF (val%DefineOn .EQ. Nodal) THEN
!     DO ii = 1, dim4
!       CALL GetInterpolation_(obj=obj(ii), &
!                              val=Get(val, TypeFEVariableMatrix, &
!                                      TypeFEVariableSpaceTime), &
!                              interpol=interpol(:, :, :, ii), &
!                              dim1=dim1, dim2=dim2, dim3=dim3)
!     END DO
!   ELSE
!     CALL Get_(obj=val, rank=TypeFEVariableMatrix, &
!               vartype=TypeFEVariableSpaceTime, val=interpol, &
!               dim1=dim1, dim2=dim2, dim3=dim3, dim4=dim4)
!   END IF
! END SELECT
END PROCEDURE matrix_getinterpolation5_

!----------------------------------------------------------------------------
!                                                      interpolationOfVector
!----------------------------------------------------------------------------

MODULE PROCEDURE matrix_interpolation_1
! interpol = MATMUL(val, obj%N)
END PROCEDURE matrix_interpolation_1

!----------------------------------------------------------------------------
!                                                            STinterpolation
!----------------------------------------------------------------------------

MODULE PROCEDURE matrix_stinterpolation_1
! interpol = MATMUL(MATMUL(val, obj%T), obj%N)
END PROCEDURE matrix_stinterpolation_1

END SUBMODULE Methods
