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

SUBMODULE(FEVariable_DotProductMethod) Methods
USE GlobalData, ONLY: Constant, Space, Time, SpaceTime, &
                      Scalar, Vector, Matrix, Nodal, Quadrature

USE BaseType, ONLY: TypeFEVariableScalar, &
                    TypeFEVariableVector, &
                    TypeFEVariableMatrix, &
                    TypeFEVariableConstant, &
                    TypeFEVariableSpace, &
                    TypeFEVariableTime, &
                    TypeFEVariableSpaceTime

USE FEVariable_Method, ONLY: NodalVariable, QuadratureVariable, Get

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                             DOT_PRODUCT
!----------------------------------------------------------------------------

MODULE PROCEDURE fevar_dot_product
! !! Internal variable
! REAL(DFP), ALLOCATABLE :: r1(:), r2(:, :), m2(:, :), r3(:, :, :), m3(:, :, :)
! INTEGER(I4B) :: jj, kk
!
! ! main
! SELECT CASE (obj1%vartype)
!
! CASE (constant)
!
!   SELECT CASE (obj2%vartype)
!
!     ! constant = constant DOT_PRODUCT constant
!   CASE (constant)
!
!     IF (obj1%defineon .EQ. nodal) THEN
!       ans = NodalVariable( &
!         & DOT_PRODUCT(obj1%val(:), obj2%val(:)), &
!         & typeFEVariableScalar, &
!         & typeFEVariableConstant)
!     ELSE
!       ans = QuadratureVariable( &
!         & DOT_PRODUCT(obj1%val(:), obj2%val(:)), &
!         & typeFEVariableScalar, &
!         & typeFEVariableConstant)
!     END IF
!
!     ! space= constant DOT_PRODUCT space
!   CASE (space)
!
!     r2 = GET(obj2, TypeFEVariableVector, TypeFEVariableSpace)
!
!     IF (obj2%defineon .EQ. nodal) THEN
!       ans = NodalVariable(&
!         & MATMUL(obj1%val, r2), &
!         & typeFEVariableScalar, &
!         & typeFEVariableSpace)
!     ELSE
!       ans = QuadratureVariable(&
!         & MATMUL(obj1%val, r2), &
!         & typeFEVariableScalar, &
!         & typeFEVariableSpace)
!     END IF
!
!     ! time=constant DOT_PRODUCT time
!   CASE (time)
!
!     r2 = GET(obj2, TypeFEVariableVector, TypeFEVariableTime)
!
!     IF (obj2%defineon .EQ. nodal) THEN
!       ans = NodalVariable(&
!         & MATMUL(obj1%val, r2), &
!         & typeFEVariableScalar, &
!         & typeFEVariableTime)
!     ELSE
!       ans = QuadratureVariable(&
!         & MATMUL(obj1%val, r2), &
!         & typeFEVariableScalar, &
!         & typeFEVariableTime)
!     END IF
!   !!
!   !! spacetime=constant DOT_PRODUCT spacetime
!   !!
!   CASE (spacetime)
!     !!
!     r3 = GET(obj2, TypeFEVariableVector, TypeFEVariableSpaceTime)
!     !!
!     IF (obj2%defineon .EQ. nodal) THEN
!       ans = NodalVariable(&
!         & MATMUL(obj1%val, r3), &
!         & typeFEVariableScalar, &
!         & typeFEVariableSpaceTime)
!     ELSE
!       ans = QuadratureVariable(&
!         & MATMUL(obj1%val, r3), &
!         & typeFEVariableScalar, &
!         & typeFEVariableSpaceTime)
!     END IF
!     !!
!   END SELECT
! !!
! !!
! !!
! !!
! CASE (space)
! !!
!   SELECT CASE (obj2%vartype)
!   !!
!   !! space=space DOT_PRODUCT constant
!   !!
!   CASE (constant)
!     !!
!     r2 = GET(obj1, TypeFEVariableVector, TypeFEVariableSpace)
!     !!
!     IF (obj1%defineon .EQ. nodal) THEN
!       ans = NodalVariable(&
!         & MATMUL(obj2%val, r2), &
!         & typeFEVariableScalar, &
!         & typeFEVariableSpace)
!     ELSE
!       ans = QuadratureVariable(&
!         & MATMUL(obj2%val, r2), &
!         & typeFEVariableScalar, &
!         & typeFEVariableSpace)
!     END IF
!   !!
!   !! space=space DOT_PRODUCT space
!   !!
!   CASE (space)
!     !!
!     r2 = GET(obj1, TypeFEVariableVector, TypeFEVariableSpace)
!     m2 = GET(obj2, TypeFEVariableVector, TypeFEVariableSpace)
!     CALL Reallocate(r1, SIZE(r2, 2))
!     !!
!     DO jj = 1, SIZE(r1)
!       r1(jj) = DOT_PRODUCT(r2(:, jj), m2(:, jj))
!     END DO
!     !!
!     IF (obj1%defineon .EQ. nodal) THEN
!       ans = NodalVariable( &
!         & r1, &
!         & typeFEVariableScalar, &
!         & typeFEVariableSpace)
!     ELSE
!       ans = QuadratureVariable( &
!         & r1, &
!         & typeFEVariableScalar, &
!         & typeFEVariableSpace)
!     END IF
!     !!
!   END SELECT
! !!
! !!
! !!
! !!
! CASE (time)
! !!
!   SELECT CASE (obj2%vartype)
!   !!
!   !! time=time DOT_PRODUCT constant
!   !!
!   CASE (constant)
!     !!
!     r2 = GET(obj1, TypeFEVariableVector, TypeFEVariableTime)
!     !!
!     IF (obj1%defineon .EQ. nodal) THEN
!       ans = NodalVariable(&
!         & MATMUL(obj2%val, r2), &
!         & typeFEVariableScalar, &
!         & typeFEVariableTime)
!     ELSE
!       ans = QuadratureVariable(&
!         & MATMUL(obj2%val, r2), &
!         & typeFEVariableScalar, &
!         & typeFEVariableTime)
!     END IF
!   !!
!   !! time=time DOT_PRODUCT time
!   !!
!   CASE (time)
!     !!
!     r2 = GET(obj1, TypeFEVariableVector, TypeFEVariableTime)
!     m2 = GET(obj2, TypeFEVariableVector, TypeFEVariableTime)
!     CALL Reallocate(r1, SIZE(r2, 2))
!     !!
!     DO jj = 1, SIZE(r1)
!       r1(jj) = DOT_PRODUCT(r2(:, jj), m2(:, jj))
!     END DO
!     !!
!     IF (obj1%defineon .EQ. nodal) THEN
!       ans = NodalVariable( &
!         & r1, &
!         & typeFEVariableScalar, &
!         & typeFEVariableTime)
!     ELSE
!       ans = QuadratureVariable( &
!         & r1, &
!         & typeFEVariableScalar, &
!         & typeFEVariableTime)
!     END IF
!     !!
!   END SELECT
! !!
! CASE (spacetime)
!   !!
!   SELECT CASE (obj2%vartype)
!   !!
!   !! spacetime= spacetime DOT_PRODUCT constant
!   !!
!   CASE (constant)
!     !!
!     r3 = GET(obj1, TypeFEVariableVector, TypeFEVariableSpaceTime)
!     CALL Reallocate(r2, SIZE(r3, 2), SIZE(r3, 3))
!     !!
!     DO kk = 1, SIZE(r3, 3)
!       DO jj = 1, SIZE(r3, 2)
!         r2(jj, kk) = DOT_PRODUCT(r3(:, jj, kk), obj2%val(:))
!       END DO
!     END DO
!     !!
!     IF (obj1%defineon .EQ. nodal) THEN
!       ans = NodalVariable(&
!         & r2, &
!         & typeFEVariableScalar, &
!         & typeFEVariableSpaceTime)
!     ELSE
!       ans = QuadratureVariable(&
!         & r2, &
!         & typeFEVariableScalar, &
!         & typeFEVariableSpaceTime)
!     END IF
!   !!
!   !! spacetime=spacetime DOT_PRODUCT spacetime
!   !!
!   CASE (spacetime)
!     !!
!     r3 = GET(obj1, TypeFEVariableVector, TypeFEVariableSpaceTime)
!     m3 = GET(obj2, TypeFEVariableVector, TypeFEVariableSpaceTime)
!     !!
!     CALL Reallocate(r2, SIZE(r3, 2), SIZE(r3, 3))
!     !!
!     DO kk = 1, SIZE(r3, 3)
!       DO jj = 1, SIZE(r3, 2)
!         r2(jj, kk) = DOT_PRODUCT(r3(:, jj, kk), m3(:, jj, kk))
!       END DO
!     END DO
!     !!
!     IF (obj1%defineon .EQ. nodal) THEN
!       ans = NodalVariable(&
!         & r2, &
!         & typeFEVariableScalar, &
!         & typeFEVariableSpaceTime)
!     ELSE
!       ans = QuadratureVariable(&
!         & r2, &
!         & typeFEVariableScalar, &
!         & typeFEVariableSpaceTime)
!     END IF
!     !!
!   END SELECT
!   !!
! END SELECT
END PROCEDURE fevar_dot_product

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE Methods
