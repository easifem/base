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

SUBMODULE(FEVariable_Method) MeanMethods
USE IntegerUtility, ONLY: Get1DIndexFortran

USE GlobalData, ONLY: Scalar, Vector, Matrix, &
                      Constant, Space, Time, &
                      SpaceTime, Nodal, Quadrature

USE BaseType, ONLY: TypeFEVariableScalar, &
                    TypeFEVariableVector, &
                    TypeFEVariableMatrix, &
                    TypeFEVariableConstant, &
                    TypeFEVariableSpace, &
                    TypeFEVariableTime, &
                    TypeFEVariableSpaceTime

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                  Addition
!----------------------------------------------------------------------------

MODULE PROCEDURE fevar_Mean1
REAL(DFP) :: val0
SELECT CASE (obj%rank)
CASE (scalar)
  IF (obj%defineOn .EQ. NODAL) THEN
  ans = NodalVariable(MEAN(obj, TypeFEVariableScalar), TypeFEVariableScalar, &
                        TypeFEVariableConstant)
  ELSE
    ans = QuadratureVariable(MEAN(obj, TypeFEVariableScalar), &
                             TypeFEVariableScalar, TypeFEVariableConstant)
  END IF

CASE (vector)
  IF (obj%defineOn .EQ. NODAL) THEN
    ans = NodalVariable(MEAN(obj, TypeFEVariableVector), &
                        TypeFEVariableVector, TypeFEVariableConstant)
  ELSE
    ans = QuadratureVariable(MEAN(obj, TypeFEVariableVector), &
                             TypeFEVariableVector, TypeFEVariableConstant)
  END IF

CASE (matrix)
  IF (obj%defineOn .EQ. NODAL) THEN
    ans = NodalVariable(MEAN(obj, TypeFEVariableMatrix), &
                        TypeFEVariableMatrix, TypeFEVariableConstant)
  ELSE
    ans = QuadratureVariable(MEAN(obj, TypeFEVariableMatrix), &
                             TypeFEVariableMatrix, TypeFEVariableConstant)
  END IF
END SELECT
END PROCEDURE fevar_Mean1

!----------------------------------------------------------------------------
!                                                                  Addition
!----------------------------------------------------------------------------

MODULE PROCEDURE fevar_Mean2
ans = SUM(obj%val(1:obj%len)) / obj%len
END PROCEDURE fevar_Mean2

!----------------------------------------------------------------------------
!                                                                  Addition
!----------------------------------------------------------------------------

MODULE PROCEDURE fevar_Mean3
INTEGER(I4B) :: ii, tsize

tsize = obj%s(1)
ALLOCATE (ans(tsize))

SELECT CASE (obj%varType)

CASE (Constant)

  ans(1:tsize) = obj%val(1:tsize)

CASE (Space, Time)

  ans = 0.0
  DO ii = 1, obj%s(2)
    ans(1:tsize) = ans(1:tsize) + obj%val((ii - 1) * tsize + 1:ii * tsize)
  END DO

  ans(1:tsize) = ans(1:tsize) / obj%s(2)

CASE (SpaceTime)

  ans = 0.0
  DO ii = 1, obj%s(2) * obj%s(3)
    ans(1:tsize) = ans(1:tsize) + obj%val((ii - 1) * tsize + 1:ii * tsize)
  END DO

  ans(1:tsize) = ans(1:tsize) / (obj%s(2) * obj%s(3))

END SELECT

END PROCEDURE fevar_Mean3

!----------------------------------------------------------------------------
!                                                                  Addition
!----------------------------------------------------------------------------

MODULE PROCEDURE fevar_Mean4
INTEGER(I4B) :: ii, jj, kk, ll

ALLOCATE (ans(obj%s(1), obj%s(2)))

SELECT CASE (obj%varType)

CASE (Constant)

  DO CONCURRENT(ii=1:obj%s(1), jj=1:obj%s(2))
    ans(ii, jj) = obj%val(Get1DIndexFortran(i=ii, j=jj, &
                                            dim1=obj%s(1), dim2=obj%s(2)))
  END DO

CASE (Space, Time)

  DO CONCURRENT(kk=1:obj%s(3))
    DO jj = 1, obj%s(2)
      DO ii = 1, obj%s(1)

        ans(ii, jj) = ans(ii, jj) &
                      + obj%val(Get1DIndexFortran(i=ii, j=jj, k=kk, &
                                 dim1=obj%s(1), dim2=obj%s(2), dim3=obj%s(3)))

      END DO
    END DO
  END DO

  ans = ans / obj%s(3)

CASE (SpaceTime)

  DO CONCURRENT(kk=1:obj%s(3), ll=1:obj%s(4))

    DO jj = 1, obj%s(2)
      DO ii = 1, obj%s(1)
        ans(ii, jj) = ans(ii, jj) + obj%val(Get1DIndexFortran( &
                                            i=ii, j=jj, k=kk, l=ll, &
                  dim1=obj%s(1), dim2=obj%s(2), dim3=obj%s(3), dim4=obj%s(4)))

      END DO
    END DO
  END DO

  ans = ans / (obj%s(3) * obj%s(4))

END SELECT

END PROCEDURE fevar_Mean4

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE MeanMethods
