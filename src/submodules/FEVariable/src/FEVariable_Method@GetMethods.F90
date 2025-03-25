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

SUBMODULE(FEVariable_Method) GetMethods

USE ReallocateUtility, ONLY: Reallocate

USE GlobalData, ONLY: Scalar, Vector, Matrix, Constant, Space, &
                      Time, SpaceTime, Nodal, Quadrature
IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                GetLambdaFromYoungsModulus
!----------------------------------------------------------------------------

MODULE PROCEDURE fevar_GetLambdaFromYoungsModulus
INTEGER(I4B) :: ii

lambda = youngsModulus

DO CONCURRENT(ii=1:lambda%len)
  lambda%val(ii) = shearModulus%val(ii) * &
                  (youngsModulus%val(ii) - 2.0_DFP * shearModulus%val(ii)) / &
                   (3.0_DFP * shearModulus%val(ii) - youngsModulus%val(ii))
END DO

END PROCEDURE fevar_GetLambdaFromYoungsModulus

!----------------------------------------------------------------------------
!                                                                      Size
!----------------------------------------------------------------------------

MODULE PROCEDURE fevar_Size
IF (PRESENT(dim)) THEN
  ans = obj%s(dim)
ELSE
  ans = obj%len
END IF
END PROCEDURE fevar_Size

!----------------------------------------------------------------------------
!                                                                      Shape
!----------------------------------------------------------------------------

MODULE PROCEDURE fevar_Shape
SELECT CASE (obj%rank)
CASE (Scalar)
  SELECT CASE (obj%vartype)
  CASE (Constant)
    ans = [1]
  CASE (Space, Time)
    ans = obj%s(1:1)
  CASE (SpaceTime)
    ans = obj%s(1:2)
  END SELECT
CASE (Vector)
  SELECT CASE (obj%vartype)
  CASE (Constant)
    ans = obj%s(1:1)
  CASE (Space, Time)
    ans = obj%s(1:2)
  CASE (SpaceTime)
    ans = obj%s(1:3)
  END SELECT
CASE (Matrix)
  SELECT CASE (obj%vartype)
  CASE (Constant)
    ans = obj%s(1:2)
  CASE (Space, Time)
    ans = obj%s(1:3)
  CASE (SpaceTime)
    ans = obj%s(1:4)
  END SELECT
END SELECT
END PROCEDURE fevar_Shape

!----------------------------------------------------------------------------
!                                                                      rank
!----------------------------------------------------------------------------

MODULE PROCEDURE fevar_rank
ans = obj%rank
END PROCEDURE fevar_rank

!----------------------------------------------------------------------------
!                                                                    vartype
!----------------------------------------------------------------------------

MODULE PROCEDURE fevar_vartype
ans = obj%vartype
END PROCEDURE fevar_vartype

!----------------------------------------------------------------------------
!                                                                   defineon
!----------------------------------------------------------------------------

MODULE PROCEDURE fevar_defineon
ans = obj%defineon
END PROCEDURE fevar_defineon

!----------------------------------------------------------------------------
!                                                            isNodalVariable
!----------------------------------------------------------------------------

MODULE PROCEDURE fevar_isNodalVariable
ans = obj%defineon .EQ. nodal
END PROCEDURE fevar_isNodalVariable

!----------------------------------------------------------------------------
!                                                            isNodalVariable
!----------------------------------------------------------------------------

MODULE PROCEDURE fevar_isQuadratureVariable
ans = obj%defineon .NE. nodal
END PROCEDURE fevar_isQuadratureVariable

!----------------------------------------------------------------------------
!                                                            getNodalvalues
!----------------------------------------------------------------------------

MODULE PROCEDURE Scalar_Constant
val = obj%val(1)
END PROCEDURE Scalar_Constant

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

PURE SUBROUTINE Master_Get_vec_(obj, val, tsize)
  CLASS(FEVariable_), INTENT(IN) :: obj
  REAL(DFP), INTENT(INOUT) :: val(:)
  INTEGER(I4B), INTENT(OUT) :: tsize

  tsize = obj%len
  val(1:tsize) = obj%val(1:tsize)

END SUBROUTINE Master_Get_vec_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

PURE SUBROUTINE Master_Get_mat_(obj, val, nrow, ncol)
  CLASS(FEVariable_), INTENT(IN) :: obj
  REAL(DFP), INTENT(INOUT) :: val(:, :)
  INTEGER(I4B), INTENT(OUT) :: nrow, ncol

  INTEGER(I4B) :: ii, jj, cnt

  nrow = obj%s(1)
  ncol = obj%s(2)

  cnt = 0
  DO jj = 1, ncol
    DO ii = 1, nrow
      cnt = cnt + 1
      val(ii, jj) = obj%val(cnt)
    END DO
  END DO
END SUBROUTINE Master_Get_mat_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

PURE SUBROUTINE Master_get_mat3_(obj, val, dim1, dim2, dim3)
  CLASS(FEVariable_), INTENT(IN) :: obj
  REAL(DFP), INTENT(INOUT) :: val(:, :, :)
  INTEGER(I4B), INTENT(OUT) :: dim1, dim2, dim3
  INTEGER(I4B) :: ii, jj, kk, cnt

  dim1 = obj%s(1)
  dim2 = obj%s(2)
  dim3 = obj%s(3)

  cnt = 0
  DO kk = 1, dim3
    DO jj = 1, dim2
      DO ii = 1, dim1
        cnt = cnt + 1
        val(ii, jj, kk) = obj%val(cnt)
      END DO
    END DO
  END DO

END SUBROUTINE Master_get_mat3_

!----------------------------------------------------------------------------
!                                                            getNodalvalues
!----------------------------------------------------------------------------

MODULE PROCEDURE Scalar_Space
ALLOCATE (val(obj%len))
val = obj%val(1:obj%len)
END PROCEDURE Scalar_Space

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE Scalar_Space_
CALL Master_Get_vec_(obj, val, tsize)
END PROCEDURE Scalar_Space_

!----------------------------------------------------------------------------
!                                                            getNodalvalues
!----------------------------------------------------------------------------

MODULE PROCEDURE Scalar_Time
ALLOCATE (val(obj%len))
val = obj%val(1:obj%len)
END PROCEDURE Scalar_Time

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE Scalar_Time_
CALL Master_Get_vec_(obj, val, tsize)
END PROCEDURE Scalar_Time_

!----------------------------------------------------------------------------
!                                                            getNodalvalues
!----------------------------------------------------------------------------

MODULE PROCEDURE Scalar_SpaceTime
INTEGER(I4B) :: ii, jj, cnt

ALLOCATE (val(obj%s(1), obj%s(2)))

cnt = 0
DO jj = 1, obj%s(2)
  DO ii = 1, obj%s(1)
    cnt = cnt + 1
    val(ii, jj) = obj%val(cnt)

  END DO
END DO

END PROCEDURE Scalar_SpaceTime

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE Scalar_SpaceTime_
CALL Master_Get_mat_(obj, val, nrow, ncol)
END PROCEDURE Scalar_SpaceTime_

!----------------------------------------------------------------------------
!                                                            getNodalvalues
!----------------------------------------------------------------------------

MODULE PROCEDURE Vector_Constant
ALLOCATE (val(obj%len))
val = obj%val(1:obj%len)
END PROCEDURE Vector_Constant

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE Vector_Constant_
CALL Master_Get_vec_(obj, val, tsize)
END PROCEDURE Vector_Constant_

!----------------------------------------------------------------------------
!                                                            getNodalvalues
!----------------------------------------------------------------------------

MODULE PROCEDURE Vector_Space
INTEGER(I4B) :: ii, jj, cnt

ALLOCATE (val(obj%s(1), obj%s(2)))

cnt = 0
DO jj = 1, obj%s(2)
  DO ii = 1, obj%s(1)
    cnt = cnt + 1
    val(ii, jj) = obj%val(cnt)
  END DO
END DO

END PROCEDURE Vector_Space

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE Vector_Space_
CALL Master_Get_mat_(obj, val, nrow, ncol)
END PROCEDURE Vector_Space_

!----------------------------------------------------------------------------
!                                                            getNodalvalues
!----------------------------------------------------------------------------

MODULE PROCEDURE Vector_Time
INTEGER(I4B) :: ii, jj, cnt

ALLOCATE (val(obj%s(1), obj%s(2)))

cnt = 0
DO jj = 1, obj%s(2)
  DO ii = 1, obj%s(1)
    cnt = cnt + 1
    val(ii, jj) = obj%val(cnt)
  END DO
END DO
END PROCEDURE Vector_Time

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE Vector_Time_
CALL Master_Get_mat_(obj, val, nrow, ncol)
END PROCEDURE Vector_Time_

!----------------------------------------------------------------------------
!                                                            getNodalvalues
!----------------------------------------------------------------------------

MODULE PROCEDURE Vector_SpaceTime
INTEGER(I4B) :: ii, jj, kk, cnt

ALLOCATE (val(obj%s(1), obj%s(2), obj%s(3)))

cnt = 0
DO kk = 1, obj%s(3)
  DO jj = 1, obj%s(2)
    DO ii = 1, obj%s(1)
      cnt = cnt + 1
      val(ii, jj, kk) = obj%val(cnt)
    END DO
  END DO
END DO
END PROCEDURE Vector_SpaceTime

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE Vector_SpaceTime_
CALL Master_Get_mat3_(obj, val, dim1, dim2, dim3)
END PROCEDURE Vector_SpaceTime_

!----------------------------------------------------------------------------
!                                                            getNodalvalues
!----------------------------------------------------------------------------

MODULE PROCEDURE Matrix_Constant
INTEGER(I4B) :: ii, jj, cnt

ALLOCATE (val(obj%s(1), obj%s(2)))

cnt = 0
DO jj = 1, obj%s(2)
  DO ii = 1, obj%s(1)
    cnt = cnt + 1
    val(ii, jj) = obj%val(cnt)
  END DO
END DO
END PROCEDURE Matrix_Constant

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE Matrix_Constant_
CALL Master_Get_mat_(obj, val, nrow, ncol)
END PROCEDURE Matrix_Constant_

!----------------------------------------------------------------------------
!                                                            getNodalvalues
!----------------------------------------------------------------------------

MODULE PROCEDURE Matrix_Space
INTEGER(I4B) :: ii, jj, kk, cnt

ALLOCATE (val(obj%s(1), obj%s(2), obj%s(3)))

cnt = 0
DO kk = 1, obj%s(3)
  DO jj = 1, obj%s(2)
    DO ii = 1, obj%s(1)
      cnt = cnt + 1
      val(ii, jj, kk) = obj%val(cnt)
    END DO
  END DO
END DO
END PROCEDURE Matrix_Space

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE Matrix_Space_
CALL Master_Get_mat3_(obj, val, dim1, dim2, dim3)
END PROCEDURE Matrix_Space_

!----------------------------------------------------------------------------
!                                                            getNodalvalues
!----------------------------------------------------------------------------

MODULE PROCEDURE Matrix_Time
INTEGER(I4B) :: ii, jj, kk, cnt

ALLOCATE (val(obj%s(1), obj%s(2), obj%s(3)))

cnt = 0
DO kk = 1, obj%s(3)
  DO jj = 1, obj%s(2)
    DO ii = 1, obj%s(1)
      cnt = cnt + 1
      val(ii, jj, kk) = obj%val(cnt)
    END DO
  END DO
END DO
END PROCEDURE Matrix_Time

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE Matrix_Time_
CALL Master_Get_mat3_(obj, val, dim1, dim2, dim3)
END PROCEDURE Matrix_Time_

!----------------------------------------------------------------------------
!                                                            getNodalvalues
!----------------------------------------------------------------------------

MODULE PROCEDURE Matrix_SpaceTime
INTEGER(I4B) :: ii, jj, kk, ll, cnt

ALLOCATE (val(obj%s(1), obj%s(2), obj%s(3), obj%s(4)))

cnt = 0
DO ll = 1, obj%s(4)
  DO kk = 1, obj%s(3)
    DO jj = 1, obj%s(2)
      DO ii = 1, obj%s(1)
        cnt = cnt + 1
        val(ii, jj, kk, ll) = obj%val(cnt)
      END DO
    END DO
  END DO
END DO
END PROCEDURE Matrix_SpaceTime

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE Matrix_SpaceTime_
INTEGER(I4B) :: ii, jj, kk, ll, cnt

dim1 = obj%s(1)
dim2 = obj%s(2)
dim3 = obj%s(3)
dim4 = obj%s(4)

cnt = 0
DO ll = 1, dim4
  DO kk = 1, dim3
    DO jj = 1, dim2
      DO ii = 1, dim1
        cnt = cnt + 1
        val(ii, jj, kk, ll) = obj%val(cnt)
      END DO
    END DO
  END DO
END DO

END PROCEDURE Matrix_SpaceTime_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE GetMethods
