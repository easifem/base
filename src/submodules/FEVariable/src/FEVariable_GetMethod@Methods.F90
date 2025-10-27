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

SUBMODULE(FEVariable_GetMethod) Methods
USE ReallocateUtility, ONLY: Reallocate
USE StringUtility, ONLY: UpperCase
USE BaseType, ONLY: feopt => TypeFEVariableOpt

IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                                        Len
!----------------------------------------------------------------------------

MODULE PROCEDURE fevar_len
ans = obj%len
END PROCEDURE fevar_len

!----------------------------------------------------------------------------
!                                                        FEVariable_ToString
!----------------------------------------------------------------------------

MODULE PROCEDURE FEVariable_ToChar

SELECT CASE (name)
CASE (feopt%scalar)
  ans = "Scalar"

CASE (feopt%vector)
  ans = "Vector"

CASE (feopt%matrix)
  ans = "Matrix"

CASE DEFAULT
  ans = "Scalar"

END SELECT

IF (PRESENT(isUpper)) THEN
  IF (isUpper) THEN
    ans = UpperCase(ans)
  END IF
END IF

END PROCEDURE FEVariable_ToChar

!----------------------------------------------------------------------------
!                                                        FEVariable_ToInteger
!----------------------------------------------------------------------------

MODULE PROCEDURE FEVariable_ToInteger
CHARACTER(1) :: name0

name0 = name(1:1)

SELECT CASE (name0)
CASE ("S", "s")
  ans = feopt%scalar

CASE ("V", "v")
  ans = feopt%vector

CASE ("M", "m")
  ans = feopt%matrix

CASE DEFAULT
  ans = feopt%scalar

END SELECT

END PROCEDURE FEVariable_ToInteger

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
LOGICAL(LGT) :: isok

isok = PRESENT(dim)
IF (isok) THEN
  ans = obj%s(dim)
ELSE
  ans = obj%len
END IF
END PROCEDURE fevar_Size

!----------------------------------------------------------------------------
!                                                              GetTotalShape
!----------------------------------------------------------------------------

MODULE PROCEDURE fevar_GetTotalShape
SELECT CASE (obj%rank)
CASE (feopt%scalar)
  SELECT CASE (obj%vartype)
  CASE (feopt%constant, feopt%space, feopt%time)
    ans = 1
  CASE (feopt%spaceTime)
    ans = 2
  END SELECT

CASE (feopt%vector)
  SELECT CASE (obj%vartype)
  CASE (feopt%constant)
    ans = 1
  CASE (feopt%space, feopt%time)
    ans = 2
  CASE (feopt%spaceTime)
    ans = 3
  END SELECT

CASE (feopt%matrix)
  SELECT CASE (obj%vartype)
  CASE (feopt%constant)
    ans = 2
  CASE (feopt%space, feopt%time)
    ans = 3
  CASE (feopt%spaceTime)
    ans = 4
  END SELECT

END SELECT
END PROCEDURE fevar_GetTotalShape

!----------------------------------------------------------------------------
!                                                                      Shape
!----------------------------------------------------------------------------

MODULE PROCEDURE fevar_Shape
INTEGER(I4B) :: tsize
tsize = GetTotalShape(obj=obj)
CALL Reallocate(ans, tsize)
ans(1:tsize) = obj%s(1:tsize)
END PROCEDURE fevar_Shape

!----------------------------------------------------------------------------
!                                                                      Shape
!----------------------------------------------------------------------------

MODULE PROCEDURE fevar_GetShape
tsize = GetTotalShape(obj=obj)
ans(1:tsize) = obj%s(1:tsize)
END PROCEDURE fevar_GetShape

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
ans = obj%defineon .EQ. feopt%nodal
END PROCEDURE fevar_isNodalVariable

!----------------------------------------------------------------------------
!                                                            isNodalVariable
!----------------------------------------------------------------------------

MODULE PROCEDURE fevar_isQuadratureVariable
ans = obj%defineon .NE. feopt%nodal
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
!
!----------------------------------------------------------------------------

PURE SUBROUTINE Master_get_mat4_(obj, val, dim1, dim2, dim3, dim4)
  CLASS(FEVariable_), INTENT(IN) :: obj
  REAL(DFP), INTENT(INOUT) :: val(:, :, :, :)
  INTEGER(I4B), INTENT(OUT) :: dim1, dim2, dim3, dim4

  ! Internal variables
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
END SUBROUTINE Master_get_mat4_

!----------------------------------------------------------------------------
!                                                            getNodalvalues
!----------------------------------------------------------------------------

MODULE PROCEDURE Scalar_Space
INTEGER(I4B) :: tsize
ALLOCATE (val(obj%len))
CALL Master_Get_vec_(obj=obj, val=val, tsize=tsize)
END PROCEDURE Scalar_Space

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE Scalar_Space_
CALL Master_Get_vec_(obj=obj, val=val, tsize=tsize)
END PROCEDURE Scalar_Space_

!----------------------------------------------------------------------------
!                                                            getNodalvalues
!----------------------------------------------------------------------------

MODULE PROCEDURE Scalar_Time
INTEGER(I4B) :: tsize
ALLOCATE (val(obj%len))
CALL Master_Get_vec_(obj=obj, val=val, tsize=tsize)
END PROCEDURE Scalar_Time

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE Scalar_Time_
CALL Master_Get_vec_(obj=obj, val=val, tsize=tsize)
END PROCEDURE Scalar_Time_

!----------------------------------------------------------------------------
!                                                            getNodalvalues
!----------------------------------------------------------------------------

MODULE PROCEDURE Scalar_SpaceTime
INTEGER(I4B) :: nrow, ncol
ALLOCATE (val(obj%s(1), obj%s(2)))
CALL Master_Get_mat_(obj=obj, val=val, nrow=nrow, ncol=ncol)
END PROCEDURE Scalar_SpaceTime

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE Scalar_SpaceTime_
CALL Master_Get_mat_(obj=obj, val=val, nrow=nrow, ncol=ncol)
END PROCEDURE Scalar_SpaceTime_

!----------------------------------------------------------------------------
!                                                            getNodalvalues
!----------------------------------------------------------------------------

MODULE PROCEDURE Vector_Constant
INTEGER(I4B) :: tsize
ALLOCATE (val(obj%len))
CALL Master_Get_vec_(obj=obj, val=val, tsize=tsize)
END PROCEDURE Vector_Constant

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE Vector_Constant_
CALL Master_Get_vec_(obj=obj, val=val, tsize=tsize)
END PROCEDURE Vector_Constant_

!----------------------------------------------------------------------------
!                                                            getNodalvalues
!----------------------------------------------------------------------------

MODULE PROCEDURE Vector_Space
INTEGER(I4B) :: nrow, ncol
ALLOCATE (val(obj%s(1), obj%s(2)))
CALL Master_Get_mat_(obj=obj, val=val, nrow=nrow, ncol=ncol)
END PROCEDURE Vector_Space

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE Vector_Space_
CALL Master_Get_mat_(obj=obj, val=val, nrow=nrow, ncol=ncol)
END PROCEDURE Vector_Space_

!----------------------------------------------------------------------------
!                                                            getNodalvalues
!----------------------------------------------------------------------------

MODULE PROCEDURE Vector_Time
INTEGER(I4B) :: nrow, ncol
ALLOCATE (val(obj%s(1), obj%s(2)))
CALL Master_Get_mat_(obj=obj, val=val, nrow=nrow, ncol=ncol)
END PROCEDURE Vector_Time

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE Vector_Time_
CALL Master_Get_mat_(obj=obj, val=val, nrow=nrow, ncol=ncol)
END PROCEDURE Vector_Time_

!----------------------------------------------------------------------------
!                                                            getNodalvalues
!----------------------------------------------------------------------------

MODULE PROCEDURE Vector_SpaceTime
INTEGER(I4B) :: dim1, dim2, dim3
ALLOCATE (val(obj%s(1), obj%s(2), obj%s(3)))
CALL Master_Get_mat3_(obj=obj, val=val, dim1=dim1, dim2=dim2, dim3=dim3)
END PROCEDURE Vector_SpaceTime

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE Vector_SpaceTime_
CALL Master_Get_mat3_(obj=obj, val=val, dim1=dim1, dim2=dim2, dim3=dim3)
END PROCEDURE Vector_SpaceTime_

!----------------------------------------------------------------------------
!                                                            getNodalvalues
!----------------------------------------------------------------------------

MODULE PROCEDURE Matrix_Constant
INTEGER(I4B) :: nrow, ncol
ALLOCATE (val(obj%s(1), obj%s(2)))
CALL Master_Get_mat_(obj=obj, val=val, nrow=nrow, ncol=ncol)
END PROCEDURE Matrix_Constant

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE Matrix_Constant_
CALL Master_Get_mat_(obj=obj, val=val, nrow=nrow, ncol=ncol)
END PROCEDURE Matrix_Constant_

!----------------------------------------------------------------------------
!                                                            getNodalvalues
!----------------------------------------------------------------------------

MODULE PROCEDURE Matrix_Space
INTEGER(I4B) :: dim1, dim2, dim3
ALLOCATE (val(obj%s(1), obj%s(2), obj%s(3)))
CALL Master_Get_mat3_(obj=obj, val=val, dim1=dim1, dim2=dim2, dim3=dim3)
END PROCEDURE Matrix_Space

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE Matrix_Space_
CALL Master_Get_mat3_(obj=obj, val=val, dim1=dim1, dim2=dim2, dim3=dim3)
END PROCEDURE Matrix_Space_

!----------------------------------------------------------------------------
!                                                            getNodalvalues
!----------------------------------------------------------------------------

MODULE PROCEDURE Matrix_Time
INTEGER(I4B) :: dim1, dim2, dim3
ALLOCATE (val(obj%s(1), obj%s(2), obj%s(3)))
CALL Master_Get_mat3_(obj=obj, val=val, dim1=dim1, dim2=dim2, dim3=dim3)
END PROCEDURE Matrix_Time

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE Matrix_Time_
CALL Master_Get_mat3_(obj=obj, val=val, dim1=dim1, dim2=dim2, dim3=dim3)
END PROCEDURE Matrix_Time_

!----------------------------------------------------------------------------
!                                                            getNodalvalues
!----------------------------------------------------------------------------

MODULE PROCEDURE Matrix_SpaceTime
INTEGER(I4B) :: dim1, dim2, dim3, dim4
ALLOCATE (val(obj%s(1), obj%s(2), obj%s(3), obj%s(4)))
CALL Master_get_mat4_(obj=obj, val=val, dim1=dim1, dim2=dim2, dim3=dim3, &
                      dim4=dim4)
END PROCEDURE Matrix_SpaceTime

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE Matrix_SpaceTime_
CALL Master_get_mat4_(obj=obj, val=val, dim1=dim1, dim2=dim2, dim3=dim3, &
                      dim4=dim4)
END PROCEDURE Matrix_SpaceTime_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE Methods
