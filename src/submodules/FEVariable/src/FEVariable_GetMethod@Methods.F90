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
!                                                              GetTotalShape
!----------------------------------------------------------------------------

MODULE PROCEDURE GetTotalShapeFromRankVarCase
! scalar 0 vector 1 matrix 2
! constant 0, space 1, time 2, spacetime 3

SELECT CASE (rankCase)
! scalar scalar
CASE (00)

  SELECT CASE (varCase)
  CASE (00, 11, 22, 33)
    ans = tshape1 ! tshape1 and tshape2 are same in this case

  CASE (01, 02, 03, 13, 23)
    ans = tshape2

  CASE (10, 20, 30, 31, 32)
    ans = tshape1

  CASE (12, 21)
    ans = 2
  END SELECT

! vector vector
CASE (11)

  SELECT CASE (varCase)
  CASE (00, 11, 22, 33)
    ans = MAX(tshape1, tshape2)

  CASE (01, 02, 03, 13, 23)
    ans = tshape2

  CASE (10, 20, 30, 31, 32)
    ans = tshape1

  CASE (12, 21)
    ans = 3
  END SELECT

! matrix matrix
CASE (22)

  SELECT CASE (varCase)
  CASE (00, 11, 22, 33)
    ans = MAX(tshape1, tshape2)

  CASE (01, 02, 03, 13, 23)
    ans = tshape2

  CASE (10, 20, 30, 31, 32)
    ans = tshape1

  CASE (12, 21)
    ans = 4
  END SELECT

! scalar vector
CASE (01)

  SELECT CASE (varCase)
  CASE (00, 11, 22, 33, 01, 02, 03, 13, 23)
    ans = tshape2

  CASE (10, 20, 30, 31, 32)
    ans = tshape1 + 1

  CASE (12, 21)
    ans = 3
  END SELECT

! scalar matrix
CASE (02)

  SELECT CASE (varCase)
  CASE (00, 11, 22, 33, 01, 02, 03, 13, 23)
    ans = tshape2

  CASE (10, 20, 30, 31, 32)
    ans = tshape1 + 2

  CASE (12, 21)
    ans = 4
  END SELECT

! vector scalar
CASE (10)

  SELECT CASE (varCase)
  CASE (00, 11, 22, 33, 10, 20, 30, 31, 32)
    ans = tshape1

  CASE (01, 02, 03, 13, 23)
    ans = tshape2 + 1

  CASE (12, 21)
    ans = 3
  END SELECT

! vector matrix
CASE (12)

  SELECT CASE (varCase)
  CASE (00, 11, 22, 33, 01, 02, 03, 13, 23)
    ans = tshape2

  CASE (10, 20, 30, 31, 32)
    ans = tshape1 + 1

  CASE (12, 21)
    ans = 4
  END SELECT

! matrix scalar
CASE (20)

  SELECT CASE (varCase)
  CASE (00, 11, 22, 33, 10, 20, 30, 31, 32)
    ans = tshape1

  CASE (01, 02, 03, 13, 23)
    ans = tshape2 + 2

  CASE (12, 21)
    ans = 4
  END SELECT

! matrix vector
CASE (21)

  SELECT CASE (varCase)
  CASE (00, 11, 22, 33, 10, 20, 30, 31, 32)
    ans = tshape1

  CASE (01, 02, 03, 13, 23)
    ans = tshape2 + 1

  CASE (12, 21)
    ans = 4
  END SELECT

END SELECT
END PROCEDURE GetTotalShapeFromRankVarCase

!----------------------------------------------------------------------------
!                                                    GetShapeFromRankVarCase
!----------------------------------------------------------------------------

MODULE PROCEDURE GetShapeFromRankVarCase
INTEGER(I4B) :: ii

! scalar 0 vector 1 matrix 2
! constant 0, space 1, time 2, spacetime 3

SELECT CASE (rankCase)
! scalar scalar
CASE (00)

  SELECT CASE (varCase)
  CASE (00, 11, 22, 33)
    tsize = tshape1 ! tshape1 and tshape2 are same in this case
    ans(1:tsize) = s1(1:tsize)
    ! This means number of space and time points should be same

  CASE (01, 02, 03, 13, 23)
    tsize = tshape2
    ans(1:tsize) = s2(1:tsize)
    ! Eventhough we should check maximum of space and time
    ! for 13 and 23 cases, we are skipping it for now
    ! This means number of space and time points should be same

  CASE (10, 20, 30, 31, 32)
    tsize = tshape1
    ans(1:tsize) = s1(1:tsize)

  CASE (12)
    tsize = 2
    ans(1) = s1(1)
    ans(2) = s2(1)

  CASE (21)
    tsize = 2
    ans(1) = s2(1)
    ans(2) = s1(1)

  END SELECT

! vector vector
CASE (11)

  SELECT CASE (varCase)
  CASE (00, 11, 22, 33)
    tsize = tshape1
    ans(1:tsize) = s1(1:tsize)
    ! This means number of space and time points should be same

  CASE (01, 02, 03, 13, 23)
    tsize = tshape2
    ans(1:tsize) = s2(1:tsize)

  CASE (10, 20, 30, 31, 32)
    tsize = tshape1
    ans(1:tsize) = s1(1:tsize)

  CASE (12)
    tsize = 3
    ans(1) = s1(1) ! component
    ans(2) = s1(2) ! space
    ans(3) = s2(2) ! time

  CASE (21)
    tsize = 3
    ans(1) = s1(1) ! component
    ans(2) = s2(2) ! space
    ans(3) = s1(2) ! time
  END SELECT

! matrix matrix
CASE (22)

  SELECT CASE (varCase)
  CASE (00, 11, 22, 33)
    tsize = tshape1
    ans(1:tsize) = s1(1:tsize)
    ! This means number of space and time points should be same

  CASE (01, 02, 03, 13, 23)
    tsize = tshape2
    ans(1:tsize) = s2(1:tsize)

  CASE (10, 20, 30, 31, 32)
    tsize = tshape1
    ans(1:tsize) = s1(1:tsize)

  CASE (12)
    tsize = 4
    ans(1:2) = s1(1:2)
    ans(3) = s1(3)
    ans(4) = s2(3)

  CASE (21)
    tsize = 4
    ans(1:2) = s1(1:2)
    ans(3) = s2(3)
    ans(4) = s1(3)

  END SELECT

! scalar vector
CASE (01)

  SELECT CASE (varCase)
  CASE (00, 11, 22, 33, 01, 02, 03, 13, 23)
    tsize = tshape2
    ans(1:tsize) = s2(1:tsize)

  CASE (10, 20, 30, 31, 32)
    tsize = tshape1 + 1
    ans(1) = s2(1)
    ans(2:tsize) = s1(1:tshape1)

  CASE (12)
    tsize = 3
    ans(1) = s2(1)
    ans(2) = s1(1)
    ans(3) = s2(2)

  CASE (21)
    tsize = 3
    ans(1) = s2(1)
    ans(2) = s2(2)
    ans(3) = s1(1)
  END SELECT

! scalar matrix
CASE (02)

  SELECT CASE (varCase)
  CASE (00, 11, 22, 33, 01, 02, 03, 13, 23)
    tsize = tshape2
    ans(1:tsize) = s2(1:tsize)

  CASE (10, 20, 30, 31, 32)
    tsize = tshape1 + 2
    ans(1:2) = s2(1:2)
    ans(3:tsize) = s1(1:tshape1)

  CASE (12)
    tsize = 4
    ans(1:2) = s2(1:2)
    ans(3) = s1(1) ! space
    ans(4) = s2(3) ! time

  CASE (21)
    tsize = 4
    ans(1:2) = s2(1:2)
    ans(3) = s2(3) ! space
    ans(4) = s1(1) ! time

  END SELECT

! vector scalar
CASE (10)

  SELECT CASE (varCase)
  CASE (00, 11, 22, 33, 10, 20, 30, 31, 32)
    tsize = tshape1
    ans(1:tsize) = s1(1:tsize)

  CASE (01, 02, 03, 13, 23)
    tsize = tshape2 + 1
    ans(1) = s1(1)
    ans(2:tsize) = s2(1:tshape2)

  CASE (12)
    tsize = 3
    ans(1) = s1(1)
    ans(2) = s1(2)
    ans(3) = s2(1)

  CASE (21)
    tsize = 3
    ans(1) = s1(1)
    ans(2) = s2(1)
    ans(3) = s1(2)
  END SELECT

! vector matrix
CASE (12)

  SELECT CASE (varCase)
  CASE (00, 11, 22, 33, 01, 02, 03, 13, 23)
    tsize = tshape2
    ans(1:tsize) = s2(1:tsize)

  CASE (10, 20, 30, 31, 32)
    tsize = tshape1 + 1
    ans(1:2) = s2(1:2)
    DO ii = 3, tsize
      ans(ii) = s1(ii - 1)
    END DO

  CASE (12)
    tsize = 4
    ans(1:2) = s2(1:2)
    ans(3) = s1(1)
    ans(4) = s2(3)

  CASE (21)
    tsize = 4
    ans(1:2) = s2(1:2)
    ans(3) = s2(3)
    ans(4) = s1(1)
  END SELECT

! matrix scalar
CASE (20)

  SELECT CASE (varCase)
  CASE (00, 11, 22, 33, 10, 20, 30, 31, 32)
    tsize = tshape1
    ans(1:tsize) = s1(1:tsize)

  CASE (01, 02, 03, 13, 23)
    tsize = tshape2 + 2
    ans(1:2) = s1(1:2)
    DO ii = 3, tsize
      ans(ii) = s2(ii - 2)
    END DO

  CASE (12)
    tsize = 4
    ans(1:2) = s1(1:2)
    ans(3) = s1(3)
    ans(4) = s2(1)

  CASE (21)
    tsize = 4
    ans(1:2) = s1(1:2)
    ans(3) = s2(1)
    ans(4) = s1(3)
  END SELECT

! matrix vector
CASE (21)

  SELECT CASE (varCase)
  CASE (00, 11, 22, 33, 10, 20, 30, 31, 32)
    tsize = tshape1
    ans(1:tsize) = s1(1:tsize)

  CASE (01, 02, 03, 13, 23)
    tsize = tshape2 + 1
    ans(1:2) = s1(1:2)
    DO ii = 3, tsize
      ans(ii) = s2(ii - 1)
    END DO

  CASE (12)
    tsize = 4
    ans(1:3) = s1(1:3)
    ans(4) = s2(2)

  CASE (21)
    tsize = 4
    ans(1:2) = s1(1:2)
    ans(4) = s1(3)
    ans(3) = s2(2)

  END SELECT

END SELECT
END PROCEDURE GetShapeFromRankVarCase

!----------------------------------------------------------------------------
!                                                                 GetVarType
!----------------------------------------------------------------------------

MODULE PROCEDURE GetVarTypeFromVarCase
SELECT CASE (varCase)
CASE (00)
  ans = feopt%constant

CASE (01, 10, 11)
  ans = feopt%space

CASE (02, 20, 22)
  ans = feopt%time

CASE (03, 13, 23, 30, 31, 32, 33, 12, 21)
  ans = feopt%spacetime

END SELECT
END PROCEDURE GetVarTypeFromVarCase

!----------------------------------------------------------------------------
!                                                                    GetRank
!----------------------------------------------------------------------------

MODULE PROCEDURE GetRankFromRankCase
SELECT CASE (rankCase)
! scalar
CASE (00)
  ans = feopt%scalar

! vector: scalar vector, vector scalar, vector vector
CASE (01, 10, 11)
  ans = feopt%vector

! matrix: scalar matrix, matrix scalar, matrix matrix
CASE (02, 20, 22)
  ans = feopt%matrix

END SELECT
END PROCEDURE GetRankFromRankCase

!----------------------------------------------------------------------------
!                                                              GetRankCase
!----------------------------------------------------------------------------

MODULE PROCEDURE GetRankCase
INTEGER(I4B) :: a, b

a = 0
b = 0

SELECT CASE (rank1)
CASE (feopt%scalar)
  a = 0
CASE (feopt%vector)
  a = 1
CASE (feopt%matrix)
  a = 2
END SELECT

SELECT CASE (rank2)
CASE (feopt%scalar)
  b = 0
CASE (feopt%vector)
  b = 1
CASE (feopt%matrix)
  b = 2
END SELECT

ans = a * 10 + b
END PROCEDURE GetRankCase

!----------------------------------------------------------------------------
!                                                                  GetVarCase
!----------------------------------------------------------------------------

MODULE PROCEDURE GetVarCase
INTEGER(I4B) :: a, b

a = 0
b = 0

SELECT CASE (vartype1)
CASE (feopt%constant)
  a = 0
CASE (feopt%space)
  a = 1
CASE (feopt%time)
  a = 2
CASE (feopt%spacetime)
  a = 3
END SELECT

SELECT CASE (vartype2)
CASE (feopt%constant)
  b = 0
CASE (feopt%space)
  b = 1
CASE (feopt%time)
  b = 2
CASE (feopt%spacetime)
  b = 3
END SELECT

ans = a * 10 + b
END PROCEDURE GetVarCase

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
ans = obj%tshape
! SELECT CASE (obj%rank)
! CASE (feopt%scalar)
!   SELECT CASE (obj%vartype)
!   CASE (feopt%constant, feopt%space, feopt%time)
!     ans = 1
!   CASE (feopt%spaceTime)
!     ans = 2
!   END SELECT
!
! CASE (feopt%vector)
!   SELECT CASE (obj%vartype)
!   CASE (feopt%constant)
!     ans = 1
!   CASE (feopt%space, feopt%time)
!     ans = 2
!   CASE (feopt%spaceTime)
!     ans = 3
!   END SELECT
!
! CASE (feopt%matrix)
!   SELECT CASE (obj%vartype)
!   CASE (feopt%constant)
!     ans = 2
!   CASE (feopt%space, feopt%time)
!     ans = 3
!   CASE (feopt%spaceTime)
!     ans = 4
!   END SELECT
!
! END SELECT
END PROCEDURE fevar_GetTotalShape

!----------------------------------------------------------------------------
!                                                                      Shape
!----------------------------------------------------------------------------

MODULE PROCEDURE fevar_Shape
! INTEGER(I4B) :: tsize
! tsize = GetTotalShape(obj=obj)
CALL Reallocate(ans, obj%tshape)
ans(1:obj%tshape) = obj%s(1:obj%tshape)
END PROCEDURE fevar_Shape

!----------------------------------------------------------------------------
!                                                                      Shape
!----------------------------------------------------------------------------

MODULE PROCEDURE fevar_GetShape
! tsize = GetTotalShape(obj=obj)
tsize = obj%tshape
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
