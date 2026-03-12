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

SUBMODULE(ElemshapeData_GradientMethods) Methods
USE MatmulUtility, ONLY: MyMatmul => MATMUL
USE ContractionUtility, ONLY: Contraction
USE ReallocateUtility, ONLY: Reallocate
USE SwapUtility, ONLY: SWAP
USE SwapUtility, ONLY: SWAP_
USE BaseType, ONLY: fevaropt => TypeFEVariableOpt
USE BaseType, ONLY: math => TypeMathOpt
USE FEVariable_Method, ONLY: FEVariableGet => Get
USE FEVariable_Method, ONLY: FEVariableGet_ => Get_
USE FEVariable_Method, ONLY: FEVariableSize => Size
USE FEVariable_Method, ONLY: FEVariableShape => Shape
USE FEVariable_Method, ONLY: FEVariableSet => Set
USE FEVariable_Method, ONLY: QuadratureVariable
USE BaseType, ONLY: TypeFEVariableScalar
USE BaseType, ONLY: TypeFEVariableVector
USE BaseType, ONLY: TypeFEVariableMatrix
USE BaseType, ONLY: TypeFEVariableConstant
USE BaseType, ONLY: TypeFEVariableSpace
USE BaseType, ONLY: TypeFEVariableSpaceTime

IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                         GetSpatialGradient
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetSpatialGradient1
INTEGER(I4B) :: nrow, ncol

nrow = obj%nsd
ncol = obj%nips
CALL Reallocate(ans, nrow, ncol)
CALL GetSpatialGradient_(obj=obj, val=val, ans=ans, nrow=nrow, ncol=ncol)
END PROCEDURE obj_GetSpatialGradient1

!----------------------------------------------------------------------------
!                                                        GetSpatialGradient_
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetSpatialGradient_1
LOGICAL(LGT) :: isok

nrow = obj%nsd
ncol = obj%nips
isok = obj%nsd .EQ. obj%xidim

IF (isok) THEN
  ans(1:nrow, 1:ncol) = MyMatmul( &
                        val(1:obj%nns), &
                        obj%dNdXt(1:obj%nns, 1:obj%nsd, 1:obj%nips))
ELSE
  ans(1:nrow, 1:ncol) = math%zero
END IF
END PROCEDURE obj_GetSpatialGradient_1

!----------------------------------------------------------------------------
!                                                         GetSpatialGradient
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetSpatialGradient2
INTEGER(I4B) :: dim1, dim2, dim3

dim1 = SIZE(val, 1)
dim2 = obj%nsd
dim3 = obj%nips

CALL Reallocate(ans, dim1, dim2, dim3)

CALL GetSpatialGradient_(obj=obj, val=val, ans=ans, dim1=dim1, dim2=dim2, &
                         dim3=dim3)
END PROCEDURE obj_GetSpatialGradient2

!----------------------------------------------------------------------------
!                                                        GetSpatialGradient_
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetSpatialGradient_2
LOGICAL(LGT) :: isok

dim1 = SIZE(val, 1)
dim2 = obj%nsd
dim3 = obj%nips

isok = obj%nsd .EQ. obj%xidim
IF (isok) THEN
  ans(1:dim1, 1:dim2, 1:dim3) = MyMatmul( &
                                val(1:dim1, 1:obj%nns), &
                                obj%dNdXt(1:obj%nns, 1:obj%nsd, 1:obj%nips))
ELSE
  ans(1:dim1, 1:dim2, 1:dim3) = math%zero
END IF
END PROCEDURE obj_GetSpatialGradient_2

!----------------------------------------------------------------------------
!                                                         GetSpatialGradient
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetSpatialGradient3
INTEGER(I4B) :: nrow, ncol
nrow = obj%nsd
ncol = obj%nips
CALL Reallocate(ans, nrow, ncol)
CALL GetSpatialGradient_(obj=obj, val=val, ans=ans, nrow=nrow, ncol=ncol)
END PROCEDURE obj_GetSpatialGradient3

!----------------------------------------------------------------------------
!                                                        GetSpatialGradient_
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetSpatialGradient_3
LOGICAL(LGT) :: isok

nrow = obj%nsd
ncol = obj%nips

isok = obj%nsd .EQ. obj%xidim
IF (isok) THEN
  ans(1:nrow, 1:ncol) = Contraction( &
                        val(1:obj%nns, 1:obj%nnt), &
                        obj%dNTdXt(1:obj%nns, 1:obj%nnt, &
                                   1:obj%nsd, 1:obj%nips))
ELSE
  ans(1:nrow, 1:ncol) = math%zero
END IF
END PROCEDURE obj_GetSpatialGradient_3

!----------------------------------------------------------------------------
!                                                         GetSpatialGradient
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetSpatialGradient4
INTEGER(I4B) :: dim1, dim2, dim3

dim1 = SIZE(val, 1)
dim2 = obj%nsd
dim3 = obj%nips
CALL Reallocate(ans, dim1, dim2, dim3)
CALL GetSpatialGradient_( &
  obj=obj, val=val, ans=ans, dim1=dim1, dim2=dim2, dim3=dim3)
END PROCEDURE obj_GetSpatialGradient4

!----------------------------------------------------------------------------
!                                                        GetSpatialGradient_
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetSpatialGradient_4
INTEGER(I4B) :: ii, jj, ips
LOGICAL(LGT) :: isok

dim1 = SIZE(val, 1)
dim2 = obj%nsd
dim3 = obj%nips
ans(1:dim1, 1:dim2, 1:dim3) = math%zero

isok = obj%nsd .EQ. obj%xidim
IF (isok) THEN

  DO ips = 1, dim3
    DO jj = 1, dim2
      DO ii = 1, dim1

        ans(ii, jj, ips) = Contraction( &
                           a1=val(ii, 1:obj%nns, 1:obj%nnt), &
                           a2=obj%dNTdXt(1:obj%nns, 1:obj%nnt, jj, ips))

      END DO
    END DO
  END DO

ELSE

  ans(1:dim1, 1:dim2, 1:dim3) = math%zero
END IF
END PROCEDURE obj_GetSpatialGradient_4

!----------------------------------------------------------------------------
!                                                        GetSpatialGradient
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetSpatialGradient5
INTEGER(I4B) :: nrow, ncol
REAL(DFP), ALLOCATABLE :: tempVec(:)

nrow = obj%nsd
ncol = obj%nips
CALL Reallocate(ans, nrow, ncol)
CALL Reallocate(tempVec, obj%nns)
CALL GetSpatialGradient_(obj=obj, val=val, valRank=valRank, &
                         ans=ans, nrow=nrow, ncol=ncol, tempVec=tempVec)
END PROCEDURE obj_GetSpatialGradient5

!----------------------------------------------------------------------------
!                                                        GetSpatialGradient
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetSpatialGradient_5
INTEGER(I4B) :: i1

SELECT CASE (val%varType)
CASE (fevaropt%space)
  CALL FEVariableGet_( &
    obj=val, rank=TypeFEVariableScalar, varType=TypeFEVariableSpace, &
    val=tempVec, tsize=i1)

  CALL GetSpatialGradient_(obj=obj, ans=ans, nrow=nrow, ncol=ncol, &
                           val=tempVec(1:i1))

CASE DEFAULT
  nrow = obj%nsd
  ncol = obj%nips
  ans(1:nrow, 1:ncol) = math%zero
END SELECT
END PROCEDURE obj_GetSpatialGradient_5

!----------------------------------------------------------------------------
!                                                        GetSpatialGradient
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetSpatialGradient6
INTEGER(I4B) :: dim1, dim2, dim3, s(1)
REAL(DFP), ALLOCATABLE :: tempMat2(:, :)

s = FEVariableShape(val)
dim1 = s(1)
dim2 = obj%nsd
dim3 = obj%nips
CALL Reallocate(ans, dim1, dim2, dim3)
CALL Reallocate(tempMat2, dim1, obj%nns)

CALL GetSpatialGradient_(obj=obj, val=val, valRank=valRank, &
                         ans=ans, dim1=dim1, dim2=dim2, &
                         dim3=dim3, tempMat2=tempMat2)

DEALLOCATE (tempMat2)
END PROCEDURE obj_GetSpatialGradient6

!----------------------------------------------------------------------------
!                                                        GetSpatialGradient
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetSpatialGradient_6
INTEGER(I4B) :: s(1), i1, i2

SELECT CASE (val%varType)
CASE (fevaropt%space)
  CALL FEVariableGet_(obj=val, rank=TypeFEVariableVector, &
                      varType=TypeFEVariableSpace, &
                      val=tempMat2, nrow=i1, ncol=i2)

  CALL GetSpatialGradient_(obj=obj, ans=ans, &
                           val=tempMat2(1:i1, 1:i2), &
                           dim1=dim1, dim2=dim2, dim3=dim3)

CASE DEFAULT

  s = FEVariableShape(val)
  dim1 = s(1)
  dim2 = obj%nsd
  dim3 = obj%nips
  ans(1:dim1, 1:dim2, 1:dim3) = math%zero
END SELECT
END PROCEDURE obj_GetSpatialGradient_6

!----------------------------------------------------------------------------
!                                                         GetSpatialGradient
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetSpatialGradient7
INTEGER(I4B) :: dim1, dim2, dim3, dim4

dim1 = SIZE(val, 1)
dim2 = SIZE(val, 2)
dim3 = obj%nsd
dim4 = obj%nips

CALL Reallocate(ans, dim1, dim2, dim3, dim4)
CALL GetSpatialGradient_(obj=obj, val=val, ans=ans, dim1=dim1, &
                         dim2=dim2, dim3=dim3, dim4=dim4)
END PROCEDURE obj_GetSpatialGradient7

!----------------------------------------------------------------------------
!                                                        GetSpatialGradient_
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetSpatialGradient_7
LOGICAL(LGT) :: isok

dim1 = SIZE(val, 1)
dim2 = SIZE(val, 2)
dim3 = obj%nsd
dim4 = obj%nips

isok = obj%nsd .EQ. obj%xidim
IF (isok) THEN
  ans(1:dim1, 1:dim2, 1:dim3, 1:dim4) = &
    MyMatmul(val(1:dim1, 1:dim2, 1:obj%nns), &
             obj%dNdXt(1:obj%nns, 1:obj%nsd, 1:obj%nips))
ELSE
  ans(1:dim1, 1:dim2, 1:dim3, 1:dim4) = math%zero
END IF
END PROCEDURE obj_GetSpatialGradient_7

!----------------------------------------------------------------------------
!                                                         GetSpatialGradient
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetSpatialGradient8
INTEGER(I4B) :: dim1, dim2, dim3, dim4

dim1 = SIZE(val, 1)
dim2 = SIZE(val, 2)
dim3 = obj%nsd
dim4 = obj%nips

CALL Reallocate(ans, dim1, dim2, dim3, dim4)

CALL GetSpatialGradient_(obj=obj, val=val, ans=ans, dim1=dim1, dim2=dim2, &
                         dim3=dim3, dim4=dim4)
END PROCEDURE obj_GetSpatialGradient8

!----------------------------------------------------------------------------
!                                                         GetSpatialGradient
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetSpatialGradient_8
LOGICAL(LGT) :: isok
INTEGER(I4B) :: ii, jj

dim1 = SIZE(val, 1)
dim2 = SIZE(val, 2)
dim3 = obj%nsd
dim4 = obj%nips

isok = obj%nsd .EQ. obj%xidim
IF (isok) THEN

  DO jj = 1, dim4
    DO ii = 1, dim3

      ans(1:dim1, 1:dim2, ii, jj) = &
        Contraction( &
        a1=val(1:dim1, 1:dim2, 1:obj%nns, 1:obj%nnt), &
        a2=obj%dNTdXt(1:obj%nns, 1:obj%nnt, ii, jj))

    END DO
  END DO

ELSE
  ans(1:dim1, 1:dim2, 1:dim3, 1:dim4) = math%zero
END IF
END PROCEDURE obj_GetSpatialGradient_8

!----------------------------------------------------------------------------
!                                                         GetSpatialGradient
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetSpatialGradient9
INTEGER(I4B) :: s(2)

SELECT CASE (val%varType)
CASE (fevaropt%constant)
  s = FEVariableShape(val)
  CALL Reallocate(ans, s(1), s(2), obj%nsd, obj%nips)

CASE (fevaropt%space)
  CALL GetSpatialGradient(obj=obj, ans=ans, &
                          Val=FEVariableGet( &
                          val, TypeFEVariableMatrix, TypeFEVariableSpace))

CASE (fevaropt%spacetime)
  SELECT TYPE (obj)
  TYPE is (STElemShapeData_)
    CALL GetSpatialGradient(obj=obj, ans=ans, &
                            Val=FEVariableGet( &
                            val, TypeFEVariableMatrix, &
                            TypeFEVariableSpaceTime))
  END SELECT

CASE DEFAULT
END SELECT
END PROCEDURE obj_GetSpatialGradient9

!----------------------------------------------------------------------------
!                                                        GetSpatialGradient_
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetSpatialGradient_9
INTEGER(I4B) :: s(2), i1, i2, i3

s = FEVariableShape(val)
dim1 = s(1)
dim2 = s(2)
dim3 = obj%nsd
dim4 = obj%nips

SELECT CASE (val%varType)

CASE (fevaropt%space)

  CALL FEVariableGet_(obj=val, vartype=TypeFEVariableSpace, &
                      rank=TypeFEVariableMatrix, val=tempMat3, &
                      dim1=i1, dim2=i2, dim3=i3)

  CALL GetSpatialGradient_(obj=obj, ans=ans, val=tempMat3, dim1=dim1, &
                           dim2=dim2, dim3=dim3, dim4=dim4)
CASE DEFAULT

CASE (fevaropt%constant)
  ans(1:dim1, 1:dim2, 1:dim3, 1:dim4) = math%zero

END SELECT
END PROCEDURE obj_GetSpatialGradient_9

!----------------------------------------------------------------------------
!                                                         GetSpatialGradient
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetSpatialGradient10
INTEGER(I4B) :: nrow, ncol
REAL(DFP), ALLOCATABLE :: tempMat2(:, :)

nrow = obj%nsd
ncol = obj%nips
CALL Reallocate(ans, nrow, ncol)
CALL Reallocate(tempMat2, obj%nns, obj%nnt)
CALL GetSpatialGradient_(obj=obj, val=val, valRank=valRank, ans=ans, &
                         nrow=nrow, ncol=ncol, tempMat2=tempMat2)
DEALLOCATE (tempMat2)
END PROCEDURE obj_GetSpatialGradient10

!----------------------------------------------------------------------------
!                                                        GetSpatialGradient_
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetSpatialGradient_10
INTEGER(I4B) :: i1, i2

SELECT CASE (val%varType)
CASE (fevaropt%space)

  CALL FEVariableGet_( &
    obj=val, rank=TypeFEVariableScalar, varType=TypeFEVariableSpace, &
    val=tempMat2(:, 1), tsize=i1)

  CALL GetSpatialGradient_(obj=obj, ans=ans, nrow=nrow, ncol=ncol, &
                           val=tempMat2(1:i1, 1))

CASE (fevaropt%spaceTime)

  CALL FEVariableGet_( &
    obj=val, rank=TypeFEVariableScalar, varType=TypeFEVariableSpaceTime, &
    val=tempMat2, nrow=i1, ncol=i2)

  CALL GetSpatialGradient_(obj=obj, ans=ans, nrow=nrow, ncol=ncol, &
                           val=tempMat2(1:i1, 1:i2))

CASE DEFAULT
  nrow = obj%nsd
  ncol = obj%nips
  ans(1:nrow, 1:ncol) = math%zero
END SELECT
END PROCEDURE obj_GetSpatialGradient_10

!----------------------------------------------------------------------------
!                                                         GetSpatialGradient
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetSpatialGradient11
INTEGER(I4B) :: s(1), dim1, dim2, dim3
REAL(DFP), ALLOCATABLE :: tempMat3(:, :, :)

s = FEVariableShape(val)
dim1 = s(1)
dim2 = obj%nsd
dim3 = obj%nips

CALL Reallocate(ans, dim1, dim2, dim3)
CALL Reallocate(tempMat3, dim1, obj%nns, obj%nnt)
CALL GetSpatialGradient_(obj=obj, val=val, valRank=valRank, &
                         ans=ans, dim1=dim1, dim2=dim2, dim3=dim3, &
                         tempMat3=tempMat3)

DEALLOCATE (tempMat3)
END PROCEDURE obj_GetSpatialGradient11

!----------------------------------------------------------------------------
!                                                        GetSpatialGradient_
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetSpatialGradient_11
INTEGER(I4B) :: s(1), i1, i2, i3

SELECT CASE (val%varType)
CASE (fevaropt%space)
  CALL FEVariableGet_(obj=val, rank=TypeFEVariableVector, &
                      varType=TypeFEVariableSpace, &
                      val=tempMat3(:, :, 1), nrow=i1, &
                      ncol=i2)

  CALL GetSpatialGradient_(obj=obj, ans=ans, &
                           val=tempMat3(1:i1, 1:i2, 1), &
                           dim1=dim1, dim2=dim2, dim3=dim3)

CASE (fevaropt%spaceTime)
  CALL FEVariableGet_(obj=val, rank=TypeFEVariableVector, &
                      varType=TypeFEVariableSpaceTime, &
                      val=tempMat3, dim1=i1, dim2=i2, dim3=i3)

  CALL GetSpatialGradient_(obj=obj, ans=ans, &
                           val=tempMat3(1:i1, 1:i2, 1:i3), &
                           dim1=dim1, dim2=dim2, dim3=dim3)

CASE DEFAULT

  s = FEVariableShape(val)
  dim1 = s(1)
  dim2 = obj%nsd
  dim3 = obj%nips
  ans(1:dim1, 1:dim2, 1:dim3) = math%zero
END SELECT
END PROCEDURE obj_GetSpatialGradient_11

!----------------------------------------------------------------------------
!                                                         GetSpatialGradient
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetSpatialGradient12
INTEGER(I4B) :: dim1, dim2, dim3, dim4, s(2)
REAL(DFP), ALLOCATABLE :: tempMat4(:, :, :, :)

s = FEVariableShape(val)
dim1 = s(1)
dim2 = s(2)
dim3 = obj%nsd
dim4 = obj%nips

CALL Reallocate(tempMat4, dim1, dim2, obj%nns, obj%nnt)
CALL Reallocate(ans, dim1, dim2, dim3, dim4)
CALL GetSpatialGradient_(obj=obj, val=val, valRank=valRank, ans=ans, &
                         dim1=dim1, dim2=dim2, dim3=dim3, dim4=dim4, &
                         tempMat4=tempMat4)

DEALLOCATE (tempMat4)
END PROCEDURE obj_GetSpatialGradient12

!----------------------------------------------------------------------------
!                                                        GetSpatialGradient_
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetSpatialGradient_12
INTEGER(I4B) :: s(2), i1, i2, i3, i4

SELECT CASE (val%varType)

CASE (fevaropt%space)
  CALL FEVariableGet_(obj=val, vartype=TypeFEVariableSpace, &
                      rank=TypeFEVariableMatrix, val=tempMat4(:, :, :, 1), &
                      dim1=i1, dim2=i2, dim3=i3)

  CALL GetSpatialGradient_( &
    obj=obj, ans=ans, val=tempMat4(1:i1, 1:i2, 1:i3, 1), dim1=dim1, &
    dim2=dim2, dim3=dim3, dim4=dim4)

CASE (fevaropt%spaceTime)
  CALL FEVariableGet_(obj=val, vartype=TypeFEVariableSpaceTime, &
                      rank=TypeFEVariableMatrix, val=tempMat4, &
                      dim1=i1, dim2=i2, dim3=i3, dim4=i4)

  CALL GetSpatialGradient_( &
    obj=obj, val=tempMat4(1:i1, 1:i2, 1:i3, 1:i4), ans=ans, &
    dim1=dim1, dim2=dim2, dim3=dim3, dim4=dim4)

CASE DEFAULT
  s = FEVariableShape(val)
  dim1 = s(1)
  dim2 = s(2)
  dim3 = obj%nsd
  dim4 = obj%nips

  ans(1:dim1, 1:dim2, 1:dim3, 1:dim4) = math%zero

END SELECT
END PROCEDURE obj_GetSpatialGradient_12

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE Methods
