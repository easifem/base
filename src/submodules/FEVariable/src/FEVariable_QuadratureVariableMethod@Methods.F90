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

SUBMODULE(FEVariable_QuadratureVariableMethod) Methods
USE ReallocateUtility, ONLY: Reallocate
USE FEVariable_ConstructorMethod, ONLY: FEVariableInitiate => Initiate

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                          QuadratureVariable
!----------------------------------------------------------------------------

MODULE PROCEDURE Quadrature_Scalar_Constant
INTEGER(I4B) :: s(1)

s(1) = 1
CALL FEVariableInitiate(obj=obj, s=s, &
                        defineon=TypeFEVariableOpt%quadrature, &
                        vartype=TypeFEVariableOpt%constant, &
                        rank=TypeFEVariableOpt%scalar, len=1)
obj%val(1) = val
END PROCEDURE Quadrature_Scalar_Constant

!----------------------------------------------------------------------------
!                                                          QuadratureVariable
!----------------------------------------------------------------------------

MODULE PROCEDURE Quadrature_Scalar_Space
INTEGER(I4B) :: s(1)

s(1) = SIZE(val)
CALL FEVariableInitiate(obj=obj, s=s, defineon=TypeFEVariableOpt%Quadrature, &
                        vartype=TypeFEVariableOpt%space, &
                        rank=TypeFEVariableOpt%scalar, len=s(1))
obj%val(1:obj%len) = val
END PROCEDURE Quadrature_Scalar_Space

!----------------------------------------------------------------------------
!                                                          QuadratureVariable
!----------------------------------------------------------------------------

MODULE PROCEDURE Quadrature_Scalar_Space2
INTEGER(I4B) :: s(1)

s(1) = tsize
CALL FEVariableInitiate(obj=obj, s=s, defineon=TypeFEVariableOpt%Quadrature, &
                        vartype=TypeFEVariableOpt%space, &
                        rank=TypeFEVariableOpt%scalar, len=s(1))
obj%val(1:obj%len) = 0.0_DFP
END PROCEDURE Quadrature_Scalar_Space2

!----------------------------------------------------------------------------
!                                                         QuadratureVariable
!----------------------------------------------------------------------------

MODULE PROCEDURE Quadrature_Scalar_Time
INTEGER(I4B) :: s(1)

s(1) = SIZE(val)
CALL FEVariableInitiate(obj=obj, s=s, defineon=TypeFEVariableOpt%Quadrature, &
                        vartype=TypeFEVariableOpt%time, &
                        rank=TypeFEVariableOpt%scalar, len=s(1))
obj%val(1:obj%len) = val
END PROCEDURE Quadrature_Scalar_Time

!----------------------------------------------------------------------------
!                                                         QuadratureVariable
!----------------------------------------------------------------------------

MODULE PROCEDURE Quadrature_Scalar_Time2
INTEGER(I4B) :: s(1)

s(1) = tsize
CALL FEVariableInitiate( &
  obj=obj, s=s, defineon=TypeFEVariableOpt%Quadrature, len=s(1), &
  vartype=TypeFEVariableOpt%time, rank=TypeFEVariableOpt%scalar)
obj%val(1:obj%len) = 0.0_DFP
END PROCEDURE Quadrature_Scalar_Time2

!----------------------------------------------------------------------------
!                                                         QuadratureVariable
!----------------------------------------------------------------------------

MODULE PROCEDURE Quadrature_Scalar_SpaceTime
INTEGER(I4B) :: s(2), tsize, ii, jj, kk
s = SHAPE(val)
tsize = s(1) * s(2)

CALL FEVariableInitiate(obj=obj, s=s, defineon=TypeFEVariableOpt%Quadrature, &
                        vartype=TypeFEVariableOpt%spacetime, &
                        rank=TypeFEVariableOpt%scalar, len=tsize)

kk = 0
DO jj = 1, s(2)
  DO ii = 1, s(1)
    kk = kk + 1
    obj%val(kk) = val(ii, jj)
  END DO
END DO
END PROCEDURE Quadrature_Scalar_SpaceTime

!----------------------------------------------------------------------------
!                                                          QuadratureVariable
!----------------------------------------------------------------------------

MODULE PROCEDURE Quadrature_Scalar_SpaceTime2
INTEGER(I4B) :: tsize

tsize = s(1) * s(2)

CALL FEVariableInitiate( &
  obj=obj, s=s, defineon=TypeFEVariableOpt%Quadrature, len=tsize, val=val, &
  vartype=TypeFEVariableOpt%spacetime, rank=TypeFEVariableOpt%scalar)
END PROCEDURE Quadrature_Scalar_SpaceTime2

!----------------------------------------------------------------------------
!                                                          QuadratureVariable
!----------------------------------------------------------------------------

MODULE PROCEDURE Quadrature_Scalar_SpaceTime3
INTEGER(I4B) :: tsize, s(2)

s(1) = nrow
s(2) = ncol
tsize = s(1) * s(2)

CALL FEVariableInitiate( &
  obj=obj, s=s, defineon=TypeFEVariableOpt%Quadrature, len=tsize, &
  vartype=TypeFEVariableOpt%spacetime, rank=TypeFEVariableOpt%scalar)
END PROCEDURE Quadrature_Scalar_SpaceTime3

!----------------------------------------------------------------------------
!                                                          QuadratureVariable
!----------------------------------------------------------------------------

MODULE PROCEDURE Quadrature_Vector_Constant
INTEGER(I4B) :: s(1), tsize

tsize = SIZE(val)
s(1) = tsize

CALL FEVariableInitiate(obj=obj, s=s, defineon=TypeFEVariableOpt%Quadrature, &
                        vartype=TypeFEVariableOpt%constant, &
                        rank=TypeFEVariableOpt%vector, len=tsize, &
                        val=val)
END PROCEDURE Quadrature_Vector_Constant

!----------------------------------------------------------------------------
!                                                          QuadratureVariable
!----------------------------------------------------------------------------

MODULE PROCEDURE Quadrature_Vector_Space
INTEGER(I4B) :: s(2), tsize, ii, jj, cnt

s = SHAPE(val)
tsize = s(1) * s(2)

CALL FEVariableInitiate(obj=obj, s=s, defineon=TypeFEVariableOpt%Quadrature, &
                        vartype=TypeFEVariableOpt%space, &
                        rank=TypeFEVariableOpt%vector, len=tsize)

cnt = 0
DO jj = 1, s(2)
  DO ii = 1, s(1)
    cnt = cnt + 1
    obj%val(cnt) = val(ii, jj)
  END DO
END DO
END PROCEDURE Quadrature_Vector_Space

!----------------------------------------------------------------------------
!                                                          QuadratureVariable
!----------------------------------------------------------------------------

MODULE PROCEDURE Quadrature_Vector_Space2
INTEGER(I4B) :: tsize

tsize = s(1) * s(2)
CALL FEVariableInitiate(obj=obj, s=s, defineon=TypeFEVariableOpt%Quadrature, &
                        vartype=TypeFEVariableOpt%space, &
                        rank=TypeFEVariableOpt%vector, len=tsize, val=val)

END PROCEDURE Quadrature_Vector_Space2

!----------------------------------------------------------------------------
!                                                          QuadratureVariable
!----------------------------------------------------------------------------

MODULE PROCEDURE Quadrature_Vector_Space3
INTEGER(I4B) :: s(2), tsize

s(1) = nrow
s(2) = ncol
tsize = s(1) * s(2)

CALL FEVariableInitiate(obj=obj, s=s, defineon=TypeFEVariableOpt%Quadrature, &
                        vartype=TypeFEVariableOpt%space, &
                        rank=TypeFEVariableOpt%vector, len=tsize)

obj%val(1:obj%len) = 0.0_DFP
END PROCEDURE Quadrature_Vector_Space3

!----------------------------------------------------------------------------
!                                                         QuadratureVariable
!----------------------------------------------------------------------------

MODULE PROCEDURE Quadrature_Vector_Time
INTEGER(I4B) :: s(2), tsize, ii, jj, cnt

s = SHAPE(val)
tsize = s(1) * s(2)

CALL FEVariableInitiate(obj=obj, s=s, defineon=TypeFEVariableOpt%Quadrature, &
                        vartype=TypeFEVariableOpt%time, &
                        rank=TypeFEVariableOpt%vector, len=tsize)

cnt = 0
DO jj = 1, s(2)
  DO ii = 1, s(1)
    cnt = cnt + 1
    obj%val(cnt) = val(ii, jj)
  END DO
END DO
END PROCEDURE Quadrature_Vector_Time

!----------------------------------------------------------------------------
!                                                          QuadratureVariable
!----------------------------------------------------------------------------

MODULE PROCEDURE Quadrature_Vector_Time2
INTEGER(I4B) :: tsize

tsize = s(1) * s(2)

CALL FEVariableInitiate(obj=obj, s=s, defineon=TypeFEVariableOpt%Quadrature, &
                        vartype=TypeFEVariableOpt%time, &
                        rank=TypeFEVariableOpt%vector, len=tsize, val=val)

END PROCEDURE Quadrature_Vector_Time2

!----------------------------------------------------------------------------
!                                                          QuadratureVariable
!----------------------------------------------------------------------------

MODULE PROCEDURE Quadrature_Vector_SpaceTime
INTEGER(I4B) :: s(3), tsize, ii, jj, kk, cnt
s = SHAPE(val)
tsize = s(1) * s(2) * s(3)
CALL FEVariableInitiate(obj=obj, s=s, defineon=TypeFEVariableOpt%Quadrature, &
                        vartype=TypeFEVariableOpt%spacetime, &
                        rank=TypeFEVariableOpt%vector, len=tsize)

cnt = 0
DO kk = 1, SIZE(val, 3)
  DO jj = 1, SIZE(val, 2)
    DO ii = 1, SIZE(val, 1)
      cnt = cnt + 1
      obj%val(cnt) = val(ii, jj, kk)
    END DO
  END DO
END DO
END PROCEDURE Quadrature_Vector_SpaceTime

!----------------------------------------------------------------------------
!                                                          QuadratureVariable
!----------------------------------------------------------------------------

MODULE PROCEDURE Quadrature_Vector_SpaceTime2
INTEGER(I4B) :: tsize

tsize = s(1) * s(2) * s(3)
CALL FEVariableInitiate(obj=obj, s=s, defineon=TypeFEVariableOpt%Quadrature, &
                        vartype=TypeFEVariableOpt%spacetime, &
                        rank=TypeFEVariableOpt%vector, len=tsize, &
                        val=val)
END PROCEDURE Quadrature_Vector_SpaceTime2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE Quadrature_Vector_SpaceTime3
INTEGER(I4B) :: tsize, s(3)

s(1) = dim1
s(2) = dim2
s(3) = dim3
tsize = dim1 * dim2 * dim3

CALL FEVariableInitiate(obj=obj, s=s, defineon=TypeFEVariableOpt%Quadrature, &
                        vartype=TypeFEVariableOpt%spacetime, &
                        rank=TypeFEVariableOpt%vector, len=tsize)

END PROCEDURE Quadrature_Vector_SpaceTime3

!----------------------------------------------------------------------------
!                                                          QuadratureVariable
!----------------------------------------------------------------------------

MODULE PROCEDURE Quadrature_Matrix_Constant
INTEGER(I4B) :: s(2), tsize, ii, jj, cnt

s = SHAPE(val)
tsize = s(1) * s(2)

CALL FEVariableInitiate(obj=obj, s=s, defineon=TypeFEVariableOpt%Quadrature, &
                        vartype=TypeFEVariableOpt%constant, &
                        rank=TypeFEVariableOpt%matrix, len=tsize)

cnt = 0
DO jj = 1, s(2)
  DO ii = 1, s(1)
    cnt = cnt + 1
    obj%val(cnt) = val(ii, jj)
  END DO
END DO

END PROCEDURE Quadrature_Matrix_Constant

!----------------------------------------------------------------------------
!                                                          QuadratureVariable
!----------------------------------------------------------------------------

MODULE PROCEDURE Quadrature_Matrix_Constant2
INTEGER(I4B) :: tsize

tsize = s(1) * s(2)
CALL FEVariableInitiate(obj=obj, s=s, defineon=TypeFEVariableOpt%Quadrature, &
                        vartype=TypeFEVariableOpt%constant, &
                        rank=TypeFEVariableOpt%matrix, len=tsize, val=val)
END PROCEDURE Quadrature_Matrix_Constant2

!----------------------------------------------------------------------------
!                                                          QuadratureVariable
!----------------------------------------------------------------------------

MODULE PROCEDURE Quadrature_Matrix_Space
INTEGER(I4B) :: s(3), tsize, ii, jj, kk, cnt

s = SHAPE(val)
tsize = s(1) * s(2) * s(3)
CALL FEVariableInitiate(obj=obj, s=s, defineon=TypeFEVariableOpt%Quadrature, &
                        vartype=TypeFEVariableOpt%space, &
                        rank=TypeFEVariableOpt%matrix, len=tsize)

cnt = 0
DO kk = 1, s(3)
  DO jj = 1, s(2)
    DO ii = 1, s(1)
      cnt = cnt + 1
      obj%val(cnt) = val(ii, jj, kk)
    END DO
  END DO
END DO
END PROCEDURE Quadrature_Matrix_Space

!----------------------------------------------------------------------------
!                                                          QuadratureVariable
!----------------------------------------------------------------------------

MODULE PROCEDURE Quadrature_Matrix_Space2
INTEGER(I4B) :: tsize

tsize = s(1) * s(2) * s(3)
CALL FEVariableInitiate(obj=obj, s=s, defineon=TypeFEVariableOpt%Quadrature, &
                        vartype=TypeFEVariableOpt%space, &
                        rank=TypeFEVariableOpt%matrix, &
                        len=tsize, val=val)
END PROCEDURE Quadrature_Matrix_Space2

!----------------------------------------------------------------------------
!                                                          QuadratureVariable
!----------------------------------------------------------------------------

MODULE PROCEDURE Quadrature_Matrix_Time
INTEGER(I4B) :: s(3), tsize, ii, jj, kk, cnt

s = SHAPE(val)
tsize = s(1) * s(2) * s(3)

CALL FEVariableInitiate(obj=obj, s=s, defineon=TypeFEVariableOpt%Quadrature, &
                        vartype=TypeFEVariableOpt%time, &
                        rank=TypeFEVariableOpt%matrix, len=tsize)

cnt = 0
DO kk = 1, SIZE(val, 3)
  DO jj = 1, SIZE(val, 2)
    DO ii = 1, SIZE(val, 1)
      cnt = cnt + 1
      obj%val(cnt) = val(ii, jj, kk)
    END DO
  END DO
END DO
END PROCEDURE Quadrature_Matrix_Time

!----------------------------------------------------------------------------
!                                                          QuadratureVariable
!----------------------------------------------------------------------------

MODULE PROCEDURE Quadrature_Matrix_Time2
INTEGER(I4B) :: tsize

tsize = s(1) * s(2) * s(3)

CALL FEVariableInitiate(obj=obj, s=s, defineon=TypeFEVariableOpt%Quadrature, &
                        vartype=TypeFEVariableOpt%time, &
                        rank=TypeFEVariableOpt%matrix, &
                        len=tsize, val=val)

END PROCEDURE Quadrature_Matrix_Time2

!----------------------------------------------------------------------------
!                                                          QuadratureVariable
!----------------------------------------------------------------------------

MODULE PROCEDURE Quadrature_Matrix_SpaceTime
INTEGER(I4B) :: s(4), tsize, ii, jj, kk, ll, cnt

s = SHAPE(val)
tsize = s(1) * s(2) * s(3) * s(4)

CALL FEVariableInitiate(obj=obj, s=s, defineon=TypeFEVariableOpt%Quadrature, &
                        vartype=TypeFEVariableOpt%spacetime, &
                        rank=TypeFEVariableOpt%matrix, len=tsize)

cnt = 0
DO ll = 1, SIZE(val, 4)
  DO kk = 1, SIZE(val, 3)
    DO jj = 1, SIZE(val, 2)
      DO ii = 1, SIZE(val, 1)
        cnt = cnt + 1
        obj%val(cnt) = val(ii, jj, kk, ll)
      END DO
    END DO
  END DO
END DO
END PROCEDURE Quadrature_Matrix_SpaceTime

!----------------------------------------------------------------------------
!                                                          QuadratureVariable
!----------------------------------------------------------------------------

MODULE PROCEDURE Quadrature_Matrix_SpaceTime2
INTEGER(I4B) :: tsize

tsize = PRODUCT(s)
CALL FEVariableInitiate(obj=obj, s=s, defineon=TypeFEVariableOpt%Quadrature, &
                        vartype=TypeFEVariableOpt%spacetime, &
                        rank=TypeFEVariableOpt%matrix, len=tsize, val=val)
END PROCEDURE Quadrature_Matrix_SpaceTime2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE Methods
