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
USE BaseType, ONLY: TypeFEVariableConstant
USE BaseType, ONLY: TypeFEVariableOpt
USE BaseType, ONLY: TypeFEVariableSpace
USE BaseType, ONLY: TypeFEVariableSpaceTime
USE BaseType, ONLY: TypeFEVariableVector
USE BaseType, ONLY: math => TypeMathOpt
USE FEVariable_Method, ONLY: FEVariableSize => Size
USE FEVariable_Method, ONLY: FEVariableGetInterpolation_ => GetInterpolation_
USE ReallocateUtility, ONLY: Reallocate
IMPLICIT NONE

CONTAINS

!---------------------------------------------------------------------------
!                                                          GetInterpolation_
!----------------------------------------------------------------------------

MODULE PROCEDURE GetInterpolation1
INTEGER(I4B) :: nrow, ncol

nrow = SIZE(val, 1)
ncol = obj%nips
CALL Reallocate(ans, nrow, ncol)
CALL GetInterpolation_(obj=obj, ans=ans, val=val, nrow=nrow, ncol=ncol, &
                       scale=math%one, addContribution=math%no)
END PROCEDURE GetInterpolation1

!----------------------------------------------------------------------------
!                                                           GetInterpolation_
!----------------------------------------------------------------------------

MODULE PROCEDURE GetInterpolation_1
CALL GetInterpolation_(obj=obj, ans=ans, val=val, nrow=nrow, ncol=ncol, &
                       scale=math%one, addContribution=math%no)
END PROCEDURE GetInterpolation_1

!----------------------------------------------------------------------------
!                                                           GetInterpolation_
!----------------------------------------------------------------------------

MODULE PROCEDURE GetInterpolation_1a
INTEGER(I4B) :: valNNS, minNNS
nrow = SIZE(val, 1)
ncol = obj%nips

valNNS = SIZE(val, 2)
minNNS = MIN(valNNS, obj%nns)

IF (.NOT. addContribution) ans(1:nrow, 1:ncol) = 0.0_DFP
ans(1:nrow, 1:ncol) = ans(1:nrow, 1:ncol) + &
                      scale * MATMUL(val(1:nrow, 1:minNNS), &
                                     obj%N(1:minNNS, 1:ncol))
END PROCEDURE GetInterpolation_1a

!----------------------------------------------------------------------------
!                                                           GetInterpolation_
!----------------------------------------------------------------------------

MODULE PROCEDURE GetInterpolation2
INTEGER(I4B) :: nrow, ncol

nrow = SIZE(val, 1)
ncol = obj%nips
CALL Reallocate(ans, nrow, ncol)
CALL GetInterpolation_(obj=obj, ans=ans, val=val, nrow=nrow, ncol=ncol, &
                       scale=math%one, addContribution=math%no)
END PROCEDURE GetInterpolation2

!----------------------------------------------------------------------------
!                                                           GetInterpolation_
!----------------------------------------------------------------------------

MODULE PROCEDURE GetInterpolation_2
CALL GetInterpolation_(obj=obj, ans=ans, val=val, nrow=nrow, ncol=ncol, &
                       scale=math%one, addContribution=math%no)
END PROCEDURE GetInterpolation_2

!----------------------------------------------------------------------------
!                                                           GetInterpolation_
!----------------------------------------------------------------------------

MODULE PROCEDURE GetInterpolation_2a
INTEGER(I4B) :: minNNT, valNNT, aa
REAL(DFP) :: myscale

nrow = SIZE(val, 1)
ncol = obj%nips

valNNT = SIZE(val, 3)
minNNT = MIN(valNNT, obj%nnt)

IF (.NOT. addContribution) ans(1:nrow, 1:ncol) = 0.0_DFP

DO aa = 1, minNNT
  myscale = obj%T(aa) * scale
  CALL GetInterpolation_(obj=obj, ans=ans, val=val(:, :, aa), nrow=nrow, &
                         ncol=ncol, scale=myscale, addContribution=math%yes)
END DO
END PROCEDURE GetInterpolation_2a

!----------------------------------------------------------------------------
!                                                           GetInterpolation_
!----------------------------------------------------------------------------

MODULE PROCEDURE GetInterpolation3
INTEGER(I4B) :: dim1, dim2, dim3

dim1 = SIZE(val, 1)
dim2 = 0
dim3 = SIZE(obj)
IF (dim3 .GT. 0) dim2 = obj(1)%nips

CALL Reallocate(ans, dim1, dim2, dim3)
CALL GetInterpolation_(obj=obj, ans=ans, val=val, dim1=dim1, dim2=dim2, &
                       dim3=dim3, scale=math%one, addContribution=math%no)
END PROCEDURE GetInterpolation3

!----------------------------------------------------------------------------
!                                                           GetInterpolation_
!----------------------------------------------------------------------------

MODULE PROCEDURE GetInterpolation_3
CALL GetInterpolation_(obj=obj, ans=ans, val=val, dim1=dim1, dim2=dim2, &
                       dim3=dim3, scale=math%one, addContribution=math%no)
END PROCEDURE GetInterpolation_3

!----------------------------------------------------------------------------
!                                                           GetInterpolation_
!----------------------------------------------------------------------------

MODULE PROCEDURE GetInterpolation_3a
INTEGER(I4B) :: ipt

dim1 = 0
dim2 = 0
dim3 = SIZE(obj)

DO ipt = 1, dim3
  CALL GetInterpolation_(obj=obj(ipt), ans=ans(:, :, ipt), &
                         val=val, nrow=dim1, ncol=dim2, scale=scale, &
                         addContribution=addContribution)
END DO
END PROCEDURE GetInterpolation_3a

!----------------------------------------------------------------------------
!                                                           GetInterpolation_
!----------------------------------------------------------------------------

MODULE PROCEDURE GetInterpolation4
INTEGER(I4B) :: nrow, ncol

nrow = FEVariableSize(val, 1)
ncol = obj%nips

CALL Reallocate(ans, nrow, ncol)
CALL GetInterpolation_(obj=obj, ans=ans, val=val, nrow=nrow, ncol=ncol, &
                       scale=math%one, addContribution=math%no)
IF (nrow .EQ. 0 .AND. ncol .EQ. 0) CALL Reallocate(ans, 0, 0)
END PROCEDURE GetInterpolation4

!----------------------------------------------------------------------------
!                                                           GetInterpolation_
!----------------------------------------------------------------------------

MODULE PROCEDURE GetInterpolation_4
CALL GetInterpolation_(obj=obj, ans=ans, val=val, nrow=nrow, ncol=ncol, &
                       scale=math%one, addContribution=math%no)
END PROCEDURE GetInterpolation_4

!----------------------------------------------------------------------------
!                                                           GetInterpolation_
!----------------------------------------------------------------------------

MODULE PROCEDURE GetInterpolation_4a
INTEGER(I4B) :: timeIndx0

nrow = 0
ncol = 0
timeIndx0 = 1_I4B
IF (PRESENT(timeIndx)) timeIndx0 = timeIndx

SELECT CASE (val%vartype)
CASE (TypeFEVariableOpt%constant)
  CALL FEVariableGetInterpolation_(obj=val, RANK=TypeFEVariableVector, &
                                   vartype=TypeFEVariableConstant, &
                                   N=obj%N, nns=obj%nns, nips=obj%nips, &
                                   scale=scale, &
                                   addContribution=addContribution, &
                                   ans=ans, nrow=nrow, ncol=ncol)

CASE (TypeFEVariableOpt%space)

  CALL FEVariableGetInterpolation_(obj=val, RANK=TypeFEVariableVector, &
                                   vartype=TypeFEVariableSpace, &
                                   N=obj%N, nns=obj%nns, nips=obj%nips, &
                                   scale=scale, &
                                   addContribution=addContribution, &
                                   ans=ans, nrow=nrow, ncol=ncol)

CASE (TypeFEVariableOpt%spacetime)
  SELECT TYPE (obj)
  TYPE IS (STElemShapeData_)
    CALL FEVariableGetInterpolation_(obj=val, RANK=TypeFEVariableVector, &
                                     vartype=TypeFEVariableSpaceTime, &
                                     N=obj%N, nns=obj%nns, nips=obj%nips, &
                                     T=obj%T, nnt=obj%nnt, &
                                     scale=scale, &
                                     addContribution=addContribution, &
                                     ans=ans, nrow=nrow, ncol=ncol, &
                                     timeIndx=timeIndx0)
  END SELECT
END SELECT

END PROCEDURE GetInterpolation_4a

!----------------------------------------------------------------------------
!                                                           GetInterpolation_
!----------------------------------------------------------------------------

MODULE PROCEDURE GetInterpolation_4b
tsize = 0
SELECT CASE (val%vartype)
CASE (TypeFEVariableOpt%constant)
  CALL FEVariableGetInterpolation_(obj=val, RANK=TypeFEVariableVector, &
                                   vartype=TypeFEVariableConstant, &
                                   N=obj%N, nns=obj%nns, &
                                   spaceIndx=spaceIndx, &
                                   timeIndx=timeIndx, &
                                   scale=scale, &
                                   addContribution=addContribution, &
                                   ans=ans, tsize=tsize)

CASE (TypeFEVariableOpt%space)

  CALL FEVariableGetInterpolation_(obj=val, RANK=TypeFEVariableVector, &
                                   vartype=TypeFEVariableSpace, &
                                   N=obj%N, nns=obj%nns, &
                                   spaceIndx=spaceIndx, &
                                   timeIndx=timeIndx, &
                                   scale=scale, &
                                   addContribution=addContribution, &
                                   ans=ans, tsize=tsize)

CASE (TypeFEVariableOpt%spacetime)

  SELECT TYPE (obj)
  TYPE IS (STElemShapeData_)
    CALL FEVariableGetInterpolation_(obj=val, &
                                     RANK=TypeFEVariableVector, &
                                     vartype=TypeFEVariableSpaceTime, &
                                     N=obj%N, nns=obj%nns, &
                                     spaceIndx=spaceIndx, &
                                     timeIndx=timeIndx, &
                                     T=obj%T, nnt=obj%nnt, &
                                     scale=scale, &
                                     addContribution=addContribution, &
                                     ans=ans, tsize=tsize)

  END SELECT
END SELECT
END PROCEDURE GetInterpolation_4b

!----------------------------------------------------------------------------
!                                                           GetInterpolation_
!----------------------------------------------------------------------------

MODULE PROCEDURE GetInterpolation5
INTEGER(I4B) :: dim1, dim2, dim3

dim1 = FEVariableSIZE(val, 1)
dim2 = 0
dim3 = SIZE(obj)
IF (dim3 .GT. 0) dim2 = obj(1)%nips

CALL Reallocate(ans, dim1, dim2, dim3)

CALL GetInterpolation_(obj=obj, ans=ans, val=val, dim1=dim1, &
                       dim2=dim2, dim3=dim3, scale=math%one, &
                       addContribution=math%no)
IF (dim3 .GT. 0 .AND. dim1 .EQ. 0 .AND. dim2 .EQ. 0) THEN
  CALL Reallocate(ans, 0, 0, dim3)
END IF
END PROCEDURE GetInterpolation5

!----------------------------------------------------------------------------
!                                                           GetInterpolation_
!----------------------------------------------------------------------------

MODULE PROCEDURE GetInterpolation_5
dim1 = 0
dim2 = 0
dim3 = SIZE(obj)
IF (dim3 .EQ. 0) RETURN
CALL GetInterpolation_(obj=obj, ans=ans, val=val, dim1=dim1, &
                       dim2=dim2, dim3=dim3, scale=math%one, &
                       addContribution=math%no)
END PROCEDURE GetInterpolation_5

!----------------------------------------------------------------------------
!                                                          GetInterpolation_
!----------------------------------------------------------------------------

MODULE PROCEDURE GetInterpolation_5a
INTEGER(I4B) :: ipt

dim1 = 0
dim2 = 0
dim3 = SIZE(obj)
DO ipt = 1, dim3
  CALL GetInterpolation_(obj=obj(ipt), ans=ans(:, :, ipt), &
                         val=val, nrow=dim1, ncol=dim2, &
                         scale=scale, &
                         addContribution=addContribution, &
                         timeIndx=ipt)
END DO
END PROCEDURE GetInterpolation_5a

!----------------------------------------------------------------------------
!                                                               Interpolation
!----------------------------------------------------------------------------

MODULE PROCEDURE Interpolation1
CALL GetInterpolation(obj=obj, ans=ans, val=val)
END PROCEDURE Interpolation1

!----------------------------------------------------------------------------
!                                                             STInterpolation
!----------------------------------------------------------------------------

MODULE PROCEDURE STInterpolation1
CALL GetInterpolation(obj=obj, ans=ans, val=val)
END PROCEDURE STInterpolation1

END SUBMODULE Methods
