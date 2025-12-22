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
USE FEVariable_Method, ONLY: FEVariableSize => Size, &
                             FEVariableGetInterpolation_ => GetInterpolation_
USE BaseType, ONLY: TypeFEVariableMatrix, TypeFEVariableConstant, &
                    TypeFEVariableSpace, TypeFEVariableSpaceTime, &
                    TypeFEVariableOpt

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                           getinterpolation
!----------------------------------------------------------------------------

MODULE PROCEDURE GetInterpolation1
REAL(DFP), PARAMETER :: one = 1.0_DFP
LOGICAL(LGT), PARAMETER :: no = .FALSE.
INTEGER(I4B) :: dim1, dim2, dim3

dim1 = SIZE(val, 1)
dim2 = SIZE(val, 2)
dim3 = obj%nips
CALL Reallocate(ans, dim1, dim2, dim3)
CALL GetInterpolation_(obj=obj, val=val, ans=ans, dim1=dim1, dim2=dim2, &
                       dim3=dim3, scale=one, addContribution=no)
END PROCEDURE GetInterpolation1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE GetInterpolation_1
REAL(DFP), PARAMETER :: one = 1.0_DFP
LOGICAL(LGT), PARAMETER :: no = .FALSE.

dim1 = SIZE(val, 1)
dim2 = SIZE(val, 2)
dim3 = obj%nips
CALL GetInterpolation_(obj=obj, val=val, ans=ans, dim1=dim1, dim2=dim2, &
                       dim3=dim3, scale=one, addContribution=no)
END PROCEDURE GetInterpolation_1

!----------------------------------------------------------------------------
!                                                          GetInterpolation_
!----------------------------------------------------------------------------

MODULE PROCEDURE GetInterpolation_1a
INTEGER(I4B) :: ips, ii, valNNS, minNNS

dim1 = SIZE(val, 1)
dim2 = SIZE(val, 2)
dim3 = obj%nips

IF (.NOT. addContribution) ans(1:dim1, 1:dim2, 1:dim3) = 0.0_DFP

valNNS = SIZE(val, 3)
minNNS = MIN(valNNS, obj%nns)

DO ips = 1, dim3
  DO ii = 1, minNNS
    ans(1:dim1, 1:dim2, ips) = ans(1:dim1, 1:dim2, ips) + &
                              scale * val(1:dim1, 1:dim2, ii) * obj%N(ii, ips)
  END DO
END DO
END PROCEDURE GetInterpolation_1a

!----------------------------------------------------------------------------
!                                                         getSTinterpolation
!----------------------------------------------------------------------------

MODULE PROCEDURE GetInterpolation2
REAL(DFP), PARAMETER :: one = 1.0_DFP
LOGICAL(LGT), PARAMETER :: no = .FALSE.
INTEGER(I4B) :: dim1, dim2, dim3

dim1 = SIZE(val, 1)
dim2 = SIZE(val, 2)
dim3 = obj%nips
CALL Reallocate(ans, dim1, dim2, dim3)
CALL GetInterpolation_(obj=obj, ans=ans, val=val, dim1=dim1, dim2=dim2, &
                       dim3=dim3, scale=one, addContribution=no)
END PROCEDURE GetInterpolation2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE GetInterpolation_2
REAL(DFP), PARAMETER :: one = 1.0_DFP
LOGICAL(LGT), PARAMETER :: no = .FALSE.

dim1 = SIZE(val, 1)
dim2 = SIZE(val, 2)
dim3 = obj%nips
CALL GetInterpolation_(obj=obj, ans=ans, val=val, dim1=dim1, dim2=dim2, &
                       dim3=dim3, scale=one, addContribution=no)
END PROCEDURE GetInterpolation_2

!----------------------------------------------------------------------------
!                                                           GetInterpolation_
!----------------------------------------------------------------------------

MODULE PROCEDURE GetInterpolation_2a
LOGICAL(LGT), PARAMETER :: yes = .TRUE.
INTEGER(I4B) :: minNNT, valNNT, aa
REAL(DFP) :: myscale

dim1 = SIZE(val, 1)
dim2 = SIZE(val, 2)
dim3 = obj%nips

valNNT = SIZE(val, 4)
minNNT = MIN(valNNT, obj%nnt)

IF (.NOT. addContribution) ans(1:dim1, 1:dim2, 1:dim3) = 0.0_DFP

DO aa = 1, minNNT
  myscale = obj%T(aa) * scale
  CALL GetInterpolation_(obj=obj, ans=ans, val=val(:, :, :, aa), &
                         dim1=dim1, dim2=dim2, dim3=dim3, scale=myscale, &
                         addContribution=yes)
END DO
END PROCEDURE GetInterpolation_2a

!----------------------------------------------------------------------------
!                                                         getSTinterpolation
!----------------------------------------------------------------------------

MODULE PROCEDURE GetInterpolation3
REAL(DFP), PARAMETER :: one = 1.0_DFP
LOGICAL(LGT), PARAMETER :: no = .FALSE.

INTEGER(I4B) :: dim1, dim2, dim3, dim4

dim1 = SIZE(val, 1)
dim2 = SIZE(val, 2)
dim3 = obj(1)%nips
dim4 = SIZE(obj)

CALL Reallocate(ans, dim1, dim2, dim3, dim4)
CALL GetInterpolation_(obj=obj, ans=ans, val=val, dim1=dim1, dim2=dim2, &
                       dim3=dim3, dim4=dim4, scale=one, addContribution=no)
END PROCEDURE GetInterpolation3

!----------------------------------------------------------------------------
!                                                            GetInterpolation
!----------------------------------------------------------------------------

MODULE PROCEDURE GetInterpolation_3
REAL(DFP), PARAMETER :: one = 1.0_DFP
LOGICAL(LGT), PARAMETER :: no = .FALSE.

dim1 = SIZE(val, 1)
dim2 = SIZE(val, 2)
dim3 = obj(1)%nips
dim4 = SIZE(obj)

CALL GetInterpolation_(obj=obj, ans=ans, val=val, dim1=dim1, dim2=dim2, &
                       dim3=dim3, dim4=dim4, scale=one, addContribution=no)
END PROCEDURE GetInterpolation_3

!----------------------------------------------------------------------------
!                                                         GetInterpolation_3a
!----------------------------------------------------------------------------

MODULE PROCEDURE GetInterpolation_3a
INTEGER(I4B) :: ipt

dim1 = 0
dim2 = 0
dim3 = 0
dim4 = SIZE(obj)

DO ipt = 1, dim4
  CALL GetInterpolation_(obj=obj(ipt), ans=ans(:, :, :, ipt), &
                         val=val, dim1=dim1, dim2=dim2, dim3=dim3, &
                         scale=scale, addContribution=addContribution)
END DO
END PROCEDURE GetInterpolation_3a

!----------------------------------------------------------------------------
!                                                           getinterpolation
!----------------------------------------------------------------------------

MODULE PROCEDURE GetInterpolation4
REAL(DFP), PARAMETER :: one = 1.0_DFP
LOGICAL(LGT), PARAMETER :: no = .FALSE.
INTEGER(I4B) :: dim1, dim2, dim3

dim1 = FEVariableSize(val, 1)
dim2 = FEVariableSize(val, 2)
dim3 = obj%nips

CALL Reallocate(ans, dim1, dim2, dim3)
CALL GetInterpolation_(obj=obj, ans=ans, val=val, dim1=dim1, dim2=dim2, &
                       dim3=dim3, scale=one, addContribution=no)
END PROCEDURE GetInterpolation4

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE GetInterpolation_4
REAL(DFP), PARAMETER :: one = 1.0_DFP
LOGICAL(LGT), PARAMETER :: no = .FALSE.

CALL GetInterpolation_(obj=obj, ans=ans, val=val, dim1=dim1, dim2=dim2, &
                       dim3=dim3, scale=one, addContribution=no)
END PROCEDURE GetInterpolation_4

!----------------------------------------------------------------------------
!                                                         GetInterpolation_4a
!----------------------------------------------------------------------------

MODULE PROCEDURE GetInterpolation_4a
INTEGER(I4B) :: timeIndx0
timeIndx0 = 1_I4B
IF (PRESENT(timeIndx)) timeIndx0 = timeIndx

SELECT CASE (val%vartype)
CASE (TypeFEVariableOpt%constant)
  CALL FEVariableGetInterpolation_(obj=val, rank=TypeFEVariableMatrix, &
                                   vartype=TypeFEVariableConstant, &
                                   N=obj%N, nns=obj%nns, nips=obj%nips, &
                                   scale=scale, &
                                   addContribution=addContribution, &
                                   ans=ans, dim1=dim1, dim2=dim2, dim3=dim3)

CASE (TypeFEVariableOpt%space)
  CALL FEVariableGetInterpolation_(obj=val, rank=TypeFEVariableMatrix, &
                                   vartype=TypeFEVariableSpace, &
                                   N=obj%N, nns=obj%nns, nips=obj%nips, &
                                   scale=scale, &
                                   addContribution=addContribution, &
                                   ans=ans, dim1=dim1, dim2=dim2, dim3=dim3)

CASE (TypeFEVariableOpt%spacetime)
  SELECT TYPE (obj); TYPE IS (STElemShapeData_)
    CALL FEVariableGetInterpolation_(obj=val, rank=TypeFEVariableMatrix, &
                                     vartype=TypeFEVariableSpaceTime, &
                                     N=obj%N, nns=obj%nns, nips=obj%nips, &
                                     T=obj%T, nnt=obj%nnt, &
                                     scale=scale, &
                                     addContribution=addContribution, &
                                     ans=ans, dim1=dim1, dim2=dim2, &
                                     dim3=dim3, timeIndx=timeIndx0)

  END SELECT

END SELECT
END PROCEDURE GetInterpolation_4a

!----------------------------------------------------------------------------
!                                                           GetInterpolation_
!----------------------------------------------------------------------------

MODULE PROCEDURE GetInterpolation_4b
SELECT CASE (val%vartype)
CASE (TypeFEVariableOpt%constant)
  CALL FEVariableGetInterpolation_(obj=val, rank=TypeFEVariableMatrix, &
                                   vartype=TypeFEVariableConstant, &
                                   N=obj%N, nns=obj%nns, &
                                   spaceIndx=spaceIndx, &
                                   timeIndx=timeIndx, &
                                   scale=scale, &
                                   addContribution=addContribution, &
                                   ans=ans, nrow=nrow, ncol=ncol)

CASE (TypeFEVariableOpt%space)
  CALL FEVariableGetInterpolation_(obj=val, rank=TypeFEVariableMatrix, &
                                   vartype=TypeFEVariableSpace, &
                                   N=obj%N, nns=obj%nns, &
                                   spaceIndx=spaceIndx, &
                                   timeIndx=timeIndx, &
                                   scale=scale, &
                                   addContribution=addContribution, &
                                   ans=ans, nrow=nrow, ncol=ncol)

CASE (TypeFEVariableOpt%spacetime)
  SELECT TYPE (obj); TYPE IS (STElemShapeData_)
    CALL FEVariableGetInterpolation_(obj=val, rank=TypeFEVariableMatrix, &
                                     vartype=TypeFEVariableSpaceTime, &
                                     N=obj%N, nns=obj%nns, &
                                     spaceIndx=spaceIndx, &
                                     timeIndx=timeIndx, &
                                     T=obj%T, nnt=obj%nnt, &
                                     scale=scale, &
                                     addContribution=addContribution, &
                                     ans=ans, nrow=nrow, ncol=ncol)

  END SELECT

END SELECT
END PROCEDURE GetInterpolation_4b

!----------------------------------------------------------------------------
!                                                           getinterpolation
!----------------------------------------------------------------------------

MODULE PROCEDURE GetInterpolation5
INTEGER(I4B) :: dim1, dim2, dim3, dim4
REAL(DFP), PARAMETER :: one = 1.0_DFP
LOGICAL(LGT), PARAMETER :: no = .FALSE.

dim1 = FEVariableSIZE(val, 1)
dim2 = FEVariableSIZE(val, 2)
dim3 = obj(1)%nips
dim4 = SIZE(obj)

CALL Reallocate(ans, dim1, dim2, dim3, dim4)
CALL GetInterpolation_(obj=obj, ans=ans, val=val, dim1=dim1, dim2=dim2, &
                       dim3=dim3, dim4=dim4, scale=one, addContribution=no)
END PROCEDURE GetInterpolation5

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE GetInterpolation_5
REAL(DFP), PARAMETER :: one = 1.0_DFP
LOGICAL(LGT), PARAMETER :: no = .FALSE.

CALL GetInterpolation_(obj=obj, ans=ans, val=val, dim1=dim1, dim2=dim2, &
                       dim3=dim3, dim4=dim4, scale=one, addContribution=no)
END PROCEDURE GetInterpolation_5

!----------------------------------------------------------------------------
!                                                         GetInterpolation_5a
!----------------------------------------------------------------------------

MODULE PROCEDURE GetInterpolation_5a
INTEGER(I4B) :: ipt

dim1 = 0
dim2 = 0
dim3 = 0
dim4 = SIZE(obj)
DO ipt = 1, dim4
  CALL GetInterpolation_(obj=obj(ipt), ans=ans(:, :, :, ipt), &
                         val=val, dim1=dim1, dim2=dim2, dim3=dim3, &
                         scale=scale, addContribution=addContribution, &
                         timeIndx=ipt)
END DO
END PROCEDURE GetInterpolation_5a

!----------------------------------------------------------------------------
!                                                      interpolationOfVector
!----------------------------------------------------------------------------

MODULE PROCEDURE Interpolation1
CALL GetInterpolation(obj=obj, val=val, ans=ans)
END PROCEDURE Interpolation1

!----------------------------------------------------------------------------
!                                                            STinterpolation
!----------------------------------------------------------------------------

MODULE PROCEDURE STInterpolation1
CALL GetInterpolation(obj=obj, val=val, ans=ans)
END PROCEDURE STInterpolation1

END SUBMODULE Methods
