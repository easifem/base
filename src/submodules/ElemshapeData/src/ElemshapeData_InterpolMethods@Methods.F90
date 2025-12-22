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

SUBMODULE(ElemshapeData_InterpolMethods) Methods
USE BaseType, ONLY: TypeFEVariableOpt
USE FEVariable_Method, ONLY: FEVariableGetInterpolation_ => GetInterpolation_,&
                             FEVariableInitiate => Initiate, &
                             FEVariableGetRank => GetRank, &
                             FEVariableGetTotalShape => GetTotalShape, &
                             FEVariableSize => Size

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                           GetInterpolation
!----------------------------------------------------------------------------

MODULE PROCEDURE GetInterpolation1
INTEGER(I4B) :: s(TypeFEVariableOpt%maxRank), totalShape, myrank, mylen

IF (ans%isInit) THEN
  CALL GetInterpolation_(obj=obj, ans=ans, val=val)
ELSE

  myrank = FEVariableGetRank(val)
  totalShape = 0

  SELECT CASE (myrank)
  CASE (TypeFEVariableOpt%scalar)
    totalShape = 1
    s(1) = obj%nips
    mylen = s(1)

  CASE (TypeFEVariableOpt%vector)
    totalShape = 2
    s(1) = FEVariableSize(val, 1)
    s(2) = obj%nips
    mylen = s(1) * s(2)

  CASE (TypeFEVariableOpt%matrix)
    totalShape = 3
    s(1) = FEVariableSize(val, 1)
    s(2) = FEVariableSize(val, 2)
    s(3) = obj%nips
    mylen = s(1) * s(2) * s(3)

  END SELECT

  CALL FEVariableInitiate(obj=ans, &
                          s=s(1:totalShape), &
                          defineon=TypeFEVariableOpt%quadrature, &
                          vartype=TypeFEVariableOpt%space, &
                          rank=FEVariableGetRank(val), &
                          len=mylen)

  CALL GetInterpolation_(obj=obj, ans=ans, val=val)
END IF
END PROCEDURE GetInterpolation1

!----------------------------------------------------------------------------
!                                                           GetInterpolation_
!----------------------------------------------------------------------------

MODULE PROCEDURE GetInterpolation_1
REAL(DFP), PARAMETER :: one = 1.0_DFP
LOGICAL, PARAMETER :: no = .FALSE.

CALL FEVariableGetInterpolation_(obj=val, ans=ans, N=obj%N, nns=obj%nns, &
                                 nips=obj%nips, scale=one, addContribution=no)
END PROCEDURE GetInterpolation_1

!----------------------------------------------------------------------------
!                                                           GetInterpolation_
!----------------------------------------------------------------------------

MODULE PROCEDURE GetInterpolation_1a
INTEGER(I4B), PARAMETER :: timeIndx = 1

SELECT TYPE (obj)
TYPE IS (ElemShapeData_)
  CALL FEVariableGetInterpolation_(obj=val, ans=ans, N=obj%N, nns=obj%nns, &
                                   nips=obj%nips, scale=scale, &
                                   addContribution=addContribution)
CLASS IS (STElemShapeData_)
  CALL FEVariableGetInterpolation_(obj=val, N=obj%N, nns=obj%nns, &
                                   nips=obj%nips, T=obj%T, nnt=obj%nnt, &
                                   scale=scale, &
                                   addContribution=addContribution, &
                                   timeIndx=timeIndx, ans=ans)
END SELECT
END PROCEDURE GetInterpolation_1a

!----------------------------------------------------------------------------
!                                                            GetInterpolation
!----------------------------------------------------------------------------

MODULE PROCEDURE GetInterpolation2
INTEGER(I4B) :: s(TypeFEVariableOpt%maxRank), totalShape, myrank, mylen, &
                nipt

IF (ans%isInit) THEN
  CALL GetInterpolation_(obj=obj, ans=ans, val=val)
ELSE

  myrank = FEVariableGetRank(val)
  totalShape = 0
  nipt = SIZE(obj)

  SELECT CASE (myrank)
  CASE (TypeFEVariableOpt%scalar)

    totalShape = 2
    s(1) = obj(1)%nips
    s(2) = nipt
    mylen = s(1) * s(2)

  CASE (TypeFEVariableOpt%vector)
    totalShape = 3
    s(1) = FEVariableSize(val, 1)
    s(2) = obj(1)%nips
    s(3) = nipt
    mylen = s(1) * s(2) * s(3)

  CASE (TypeFEVariableOpt%matrix)
    totalShape = 4
    s(1) = FEVariableSize(val, 1)
    s(2) = FEVariableSize(val, 2)
    s(3) = obj(1)%nips
    s(4) = nipt
    mylen = s(1) * s(2) * s(3) * s(4)

  END SELECT

  CALL FEVariableInitiate(obj=ans, &
                          s=s(1:totalShape), &
                          defineon=TypeFEVariableOpt%quadrature, &
                          vartype=TypeFEVariableOpt%spacetime, &
                          rank=FEVariableGetRank(val), &
                          len=mylen)

  CALL GetInterpolation_(obj=obj, ans=ans, val=val)
END IF
END PROCEDURE GetInterpolation2

!----------------------------------------------------------------------------
!                                                         GetInterpolation_
!----------------------------------------------------------------------------

MODULE PROCEDURE GetInterpolation_2
REAL(DFP), PARAMETER :: one = 1.0_DFP
LOGICAL, PARAMETER :: no = .FALSE.

CALL GetInterpolation_(obj=obj, ans=ans, val=val, scale=one, &
                       addContribution=no)
END PROCEDURE GetInterpolation_2

!----------------------------------------------------------------------------
!                                                          GetInterpolation_
!----------------------------------------------------------------------------

MODULE PROCEDURE GetInterpolation_2a
INTEGER(I4B) :: aa, nipt

nipt = SIZE(obj)

DO aa = 1, nipt
  CALL FEVariableGetInterpolation_(obj=val, N=obj(aa)%N, nns=obj(aa)%nns, &
                                   nips=obj(aa)%nips, T=obj(aa)%T, &
                                   nnt=obj(aa)%nnt, scale=scale, &
                                   addContribution=addContribution, &
                                   timeIndx=aa, ans=ans)
END DO
END PROCEDURE GetInterpolation_2a

!----------------------------------------------------------------------------
!                                                      interpolationOfVector
!----------------------------------------------------------------------------

MODULE PROCEDURE Interpolation1
CALL GetInterpolation(obj=obj, val=val, ans=ans)
END PROCEDURE Interpolation1

END SUBMODULE Methods
